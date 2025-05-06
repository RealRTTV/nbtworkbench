use std::alloc::{alloc, Layout};
use std::array;
use std::fmt::{Display, Formatter};
use std::hint::likely;
use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut};
use std::slice::{Iter, IterMut};

use compact_str::{format_compact, CompactString};
use zune_inflate::{DeflateDecoder, DeflateOptions};

use crate::assets::{ZOffset, BASE_Z, CHUNK_GHOST_UV, CHUNK_UV, CONNECTION_UV, HEADER_SIZE, JUST_OVERLAPPING_BASE_TEXT_Z, JUST_OVERLAPPING_BOOKMARK_Z, LINE_NUMBER_CONNECTOR_Z, LINE_NUMBER_SEPARATOR_UV, REGION_GRID_UV, REGION_UV};
use crate::elements::{NbtCompound, NbtElement};
use crate::elements::nbt_parse_result::NbtParseResult;
use crate::render::{RenderContext, TextColor, VertexBufferBuilder};
use crate::serialization::{PrettyFormatter, UncheckedBufWriter};
use crate::util::StrExt;
use crate::workbench::{FileFormat, MarkedLines};

#[repr(C)]
pub struct NbtRegion {
	pub chunks: Box<[NbtElement; 32 * 32]>,
	height: u32,
	true_height: u32,
	max_depth: u32,
	loaded_chunks: u16,
	flags: u8,
}

impl NbtRegion {
	pub fn matches(&self, other: &Self) -> bool {
		for (a, b) in self.chunks.iter().zip(other.chunks.iter()) {
			if !a.matches(b) {
				return false
			}
		}
		true
	}
}

impl PartialEq for NbtRegion {
	fn eq(&self, other: &Self) -> bool {
		self.chunks.eq(&other.chunks)
	}
}

impl Clone for NbtRegion {
	#[allow(clippy::cast_ptr_alignment)]
	fn clone(&self) -> Self {
		unsafe {
			let box_ptr = alloc(Layout::new::<[NbtElement; 32 * 32]>()).cast::<[NbtElement; 32 * 32]>();
			let chunks_ptr = alloc(Layout::array::<NbtElement>(32 * 32).unwrap_unchecked()).cast::<NbtElement>();
			for n in 0..1024 {
				chunks_ptr.add(n).write(self.chunks.get_unchecked(n).clone());
			}
			box_ptr.write(chunks_ptr.cast::<[NbtElement; 32 * 32]>().read());

			Self {
				chunks: Box::from_raw(box_ptr),
				height: self.height,
				true_height: self.true_height,
				max_depth: self.max_depth,
				loaded_chunks: self.loaded_chunks,
				flags: self.flags,
			}
		}
	}
}

impl Default for NbtRegion {
	fn default() -> Self {
		Self {
			chunks: Box::new(array::from_fn(|pos| NbtElement::Chunk(NbtChunk::unloaded_from_pos(pos)))),
			height: 1024 + 1,
			true_height: 1024 + 1,
			flags: 0b00,
			loaded_chunks: 0,
			max_depth: 0,
		}
	}
}

impl NbtRegion {
	pub const ID: u8 = 128;
	pub const CHUNK_BANDWIDTH: usize = 32;

	#[must_use]
	pub fn new() -> Self { Self::default() }

	#[must_use]
	pub fn from_be_bytes(bytes: &[u8]) -> NbtParseResult<Self> {
		use super::nbt_parse_result::*;
		
		fn parse(raw: u32, bytes: &[u8]) -> NbtParseResult<(FileFormat, NbtParseResult<NbtCompound>)> {
			if raw < 512 { return ok((FileFormat::Zlib, err("Offset was within header"))) }

			let len = (raw as usize & 0xFF) * 4096;
			let offset = ((raw >> 8) - 2) as usize * 4096;
			if bytes.len() < offset + len { return err("Offset goes outside bytes") }
			let data = &bytes[offset..(offset + len)];

			if let &[a, b, c, d, compression, ref data @ ..] = data {
				let chunk_len = from_opt((u32::from_be_bytes([a, b, c, d]) as usize).checked_sub(1), "Chunk was inside header.")?;
				if data.len() < chunk_len { return err("Offset is invalid") }
				let data = &data[..chunk_len];
				let (compression, element) = match compression {
					1 => (
						FileFormat::Gzip,
						NbtElement::from_be_file(
							&from_result(DeflateDecoder::new_with_options(data, DeflateOptions::default().set_confirm_checksum(false)).decode_gzip())?,
						)?,
					),
					2 => (
						FileFormat::Zlib,
						NbtElement::from_be_file(
							&from_result(DeflateDecoder::new_with_options(data, DeflateOptions::default().set_confirm_checksum(false)).decode_zlib())?,
						)?,
					),
					3 => (FileFormat::Nbt, NbtElement::from_be_file(data)?),
					4 => (FileFormat::Lz4, NbtElement::from_be_file(&from_result(lz4_flex::decompress(data, data.len()))?)?),
					_ => return err("Unknown compression format"),
				};
				if let Some(element) = element.into_compound() {
					return ok((compression, ok(element)));
				}
			}
			err("Invalid chunk data")
		}

		if bytes.len() < 8192 { return err("header didn't exist") }

		#[cfg(not(target_arch = "wasm32"))]
		return std::thread::scope(|s| {
			let mut region = Self::new();

			let (&offsets, bytes) = from_opt(bytes.split_first_chunk::<4096>(), "header didn't exist")?;
			let (&timestamps, bytes) = from_opt(bytes.split_first_chunk::<4096>(), "header didn't exist")?;
			let mut threads = Vec::with_capacity(1024);


			for (&offset, &timestamp) in offsets
				.array_chunks::<4>()
				.zip(timestamps.array_chunks::<4>())
			{
				let timestamp = u32::from_be_bytes(timestamp);
				let offset = u32::from_be_bytes(offset);
				threads.push((timestamp, s.spawn(move || parse(offset, bytes))));
			}

			let mut pos = 0;
			for (timestamp, thread) in threads {
				let (format, element) = from_opt(thread.join().ok(), "Thread panicked")??;
				let element = element?;
				unsafe { region.replace_overwrite(NbtElement::Chunk(NbtChunk::from_compound(element, ((pos >> 5) as u8 & 31, pos as u8 & 31), format, timestamp)), ); }
				pos += 1;
			}

			region.recache();

			ok(region)
		});

		#[cfg(target_arch = "wasm32")]
		return {
			let mut region = Self::new();

			let (&offsets, bytes) = bytes.split_first_chunk::<4096>()?;
			let (&timestamps, bytes) = bytes.split_first_chunk::<4096>()?;
			let mut threads = Vec::with_capacity(1024);


			for (&offset, &timestamp) in offsets
				.array_chunks::<4>()
				.zip(timestamps.array_chunks::<4>())
			{
				let timestamp = u32::from_be_bytes(timestamp);
				let offset = u32::from_be_bytes(offset);
				threads.push((timestamp, parse(offset, bytes)));
			}


			for (pos, (timestamp, thread)) in threads.into_iter().enumerate() {
				let (format, element) = thread?;
				let element = element?;
				unsafe { region.replace_overwrite(NbtElement::Chunk(NbtChunk::from_compound(element, ((pos >> 5) as u8 & 31, pos as u8 & 31), format, timestamp)), ); }
			}

			ok(region)
		};
	}
	pub fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) {
		unsafe {
			std::thread::scope(move |s| {
				let mut chunks = Vec::with_capacity(1024);
				for chunk in self.chunks.iter() {
					let chunk = chunk.as_chunk_unchecked();
					chunks.push(s.spawn(move || {
						if chunk.is_unloaded() {
							(vec![], 0)
						} else {
							let chunk = &(chunk as *const NbtChunk as *const NbtElement)
								.cast::<ManuallyDrop<NbtChunk>>()
								.read();
							let mut writer = UncheckedBufWriter::new();
							chunk.to_be_bytes(&mut writer);
							(writer.finish(), chunk.last_modified)
						}
					}));
				}
				let mut o = 2_u32;
				let mut offsets = [0; 1024];
				let mut timestamps = [0; 1024];
				let mut new_chunks = Vec::with_capacity(chunks.len());
				for (chunk, (offset, timestamp)) in chunks
					.into_iter()
					.zip(offsets.iter_mut().zip(timestamps.iter_mut()))
				{
					let Ok((chunk, last_modified)) = chunk.join() else {
						return;
					};
					let sectors = (chunk.len() / 4096) as u32;
					if sectors > 0 {
						*offset = (o.to_be() >> 8) | (sectors << 24);
						o += sectors;
						*timestamp = last_modified;
						new_chunks.push(chunk);
					} else {
						*offset = 0;
						*timestamp = 0;
					}
				}
				writer.write(&core::mem::transmute::<_, [u8; 4096]>(offsets));
				writer.write(&core::mem::transmute::<_, [u8; 4096]>(timestamps));
				for chunk in new_chunks {
					writer.write(&chunk);
				}
			});
		}
	}

	pub fn increment(&mut self, amount: usize, true_amount: usize) {
		self.height = self.height.wrapping_add(amount as u32);
		self.true_height = self.true_height.wrapping_add(true_amount as u32);
	}

	pub fn decrement(&mut self, amount: usize, true_amount: usize) {
		self.height = self.height.wrapping_sub(amount as u32);
		self.true_height = self.true_height.wrapping_sub(true_amount as u32);
	}

	#[must_use]
	pub fn height(&self) -> usize {
		if self.is_open() {
			self.height as usize
		} else {
			1
		}
	}

	#[must_use]
	pub const fn true_height(&self) -> usize { self.true_height as usize }

	pub fn toggle(&mut self) {
		let open = !self.is_open() && !self.is_empty();
		self.set_open(open);
		if !open && !self.is_empty() {
			self.shut();
		}
	}

	#[must_use]
	pub fn is_open(&self) -> bool { (self.flags & 0b1) > 0 }

	pub fn set_open(&mut self, open: bool) { self.flags = (self.flags & !0b1) | open as u8 }

	#[must_use]
	pub fn is_grid_layout(&self) -> bool { (self.flags & 0b10) > 0 }

	pub fn toggle_grid_layout(&mut self, bookmarks: &mut MarkedLines) {
		self.flags ^= 0b10;
		if self.is_grid_layout() {
			self.height = 32 + 1;
			// one for the region + one for the chunk head
			let mut true_line_number = 2_usize;
			for (idx, chunk) in self.children_mut().enumerate() {
				chunk.shut();
				// skip the head because that shouldn't be hidden
				for bookmark in &mut bookmarks[true_line_number + 1..=true_line_number + chunk.true_height()] {
					*bookmark = bookmark.hidden(idx + 1);
				}
				true_line_number += chunk.true_height();
			}
		} else {
			self.height = 1024 + 1;
		}
	}

	#[must_use]
	pub fn len(&self) -> usize { 1024 }

	#[must_use]
	pub fn is_empty(&self) -> bool { false }

	pub fn insert(&mut self, idx: usize, value: NbtElement) -> Result<Option<NbtElement>, NbtElement> {
		self.replace(idx, value)
	}

	pub fn replace(&mut self, idx: usize, mut value: NbtElement) -> Result<Option<NbtElement>, NbtElement> {
		let (height, true_height) = (value.height(), value.true_height());
		let Some(chunk) = value.as_chunk_mut() else { return Err(value) };
		if let Some(old) = self.chunks.get(idx).and_then(NbtElement::as_chunk) {
			self.decrement(old.height(), old.true_height());
		}
		chunk.set_pos(idx);
		let result = match self.chunks.get_mut(idx) {
			Some(slot) => {
				core::mem::replace(slot, value)
			},
			None => return Err(value)
		};
		self.increment(height, true_height);
		Ok(Some(result))
	}

	/// # Errors
	///
	/// * `NbtElement` is not of `NbtChunk`
	///
	/// * Index is outside the range of `0..=1023`
	unsafe fn replace_overwrite(&mut self, value: NbtElement) {
		let pos = value.as_chunk_unchecked().pos();
		self.increment(value.height(), value.true_height());
		let old = core::mem::replace(&mut self.chunks[pos], value);
		self.decrement(old.height(), old.true_height());
	}

	#[must_use]
	pub fn replace_with_empty(&mut self, pos: usize) -> NbtElement {
		let removed = unsafe {
			core::ptr::replace(
				core::ptr::addr_of_mut!(self.chunks[pos]),
				NbtElement::Chunk(NbtChunk::unloaded_from_pos(pos)),
			)
		};
		self.increment(1, 1);
		self.decrement(removed.height(), removed.true_height());
		removed
	}

	pub fn swap(&mut self, a: usize, b: usize) {
		if a >= self.chunks.len() || b >= self.chunks.len() { return; }
		let a_value = self.replace_with_empty(a);
		let b_value = self.replace_with_empty(b);
		let _ = self.replace(a, b_value);
		let _ = self.replace(b, a_value);
	}

	#[must_use]
	pub fn get(&self, idx: usize) -> Option<&NbtElement> {
		self.chunks.get(idx)
	}

	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<&mut NbtElement> {
		self.chunks.get_mut(idx)
	}

	#[must_use]
	pub fn value(&self) -> CompactString {
		format_compact!(
			"{} chunk{}",
			self.loaded_chunks,
			if self.loaded_chunks == 1 { "" } else { "s" }
		)
	}

	#[must_use]
	pub fn loaded_chunks(&self) -> usize {
		self.loaded_chunks as usize
	}

	pub fn on_root_style_change(&mut self, bookmarks: &mut MarkedLines) {
		self.toggle_grid_layout(bookmarks);
	}

	#[allow(clippy::too_many_lines)]
	pub fn render_root(&self, builder: &mut VertexBufferBuilder, str: &str, ctx: &mut RenderContext) {
		use std::fmt::Write as _;

		builder.draw_texture_z(
			ctx.pos() - (20, 2),
			LINE_NUMBER_CONNECTOR_Z,
			LINE_NUMBER_SEPARATOR_UV,
			(2, 2),
		);

		let mut remaining_scroll = builder.scroll() / 16;
		'head: {
			if remaining_scroll > 0 {
				remaining_scroll -= 1;
				ctx.skip_line_numbers(1);
				break 'head;
			}

			let pos = ctx.pos();

			// not used on the grid layout
			if !self.is_grid_layout() {
				ctx.line_number();
			} else {
				ctx.skip_line_numbers(1);
			}
			// fun hack for connection
			self.render_icon(pos, BASE_Z, builder);
			builder.draw_texture(pos - (16, 0), CONNECTION_UV, (16, 9));
			if !self.is_empty() {
				ctx.draw_toggle(pos - (16, 0), self.is_open(), builder);
			}
			ctx.render_errors(pos, builder);
			if ctx.forbid(pos) {
				builder.settings(pos + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "{str}: [{}]", self.value());
			}

			ctx.offset_pos(0, 16);
		}

		ctx.offset_pos(16, 0);

		if self.is_open() {
			if self.is_grid_layout() {
				let initial_x_offset = ctx.pos().x;
				for z in 0..32 {
					if ctx.pos().y > builder.window_height() {
						break;
					}

					if remaining_scroll >= 1 {
						remaining_scroll -= 1;
						for x in 0..32 {
							ctx.line_number();
							ctx.skip_line_numbers(self.chunks[z * 32 + x].true_height() - 1);
						}
						continue;
					}

					if remaining_scroll == 0 {
						builder.draw_texture(
							ctx.pos() - (16, 0),
							CONNECTION_UV,
							(
								16,
								(z != 32 - 1) as usize * 7 + 9,
							),
						);
					}

					for x in 0..32 {
						let chunk = self.chunks[z * 32 + x].as_chunk().expect("All region children are chunks");

						ctx.line_number();
						ctx.skip_line_numbers(chunk.true_height() - 1);

						chunk.render_icon(ctx.pos(), JUST_OVERLAPPING_BOOKMARK_Z, builder);

						if ctx.mouse_pos().x > ctx.left_margin() && ctx.mouse_pos().y > HEADER_SIZE {
							let mx = ((ctx.mouse_pos().x - ctx.left_margin()) & !15) + ctx.left_margin();
							let my = ((ctx.mouse_pos().y - HEADER_SIZE) & !15) + HEADER_SIZE;
							if ctx.pos() == (mx, my) {
								let text = chunk.value();
								builder.color = TextColor::White.to_raw();
								builder.draw_tooltip(&[&text], ctx.pos(), false);
							}
						}

						let pos = ctx.pos();
						ctx.draw_held_entry_grid_chunk(pos, builder, |x, y| pos == (x, y) || pos == (x, y - 8), |x| self.can_insert(x));


						ctx.offset_pos(16, 0);
					}

					ctx.offset_pos(0, 16);
					let x = ctx.pos().x;
					ctx.offset_pos(initial_x_offset as isize - x as isize, 0);
				}
			} else {
				for (idx, value) in self.children().enumerate() {
					if ctx.pos().y > builder.window_height() {
						break;
					}

					let height = value.height();
					if remaining_scroll >= height {
						remaining_scroll -= height;
						ctx.skip_line_numbers(value.true_height());
						continue;
					}

					if remaining_scroll == 0 {
						builder.draw_texture(
							ctx.pos() - (16, 0),
							CONNECTION_UV,
							(
								16,
								(idx != self.len() - 1) as usize * 7 + 9,
							),
						);
					}

					let pos = ctx.pos();
					ctx.draw_held_entry_chunk(pos, builder, |x, y| pos == (x, y) || pos == (x, y - 8), |x| self.can_insert(x));

					value.render(
						&mut remaining_scroll,
						builder,
						None,
						idx == self.len() - 1,
						ctx,
					);
				}
			}
		}
	}

	pub fn render_icon(&self, pos: impl Into<(usize, usize)>, z: ZOffset, builder: &mut VertexBufferBuilder) { builder.draw_texture_z(pos, z, if self.is_grid_layout() { REGION_GRID_UV } else { REGION_UV }, (16, 16)); }

	pub fn children(&self) -> Iter<'_, NbtElement> {
		self.chunks.iter()
	}

	pub fn children_mut(&mut self) -> IterMut<'_, NbtElement> {
		self.chunks.iter_mut()
	}

	pub fn shut(&mut self) {
		for element in self.children_mut() {
			if element.is_open() {
				element.shut();
			}
		}
		self.set_open(false);
		self.height = if self.is_grid_layout() { 33 } else { 1025 };
	}

	pub fn expand<'a, 'b>(&'b mut self, #[cfg(not(target_arch = "wasm32"))] scope: &'a std::thread::Scope<'a, 'b>) {
		self.set_open(!self.is_empty());
		if !self.is_grid_layout() {
			self.height = self.true_height;
			let mut iter = self
				.children_mut()
				.array_chunks::<{ Self::CHUNK_BANDWIDTH }>();
			for elements in iter.by_ref() {
				#[cfg(target_arch = "wasm32")]
				for element in elements {
					element.expand();
				}
				#[cfg(not(target_arch = "wasm32"))]
				scope.spawn(|| {
					for element in elements {
						element.expand(scope);
					}
				});
			}
			if let Some(rem) = iter.into_remainder() {
				#[cfg(target_arch = "wasm32")]
				for element in rem {
					element.expand();
				}
				#[cfg(not(target_arch = "wasm32"))]
				scope.spawn(|| {
					for element in rem {
						element.expand(scope);
					}
				});
			}
		}
	}

	#[cfg(target_arch = "wasm32")]
	pub fn expand(&mut self) {
		self.set_open(!self.is_empty());
		if !self.is_grid_layout() {
			self.height = self.true_height;
			for element in self.children_mut() {
				element.expand();
			}
		}
	}

	pub fn recache(&mut self) {
		let mut true_height = 1;
		let mut height = 1;
		let mut loaded_chunks = 0_usize;
		for child in self.children() {
			if child.as_chunk().is_some_and(|chunk| chunk.is_loaded()) {
				loaded_chunks += 1;
			}
			true_height += child.true_height() as u32;
			height += child.height() as u32;
		}
		self.true_height = true_height;
		self.height = height;
		self.loaded_chunks = loaded_chunks as u16;

		let mut max_depth = 0;
		if self.is_open() {
			if self.is_grid_layout() {
				max_depth = 16 + 32 * 16;
				self.height = 33;
			} else {
				for child in self.children() {
					max_depth = usize::max(max_depth, 16 + 4 + child.value_width());
					max_depth = usize::max(max_depth, 16 + child.max_depth());
				}
			}
		}
		self.max_depth = max_depth as u32;
	}

	#[must_use]
	pub const fn max_depth(&self) -> usize { self.max_depth as usize }

	#[must_use]
	pub fn can_insert(&self, value: &NbtElement) -> bool {
		value.id() == NbtChunk::ID
	}
}

impl NbtRegion {
	pub fn pretty_fmt(&self, f: &mut PrettyFormatter) {
		if self.is_empty() {
			f.write_str("Region {}")
		} else {
			f.write_str("Region {\n");
			f.increase();
			let len = self.len();
			for (idx, chunk) in self.children().map(|x| unsafe { x.as_chunk_unchecked() }).enumerate() {
				f.indent();
				chunk.pretty_fmt(f);
				if idx + 1 < len {
					f.write_str(",\n");
				} else {
					f.write_str("\n");
				}
			}
			f.decrease();
			f.indent();
			f.write_str("}");
		}
	}
}

#[repr(C)]
#[allow(clippy::module_name_repetitions)]
pub struct NbtChunk {
	inner: Box<NbtCompound>,
	last_modified: u32,
	// need to restrict this file format to only use GZIP, ZLIB, Uncompressed, and LZ4
	compression: FileFormat,
	pub x: u8,
	pub z: u8,
}

impl NbtChunk {
	pub fn matches(&self, other: &Self) -> bool {
		self.inner.matches(&other.inner)
	}
}

impl PartialEq for NbtChunk {
	fn eq(&self, other: &Self) -> bool {
		(&*self.inner).eq(&*other.inner)
	}
}

impl Clone for NbtChunk {
	#[allow(clippy::cast_ptr_alignment)]
	fn clone(&self) -> Self {
		unsafe {
			let box_ptr = alloc(Layout::new::<NbtCompound>()).cast::<NbtCompound>();
			box_ptr.write(self.inner.deref().clone());
			Self {
				inner: Box::from_raw(box_ptr),
				last_modified: self.last_modified,
				compression: self.compression,
				x: self.x,
				z: self.z,
			}
		}
	}
}

impl NbtChunk {
	pub const ID: u8 = 129;
}

impl NbtChunk {
	#[must_use]
	pub fn from_compound(compound: NbtCompound, pos: (u8, u8), compression: FileFormat, last_modified: u32) -> Self {
		Self {
			x: pos.0,
			z: pos.1,
			inner: Box::new(compound),
			compression,
			last_modified,
		}
	}

	#[must_use]
	pub fn unloaded_from_pos(pos: usize) -> NbtChunk {
		Self::from_compound(NbtCompound::new(), ((pos / 32) as u8, (pos % 32) as u8), FileFormat::Zlib, 0)
	}

	pub fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) {
		// todo, mcc files
		unsafe {
			let encoded = self
				.compression
				.encode(&*(self.inner.as_ref() as *const NbtCompound).cast::<NbtElement>());
			let len = encoded.len() + 1;
			// plus four for the len field writing, and + 1 for the compression
			let pad_len = (4096 - (len + 4) % 4096) % 4096;
			writer.write(&(len as u32).to_be_bytes());
			writer.write(
				&match self.compression {
					FileFormat::Gzip => 1_u8,
					FileFormat::Zlib => 2_u8,
					FileFormat::Nbt => 3_u8,
					FileFormat::Lz4 => 4_u8,
					_ => core::hint::unreachable_unchecked(),
				}
				.to_be_bytes(),
			);
			writer.write(&encoded);
			drop(encoded);
			let pad = vec![0; pad_len];
			writer.write(&pad);
		}
	}

	#[must_use]
	pub fn value(&self) -> CompactString { format_compact!("{}, {}", self.x, self.z) }

	#[must_use]
	pub fn pos(&self) -> usize {
		self.x as usize * 32 + self.z as usize
	}

	pub fn set_pos(&mut self, pos: usize) {
		self.x = (pos / 32) as u8;
		self.z = (pos % 32) as u8;
	}

	#[must_use]
	pub fn is_unloaded(&self) -> bool {
		self.inner.is_empty() && self.last_modified == 0
	}

	#[must_use]
	pub fn is_loaded(&self) -> bool {
		!self.is_unloaded()
	}

	#[allow(clippy::too_many_lines)]
	pub fn render(&self, builder: &mut VertexBufferBuilder, remaining_scroll: &mut usize, tail: bool, ctx: &mut RenderContext) {
		use std::fmt::Write as _;

		let mut y_before = ctx.pos().y;

		'head: {
			if *remaining_scroll > 0 {
				*remaining_scroll -= 1;
				ctx.skip_line_numbers(1);
				break 'head;
			}

			let pos = ctx.pos();

			ctx.line_number();
			self.render_icon(pos, BASE_Z, builder);
			if !self.is_empty() {
				ctx.draw_toggle(pos - (16, 0), self.is_open(), builder);
			}
			ctx.check_for_invalid_key(|key| !key.parse::<usize>().is_ok_and(|x| (0..=31).contains(&x)));
			ctx.check_for_invalid_value(|value| !value.parse::<usize>().is_ok_and(|z| (0..=31).contains(&z)));
			ctx.render_errors(pos, builder);
			if ctx.forbid(pos) {
				builder.settings(pos + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "{}, {}", self.x, self.z);
			}

			ctx.offset_pos(0, 16);
			y_before += 16;
		}

		let x_before = ctx.pos().x - 16;

		if self.is_open() {
			ctx.offset_pos(16, 0);

			{
				let children_contains_forbidden = 'f: {
					let mut y = ctx.pos().y;
					for (_, value) in self.children() {
						if ctx.selected_text_y() == Some(y.saturating_sub(*remaining_scroll * 16)) && ctx.selected_text_y().is_some_and(|y| y >= HEADER_SIZE) {
							break 'f true;
						}
						y += value.height() * 16;
					}
					false
				};
				if children_contains_forbidden {
					let mut y = ctx.pos().y;
					for (name, value) in self.children() {
						ctx.check_for_key_duplicate(|text, _| text == name, false);
						if ctx.selected_text_y() == Some(y.saturating_sub(*remaining_scroll * 16)) && y.saturating_sub(*remaining_scroll * 16) >= HEADER_SIZE && ctx.has_duplicate_key_error() {
							ctx.set_red_line_number(y.saturating_sub(*remaining_scroll * 16), 1);
							ctx.draw_error_underline(
								ctx.pos().x,
								y.saturating_sub(*remaining_scroll * 16),
								builder,
							);
							break;
						}
						y += value.height() * 16;
					}
				}
			}

			for (idx, (key, entry)) in self.children().enumerate() {
				let pos = ctx.pos();
				if pos.y > builder.window_height() {
					break;
				}

				let height = entry.height();
				if *remaining_scroll >= height {
					*remaining_scroll -= height;
					ctx.skip_line_numbers(entry.true_height());
					continue;
				}

				if *remaining_scroll == 0 {
					builder.draw_texture(
						pos - (16, 0),
						CONNECTION_UV,
						(
							16,
							(idx != self.len() - 1) as usize * 7 + 9,
						),
					);
				}
				ctx.check_for_key_duplicate(|text, _| self.inner.entries.has(text) && key != text, false);
				if ctx.has_duplicate_key_error() && Some(pos.y) == ctx.selected_text_y() {
					ctx.set_red_line_number(pos.y, 0);
				}
				entry.render(
					remaining_scroll,
					builder,
					Some(key),
					tail && idx == self.len() - 1,
					ctx,
				);
			}

			if !tail {
				let len = (ctx.pos().y - y_before) / 16;
				for i in 0..len {
					builder.draw_texture((x_before, y_before + i * 16), CONNECTION_UV, (8, 16));
				}
			}

			ctx.offset_pos(-16, 0);
		} else {
			ctx.skip_line_numbers(self.true_height() - 1);
		}
	}

	pub fn render_icon(&self, pos: impl Into<(usize, usize)>, z: ZOffset, builder: &mut VertexBufferBuilder) { builder.draw_texture_z(pos, z, if self.is_unloaded() { CHUNK_GHOST_UV } else { CHUNK_UV }, (16, 16)); }
}

impl Deref for NbtChunk {
	type Target = NbtCompound;

	fn deref(&self) -> &Self::Target { &self.inner }
}

impl DerefMut for NbtChunk {
	fn deref_mut(&mut self) -> &mut Self::Target { &mut self.inner }
}

impl Display for NbtChunk {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}|{}{{", self.x, self.z)?;
		for (idx, (key, value)) in self.children().enumerate() {
			if key.needs_escape() {
				write!(f, "{key:?}")?;
			} else {
				write!(f, "{key}")?;
			}
			write!(f, ":{value}")?;
			if likely(idx < self.len() - 1) {
				write!(f, ",")?;
			}
		}
		write!(f, "}}")
	}
}

#[allow(clippy::missing_fields_in_debug)]
impl NbtChunk {
	pub fn pretty_fmt(&self, f: &mut PrettyFormatter) {
		f.write_str(&format!("{} | {} ", self.x, self.z));
		self.inner.pretty_fmt(f)
	}
}
