use std::alloc::{alloc, Layout};
use std::array;
use std::fmt::{Display, Formatter};
use std::intrinsics::likely;
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::{Deref, DerefMut};
use std::slice::{Iter, IterMut};
#[cfg(not(target_arch = "wasm32"))]
use std::thread::Scope;

use compact_str::{CompactString, format_compact};
use zune_inflate::{DeflateDecoder, DeflateOptions};

use crate::{DropFn, OptionExt, RenderContext, StrExt};
use crate::assets::{BASE_Z, CHUNK_UV, CONNECTION_UV, HEADER_SIZE, JUST_OVERLAPPING_BASE_TEXT_Z, LINE_NUMBER_CONNECTOR_Z, LINE_NUMBER_SEPARATOR_UV, REGION_UV, ZOffset, CHUNK_GHOST_UV};
use crate::color::TextColor;
use crate::elements::compound::NbtCompound;
use crate::elements::element::NbtElement;
use crate::encoder::UncheckedBufWriter;
use crate::formatter::PrettyFormatter;
use crate::tab::FileFormat;
use crate::vertex_buffer_builder::VertexBufferBuilder;

#[repr(C)]
pub struct NbtRegion {
	pub chunks: Box<[NbtElement; 32 * 32]>,
	height: u32,
	true_height: u32,
	max_depth: u32,
	loaded_chunks: u16,
	open: bool,
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

impl Clone for NbtRegion {
	#[allow(clippy::cast_ptr_alignment)]
	#[inline]
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
				open: self.open,
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
			open: false,
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
	pub fn from_be_bytes(bytes: &[u8]) -> Option<Self> {
		fn parse(raw: u32, bytes: &[u8]) -> Option<(FileFormat, Option<NbtCompound>)> {
			if raw < 512 { return Some((FileFormat::Zlib, None)) }

			let len = (raw as usize & 0xFF) * 4096;
			let offset = ((raw >> 8) - 2) as usize * 4096;
			if bytes.len() < offset + len { return None }
			let data = &bytes[offset..(offset + len)];

			if let &[a, b, c, d, compression, ref data @ ..] = data {
				let chunk_len = (u32::from_be_bytes([a, b, c, d]) as usize).checked_sub(1)?;
				if data.len() < chunk_len { return None }
				let data = &data[..chunk_len];
				let (compression, element) = match compression {
					1 => (
						FileFormat::Gzip,
						NbtElement::from_be_file(
							&DeflateDecoder::new_with_options(data, DeflateOptions::default().set_confirm_checksum(false))
								.decode_gzip()
								.ok()?,
						)?,
					),
					2 => (
						FileFormat::Zlib,
						NbtElement::from_be_file(
							&DeflateDecoder::new_with_options(data, DeflateOptions::default().set_confirm_checksum(false))
								.decode_zlib()
								.ok()?,
						)?,
					),
					3 => (FileFormat::Nbt, NbtElement::from_be_file(data)?),
					4 => (FileFormat::Lz4, NbtElement::from_be_file(&lz4_flex::decompress(data, data.len()).ok()?)?),
					_ => return None,
				};
				if let Some(element) = element.into_compound() {
					return Some((compression, Some(element)));
				}
			}
			None
		}

		if bytes.len() < 8192 { return None }

		#[cfg(not(target_arch = "wasm32"))]
		return std::thread::scope(|s| {
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
				threads.push((timestamp, s.spawn(move || parse(offset, bytes))));
			}

			let mut pos = 0;
			for (timestamp, thread) in threads {
				let (format, element) = thread.join().ok()??;
				if let Some(element) = element {
					unsafe {
						region.replace_overwrite(
							NbtElement::Chunk(NbtChunk::from_compound(element, ((pos >> 5) as u8 & 31, pos as u8 & 31), format, timestamp)),
						);
					}
				}
				pos += 1;
			}

			Some(region)
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
				if let Some(element) = element {
					unsafe {
						region.replace_overwrite(
							NbtElement::Chunk(NbtChunk::from_compound(element, ((pos >> 5) as u8 & 31, pos as u8 & 31), format, timestamp)),
						);
					}
				}
			}

			Some(region)
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
				let mut offsets = MaybeUninit::<u32>::uninit_array::<1024>();
				let mut timestamps = MaybeUninit::<u32>::uninit_array::<1024>();
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
						offset.write((o.to_be() >> 8) | (sectors << 24));
						o += sectors;
						timestamp.write(last_modified);
						new_chunks.push(chunk);
					} else {
						offset.write(0);
						timestamp.write(0);
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

	#[inline]
	pub fn increment(&mut self, amount: usize, true_amount: usize) {
		self.height = self.height.wrapping_add(amount as u32);
		self.true_height = self.true_height.wrapping_add(true_amount as u32);
	}

	#[inline]
	pub fn decrement(&mut self, amount: usize, true_amount: usize) {
		self.height = self.height.wrapping_sub(amount as u32);
		self.true_height = self.true_height.wrapping_sub(true_amount as u32);
	}

	#[inline]
	#[must_use]
	pub const fn height(&self) -> usize {
		if self.open {
			self.height as usize
		} else {
			1
		}
	}

	#[inline]
	#[must_use]
	pub const fn true_height(&self) -> usize { self.true_height as usize }

	#[inline]
	pub fn toggle(&mut self) -> Option<()> {
		self.open = !self.open && !self.is_empty();
		if !self.open {
			self.shut();
		}
		Some(())
	}

	#[inline]
	#[must_use]
	pub const fn open(&self) -> bool { self.open }

	#[inline]
	#[must_use]
	pub fn len(&self) -> usize { 1024 }

	#[inline]
	#[must_use]
	pub fn is_empty(&self) -> bool { false }

	#[inline]
	pub fn insert(&mut self, idx: usize, value: NbtElement) -> Result<Option<NbtElement>, NbtElement> {
		let Some(chunk) = value.as_chunk() else { return Err(value) };
		self.increment(chunk.height(), chunk.true_height());
		self.replace(idx, value)
	}

	/// # Errors
	///
	/// * `NbtElement` is not of `NbtChunk`
	///
	/// * Index is outside the range of `NbtRegion`
	#[inline]
	pub fn replace(&mut self, idx: usize, mut value: NbtElement) -> Result<Option<NbtElement>, NbtElement> {
		let Some(chunk) = value.as_chunk_mut() else { return Err(value) };
		if let Some(old) = self.chunks.get(idx).and_then(NbtElement::as_chunk) {
			self.decrement(old.height(), old.true_height());
		}
		chunk.set_pos(idx);
		match self.chunks.get_mut(idx) {
			Some(slot) => Ok(Some(core::mem::replace(slot, value))),
			None => Err(value)
		}
	}

	unsafe fn replace_overwrite(&mut self, value: NbtElement) {
		let pos = value.as_chunk_unchecked().pos();
		self.increment(value.height(), value.true_height());
		let old = core::mem::replace(&mut self.chunks[pos], value);
		self.decrement(old.height(), old.true_height());
	}

	#[inline]
	#[must_use]
	pub fn replace_with_empty(&mut self, pos: usize) -> NbtElement {
		unsafe {
			core::ptr::replace(
				core::ptr::addr_of_mut!(self.chunks[pos]),
				NbtElement::Chunk(NbtChunk::unloaded_from_pos(pos)),
			)
		}
	}

	#[inline]
	pub fn swap(&mut self, a: usize, b: usize) {
		if a >= self.chunks.len() || b >= self.chunks.len() { return; }
		let a_value = self.replace_with_empty(a);
		let b_value = self.replace_with_empty(b);
		let _ = self.replace(a, b_value);
		let _ = self.replace(b, a_value);
	}

	#[inline]
	#[must_use]
	pub fn get(&self, idx: usize) -> Option<&NbtElement> {
		self.chunks.get(idx)
	}

	#[inline]
	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<&mut NbtElement> {
		self.chunks.get_mut(idx)
	}

	#[inline]
	#[must_use]
	pub fn value(&self) -> CompactString {
		format_compact!(
			"{} chunk{}",
			self.loaded_chunks,
			if self.loaded_chunks == 1 { "" } else { "s" }
		)
	}

	#[inline]
	#[allow(clippy::too_many_lines)]
	pub fn render_root(&self, builder: &mut VertexBufferBuilder, str: &str, ctx: &mut RenderContext) {
		use std::fmt::Write;

		let mut remaining_scroll = builder.scroll() / 16;
		'head: {
			if remaining_scroll > 0 {
				remaining_scroll -= 1;
				ctx.skip_line_numbers(1);
				break 'head;
			}

			ctx.line_number();
			// fun hack for connection
			builder.draw_texture_z(
				ctx.pos() - (20, 2),
				LINE_NUMBER_CONNECTOR_Z,
				LINE_NUMBER_SEPARATOR_UV,
				(2, 2),
			);
			Self::render_icon(ctx.pos(), BASE_Z, builder);
			builder.draw_texture(ctx.pos() - (16, 0), CONNECTION_UV, (16, 9));
			if !self.is_empty() {
				ctx.draw_toggle(ctx.pos() - (16, 0), self.open, builder);
			}
			ctx.render_errors(ctx.pos(), builder);
			if ctx.forbid(ctx.pos()) {
				builder.settings(ctx.pos() + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "{str} ");
				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "[{}]", self.value());
			}

			ctx.y_offset += 16;
		}

		ctx.x_offset += 16;

		if self.open {
			for (idx, value) in self.children().enumerate() {
				if ctx.y_offset > builder.window_height() {
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

	#[inline]
	pub fn render_icon(pos: impl Into<(usize, usize)>, z: ZOffset, builder: &mut VertexBufferBuilder) { builder.draw_texture_z(pos, z, REGION_UV, (16, 16)); }

	#[inline]
	pub fn children(&self) -> Iter<'_, NbtElement> {
		self.chunks.iter()
	}

	#[inline]
	pub fn children_mut(&mut self) -> IterMut<'_, NbtElement> {
		self.chunks.iter_mut()
	}

	#[inline]
	pub fn drop(&mut self, mut key: Option<CompactString>, mut element: NbtElement, y: &mut usize, depth: usize, target_depth: usize, mut line_number: usize, indices: &mut Vec<usize>) -> DropFn {
		if *y < 16 {
			return DropFn::Missed(key, element);
		} else {
			*y -= 16;
		}

		if self.open && !self.is_empty() {
			indices.push(0);
			let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
			for (idx, value) in self.children_mut().enumerate() {
				*ptr = idx;
				let heights = (element.height(), element.true_height());
				if *y >= value.height() * 16 - 16
					&& *y < value.height() * 16
					&& depth == target_depth
					&& element.as_chunk().is_some()
				{
					return match self.insert(idx, element) {
						Ok(old) => DropFn::Dropped(
							heights.0,
							heights.1,
							None,
							line_number + heights.1 + 1,
							if old.as_ref().is_some_and(|old| old.as_chunk().is_some_and(|chunk| chunk.is_loaded())) { Some((None, unsafe { old.panic_unchecked("we know this will be Some since it's a region") })) } else { None },
						),
						Err(element) => DropFn::InvalidType(key, element),
					}
				}

				match value.drop(
					key,
					element,
					y,
					depth + 1,
					target_depth,
					line_number,
					indices,
				) {
					x @ DropFn::InvalidType(_, _) => return x,
					DropFn::Missed(k, e) => {
						key = k;
						element = e;
					}
					DropFn::Dropped(increment, true_increment, key, line_number, value) => {
						self.increment(increment, true_increment);
						return DropFn::Dropped(increment, true_increment, key, line_number, value);
					}
				}

				line_number += value.true_height();
			}
			indices.pop();
		}
		DropFn::Missed(key, element)
	}

	#[inline]
	pub fn shut(&mut self) {
		for element in self.children_mut() {
			element.shut();
		}
		self.open = false;
		self.height = self.len() as u32 + 1;
	}

	#[inline]
	#[cfg(not(target_arch = "wasm32"))]
	pub fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		self.open = !self.is_empty();
		self.height = self.true_height;
		let mut iter = self
			.children_mut()
			.array_chunks::<{ Self::CHUNK_BANDWIDTH }>();
		for elements in iter.by_ref() {
			scope.spawn(|| {
				for element in elements {
					element.expand(scope);
				}
			});
		}
		if let Some(rem) = iter.into_remainder() {
			scope.spawn(|| {
				for element in rem {
					element.expand(scope);
				}
			});
		}
	}

	#[inline]
	#[cfg(target_arch = "wasm32")]
	pub fn expand(&mut self) {
		self.open = !self.is_empty();
		self.height = self.true_height;
		for element in self.children_mut() {
			element.expand();
		}
	}

	#[inline]
	pub fn recache(&mut self) {
		let mut max_depth = 0;
		let mut loaded_chunks = 0_usize;
		if self.open() {
			for child in self.children() {
				max_depth = usize::max(max_depth, 16 + 4 + child.value().0.width());
				max_depth = usize::max(max_depth, 16 + child.max_depth());
				if child.as_chunk().is_some_and(|chunk| chunk.is_loaded()) {
					loaded_chunks += 1;
				}
			}
		}
		self.max_depth = max_depth as u32;
		self.loaded_chunks = loaded_chunks as u16;
	}

	#[inline]
	#[must_use]
	pub const fn max_depth(&self) -> usize { self.max_depth as usize }

	#[inline]
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

impl Clone for NbtChunk {
	#[allow(clippy::cast_ptr_alignment)]
	#[inline]
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
			let mut pad = Box::<[u8]>::new_uninit_slice(pad_len);
			pad.as_mut_ptr().write_bytes(0, pad_len);
			writer.write(&pad.assume_init());
		}
	}

	#[inline]
	#[must_use]
	pub fn value(&self) -> CompactString { format_compact!("{}, {}", self.x, self.z) }

	#[inline]
	#[must_use]
	pub fn pos(&self) -> usize {
		self.x as usize * 32 + self.z as usize
	}

	#[inline]
	pub fn set_pos(&mut self, pos: usize) {
		self.x = (pos / 32) as u8;
		self.z = (pos % 32) as u8;
	}

	#[inline]
	#[must_use]
	pub fn is_unloaded(&self) -> bool {
		self.inner.is_empty() && self.last_modified == 0
	}

	#[inline]
	#[must_use]
	pub fn is_loaded(&self) -> bool {
		!self.is_unloaded()
	}

	#[inline]
	#[allow(clippy::too_many_lines)]
	pub fn render(&self, builder: &mut VertexBufferBuilder, remaining_scroll: &mut usize, tail: bool, ctx: &mut RenderContext) {
		use std::fmt::Write;

		let mut y_before = ctx.y_offset;

		'head: {
			if *remaining_scroll > 0 {
				*remaining_scroll -= 1;
				ctx.skip_line_numbers(1);
				break 'head;
			}

			ctx.line_number();
			Self::render_icon(ctx.pos(), self.is_unloaded(), BASE_Z, builder);
			if !self.is_empty() {
				ctx.draw_toggle(ctx.pos() - (16, 0), self.open(), builder);
			}
			ctx.check_for_invalid_key(|key| !key.parse::<usize>().is_ok_and(|x| (0..=31).contains(&x)));
			ctx.check_for_invalid_value(|value| !value.parse::<usize>().is_ok_and(|z| (0..=31).contains(&z)));
			ctx.render_errors(ctx.pos(), builder);
			if ctx.forbid(ctx.pos()) {
				builder.settings(ctx.pos() + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "{}, {}", self.x, self.z);
			}

			ctx.y_offset += 16;
			y_before += 16;
		}

		let x_before = ctx.x_offset - 16;

		if self.open() {
			ctx.x_offset += 16;

			{
				let children_contains_forbidden = 'f: {
					let mut y = ctx.y_offset;
					for (_, value) in self.children() {
						if y.saturating_sub(*remaining_scroll * 16) == ctx.selected_y && ctx.selected_y >= HEADER_SIZE {
							break 'f true;
						}
						y += value.height() * 16;
					}
					false
				};
				if children_contains_forbidden {
					let mut y = ctx.y_offset;
					for (name, value) in self.children() {
						ctx.check_for_key_duplicate(|text, _| text == name, false);
						if y.saturating_sub(*remaining_scroll * 16) != ctx.selected_y && y.saturating_sub(*remaining_scroll * 16) >= HEADER_SIZE && ctx.key_duplicate_error {
							ctx.red_line_numbers[1] = y.saturating_sub(*remaining_scroll * 16);
							ctx.draw_error_underline(
								ctx.x_offset,
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
				if ctx.y_offset > builder.window_height() {
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
						ctx.pos() - (16, 0),
						CONNECTION_UV,
						(
							16,
							(idx != self.len() - 1) as usize * 7 + 9,
						),
					);
				}
				ctx.check_for_key_duplicate(|text, _| self.inner.entries.has(text) && key != text, false);
				if ctx.key_duplicate_error && ctx.y_offset == ctx.selected_y {
					ctx.red_line_numbers[0] = ctx.y_offset;
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
				let len = (ctx.y_offset - y_before) / 16;
				for i in 0..len {
					builder.draw_texture((x_before, y_before + i * 16), CONNECTION_UV, (8, 16));
				}
			}

			ctx.x_offset -= 16;
		} else {
			ctx.skip_line_numbers(self.true_height() - 1);
		}
	}

	#[inline]
	pub fn render_icon(pos: impl Into<(usize, usize)>, use_ghost: bool, z: ZOffset, builder: &mut VertexBufferBuilder) { builder.draw_texture_z(pos, z, if use_ghost { CHUNK_GHOST_UV } else { CHUNK_UV }, (16, 16)); }
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
