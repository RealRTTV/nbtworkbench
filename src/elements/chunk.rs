use std::fmt::{Debug, Formatter};
use std::mem::MaybeUninit;

use zune_inflate::DeflateDecoder;

use crate::assets::{CHUNK_UV, CONNECTION_UV, HEADER_SIZE, LINE_NUMBER_SEPARATOR_UV, REGION_UV};
use crate::elements::compound::NbtCompound;
use crate::elements::element_type::NbtElement;
use crate::elements::list::{ValueIterator, ValueMutIterator};
use crate::tab::FileFormat;
use crate::vertex_buffer_builder::VertexBufferBuilder;
use crate::{DropFn, OptionExt, RenderContext, StrExt};

#[derive(Clone)]
pub struct NbtRegion {
	pub map: Vec<u16>,
	pub chunks: Box<[Option<NbtElement>; 32 * 32]>,
	height: usize,
	true_height: usize,
	open: bool,
}

impl NbtRegion {
	pub const ID: u8 = 128;

	pub fn new() -> Self {
		Self {
			map: Vec::new(),
			chunks: Box::new({
				let mut array = MaybeUninit::uninit_array();
				for entry in &mut array {
					entry.write(None);
				}
				unsafe { MaybeUninit::array_assume_init(array) }
			}),
			height: 1,
			true_height: 1,
			open: false,
		}
	}

	pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
		fn parse(offset: u32, timestamp: u32, bytes: &[u8]) -> Option<(FileFormat, NbtCompound)> {
			if offset < 512 { return None; }

			let len = (offset as usize & 0xFF) * 4096;
			let offset = ((offset >> 8) - 2) as usize * 4096;
			if bytes.len() < offset + len { return None; }
			let data = &bytes[offset..(offset + len)];

			if let &[a, b, c, d, compression, ref data @ ..] = data {
				let chunk_len = u32::from_be_bytes([a, b, c, d]) as usize - 1;
				let data = &data[..chunk_len];
				let (compression, NbtElement::Compound(compound)) = (match compression {
					1 => (FileFormat::Gzip, NbtElement::from_file(&DeflateDecoder::new(data).decode_gzip().ok()?)?),
					2 => (FileFormat::Zlib, NbtElement::from_file(&DeflateDecoder::new(data).decode_zlib().ok()?)?),
					3 => (FileFormat::Nbt, NbtElement::from_file(data)?),
					_ => return None,
				}) else {
					return None
				};
				Some((compression, compound))
			} else {
				None
			}
		}

		if bytes.len() < 8192 {
			return None;
		}

		std::thread::scope(move |s| {
			let mut region = Self::new();

			let (&offsets, bytes) = bytes.split_array_ref::<4096>();
			let (&timestamps, bytes) = bytes.split_array_ref::<4096>();
			let mut threads = Vec::new();

			let mut pos = 0_u16;

			for (&offset, &timestamp) in offsets.array_chunks::<4>().zip(timestamps.array_chunks::<4>()) {
				let timestamp = u32::from_be_bytes(timestamp);
				let offset = u32::from_be_bytes(offset);
				threads.push((pos, timestamp, s.spawn(move || {
					parse(offset, timestamp, bytes)
				})));
				pos += 1;
			}

			unsafe {
				for (pos, timestamp, thread) in threads {
					let (format, compound) = thread.join().ok()??;
					region.insert_unchecked(
						pos as usize,
						NbtElement::Chunk(NbtChunk::from_compound(compound, ((pos >> 5) as u8 & 31, pos as u8 & 31), format, timestamp)),
					);
				}
			}

			Some(region)
		})
	}

	pub fn to_bytes<W: std::io::Write>(&self, writer: &mut W) {
		todo!()
	}

	#[inline]
	pub fn increment(&mut self, amount: usize, true_amount: usize) {
		self.height = self.height.wrapping_add(amount);
		self.true_height = self.true_height.wrapping_add(true_amount);
	}

	#[inline]
	pub fn decrement(&mut self, amount: usize, true_amount: usize) {
		self.height = self.height.wrapping_sub(amount);
		self.true_height = self.true_height.wrapping_sub(true_amount);
	}

	#[inline]
	#[must_use]
	pub const fn height(&self) -> usize {
		if self.open {
			self.height
		} else {
			1
		}
	}

	#[inline]
	#[must_use]
	pub const fn true_height(&self) -> usize {
		self.true_height
	}

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
	pub const fn open(&self) -> bool {
		self.open
	}

	#[inline]
	#[must_use]
	pub fn len(&self) -> usize {
		self.map.len()
	}

	#[inline]
	#[must_use]
	pub fn is_empty(&self) -> bool {
		self.map.is_empty()
	}

	/// # Errors
	///
	/// * `NbtElement` is not of `NbtChunk`
	///
	/// * Index is outside the range of NbtRegion
	#[inline]
	pub fn insert(&mut self, idx: usize, value: NbtElement) -> Result<(), NbtElement> {
		if let NbtElement::Chunk(chunk) = &value {
			let pos = ((chunk.x as u16) << 5) | (chunk.z as u16);
			if idx <= self.map.len() && self.chunks[pos as usize].is_none() {
				self.increment(value.height(), value.true_height());
				self.map.insert(idx, pos);
				self.chunks[self.map[idx] as usize] = Some(value);
				return Ok(())
			}
		}

		Err(value)
	}

	#[inline]
	pub unsafe fn insert_unchecked(&mut self, pos: usize, value: NbtElement) {
		self.increment(value.height(), value.true_height());
		self.map.push(pos as u16);
		self.chunks[pos] = Some(value);
	}

	#[inline]
	#[must_use]
	pub fn remove(&mut self, idx: usize) -> NbtElement {
		unsafe { core::mem::replace(&mut self.chunks[self.map.remove(idx) as usize], None).panic_unchecked("Removal had a valid index") }
	}

	#[inline]
	#[must_use]
	pub fn get(&self, idx: usize) -> Option<&NbtElement> {
		self.map.get(idx).and_then(|&x| self.chunks.get(x as usize)).and_then(Option::as_ref)
	}

	#[inline]
	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<&mut NbtElement> {
		self.map.get(idx).and_then(|&x| self.chunks.get_mut(x as usize)).and_then(Option::as_mut)
	}

	#[inline]
	#[must_use]
	pub fn value(&self) -> String {
		format!("{} chunk{}", self.len(), if self.len() == 1 { "" } else { "s" })
	}

	#[inline]
	pub fn render_root(&self, builder: &mut VertexBufferBuilder, str: &str, ctx: &mut RenderContext) {
		use std::fmt::Write;

		let mut x_offset = 16 + ctx.left_margin;
		let mut y_offset = HEADER_SIZE;
		let mut remaining_scroll = builder.scroll() / 16;
		'head: {
			if remaining_scroll > 0 {
				remaining_scroll -= 1;
				ctx.line_number += 1;
				break 'head;
			}

			ctx.line_number(y_offset, builder);
			// fun hack for connection
			builder.draw_texture_z((builder.text_coords.0 + 4, y_offset - 2), 0.5, LINE_NUMBER_SEPARATOR_UV, (2, 2));
			Self::render_icon(x_offset, y_offset, builder);
			ctx.highlight((x_offset, y_offset), str.width(), builder);
			builder.draw_texture((x_offset - 16, y_offset), CONNECTION_UV, (16, 9));
			if !self.is_empty() {
				ctx.draw_toggle((x_offset - 16, y_offset), self.open, builder);
			}
			if ctx.forbid(x_offset, y_offset, builder) {
				builder.settings(x_offset + 20, y_offset, false);
				let _ = write!(builder, "{} [{}]", str, self.value());
			}

			if ctx.ghost(x_offset + 16, y_offset + 16, builder, |x, y| x == x_offset + 16 && y == y_offset + 8) {
				builder.draw_texture((x_offset, y_offset + 16), CONNECTION_UV, (16, (self.height() != 1) as usize * 7 + 9));
				y_offset += 16;
			}

			if self.height() == 1 && ctx.ghost(x_offset + 16, y_offset + 16, builder, |x, y| x == x_offset + 16 && y == y_offset + 16) {
				builder.draw_texture((x_offset, y_offset + 16), CONNECTION_UV, (16, 9));
				y_offset += 16;
			}

			y_offset += 16;
		}

		x_offset += 16;

		if self.open {
			let shadowing_other = {
				let children_contains_forbidden = 'f: {
					let mut y = y_offset;
					for value in self.children() {
						let value = unsafe { value.as_chunk_unchecked() };
						let x = value.x.to_string();
						let z = value.z.to_string();
						if y.saturating_sub(remaining_scroll * 16) == ctx.forbidden_y && ctx.forbidden_y >= HEADER_SIZE {
							break 'f true;
						}
						y += value.height() * 16;
					}
					false
				};
				if children_contains_forbidden {
					let mut y = y_offset;
					'a: {
						for value in self.children() {
							let value = unsafe { value.as_chunk_unchecked() };
							let x = value.x.to_string();
							let z = value.z.to_string();
							ctx.check_key(|key, value| key.parse::<u8>().ok() == x.parse::<u8>().ok() && value.parse::<u8>().ok() == z.parse::<u8>().ok(), true);
							// first check required so this don't render when it's the only selected
							let y2 = y.saturating_sub(remaining_scroll * 16);
							if y2 != ctx.forbidden_y && y2 >= HEADER_SIZE && ctx.key_invalid {
								ctx.red_line_numbers[1] = y2;
								ctx.draw_forbid_underline_width(x_offset, y2, x.width() + ", ".width() + z.width() ,builder);
								break 'a true;
							}
							y += value.height() * 16;
						}
						false
					}
				} else {
					false
				}
			};

			for (idx, value) in self.children().enumerate() {
				let value = unsafe { value.as_chunk_unchecked() };
				let x = value.x.to_string();
				let z = value.z.to_string();
				if y_offset > builder.window_height() {
					break;
				}

				let height = value.height();
				if remaining_scroll >= height {
					remaining_scroll -= height;
					ctx.line_number += value.true_height();
					continue;
				}

				if ctx.ghost(x_offset, y_offset, builder, |x, y| x_offset == x && y_offset == y) {
					builder.draw_texture((x_offset - 16, y_offset), CONNECTION_UV, (16, 16));
					y_offset += 16;
				}

				let ghost_tail_mod = if let Some((_, x, y)) = ctx.ghost && x == x_offset && y == y_offset + height * 16 - remaining_scroll * 16 - 8 {
					false
				} else {
					true
				};

				if remaining_scroll == 0 {
					builder.draw_texture((x_offset - 16, y_offset), CONNECTION_UV, (16, (idx != self.len() - 1 || !ghost_tail_mod) as usize * 7 + 9));
				}
				let forbidden_y = ctx.forbidden_y;
				ctx.check_key(|key, value| shadowing_other && y_offset == forbidden_y, true);
				if ctx.key_invalid {
					ctx.red_line_numbers[0] = y_offset;
				}
				value.render(builder, &mut x_offset, &mut y_offset, &mut remaining_scroll, idx == self.len() - 1 && ghost_tail_mod, ctx);

				if ctx.ghost(x_offset, y_offset, builder, |x, y| x_offset == x && y_offset - 8 == y) {
					builder.draw_texture((x_offset - 16, y_offset), CONNECTION_UV, (16, (idx != self.len() - 1) as usize * 7 + 9));
					y_offset += 16;
				}
			}
		} else {
			// line_number += self.true_height - 1;
		}
		// x_offset -= 16;
	}

	#[inline]
	pub fn render_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
		builder.draw_texture((x, y), REGION_UV, (16, 16));
	}

	#[inline]
	pub fn children(&self) -> ValueIterator {
		ValueIterator::Region(&self.chunks, self.map.iter())
	}

	#[inline]
	pub fn children_mut(&mut self) -> ValueMutIterator {
		ValueMutIterator::Region(&mut self.chunks, self.map.iter())
	}

	#[inline]
	pub fn drop(&mut self, mut key: Option<Box<str>>, mut element: NbtElement, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> DropFn {
		if *y < 16 && *y >= 8 && depth == target_depth && let NbtElement::Chunk(chunk) = &element {
			let (_x, z) = (chunk.x, chunk.z);
			let before = (self.height(), self.true_height());
			indices.push(0);
			if let Err(element) = self.insert(0, element) {
				return DropFn::InvalidType(key, element);
			}
			self.open = true;
			return DropFn::Dropped(self.height - before.0, self.true_height - before.1, Some(z.to_string().into_boxed_str()));
		} else if self.height() == 1 && *y < 24 && *y >= 16 && depth == target_depth && let NbtElement::Chunk(chunk) = &element {
			let (_x, z) = (chunk.x, chunk.z);
			let before = self.true_height();
			indices.push(self.len());
			if let Err(element) = self.insert(self.len(), element) {
				// indices are never used
				return DropFn::InvalidType(key, element);
			}
			self.open = true;
			return DropFn::Dropped(self.height - 1, self.true_height - before, Some(z.to_string().into_boxed_str()));
		}

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
				if *y < 8 && depth == target_depth && let NbtElement::Chunk(chunk) = &element {
					let (_x, z) = (chunk.x, chunk.z);
					if let Err(element) = self.insert(idx, element) {
						return DropFn::InvalidType(key, element);
					}
					return DropFn::Dropped(heights.0, heights.1, Some(z.to_string().into_boxed_str()));
				} else if *y >= value.height() * 16 - 8 && *y < value.height() * 16 && depth == target_depth && let NbtElement::Chunk(chunk) = &element {
					let (_x, z) = (chunk.x, chunk.z);
					*ptr = idx + 1;
					if let Err(element) = self.insert(idx + 1, element) {
						return DropFn::InvalidType(key, element);
					}
					return DropFn::Dropped(heights.0, heights.1, Some(z.to_string().into_boxed_str()));
				}

				match value.drop(key, element, y, depth + 1, target_depth, indices) {
					x @ DropFn::InvalidType(_, _) => return x,
					DropFn::Missed(k, e) => {
						key = k;
						element = e;
					}
					DropFn::Dropped(increment, true_increment, key) => {
						self.increment(increment, true_increment);
						return DropFn::Dropped(increment, true_increment, key);
					}
				}
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
		self.height = self.len() + 1;
	}
}

impl Debug for NbtRegion {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		let mut r#struct = f.debug_struct("Region");
		for child in self.children() {
			if let NbtElement::Chunk(chunk) = child {
				r#struct.field(&format!("x: {:02}, z: {:02}", chunk.x, chunk.z), chunk);
			} else {
				continue;
			}
		}
		r#struct.finish()
	}
}

#[derive(Clone)]
pub struct NbtChunk {
	pub x: u8,
	pub z: u8,
	pub inner: NbtCompound,
	// need to restrict this file format to only use GZIP, ZLIB and Uncompressed
	compression: FileFormat,
	last_modified: u32, // todo
}

impl NbtChunk {
	pub const ID: u8 = 129;
}

impl NbtChunk {
	pub fn from_compound(compound: NbtCompound, pos: (u8, u8), compression: FileFormat, last_modified: u32) -> Self {
		Self {
			x: pos.0,
			z: pos.1,
			inner: compound,
			compression,
			last_modified,
		}
	}

	pub fn to_bytes<W: std::io::Write>(&self, writer: &mut W) {
		todo!()
	}

	#[inline]
	pub fn increment(&mut self, amount: usize, true_amount: usize) {
		self.inner.increment(amount, true_amount)
	}

	#[inline]
	pub fn decrement(&mut self, amount: usize, true_amount: usize) {
		self.inner.decrement(amount, true_amount)
	}

	#[inline]
	#[must_use]
	pub const fn height(&self) -> usize {
		self.inner.height()
	}

	#[inline]
	#[must_use]
	pub const fn true_height(&self) -> usize {
		self.inner.true_height()
	}

	#[inline]
	pub fn toggle(&mut self) -> Option<()> {
		self.inner.toggle()
	}

	#[inline]
	#[must_use]
	pub const fn open(&self) -> bool {
		self.inner.open()
	}

	#[inline]
	#[must_use]
	pub fn len(&self) -> usize {
		self.inner.len()
	}

	#[inline]
	#[must_use]
	pub fn is_empty(&self) -> bool {
		self.inner.is_empty()
	}

	/// # Errors
	///
	/// * Never
	#[inline]
	pub fn insert_full(&mut self, idx: usize, key: String, value: NbtElement) {
		self.inner.insert_full(idx, key, value)
	}

	#[inline]
	#[must_use]
	pub fn remove_idx(&mut self, idx: usize) -> Option<(Box<str>, NbtElement)> {
		self.inner.remove_idx(idx)
	}

	#[inline]
	#[must_use]
	pub fn get(&self, idx: usize) -> Option<(&str, &NbtElement)> {
		self.inner.get(idx)
	}

	#[inline]
	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<(&str, &mut NbtElement)> {
		self.inner.get_mut(idx)
	}

	#[inline]
	#[must_use]
	pub fn value(&self) -> String {
		format!("{}, {}", self.x, self.z)
	}

	#[inline]
	pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut usize, y_offset: &mut usize, remaining_scroll: &mut usize, tail: bool, ctx: &mut RenderContext) {
		use std::fmt::Write;

		let mut y_before = *y_offset;

		'head: {
			if *remaining_scroll > 0 {
				*remaining_scroll -= 1;
				ctx.line_number += 1;
				break 'head;
			}

			let name = self.value();
			ctx.line_number(*y_offset, builder);
			Self::render_icon(*x_offset, *y_offset, builder);
			ctx.highlight((*x_offset, *y_offset), name.width(), builder);
			if !self.is_empty() {
				ctx.draw_toggle((*x_offset - 16, *y_offset), self.open(), builder);
			}
			if ctx.forbid(*x_offset, *y_offset, builder) {
				builder.settings(*x_offset + 20, *y_offset, false);
				let _ = write!(builder, "{name}");
			}

			if ctx.ghost(*x_offset + 16, *y_offset + 16, builder, |x, y| x == *x_offset + 16 && y == *y_offset + 8) {
				builder.draw_texture((*x_offset, *y_offset + 16), CONNECTION_UV, (16, (self.height() != 1) as usize * 7 + 9));
				if !tail {
					builder.draw_texture((*x_offset - 16, *y_offset + 16), CONNECTION_UV, (8, 16));
				}
				*y_offset += 16;
			} else if self.height() == 1 && ctx.ghost(*x_offset + 16, *y_offset + 16, builder, |x, y| x == *x_offset + 16 && y == *y_offset + 16) {
				builder.draw_texture((*x_offset, *y_offset + 16), CONNECTION_UV, (16, 9));
				if !tail {
					builder.draw_texture((*x_offset - 16, *y_offset + 16), CONNECTION_UV, (8, 16));
				}
				*y_offset += 16;
			}

			*y_offset += 16;
			y_before += 16;
		}

		let x_before = *x_offset - 16;

		if self.open() {
			*x_offset += 16;

			{
				let children_contains_forbidden = 'f: {
					let mut y = *y_offset;
					for (_, value) in self.children() {
						if y.saturating_sub(*remaining_scroll * 16) == ctx.forbidden_y && ctx.forbidden_y >= HEADER_SIZE {
							break 'f true;
						}
						y += value.height() * 16;
					}
					false
				};
				if children_contains_forbidden {
					let mut y = *y_offset;
					for (name, value) in self.children() {
						ctx.check_key(|text, _| text == name.as_ref(), false);
						// first check required so this don't render when it's the only selected
						if y.saturating_sub(*remaining_scroll * 16) != ctx.forbidden_y && y.saturating_sub(*remaining_scroll * 16) >= HEADER_SIZE && ctx.key_invalid {
							ctx.red_line_numbers[1] = y.saturating_sub(*remaining_scroll * 16);
							ctx.draw_forbid_underline(*x_offset, y.saturating_sub(*remaining_scroll * 16), builder);
							break;
						}
						y += value.height() * 16;
					}
				}
			}

			for (idx, (key, entry)) in self.children().enumerate() {
				if *y_offset > builder.window_height() {
					break;
				}

				let height = entry.height();
				if *remaining_scroll >= height {
					*remaining_scroll -= height;
					ctx.line_number += entry.true_height();
					continue;
				}

				if ctx.ghost(*x_offset, *y_offset, builder, |x, y| *x_offset == x && *y_offset == y) {
					builder.draw_texture((*x_offset - 16, *y_offset), CONNECTION_UV, (16, 16));
					*y_offset += 16;
				}

				let ghost_tail_mod = if let Some((_, x, y)) = ctx.ghost && x == *x_offset && y == *y_offset + height * 16 - *remaining_scroll * 16 - 8 {
					false
				} else {
					true
				};

				if *remaining_scroll == 0 {
					builder.draw_texture((*x_offset - 16, *y_offset), CONNECTION_UV, (16, (idx != self.len() - 1 || !ghost_tail_mod) as usize * 7 + 9));
				}
				ctx.check_key(|text, _| self.inner.entries.contains_key(text) && key.as_ref() != text, false);
				if ctx.key_invalid && *y_offset == ctx.forbidden_y {
					ctx.red_line_numbers[0] = *y_offset;
				}
				entry.render(x_offset, y_offset, remaining_scroll, builder, Some(key.as_ref()), tail && idx == self.len() - 1 && ghost_tail_mod, ctx);

				if ctx.ghost(*x_offset, *y_offset, builder, |x, y| *x_offset == x && *y_offset - 8 == y) {
					builder.draw_texture((*x_offset - 16, *y_offset), CONNECTION_UV, (16, (idx != self.len() - 1) as usize * 7 + 9));
					*y_offset += 16;
				}
			}

			if !tail {
				let len = (*y_offset - y_before) / 16;
				for i in 0..len {
					builder.draw_texture((x_before, y_before + i * 16), CONNECTION_UV, (8, 16));
				}
			}

			*x_offset -= 16;
		} else {
			ctx.line_number += self.true_height() - 1;
		}
	}

	#[inline]
	#[must_use]
	pub fn children(&self) -> indexmap::map::Iter<'_, Box<str>, NbtElement> {
		self.inner.children()
	}

	#[inline]
	#[must_use]
	pub fn children_mut(&mut self) -> indexmap::map::IterMut<'_, Box<str>, NbtElement> {
		self.inner.children_mut()
	}

	#[inline]
	pub fn shut(&mut self) {
		self.inner.shut()
	}

	#[inline]
	pub fn render_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
		builder.draw_texture((x, y), CHUNK_UV, (16, 16));
	}
}

impl Debug for NbtChunk {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		Debug::fmt(&self.inner, f)
	}
}
