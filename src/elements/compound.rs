use std::alloc::{alloc, Layout};
use std::cmp::Ordering;
use std::convert::identity;
use std::fmt::{Debug, Display, Formatter, Write};
use std::hash::Hasher;
use std::intrinsics::likely;
use std::ops::Deref;
use std::thread::Scope;

use compact_str::{format_compact, CompactString, ToCompactString};
use fxhash::FxHasher;
use hashbrown::raw::RawTable;

use crate::assets::{BASE_TEXT_Z, BASE_Z, COMPOUND_ROOT_UV, COMPOUND_UV, CONNECTION_UV, HEADER_SIZE, LINE_NUMBER_CONNECTOR_Z, LINE_NUMBER_SEPARATOR_UV};
use crate::decoder::Decoder;
use crate::elements::chunk::NbtChunk;
use crate::elements::element::NbtElement;
use crate::encoder::UncheckedBufWriter;
use crate::{Bookmark, DropFn, OptionExt, RenderContext, StrExt, VertexBufferBuilder};

#[allow(clippy::module_name_repetitions)]
#[repr(C)]
pub struct NbtCompound {
	pub entries: Box<CompoundMap>,
	height: u32,
	true_height: u32,
	max_depth: u32,
	open: bool,
}

impl Clone for NbtCompound {
	#[allow(clippy::cast_ptr_alignment)]
	fn clone(&self) -> Self {
		unsafe {
			let boxx = alloc(Layout::new::<CompoundMap>()).cast::<CompoundMap>();
			boxx.write(self.entries.deref().clone());
			Self {
				entries: Box::from_raw(boxx),
				height: self.height,
				true_height: self.true_height,
				max_depth: self.max_depth,
				open: self.open,
			}
		}
	}
}

impl NbtCompound {
	pub const ID: u8 = 10;
	#[optimize(speed)]
	pub(in crate::elements) fn from_str0(mut s: &str) -> Option<(&str, Self)> {
		s = s.strip_prefix('{')?.trim_start();
		let mut compound = Self::new();
		while !s.starts_with('}') {
			let (key, s2) = s.snbt_string_read()?;
			s = s2.trim_start().strip_prefix(':')?.trim_start();
			let (s2, value) = NbtElement::from_str0(s)?;
			compound.insert_replacing(key, value);
			s = s2.trim_start();
			if let Some(s2) = s.strip_prefix(',') {
				s = s2.trim_start();
			} else {
				break;
			}
		}
		let s = s.strip_prefix('}')?;
		Some((s, compound))
	}

	#[inline]
	pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
		let mut compound = Self::new();
		unsafe {
			decoder.assert_len(1)?;
			let mut current_element = decoder.u8();
			while current_element != 0 {
				decoder.assert_len(2)?;
				let key = decoder.string()?;
				let value = NbtElement::from_bytes(current_element, decoder)?;
				compound.insert_replacing(key, value);
				match decoder.assert_len(1) {
					Some(()) => {}
					None => break, // wow mojang, saving one byte, so cool of you
				};
				current_element = decoder.u8();
			}
			Some(compound)
		}
	}
	pub fn to_bytes(&self, writer: &mut UncheckedBufWriter) {
		for (key, value) in self.children() {
			writer.write(&[value.id()]);
			writer.write_str(key);
			value.to_bytes(writer);
		}
		writer.write(&[0x00]);
	}
}

impl Default for NbtCompound {
	#[inline]
	fn default() -> Self {
		Self {
			height: 1,
			entries: Box::<CompoundMap>::default(),
			open: false,
			true_height: 1,
			max_depth: 0,
		}
	}
}

impl NbtCompound {
	#[inline]
	#[must_use]
	pub fn new() -> Self { Self::default() }

	#[inline]
	pub fn insert(&mut self, idx: usize, mut str: CompactString, value: NbtElement) {
		loop {
			if self.entries.has(&str) {
				str += " - Copy";
			} else {
				self.height += value.height() as u32;
				self.true_height += value.true_height() as u32;
				self.entries.insert_at(str, value, idx);
				return;
			}
		}
	}

	#[inline] // has some unchecked stuff
	pub fn insert_replacing(&mut self, str: CompactString, element: NbtElement) {
		self.true_height += element.true_height() as u32;
		if let Some(element) = self.entries.insert(str, element) {
			self.true_height -= element.true_height() as u32;
		} else {
			self.height += 1;
		}
	}

	#[inline]
	pub fn remove_idx(&mut self, idx: usize) -> Option<(CompactString, NbtElement)> { self.entries.shift_remove_idx(idx) }

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
		self.open = !self.open && !self.entries.is_empty();
		if !self.open {
			self.shut();
		}
		Some(())
	}

	#[inline]
	#[must_use]
	pub const fn open(&self) -> bool { self.open }

	pub fn update_key(&mut self, idx: usize, key: CompactString) -> Option<CompactString> {
		if self.entries.has(key.as_ref()) {
			None
		} else {
			Some(unsafe { self.entries.update_key_idx_unchecked(idx, key) })
		}
	}

	#[inline]
	#[must_use]
	pub fn len(&self) -> usize { self.entries.len() }

	#[inline]
	#[must_use]
	pub fn is_empty(&self) -> bool { self.entries.is_empty() }

	#[inline]
	#[must_use]
	pub fn get(&self, idx: usize) -> Option<(&str, &NbtElement)> { self.entries.get_idx(idx) }

	#[inline]
	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<(&str, &mut NbtElement)> { self.entries.get_idx_mut(idx) }

	#[inline]
	#[must_use]
	pub fn value(&self) -> CompactString {
		format_compact!(
			"{} {}",
			self.len(),
			if self.len() == 1 { "entry" } else { "entries" }
		)
	}

	#[inline]
	#[allow(clippy::too_many_lines)]
	pub fn render_root(&self, builder: &mut VertexBufferBuilder, str: &str, ctx: &mut RenderContext) {
		let mut remaining_scroll = builder.scroll() / 16;
		'head: {
			if remaining_scroll > 0 {
				remaining_scroll -= 1;
				ctx.skip_line_numbers(1);
				break 'head;
			}

			ctx.line_number();
			// fun hack for connection
			// doesn't work on most platforms in the same way, but it adds a little detail nonetheless.
			// the intended behaviour is to connect without any new pixel changes except making the darker toned one tucked in the corner to be slightly lighter
			builder.draw_texture_z(
				ctx.pos() - (20, 2),
				LINE_NUMBER_CONNECTOR_Z,
				LINE_NUMBER_SEPARATOR_UV,
				(2, 2),
			);
			builder.draw_texture(ctx.pos(), COMPOUND_ROOT_UV, (16, 16));
			builder.draw_texture(ctx.pos() - (16, 0), CONNECTION_UV, (16, 9));
			if !self.is_empty() {
				ctx.draw_toggle(ctx.pos() - (16, 0), self.open, builder);
			}
			ctx.render_errors(ctx.pos(), builder);
			if ctx.forbid(ctx.pos()) {
				builder.settings(ctx.pos() + (20, 0), false, BASE_TEXT_Z);
				let _ = write!(builder, "{} [{}]", str, self.value());
			}

			let pos = ctx.pos();
			if ctx.ghost(
				ctx.pos() + (16, 16),
				builder,
				|x, y| pos + (16, 8) == (x, y),
				|id| id != NbtChunk::ID,
			) {
				builder.draw_texture(
					ctx.pos() + (0, 16),
					CONNECTION_UV,
					(16, (self.height() != 1) as usize * 7 + 9),
				);
				ctx.y_offset += 16;
			}

			if self.height() == 1
				&& ctx.ghost(
					ctx.pos() + (16, 16),
					builder,
					|x, y| pos + (16, 16) == (x, y),
					|id| id != NbtChunk::ID,
				) {
				builder.draw_texture(ctx.pos() + (0, 16), CONNECTION_UV, (16, 9));
				ctx.y_offset += 16;
			}

			ctx.y_offset += 16;
		}

		ctx.x_offset += 16;

		if self.open {
			{
				let children_contains_forbidden = 'f: {
					let mut y = ctx.y_offset;
					for (_, value) in self.children() {
						if y.saturating_sub(remaining_scroll * 16) == ctx.selected_y && ctx.selected_y >= HEADER_SIZE {
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
						// first check required so this don't render when it's the only selected
						if y.saturating_sub(remaining_scroll * 16) != ctx.selected_y && y.saturating_sub(remaining_scroll * 16) >= HEADER_SIZE && ctx.key_duplicate_error {
							ctx.red_line_numbers[1] = y.saturating_sub(remaining_scroll * 16);
							ctx.draw_error_underline(
								ctx.x_offset,
								y.saturating_sub(remaining_scroll * 16),
								builder,
							);
							break;
						}
						y += value.height() * 16;
					}
				}
			}

			for (idx, (name, value)) in self.children().enumerate() {
				if ctx.y_offset > builder.window_height() {
					break;
				}

				let height = value.height();
				if remaining_scroll >= height {
					remaining_scroll -= height;
					ctx.skip_line_numbers(value.true_height());
					continue;
				}

				let (gx, gy): (usize, usize) = ctx.pos().into();
				if ctx.ghost(
					ctx.pos(),
					builder,
					|x, y| x == gx && y == gy,
					|id| id != NbtChunk::ID,
				) {
					builder.draw_texture(ctx.pos() - (16, 0), CONNECTION_UV, (16, 16));
					ctx.y_offset += 16;
				}

				let ghost_tail_mod = if let Some((_, x, y, _)) = ctx.ghost
					&& ctx.pos() + (0, height * 16 - remaining_scroll * 16 - 8) == (x, y)
				{
					false
				} else {
					true
				};

				if remaining_scroll == 0 {
					builder.draw_texture(
						ctx.pos() - (16, 0),
						CONNECTION_UV,
						(
							16,
							(idx != self.len() - 1 || !ghost_tail_mod) as usize * 7 + 9,
						),
					);
				}
				ctx.check_for_key_duplicate(|text, _| self.entries.has(text) && name != text, false);
				if ctx.key_duplicate_error && ctx.y_offset == ctx.selected_y {
					ctx.red_line_numbers[0] = ctx.y_offset;
				}
				value.render(
					&mut remaining_scroll,
					builder,
					Some(name),
					idx == self.len() - 1 && ghost_tail_mod,
					ctx,
				);

				let pos = ctx.pos();
				if ctx.ghost(
					ctx.pos(),
					builder,
					|x, y| pos == (x, y + 8),
					|id| id != NbtChunk::ID,
				) {
					builder.draw_texture(
						ctx.pos() - (16, 0),
						CONNECTION_UV,
						(16, (idx != self.len() - 1) as usize * 7 + 9),
					);
					ctx.y_offset += 16;
				}
			}
		}
	}

	#[inline]
	pub fn recache_depth(&mut self) {
		let mut max_depth = 0;
		if self.open() {
			for (key, child) in self.children() {
				max_depth = usize::max(
					max_depth,
					16 + 4 + key.width() + ": ".width() + child.value().0.width(),
				);
				max_depth = usize::max(max_depth, 16 + child.max_depth());
			}
		}
		self.max_depth = max_depth as u32;
	}

	#[inline]
	#[must_use]
	pub const fn max_depth(&self) -> usize { self.max_depth as usize }
}

impl Display for NbtCompound {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{{")?;
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

impl Debug for NbtCompound {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		if self.is_empty() {
			write!(f, "{{}}")
		} else {
			let mut debug = f.debug_struct("");
			for (key, element) in self.children() {
				if key.needs_escape() {
					debug.field(&format!("{key:?}"), element);
				} else {
					debug.field(key, element);
				}
			}
			debug.finish()
		}
	}
}

impl NbtCompound {
	#[inline]
	#[allow(clippy::too_many_lines)]
	pub fn render(&self, builder: &mut VertexBufferBuilder, name: Option<&str>, remaining_scroll: &mut usize, tail: bool, ctx: &mut RenderContext) {
		let mut y_before = ctx.y_offset;

		'head: {
			if *remaining_scroll > 0 {
				*remaining_scroll -= 1;
				ctx.skip_line_numbers(1);
				break 'head;
			}

			ctx.line_number();
			Self::render_icon(ctx.pos(), BASE_Z, builder);
			if !self.is_empty() {
				ctx.draw_toggle(ctx.pos() - (16, 0), self.open, builder);
			}
			ctx.render_errors(ctx.pos(), builder);
			if ctx.forbid(ctx.pos()) {
				builder.settings(ctx.pos() + (20, 0), false, BASE_TEXT_Z);
				let _ = match name {
					Some(x) => write!(builder, "{x}: {}", self.value()),
					None => write!(builder, "{}", self.value()),
				};
			}

			let pos = ctx.pos();
			if ctx.ghost(
				ctx.pos() + (16, 16),
				builder,
				|x, y| pos + (16, 8) == (x, y),
				|id| id != NbtChunk::ID,
			) {
				builder.draw_texture(
					ctx.pos() + (0, 16),
					CONNECTION_UV,
					(16, (self.height() != 1) as usize * 7 + 9),
				);
				if !tail {
					builder.draw_texture(ctx.pos() - (16, 0) + (0, 16), CONNECTION_UV, (8, 16));
				}
				ctx.y_offset += 16;
			} else if self.height() == 1
				&& ctx.ghost(
					ctx.pos() + (16, 16),
					builder,
					|x, y| pos + (16, 16) == (x, y),
					|id| id != NbtChunk::ID,
				) {
				builder.draw_texture(ctx.pos() + (0, 16), CONNECTION_UV, (16, 9));
				if !tail {
					builder.draw_texture(ctx.pos() - (16, 0) + (0, 16), CONNECTION_UV, (8, 16));
				}
				ctx.y_offset += 16;
			}

			ctx.y_offset += 16;
			y_before += 16;
		}

		let x_before = ctx.x_offset - 16;

		if self.open {
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
						// first check required so this don't render when it's the only selected
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

				let pos = ctx.pos();
				if ctx.ghost(
					ctx.pos(),
					builder,
					|x, y| pos == (x, y),
					|id| id != NbtChunk::ID,
				) {
					builder.draw_texture(ctx.pos() - (16, 0), CONNECTION_UV, (16, 16));
					ctx.y_offset += 16;
				}

				let ghost_tail_mod = if let Some((_, x, y, _)) = ctx.ghost
					&& ctx.pos() + (0, height * 16 - *remaining_scroll * 16 - 8) == (x, y)
				{
					false
				} else {
					true
				};

				if *remaining_scroll == 0 {
					builder.draw_texture(
						ctx.pos() - (16, 0),
						CONNECTION_UV,
						(
							16,
							(idx != self.len() - 1 || !ghost_tail_mod) as usize * 7 + 9,
						),
					);
				}
				ctx.check_for_key_duplicate(|text, _| self.entries.has(text) && key != text, false);
				if ctx.key_duplicate_error && ctx.y_offset == ctx.selected_y {
					ctx.red_line_numbers[0] = ctx.y_offset;
				}
				entry.render(
					remaining_scroll,
					builder,
					Some(key),
					tail && idx == self.len() - 1 && ghost_tail_mod,
					ctx,
				);

				let pos = ctx.pos();
				if ctx.ghost(
					ctx.pos(),
					builder,
					|x, y| pos == (x, y + 8),
					|id| id != NbtChunk::ID,
				) {
					builder.draw_texture(
						ctx.pos() - (16, 0),
						CONNECTION_UV,
						(16, (idx != self.len() - 1) as usize * 7 + 9),
					);
					ctx.y_offset += 16;
				}
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
	#[must_use]
	pub fn children(&self) -> CompoundMapIter<'_> { self.entries.iter() }

	#[inline]
	#[must_use]
	pub fn children_mut(&mut self) -> CompoundMapIterMut<'_> { self.entries.iter_mut() }

	#[inline]
	pub fn drop(&mut self, mut key: Option<CompactString>, mut element: NbtElement, y: &mut usize, depth: usize, target_depth: usize, mut line_number: usize, indices: &mut Vec<usize>) -> DropFn {
		if *y < 16 && *y >= 8 && depth == target_depth {
			let before = (self.height(), self.true_height());
			self.open = true;
			self.insert(0, key.unwrap_or(CompactString::new_inline("_")), element);
			indices.push(0);
			return DropFn::Dropped(
				self.height as usize - before.0,
				self.true_height as usize - before.1,
				unsafe {
					Some(
						self.get(0)
							.panic_unchecked("We just added it")
							.0
							.to_compact_string(),
					)
				},
				line_number + 1,
			);
		}

		if self.height() == 1 && *y < 24 && *y >= 16 && depth == target_depth {
			let before = self.true_height();
			self.open = true;
			indices.push(self.len());
			self.insert(
				self.len(),
				key.unwrap_or(CompactString::new_inline("_")),
				element,
			);
			return DropFn::Dropped(
				self.height as usize - 1,
				self.true_height as usize - before,
				unsafe {
					Some(
						self.get(self.len() - 1)
							.panic_unchecked("We just added it")
							.0
							.to_compact_string(),
					)
				},
				line_number + before + 1,
			);
		}

		if *y < 16 {
			return DropFn::Missed(key, element);
		} else {
			*y -= 16;
		}

		if self.open && !self.is_empty() {
			indices.push(0);
			let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
			for (idx, (_, value)) in self.children_mut().enumerate() {
				*ptr = idx;
				let heights = (element.height(), element.true_height());
				if *y < 8 && depth == target_depth {
					*y = 0;
					self.insert(idx, key.unwrap_or(CompactString::new_inline("_")), element);
					return DropFn::Dropped(
						heights.0,
						heights.1,
						unsafe {
							Some(
								self.get(idx)
									.panic_unchecked("We just added it")
									.0
									.to_compact_string(),
							)
						},
						line_number + 1,
					);
				} else if *y >= value.height() * 16 - 8 && *y < value.height() * 16 && depth == target_depth {
					*y = 0;
					*ptr = idx + 1;
					line_number += value.true_height();
					self.insert(
						idx + 1,
						key.unwrap_or(CompactString::new_inline("_")),
						element,
					);
					return DropFn::Dropped(
						heights.0,
						heights.1,
						unsafe {
							Some(
								self.get(idx + 1)
									.panic_unchecked("We just added it")
									.0
									.to_compact_string(),
							)
						},
						line_number + 1,
					);
				}

				match value.drop(
					key,
					element,
					y,
					depth + 1,
					target_depth,
					line_number + 1,
					indices,
				) {
					x @ DropFn::InvalidType(_, _) => return x,
					DropFn::Missed(k, e) => {
						key = k;
						element = e;
					}
					DropFn::Dropped(increment, true_increment, key, line_number) => {
						self.increment(increment, true_increment);
						return DropFn::Dropped(increment, true_increment, key, line_number);
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
		for (_, element) in self.children_mut() {
			element.shut();
		}
		self.open = false;
		self.height = self.len() as u32 + 1;
	}

	#[inline]
	pub fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		self.open = !self.is_empty();
		self.height = self.true_height;
		for (_, element) in self.children_mut() {
			element.expand(scope);
		}
	}

	#[inline]
	pub fn render_icon(pos: impl Into<(usize, usize)>, z: u8, builder: &mut VertexBufferBuilder) { builder.draw_texture_z(pos, z, COMPOUND_UV, (16, 16)); }
}

// Based on indexmap, but they didn't let me clone with unchecked mem stuff
#[allow(clippy::module_name_repetitions)]
pub struct CompoundMap {
	pub indices: RawTable<usize>,
	pub entries: Vec<Entry>,
}

impl Clone for CompoundMap {
	#[allow(clippy::cast_ptr_alignment)]
	fn clone(&self) -> Self {
		pub unsafe fn clone_entries(entries: &[Entry]) -> Vec<Entry> {
			let len = entries.len();
			let ptr = alloc(Layout::array::<Entry>(len).unwrap_unchecked()).cast::<Entry>();
			for n in 0..len {
				ptr.write(entries.get_unchecked(n).clone());
			}
			Vec::from_raw_parts(ptr, len, len)
		}

		unsafe {
			let mut table = RawTable::try_with_capacity(self.indices.len()).unwrap_unchecked();
			for (idx, bucket) in self.indices.iter().enumerate() {
				let hash = hash!(self.entries.get_unchecked(idx).key.as_str());
				let _ = table.insert_in_slot(hash, core::mem::transmute(idx), *bucket.as_ref());
			}
			Self {
				indices: table,
				entries: clone_entries(&self.entries),
			}
		}
	}
}

#[derive(Clone)]
pub struct Entry {
	pub value: NbtElement,
	pub hash: u64,
	pub key: CompactString,
}

impl Default for CompoundMap {
	fn default() -> Self {
		Self {
			indices: RawTable::new(),
			entries: Vec::new(),
		}
	}
}

impl CompoundMap {
	#[must_use]
	pub fn idx_of(&self, key: &str) -> Option<usize> {
		self.indices
			.get(hash!(key), |&idx| unsafe {
				self.entries.get_unchecked(idx).key.as_str() == key
			})
			.copied()
	}

	#[must_use]
	pub fn has(&self, key: &str) -> bool { self.idx_of(key.as_ref()).is_some() }

	pub fn insert(&mut self, key: CompactString, element: NbtElement) -> Option<NbtElement> { self.insert_full(key, element).1 }

	#[must_use]
	pub fn len(&self) -> usize { self.entries.len() }

	#[must_use]
	pub fn is_empty(&self) -> bool { self.entries.is_empty() }

	pub fn insert_full(&mut self, key: CompactString, element: NbtElement) -> (usize, Option<NbtElement>) {
		unsafe {
			let hash = hash!(key.as_str());
			match self.indices.find_or_find_insert_slot(
				hash,
				|&idx| self.entries.get_unchecked(idx).key.as_str() == key.as_str(),
				|&idx| hash!(self.entries.get_unchecked(idx).key.as_str()),
			) {
				Ok(bucket) => {
					let idx = *bucket.as_ref();
					(
						idx,
						Some(core::mem::replace(
							&mut self.entries.get_unchecked_mut(idx).value,
							element,
						)),
					)
				}
				Err(slot) => {
					let len = self.entries.len();
					self.entries.try_reserve_exact(1).unwrap_unchecked();
					self.entries.as_mut_ptr().add(len).write(Entry {
						key,
						value: element,
						hash,
					});
					self.entries.set_len(len + 1);
					self.indices.insert_in_slot(hash, slot, len);
					(len, None)
				}
			}
		}
	}

	pub fn insert_at(&mut self, key: CompactString, element: NbtElement, idx: usize) -> Option<(CompactString, NbtElement)> {
		unsafe {
			let hash = hash!(key.as_str());
			let (prev, end, bucket) = match self.indices.find_or_find_insert_slot(
				hash,
				|&idx| self.entries.get_unchecked(idx).key.as_str() == key.as_str(),
				|&idx| {
					let mut hasher = FxHasher::default();
					hasher.write(self.entries.get_unchecked(idx).key.as_str().as_bytes());
					hasher.finish()
				},
			) {
				Ok(bucket) => {
					let before = core::mem::replace(bucket.as_mut(), idx);
					let Entry {
						key: k, value: v, ..
					} = self.entries.remove(before);
					self.entries.insert(
						idx,
						Entry {
							key,
							value: element,
							hash,
						},
					);
					(Some((k, v)), before, bucket)
				}
				Err(slot) => {
					let len = self.entries.len();
					self.entries.try_reserve_exact(1).unwrap_unchecked();
					let ptr = self.entries.as_mut_ptr().add(idx);
					core::ptr::copy(ptr, ptr.add(1), len - idx);
					self.entries.as_mut_ptr().add(idx).write(Entry {
						key,
						value: element,
						hash,
					});
					self.entries.set_len(len + 1);
					let bucket = self.indices.insert_in_slot(hash, slot, idx);
					(None, len, bucket)
				}
			};

			match idx.cmp(&end) {
				Ordering::Less => {
					for index in self.indices.iter() {
						let value = *index.as_ref();
						if value >= idx && value <= end {
							*index.as_mut() += 1;
						}
					}
				}
				Ordering::Equal => {}
				Ordering::Greater => {
					for index in self.indices.iter() {
						let value = *index.as_ref();
						if value <= idx && value >= end {
							*index.as_mut() -= 1;
						}
					}
				}
			}

			*bucket.as_mut() = idx;

			prev
		}
	}

	/// # Safety
	///
	/// * compound must not contain this key already somewhere else
	///
	/// * idx must be valid
	pub unsafe fn update_key_idx_unchecked(&mut self, idx: usize, key: CompactString) -> CompactString {
		let old_key = self.entries.get_unchecked(idx).key.as_str();
		let hash = hash!(old_key);
		let _ = self
			.indices
			.remove_entry(hash, |&idx| {
				self.entries.get_unchecked(idx).key.as_str() == old_key
			})
			.unwrap_unchecked();
		let old_key = core::mem::replace(&mut self.entries.get_unchecked_mut(idx).key, key);
		self.indices.insert(
			hash!(self.entries.get_unchecked(idx).key.as_str()),
			idx,
			|&idx| hash!(self.entries.get_unchecked(idx).key.as_str()),
		);
		old_key
	}

	pub fn shift_remove_idx(&mut self, idx: usize) -> Option<(CompactString, NbtElement)> {
		if idx > self.entries.len() { return None }
		unsafe {
			let Entry { key, hash, .. } = &self.entries.get_unchecked(idx);
			let _ = self.indices.remove_entry(*hash, |&idx| {
				self.entries.get_unchecked(idx).key.as_str() == key.as_str()
			});
			for bucket in self.indices.iter() {
				if *bucket.as_ref() > idx {
					*bucket.as_mut() -= 1;
				}
			}
		}
		let Entry { key, value, .. } = self.entries.remove(idx);
		self.entries.shrink_to_fit();
		Some((key, value))
	}

	pub fn swap_remove_idx(&mut self, idx: usize) -> Option<(CompactString, NbtElement)> {
		if idx > self.entries.len() { return None }
		let Entry { key, value, hash } = self.entries.swap_remove(idx);
		unsafe {
			let tail = self
				.indices
				.remove_entry(hash, |&idx| idx + 1 == self.entries.len())
				.unwrap_unchecked();
			*self
				.indices
				.get_mut(hash, |&idx| {
					self.entries.get_unchecked(idx).key.as_str() == key.as_str()
				})
				.unwrap_unchecked() = tail;
		}
		Some((key, value))
	}

	pub fn swap(&mut self, a: usize, b: usize) {
		if a < self.entries.len() && b < self.entries.len() { return }
		unsafe {
			let a_hash = self.entries.get_unchecked(a).hash;
			let b_hash = self.entries.get_unchecked(b).hash;
			let a = self
				.indices
				.get_mut(a_hash, |&idx| idx == a)
				.unwrap_unchecked() as *mut usize;
			let b = self
				.indices
				.get_mut(b_hash, |&idx| idx == b)
				.unwrap_unchecked() as *mut usize;
			core::ptr::swap(a, b);
		}
	}

	#[must_use]
	pub fn get_idx(&self, idx: usize) -> Option<(&str, &NbtElement)> {
		let Entry { key, value, .. } = self.entries.get(idx)?;
		let key = key.as_ref();
		Some((key, value))
	}

	#[must_use]
	pub fn get_idx_mut(&mut self, idx: usize) -> Option<(&str, &mut NbtElement)> {
		let entry = self.entries.get_mut(idx)?;
		Some((entry.key.as_ref(), &mut entry.value))
	}

	pub fn sort_by<F: FnMut((&str, &NbtElement), (&str, &NbtElement)) -> Ordering>(&mut self, mut f: F, line_number: usize, true_line_number: usize, true_height: usize, open: bool, bookmarks: &mut [Bookmark]) -> Box<[usize]> {
		let hashes = self.entries.iter().map(|entry| entry.hash).collect::<Vec<_>>();
		let true_line_numbers = {
			let mut current_line_number = true_line_number + 1;
			self.entries.iter().map(|entry| { let new_line_number = current_line_number; current_line_number += entry.value.true_height(); new_line_number }).collect::<Vec<_>>()
		};
		let line_numbers = {
			let mut current_line_number = line_number + 1;
			self.entries.iter().map(|entry| { let new_line_number = current_line_number; current_line_number += entry.value.height(); new_line_number }).collect::<Vec<_>>()
		};
		let bookmarks_start = bookmarks.binary_search(&Bookmark::new(true_line_number, 0)).unwrap_or_else(identity);
		let bookmarks_end = bookmarks.binary_search(&Bookmark::new(true_line_number + true_height - 1, 0)).map_or_else(identity, |x| x + 1);
		let mut new_bookmarks = Box::<[Bookmark]>::new_uninit_slice(bookmarks_end - bookmarks_start);
		let mut new_bookmarks_len = 0;
		// yeah, it's hacky but there's not much else I *can* do. plus: it works extremely well.
		for (idx, entry) in self.entries.iter_mut().enumerate() {
			entry.hash = idx as u64;
		}
		self.entries.sort_by(|a, b| f((&a.key, &a.value), (&b.key, &b.value)));
		let indices = self.entries.iter().map(|entry| entry.hash as usize).collect::<Vec<_>>();
		let mut inverted_indices = Box::<[usize]>::new_uninit_slice(self.len());
		let mut current_true_line_number = true_line_number + 1;
		let mut current_line_number = line_number + 1;
		for (new_idx, &idx) in indices.iter().enumerate() {
			// SAFETY: these indices are valid since the length did not change and since the values written were indexes
			unsafe {
				let hash = *hashes.get_unchecked(idx);
				let entry = self.entries.get_unchecked_mut(new_idx);
				entry.hash = hash;
				*self.indices.find(hash, |&x| x == idx).panic_unchecked("index obviously exists").as_mut() = new_idx;

				let true_line_number = *true_line_numbers.get_unchecked(idx);
				let line_number = *line_numbers.get_unchecked(idx);
				let true_height = entry.value.true_height();
				let height = entry.value.height();
				let true_offset = current_true_line_number as isize - true_line_number as isize;
				let offset = if open { current_line_number as isize - line_number as isize } else { 0 };
				let bookmark_start = bookmarks.binary_search(&Bookmark::new(true_line_number, 0)).unwrap_or_else(identity);
				let bookmark_end = bookmarks.binary_search(&Bookmark::new(true_line_number + true_height - 1, 0)).map_or_else(identity, |x| x + 1);
				for bookmark in bookmarks.iter().skip(bookmark_start).take(bookmark_end - bookmark_start) {
					let adjusted_bookmark = Bookmark::new(bookmark.true_line_number.wrapping_add(true_offset as usize), bookmark.line_number.wrapping_add(offset as usize));
					new_bookmarks[new_bookmarks_len].write(adjusted_bookmark);
					new_bookmarks_len += 1;
				}

				current_true_line_number += true_height;
				current_line_number += height;
				inverted_indices[idx].write(new_idx);
			}
		}
		unsafe { core::ptr::copy_nonoverlapping(new_bookmarks.as_ptr().cast::<Bookmark>(), bookmarks.as_mut_ptr().add(bookmarks_start), bookmarks_end - bookmarks_start); }
		unsafe { inverted_indices.assume_init() }
	}

	#[must_use]
	#[inline]
	pub fn iter(&self) -> CompoundMapIter<'_> { CompoundMapIter(self.entries.iter()) }

	#[must_use]
	#[inline]
	pub fn iter_mut(&mut self) -> CompoundMapIterMut<'_> { CompoundMapIterMut(self.entries.iter_mut()) }
}

#[allow(clippy::module_name_repetitions)]
pub struct CompoundMapIter<'a>(core::slice::Iter<'a, Entry>);

impl<'a> Iterator for CompoundMapIter<'a> {
	type Item = (&'a str, &'a NbtElement);

	fn next(&mut self) -> Option<Self::Item> {
		self.0
			.next()
			.map(|Entry { key, value, .. }| (key.as_ref(), value))
	}
}

impl<'a> DoubleEndedIterator for CompoundMapIter<'a> {
	fn next_back(&mut self) -> Option<Self::Item> {
		self.0
			.next_back()
			.map(|Entry { key, value, .. }| (key.as_ref(), value))
	}
}

impl<'a> ExactSizeIterator for CompoundMapIter<'a> {
	fn len(&self) -> usize { self.0.len() }
}

#[allow(clippy::module_name_repetitions)]
pub struct CompoundMapIterMut<'a>(core::slice::IterMut<'a, Entry>);

impl<'a> Iterator for CompoundMapIterMut<'a> {
	type Item = (&'a str, &'a mut NbtElement);

	fn next(&mut self) -> Option<Self::Item> {
		self.0
			.next()
			.map(|entry| (entry.key.as_str(), &mut entry.value))
	}
}

impl<'a> DoubleEndedIterator for CompoundMapIterMut<'a> {
	fn next_back(&mut self) -> Option<Self::Item> {
		self.0
			.next_back()
			.map(|entry| (entry.key.as_str(), &mut entry.value))
	}
}

impl<'a> ExactSizeIterator for CompoundMapIterMut<'a> {
	fn len(&self) -> usize { self.0.len() }
}
