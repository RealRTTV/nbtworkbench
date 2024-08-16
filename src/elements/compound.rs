use std::alloc::{alloc, Layout};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Write};
use std::intrinsics::likely;
use std::ops::Deref;
#[cfg(not(target_arch = "wasm32"))]
use std::thread::Scope;

use compact_str::{CompactString, format_compact, ToCompactString};
use hashbrown::raw::RawTable;

use crate::{config, DropFn, OptionExt, RenderContext, StrExt, VertexBufferBuilder};
use crate::assets::{BASE_Z, COMPOUND_ROOT_UV, COMPOUND_UV, CONNECTION_UV, HEADER_SIZE, JUST_OVERLAPPING_BASE_TEXT_Z, LINE_NUMBER_CONNECTOR_Z, LINE_NUMBER_SEPARATOR_UV, ZOffset};
use crate::be_decoder::BigEndianDecoder;
use crate::color::TextColor;
use crate::elements::chunk::NbtChunk;
use crate::elements::element::NbtElement;
use crate::encoder::UncheckedBufWriter;
use crate::formatter::PrettyFormatter;
use crate::le_decoder::LittleEndianDecoder;
use crate::marked_line::{MarkedLine, MarkedLineSlice};

#[allow(clippy::module_name_repetitions)]
#[repr(C)]
pub struct NbtCompound {
	pub entries: Box<CompoundMap>,
	height: u32,
	true_height: u32,
	max_depth: u32,
	open: bool,
}

impl NbtCompound {
	pub fn matches(&self, other: &Self) -> bool {
		self.entries.matches(&other.entries)
	}
}

impl Clone for NbtCompound {
	#[allow(clippy::cast_ptr_alignment)]
	fn clone(&self) -> Self {
		unsafe {
			let box_ptr = alloc(Layout::new::<CompoundMap>()).cast::<CompoundMap>();
			box_ptr.write(self.entries.deref().clone());
			Self {
				entries: Box::from_raw(box_ptr),
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
	pub(in crate::elements) fn from_str0(mut s: &str) -> Result<(&str, Self), usize> {
		s = s.strip_prefix('{').ok_or(s.len())?.trim_start();
		let mut compound = Self::new();
		while !s.starts_with('}') {
			let (key, s2) = s.snbt_string_read()?;
			s = s2.trim_start().strip_prefix(':').ok_or(s2.len())?.trim_start();
			let (s2, value) = NbtElement::from_str0(s)?;
			compound.insert_replacing(key, value);
			s = s2.trim_start();
			if let Some(s2) = s.strip_prefix(',') {
				s = s2.trim_start();
			} else {
				break;
			}
		}
		let s = s.strip_prefix('}').ok_or(s.len())?;
		// SAFETY: we can only call this on init of the compound
		unsafe { config::get_sort_algorithm().sort(&mut compound.entries); }
		Ok((s, compound))
	}

	#[inline]
	pub fn from_be_bytes(decoder: &mut BigEndianDecoder) -> Option<Self> {
		let mut compound = Self::new();
		unsafe {
			decoder.assert_len(1)?;
			let mut current_element = decoder.u8();
			while current_element != 0 {
				decoder.assert_len(2)?;
				let key = decoder.string()?;
				let value = NbtElement::from_be_bytes(current_element, decoder)?;
				compound.insert_replacing(key, value);
				match decoder.assert_len(1) {
					Some(()) => {}
					None => break, // wow mojang, saving one byte, so cool of you
				};
				current_element = decoder.u8();
			}
			decoder.sort(&mut compound.entries);
			Some(compound)
		}
	}

	#[inline]
	pub fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) {
		for (key, value) in self.children() {
			writer.write(&[value.id()]);
			writer.write_be_str(key);
			value.to_be_bytes(writer);
		}
		writer.write(&[0x00]);
	}

	#[inline]
	pub fn from_le_bytes(decoder: &mut LittleEndianDecoder) -> Option<Self> {
		let mut compound = Self::new();
		unsafe {
			decoder.assert_len(1)?;
			let mut current_element = decoder.u8();
			while current_element != 0 {
				decoder.assert_len(2)?;
				let key = decoder.string()?;
				let value = NbtElement::from_le_bytes(current_element, decoder)?;
				compound.insert_replacing(key, value);
				match decoder.assert_len(1) {
					Some(()) => {}
					None => break, // wow mojang, saving one byte, so cool of you
				};
				current_element = decoder.u8();
			}
			decoder.sort(&mut compound.entries);
			Some(compound)
		}
	}

	#[inline]
	pub fn to_le_bytes(&self, writer: &mut UncheckedBufWriter) {
		for (key, value) in self.children() {
			writer.write(&[value.id()]);
			writer.write_le_str(key);
			value.to_le_bytes(writer);
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
		if self.entries.get_idx(idx).is_some_and(|(k, _)| k == key) {
			Some(key)
		} else if self.entries.has(key.as_ref()) {
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
			// fun texture hack for connection
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
				builder.settings(ctx.pos() + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "{str} ");
				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "[{}]", self.value());
			}

			let pos = ctx.pos();
			if ctx.draw_held_entry_bar(ctx.pos() + (16, 16), builder, |x, y| pos + (16, 8) == (x, y), |x| self.can_insert(x)) {} else if self.height() == 1 && ctx.draw_held_entry_bar(ctx.pos() + (16, 16), builder, |x, y| pos + (16, 16) == (x, y), |x| self.can_insert(x)) {}

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

				let pos = ctx.pos();
				ctx.draw_held_entry_bar(ctx.pos(), builder, |x, y| pos == (x, y), |x| self.can_insert(x));

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
				ctx.check_for_key_duplicate(|text, _| self.entries.has(text) && name != text, false);
				if ctx.key_duplicate_error && ctx.y_offset == ctx.selected_y {
					ctx.red_line_numbers[0] = ctx.y_offset;
				}
				value.render(
					&mut remaining_scroll,
					builder,
					Some(name),
					idx == self.len() - 1,
					ctx,
				);

				let pos = ctx.pos();
				ctx.draw_held_entry_bar(ctx.pos(), builder, |x, y| pos == (x, y + 8), |x| self.can_insert(x));
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

	#[inline]
	#[must_use]
	pub fn can_insert(&self, value: &NbtElement) -> bool {
		value.id() != NbtChunk::ID
	}
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

impl NbtCompound {
	pub fn pretty_fmt(&self, f: &mut PrettyFormatter) {
		if self.is_empty() {
			f.write_str("{}")
		} else {
			let len = self.len();
			f.write_str("{\n");
			f.increase();
			for (idx, (key, element)) in self.children().enumerate() {
				f.indent();
				if key.needs_escape() {
					f.write_str(&format!("{key:?}"));
					f.write_str(": ");
				} else {
					f.write_str(key);
					f.write_str(": ");
				}
				element.pretty_fmt(f);
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
				builder.settings(ctx.pos() + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
				if let Some(key) = name {
					builder.color = TextColor::TreeKey.to_raw();
					let _ = write!(builder, "{key}: ");
				};

				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "{}", self.value());
			}

			let pos = ctx.pos();
			if ctx.draw_held_entry_bar(ctx.pos() + (16, 16), builder, |x, y| pos + (16, 8) == (x, y), |x| self.can_insert(x)) {} else if self.height() == 1 && ctx.draw_held_entry_bar(ctx.pos() + (16, 16), builder, |x, y| pos + (16, 16) == (x, y), |x| self.can_insert(x)) {}

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
				ctx.draw_held_entry_bar(ctx.pos(), builder, |x, y| pos == (x, y),|x| self.can_insert(x));

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
				ctx.check_for_key_duplicate(|text, _| self.entries.has(text) && key != text, false);
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

				let pos = ctx.pos();
				ctx.draw_held_entry_bar(ctx.pos(), builder, |x, y| pos == (x, y + 8), |x| self.can_insert(x));
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
			self.insert(0, key.unwrap_or(CompactString::const_new("_")), element);
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
				key.unwrap_or(CompactString::const_new("_")),
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
					self.insert(idx, key.unwrap_or(CompactString::const_new("_")), element);
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
						key.unwrap_or(CompactString::const_new("_")),
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
	#[cfg(not(target_arch = "wasm32"))]
	pub fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		self.open = !self.is_empty();
		self.height = self.true_height;
		for (_, element) in self.children_mut() {
			element.expand(scope);
		}
	}

	#[inline]
	#[cfg(target_arch = "wasm32")]
	pub fn expand(&mut self) {
		self.open = !self.is_empty();
		self.height = self.true_height;
		for (_, element) in self.children_mut() {
			element.expand();
		}
	}

	#[inline]
	pub fn render_icon(pos: impl Into<(usize, usize)>, z: ZOffset, builder: &mut VertexBufferBuilder) { builder.draw_texture_z(pos, z, COMPOUND_UV, (16, 16)); }
}

// Based on indexmap, but they didn't let me clone with unchecked mem stuff
#[allow(clippy::module_name_repetitions)]
pub struct CompoundMap {
	pub indices: RawTable<usize>,
	pub entries: Vec<Entry>,
}

impl CompoundMap {
	pub fn matches(&self, other: &Self) -> bool {
		for entry in &self.entries {
			if let Some((key, value)) = other.idx_of(&entry.key).and_then(|idx| other.get_idx(idx)) && key == entry.key && entry.value.matches(value) {
				continue
			}
			return false
		}
		true
	}
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
				let hash = hash!(self.entries.get_unchecked(idx).key);
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
	pub key: CompactString,
	pub additional: usize,
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
			.get(hash!(key), |&idx| unsafe { self.entries.get_unchecked(idx).key.as_str() == key })
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
			let hash = hash!(key);
			match self.indices.find_or_find_insert_slot(hash, |&idx| self.entries.get_unchecked(idx).key == key, |&idx| hash!(self.entries.get_unchecked(idx).key), ) {
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
					self.entries.try_reserve(1).unwrap_unchecked();
					self.entries.as_mut_ptr().add(len).write(Entry {
						key,
						value: element,
						additional: 0,
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
			let hash = hash!(key);
			let (prev, end, bucket) = match self.indices.find_or_find_insert_slot(hash, |&idx| self.entries.get_unchecked(idx).key == key, |&idx| hash!(self.entries.get_unchecked(idx).key)) {
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
							additional: 0,
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
						additional: 0,
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
		let new_hash = hash!(key);
		let old_key = core::mem::replace(&mut self.entries.get_unchecked_mut(idx).key, key);
		self.indices.remove_entry(hash!(old_key), |&target_idx| target_idx == idx).unwrap_unchecked();
		self.indices.insert(new_hash, idx, |&idx| hash!(self.entries.get_unchecked(idx).key));
		old_key
	}

	pub fn shift_remove_idx(&mut self, idx: usize) -> Option<(CompactString, NbtElement)> {
		if idx > self.entries.len() { return None }
		unsafe {
			self.indices.remove_entry(hash!(self.entries.get_unchecked(idx).key), |&found_idx| found_idx == idx);
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
		let Entry { key, value, .. } = self.entries.swap_remove(idx);
		let hash = hash!(key);
		unsafe {
			let tail = self.indices.remove_entry(hash, |&idx| idx + 1 == self.entries.len()).unwrap_unchecked();
			*self.indices.get_mut(hash, |&idx| self.entries.get_unchecked(idx).key == key).unwrap_unchecked() = tail;
		}
		Some((key, value))
	}

	pub fn swap(&mut self, a: usize, b: usize) {
		if a >= self.entries.len() || b >= self.entries.len() { return; }
		unsafe {
			let a_hash = hash!(self.entries.get_unchecked(a).key);
			let b_hash = hash!(self.entries.get_unchecked(b).key);
			self.entries.swap(a, b);
			let a = self.indices.get_mut(a_hash, |&idx| idx == a).unwrap_unchecked() as *mut usize;
			let b = self.indices.get_mut(b_hash, |&idx| idx == b).unwrap_unchecked() as *mut usize;
			core::ptr::swap(a, b);
		}
	}

	#[must_use]
	pub fn get_idx(&self, idx: usize) -> Option<(&str, &NbtElement)> {
		let Entry { key, value, .. } = self.entries.get(idx)?;
		Some((key.as_str(), value))
	}

	#[must_use]
	pub fn get_idx_mut(&mut self, idx: usize) -> Option<(&str, &mut NbtElement)> {
		let entry = self.entries.get_mut(idx)?;
		Some((entry.key.as_str(), &mut entry.value))
	}

	pub fn sort_by<F: Fn((&str, &NbtElement), (&str, &NbtElement)) -> Ordering>(&mut self, f: F, line_number: usize, true_line_number: usize, true_height: usize, open: bool, bookmarks: &mut MarkedLineSlice) -> Box<[usize]> {
		let true_line_numbers = {
			let mut current_line_number = true_line_number + 1;
			self.entries.iter().map(|entry| { let new_line_number = current_line_number; current_line_number += entry.value.true_height(); new_line_number }).collect::<Vec<_>>()
		};
		let line_numbers = {
			let mut current_line_number = line_number + 1;
			self.entries.iter().map(|entry| { let new_line_number = current_line_number; current_line_number += entry.value.height(); new_line_number }).collect::<Vec<_>>()
		};
		let mut new_bookmarks = Box::<[MarkedLine]>::new_uninit_slice(bookmarks[true_line_number..true_line_number + true_height].len());
		let mut new_bookmarks_len = 0;
		// yeah, it's hacky... but there's not much else I *can* do. plus: it works extremely well.
		for (idx, entry) in self.entries.iter_mut().enumerate() {
			entry.additional = idx;
		}
		self.entries.sort_by(|a, b| f((&a.key, &a.value), (&b.key, &b.value)));
		let indices = self.entries.iter().map(|entry| entry.additional).collect::<Vec<_>>();
		let mut inverted_indices = Box::<[usize]>::new_uninit_slice(self.len());
		let mut current_true_line_number = true_line_number + 1;
		let mut current_line_number = line_number + 1;
		for (new_idx, &idx) in indices.iter().enumerate() {
			// SAFETY: these indices are valid since the length did not change and since the values written were indexes
			unsafe {
				let entry = self.entries.get_unchecked_mut(new_idx);
				*self.indices.find(hash!(entry.key), |&target_idx| target_idx == idx).panic_unchecked("index obviously exists").as_mut() = new_idx;

				let true_line_number = *true_line_numbers.get_unchecked(idx);
				let line_number = *line_numbers.get_unchecked(idx);
				let true_height = entry.value.true_height();
				let height = entry.value.height();
				let true_offset = current_true_line_number as isize - true_line_number as isize;
				let offset = if open { current_line_number as isize - line_number as isize } else { 0 };
				for bookmark in bookmarks[true_line_number..true_line_number + true_height].iter() {
					new_bookmarks[new_bookmarks_len].write(bookmark.offset(offset as usize, true_offset as usize));
					new_bookmarks_len += 1;
				}
				current_true_line_number += true_height;
				current_line_number += height;
				inverted_indices[idx].write(new_idx);
			}
		}
		let bookmark_slice = &mut bookmarks[true_line_number..true_line_number + true_height];
		unsafe { core::ptr::copy_nonoverlapping(new_bookmarks.as_ptr().cast::<MarkedLine>(), bookmark_slice.as_mut_ptr(), bookmark_slice.len()); }
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
		self.0.next().map(|Entry { key, value, .. }| (key.as_ref(), value))
	}
}

impl<'a> DoubleEndedIterator for CompoundMapIter<'a> {
	fn next_back(&mut self) -> Option<Self::Item> {
		self.0.next_back().map(|Entry { key, value, .. }| (key.as_ref(), value))
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
		self.0.next().map(|entry| (entry.key.as_str(), &mut entry.value))
	}
}

impl<'a> DoubleEndedIterator for CompoundMapIterMut<'a> {
	fn next_back(&mut self) -> Option<Self::Item> {
		self.0.next_back().map(|entry| (entry.key.as_str(), &mut entry.value))
	}
}

impl<'a> ExactSizeIterator for CompoundMapIterMut<'a> {
	fn len(&self) -> usize { self.0.len() }
}
