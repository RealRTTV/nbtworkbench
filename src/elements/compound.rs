use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Write};
use std::hint::likely;
use std::ops::Deref;
use std::slice::{Iter, IterMut};
#[cfg(not(target_arch = "wasm32"))] use std::thread::{scope, Scope};

use compact_str::CompactString;
use hashbrown::hash_table::Entry::*;
use hashbrown::hash_table::HashTable;

use crate::assets::{COMPOUND_GHOST_UV, COMPOUND_ROOT_UV, COMPOUND_UV, CONNECTION_UV, HEADER_SIZE, JUST_OVERLAPPING_BASE_TEXT_Z};
use crate::elements::result::NbtParseResult;
use crate::elements::{ComplexNbtElementVariant, Matches, NbtElement, NbtElementAndKey, NbtElementAndKeyRef, NbtElementAndKeyRefMut, NbtElementVariant};
use crate::render::{RenderContext, TextColor, VertexBufferBuilder};
use crate::serialization::{Decoder, PrettyDisplay, PrettyFormatter, UncheckedBufWriter};
use crate::util::{width_ascii, StrExt, Vec2u};
#[cfg(target_arch = "wasm32")]
use crate::wasm::{fake_scope as scope, FakeScope as Scope};
use crate::{config, hash};

#[repr(C)]
pub struct NbtCompound {
	pub map: Box<CompoundMap>,
	height: u32,
	true_height: u32,
	max_depth: u32,
	open: bool,
}

impl Matches for NbtCompound {
	fn matches(&self, other: &Self) -> bool { self.map.matches(&other.map) }
}

impl PartialEq for NbtCompound {
	fn eq(&self, other: &Self) -> bool { self.map.eq(&other.map) }
}

impl Clone for NbtCompound {
	fn clone(&self) -> Self {
		Self {
			map: unsafe { Box::try_new(self.map.deref().clone()).unwrap_unchecked() },
			height: self.height,
			true_height: self.true_height,
			max_depth: self.max_depth,
			open: self.open,
		}
	}
}

impl Default for NbtCompound {
	fn default() -> Self {
		Self {
			height: 1,
			map: Box::<CompoundMap>::default(),
			open: false,
			true_height: 1,
			max_depth: 0,
		}
	}
}

impl Display for NbtCompound {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{{")?;
		for (idx, CompoundEntry { key, value }) in self.children().enumerate() {
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

impl PrettyDisplay for NbtCompound {
	fn pretty_fmt(&self, f: &mut PrettyFormatter) {
		if self.is_empty() {
			f.write_str("{}")
		} else {
			let len = self.len();
			f.write_str("{\n");
			f.increase();
			for (idx, CompoundEntry { key, value }) in self.children().enumerate() {
				f.indent();
				if key.needs_escape() {
					f.write_str(&format!("{key:?}"));
					f.write_str(": ");
				} else {
					f.write_str(key);
					f.write_str(": ");
				}
				value.pretty_fmt(f);
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

impl NbtElementVariant for NbtCompound {
	const ID: u8 = 10;
	const UV: Vec2u = COMPOUND_UV;
	const GHOST_UV: Vec2u = COMPOUND_GHOST_UV;

	fn from_str0(mut s: &str) -> Result<(&str, Self), usize>
	where Self: Sized {
		s = s.strip_prefix('{').ok_or(s.len())?.trim_start();
		let mut compound = Self::default();
		while !s.starts_with('}') {
			let (key, s2) = s.snbt_string_read()?;
			s = s2
				.trim_start()
				.strip_prefix(':')
				.ok_or(s2.len())?
				.trim_start();
			let (s2, value) = NbtElement::from_str0(s, NbtElement::parse_int)?;
			compound
				.map
				.insert(CompoundEntry::new(key, value));
			s = s2.trim_start();
			if let Some(s2) = s.strip_prefix(',') {
				s = s2.trim_start();
			} else if s.starts_with('}') {
				break;
			} else {
				return Err(s.len())
			}
		}
		let s = s.strip_prefix('}').ok_or(s.len())?;
		// SAFETY: we can only call this on init of the compound because no caches have even been made yet
		unsafe {
			config::get_sort_algorithm().sort(&mut compound.map);
		}
		compound.recache();
		Ok((s, compound))
	}

	fn from_bytes<'a, D: Decoder<'a>>(decoder: &mut D) -> NbtParseResult<Self>
	where Self: Sized {
		use super::result::*;

		let mut compound = Self::default();
		unsafe {
			decoder.assert_len(1)?;
			let mut current_element = decoder.u8();
			while current_element != 0 {
				decoder.assert_len(2)?;
				let key = decoder.string()?;
				let value = NbtElement::from_bytes(current_element, decoder)?;
				compound
					.map
					.insert(CompoundEntry::new(key, value));
				if is_err(&decoder.assert_len(1)) {
					break // wow mojang, saving one byte, so cool of you
				};
				current_element = decoder.u8();
			}
			decoder.sort(&mut compound.map);
			compound.recache();
			ok(compound)
		}
	}

	fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) {
		for CompoundEntry { key, value } in self.children() {
			writer.write(&[value.id()]);
			writer.write_be_str(key);
			value.to_be_bytes(writer);
		}
		writer.write(&[0x00]);
	}

	fn to_le_bytes(&self, writer: &mut UncheckedBufWriter) {
		for CompoundEntry { key, value } in self.children() {
			writer.write(&[value.id()]);
			writer.write_le_str(key);
			value.to_le_bytes(writer);
		}
		writer.write(&[0x00]);
	}

	fn render(&self, builder: &mut VertexBufferBuilder, name: Option<&str>, remaining_scroll: &mut usize, tail: bool, ctx: &mut RenderContext) {
		let pos = ctx.pos();
		let mut y_before = pos.y;

		'head: {
			if *remaining_scroll > 0 {
				*remaining_scroll -= 1;
				ctx.skip_line_numbers(1);
				break 'head;
			}

			ctx.line_number();
			builder.draw_texture(ctx.pos(), if ctx.pos() == Vec2u::new(ctx.left_margin(), HEADER_SIZE) { Self::ROOT_UV } else { Self::UV }, (16, 16));
			if !self.is_empty() {
				ctx.draw_toggle(pos - (16, 0), self.open, builder);
			}
			ctx.render_errors(pos, builder);
			if ctx.forbid(pos) {
				builder.settings(pos + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
				if let Some(key) = name {
					builder.color = TextColor::TreeKey.to_raw();
					let _ = write!(builder, "{key}: ");
				};

				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "{}", self.value());
			}

			if ctx.draw_held_entry_bar(pos + (16, 16), builder, |x, y| pos + (16, 8) == (x, y), |x| self.can_insert(x)) {
			} else if self.height() == 1 && ctx.draw_held_entry_bar(pos + (16, 16), builder, |x, y| pos + (16, 16) == (x, y), |x| self.can_insert(x)) {
			}

			ctx.offset_pos(0, 16);
			y_before += 16;
		}

		let x_before = ctx.pos().x - 16;

		if self.open {
			ctx.offset_pos(16, 0);

			{
				let children_contains_forbidden = 'f: {
					let mut y = ctx.pos().y;
					for CompoundEntry { key: _, value } in self.children() {
						if ctx.selected_text_y() == Some(y.saturating_sub(*remaining_scroll * 16))
							&& ctx
								.selected_text_y()
								.is_some_and(|y| y >= HEADER_SIZE)
						{
							break 'f true;
						}
						y += value.height() * 16;
					}
					false
				};
				if children_contains_forbidden {
					let mut y = ctx.pos().y;
					for CompoundEntry { key: name, value } in self.children() {
						ctx.check_for_key_duplicate(|text, _| text == name, false);
						if ctx.selected_text_y() != Some(y.saturating_sub(*remaining_scroll * 16)) && y.saturating_sub(*remaining_scroll * 16) >= HEADER_SIZE && ctx.has_duplicate_key_error() {
							ctx.set_red_line_number(y.saturating_sub(*remaining_scroll * 16), 1);
							ctx.draw_error_underline(ctx.pos().x, y.saturating_sub(*remaining_scroll * 16), builder);
							break;
						}
						y += value.height() * 16;
					}
				}
			}

			for (idx, CompoundEntry { key, value }) in self.children().enumerate() {
				let pos = ctx.pos();

				if pos.y > builder.window_height() {
					break;
				}

				let height = value.height();
				if *remaining_scroll >= height {
					*remaining_scroll -= height;
					ctx.skip_line_numbers(value.true_height());
					continue;
				}

				ctx.draw_held_entry_bar(pos, builder, |x, y| pos == (x, y), |x| self.can_insert(x));

				if *remaining_scroll == 0 {
					builder.draw_texture(pos - (16, 0), CONNECTION_UV, (16, (idx != self.len() - 1) as usize * 7 + 9));
				}
				ctx.check_for_key_duplicate(|text, _| self.map.has(text) && key != text, false);
				if ctx.has_duplicate_key_error() && ctx.selected_text_y() == Some(pos.y) {
					ctx.set_red_line_number(pos.y, 0);
				}
				value.render(remaining_scroll, builder, Some(key), tail && idx == self.len() - 1, ctx);

				ctx.draw_held_entry_bar(pos, builder, |x, y| pos == (x, y + 8), |x| self.can_insert(x));
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

	fn value(&self) -> Cow<'_, str> { Cow::Owned(format!("{} {}", self.len(), if self.len() == 1 { "entry" } else { "entries" })) }
}

impl ComplexNbtElementVariant for NbtCompound {
	type Entry = CompoundEntry;
	const ROOT_UV: Vec2u = COMPOUND_ROOT_UV;

	fn new(entries: Vec<Self::Entry>) -> Self
	where Self: Sized {
		let mut this = Self {
			map: Box::new(CompoundMap::from(entries)),
			height: 0,
			true_height: 0,
			max_depth: 0,
			open: false,
		};
		this.recache();
		this
	}

	fn height(&self) -> usize { self.height as usize }

	fn true_height(&self) -> usize { self.true_height as usize }

	fn len(&self) -> usize { self.map.len() }

	fn can_insert(&self, value: &NbtElement) -> bool { !value.is_chunk() }

	fn is_open(&self) -> bool { self.open }

	fn max_depth(&self) -> usize { self.max_depth as usize }

	unsafe fn toggle(&mut self) {
		self.open = !self.open && !self.is_empty();
		if !self.open && !self.is_empty() {
			scope(|scope| unsafe { self.shut(scope) });
		}
	}

	unsafe fn insert(&mut self, idx: usize, mut entry: Self::Entry) -> Result<Option<Self::Entry>, Self::Entry> {
		while self.map.has(&entry.key) {
			entry.key += " - Copy";
		}
		self.height += entry.value.height() as u32;
		self.true_height += entry.value.true_height() as u32;
		self.map.insert_at(entry, idx);
		Ok(None)
	}

	unsafe fn remove(&mut self, idx: usize) -> Option<Self::Entry> {
		let entry = self.map.shift_remove_idx(idx)?;
		Some(entry)
	}

	unsafe fn replace(&mut self, idx: usize, entry: Self::Entry) -> Result<Option<Self::Entry>, Self::Entry> {
		if !self.can_insert(&entry.value) || idx >= self.len() {
			return Err(entry);
		}

		let old_entry = unsafe { self.remove(idx) };

		// always Ok(None)
		let _ = unsafe { self.insert(idx, entry) };

		Ok(old_entry)
	}

	unsafe fn swap(&mut self, a: usize, b: usize) {
		if a >= self.len() || b >= self.len() {
			return;
		}
		unsafe {
			let a_hash = hash!(self.map.entries.get_unchecked(a).key);
			let b_hash = hash!(self.map.entries.get_unchecked(b).key);
			self.map.entries.swap(a, b);
			let a = self
				.map
				.indices
				.find_mut(a_hash, |&idx| idx == a)
				.unwrap_unchecked() as *mut usize;
			let b = self
				.map
				.indices
				.find_mut(b_hash, |&idx| idx == b)
				.unwrap_unchecked() as *mut usize;
			core::ptr::swap(a, b);
		}
	}

	unsafe fn shut<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		self.open = false;
		self.height = self.len() as u32 + 1;
		for CompoundEntry { key: _, value: element } in self.children_mut() {
			if element.is_open() {
				unsafe { element.shut(scope) };
			}
		}
	}

	unsafe fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		self.open = !self.is_empty();
		self.height = self.true_height;
		for CompoundEntry { key: _, value: element } in self.children_mut() {
			unsafe { element.expand(scope) };
		}
	}

	fn recache(&mut self) {
		let mut height = 1;
		let mut true_height = 1;
		for CompoundEntry { key: _, value: child } in self.children() {
			height += child.height() as u32;
			true_height += child.true_height() as u32;
		}
		self.height = height;
		self.true_height = true_height;
		let mut max_depth = 0;
		if self.is_open() {
			for CompoundEntry { key, value: child } in self.children() {
				max_depth = max_depth.max(16 + 4 + key.width() + const { width_ascii(": ") } + child.value_width());
				max_depth = max_depth.max(16 + child.max_depth());
			}
		}
		self.max_depth = max_depth as u32;
	}

	fn get(&self, idx: usize) -> Option<&Self::Entry> { self.map.entries.get(idx) }

	fn get_mut(&mut self, idx: usize) -> Option<&mut Self::Entry> { self.map.entries.get_mut(idx) }

	unsafe fn get_unchecked(&self, idx: usize) -> &Self::Entry { unsafe { self.map.entries.get_unchecked(idx) } }

	unsafe fn get_unchecked_mut(&mut self, idx: usize) -> &mut Self::Entry { unsafe { self.map.entries.get_unchecked_mut(idx) } }

	fn children(&self) -> Iter<'_, Self::Entry> { self.map.entries.iter() }

	fn children_mut(&mut self) -> IterMut<'_, Self::Entry> { self.map.entries.iter_mut() }
}

// Based on indexmap, but they didn't let me clone with unchecked mem stuff
pub struct CompoundMap {
	pub indices: HashTable<usize>,
	pub entries: Vec<CompoundEntry>,
}

impl CompoundMap {
	pub fn matches(&self, other: &Self) -> bool {
		for entry in &self.entries {
			if let Some(CompoundEntry { key, value }) = other
				.idx_of(&entry.key)
				.and_then(|idx| other.entries.get(idx))
				&& key == entry.key
				&& entry.value.matches(value)
			{
				continue
			}
			return false
		}
		true
	}
}

impl PartialEq for CompoundMap {
	fn eq(&self, other: &Self) -> bool {
		self.entries
			.as_slice()
			.eq(other.entries.as_slice())
	}
}

impl Clone for CompoundMap {
	fn clone(&self) -> Self {
		pub fn clone_entries(entries: &[CompoundEntry]) -> Vec<CompoundEntry> {
			let mut vec = unsafe { Vec::try_with_capacity(entries.len()).unwrap_unchecked() };
			for entry in entries.iter().cloned() {
				unsafe { vec.push_within_capacity(entry).unwrap_unchecked() };
			}
			vec
		}

		Self {
			indices: HashTable::clone(&self.indices),
			entries: clone_entries(&self.entries),
		}
	}
}

#[derive(Clone)]
#[repr(C)] // this works out well so rustc can typecast this into NbtElementAndKey
pub struct CompoundEntry {
	pub key: CompactString,
	pub value: NbtElement,
}

impl Default for CompoundMap {
	fn default() -> Self {
		Self {
			indices: HashTable::new(),
			entries: Vec::new(),
		}
	}
}

impl From<Vec<CompoundEntry>> for CompoundMap {
	fn from(entries: Vec<CompoundEntry>) -> Self {
		let mut indices = HashTable::with_capacity(entries.len());
		for (idx, entry) in entries.iter().enumerate() {
			indices.insert_unique(hash!(entry.key), idx, |&idx| hash!(unsafe { entries.get_unchecked(idx) }.key));
		}
		Self { indices, entries }
	}
}

impl PartialEq for CompoundEntry {
	fn eq(&self, other: &Self) -> bool { self.key == other.key && self.value == other.value }
}

impl From<CompoundEntry> for NbtElementAndKey {
	fn from(value: CompoundEntry) -> Self {
		(Some(value.key), value.value)
	}
}

impl<'a> From<&'a CompoundEntry> for NbtElementAndKeyRef<'a> {
	fn from(value: &'a CompoundEntry) -> Self {
		(Some(value.key.as_str()), &value.value)
	}
}

impl<'a> From<&'a mut CompoundEntry> for NbtElementAndKeyRefMut<'a> {
	fn from(value: &'a mut CompoundEntry) -> Self {
		(Some(value.key.as_str()), &mut value.value)
	}
}

impl CompoundEntry {
	#[must_use]
	pub fn new(key: CompactString, value: NbtElement) -> Self { Self { key, value } }

	#[must_use]
	pub fn as_ref(&self) -> (&str, &NbtElement) { (&self.key, &self.value) }

	#[must_use]
	pub fn as_ref_mut(&mut self) -> (&str, &mut NbtElement) { (&self.key, &mut self.value) }
}

impl CompoundMap {
	#[must_use]
	pub fn idx_of(&self, key: &str) -> Option<usize> {
		self.indices
			.find(hash!(key), |&idx| unsafe { self.entries.get_unchecked(idx).key.as_str() == key })
			.copied()
	}

	#[must_use]
	pub fn has(&self, key: &str) -> bool { self.idx_of(key.as_ref()).is_some() }

	pub fn insert(&mut self, entry: CompoundEntry) -> Option<NbtElement> { self.insert_full(entry).1 }

	#[must_use]
	pub fn len(&self) -> usize { self.entries.len() }

	#[must_use]
	pub fn is_empty(&self) -> bool { self.entries.is_empty() }

	pub fn insert_full(&mut self, entry: CompoundEntry) -> (usize, Option<NbtElement>) {
		let hash = hash!(entry.key);
		match self
			.indices
			.entry(hash, |&idx| unsafe { self.entries.get_unchecked(idx) }.key == entry.key, |&idx| hash!(unsafe { self.entries.get_unchecked(idx) }.key))
		{
			Occupied(slot) => {
				let idx = *slot.get();
				(idx, Some(core::mem::replace(&mut unsafe { self.entries.get_unchecked_mut(idx) }.value, entry.value)))
			}
			Vacant(slot) => {
				let len = self.entries.len();
				unsafe {
					self.entries.try_reserve(1).unwrap_unchecked();
					self.entries.as_mut_ptr().add(len).write(entry);
					self.entries.set_len(len + 1);
					slot.insert(len);
				}
				(len, None)
			}
		}
	}

	pub fn insert_at(&mut self, entry: CompoundEntry, idx: usize) -> Option<(CompactString, NbtElement)> {
		let hash = hash!(entry.key);
		let (prev, end, ptr) = match self
			.indices
			.entry(hash, |&idx| unsafe { self.entries.get_unchecked(idx) }.key == entry.key, |&idx| hash!(unsafe { self.entries.get_unchecked(idx) }.key))
		{
			Occupied(mut slot) => {
				let before = core::mem::replace(slot.get_mut(), idx);
				let CompoundEntry { key: k, value: v } = self.entries.remove(before);
				self.entries.insert(idx, entry);
				(Some((k, v)), before, slot.get_mut() as *mut usize)
			}
			Vacant(slot) => {
				let len = self.entries.len();
				unsafe {
					self.entries
						.try_reserve_exact(1)
						.unwrap_unchecked();
					let ptr = self.entries.as_mut_ptr().add(idx);
					core::ptr::copy(ptr, ptr.add(1), len - idx);
					self.entries.as_mut_ptr().add(idx).write(entry);
					self.entries.set_len(len + 1);
				}
				let mut entry = slot.insert(len);
				(None, len, entry.get_mut() as *mut usize)
			}
		};

		unsafe {
			core::ptr::write(ptr, idx);
		}

		match idx.cmp(&end) {
			Ordering::Less =>
				for index in self.indices.iter_mut() {
					let value = *index;
					if value >= idx && value <= end {
						*index += 1;
					}
				},
			Ordering::Equal => {}
			Ordering::Greater =>
				for index in self.indices.iter_mut() {
					let value = *index;
					if value <= idx && value >= end {
						*index -= 1;
					}
				},
		}

		prev
	}

	/// # Safety
	///
	/// * compound must not contain this key already somewhere else
	///
	/// * idx must be valid
	pub unsafe fn update_key_idx_unchecked(&mut self, idx: usize, key: CompactString) -> CompactString {
		let new_hash = hash!(key);
		let old_key = core::mem::replace(&mut self.entries.get_unchecked_mut(idx).key, key);
		if let Ok(entry) = self
			.indices
			.find_entry(hash!(old_key), |&target_idx| target_idx == idx)
		{
			entry.remove();
		} else {
			// should **never** happen
			unsafe { core::hint::unreachable_unchecked() };
		}
		self.indices
			.insert_unique(new_hash, idx, |&idx| hash!(self.entries.get_unchecked(idx).key));
		old_key
	}

	pub fn shift_remove_idx(&mut self, idx: usize) -> Option<CompoundEntry> {
		if idx >= self.entries.len() {
			return None
		}
		unsafe {
			if let Ok(entry) = self
				.indices
				.find_entry(hash!(self.entries.get_unchecked(idx).key), |&found_idx| found_idx == idx)
			{
				entry.remove();
			} else {
				// should **never** happen
				core::hint::unreachable_unchecked();
			}
			for entry in self.indices.iter_mut() {
				if *entry > idx {
					*entry -= 1;
				}
			}
		}
		let entry = self.entries.remove(idx);
		self.entries.shrink_to_fit();
		Some(entry)
	}

	/// This function creates a mapping such that the `mapping[n]`th entry should move to the `n`th index to be sorted.
	#[must_use]
	pub fn create_sort_mapping<F: FnMut(&CompoundEntry, &CompoundEntry) -> Ordering>(&self, mut f: F) -> Box<[usize]> {
		let mut mapping = (0..self.len()).collect::<Vec<_>>();
		mapping.sort_unstable_by(|&a, &b| f(unsafe { self.entries.get_unchecked(a) }, unsafe { self.entries.get_unchecked(b) }));
		mapping.into_boxed_slice()
	}

	pub fn update_key(&mut self, idx: usize, key: CompactString) -> Option<CompactString> {
		if self
			.entries
			.get(idx)
			.is_some_and(|entry| entry.key == key)
		{
			Some(key)
		} else if self.has(key.as_ref()) {
			None
		} else {
			Some(unsafe { self.update_key_idx_unchecked(idx, key) })
		}
	}
}
