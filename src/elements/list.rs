use std::alloc::{alloc, Layout};
use std::fmt::{Display, Formatter, Write};
use std::intrinsics::likely;
use std::slice::{Iter, IterMut};
#[cfg(not(target_arch = "wasm32"))]
use std::thread::Scope;

use compact_str::{format_compact, CompactString};

use crate::assets::{ZOffset, BASE_Z, CONNECTION_UV, JUST_OVERLAPPING_BASE_TEXT_Z, LIST_UV};
use crate::elements::{id_to_string_name, NbtChunk, NbtCompound, NbtElement};
use crate::render::{RenderContext, TextColor, VertexBufferBuilder};
use crate::serialization::{Decoder, PrettyFormatter, UncheckedBufWriter};
use crate::workbench::DropFn;

#[allow(clippy::module_name_repetitions)]
#[repr(C)]
pub struct NbtList {
	pub elements: Box<Vec<NbtElement>>,
	height: u32,
	true_height: u32,
	max_depth: u32,
	elements_bitset: u16,
	open: bool,
}

impl NbtList {
	pub fn matches(&self, other: &Self) -> bool {
		if self.is_empty() {
			other.is_empty()
		} else {
			self.elements.iter().all(|a| other.elements.iter().any(|b| a.matches(b)))
		}
	}
}

impl PartialEq for NbtList {
	fn eq(&self, other: &Self) -> bool {
		self.elements.as_slice().eq(other.elements.as_slice())
	}
}

impl Clone for NbtList {
	#[allow(clippy::cast_ptr_alignment)]
	#[inline]
	fn clone(&self) -> Self {
		unsafe {
			let len = self.elements.len();
			let ptr = alloc(Layout::array::<NbtElement>(len).unwrap_unchecked()).cast::<NbtElement>();
			let box_ptr = alloc(Layout::new::<Vec<NbtElement>>()).cast::<Vec<NbtElement>>();
			for n in 0..len {
				ptr.add(n).write(self.elements.get_unchecked(n).clone());
			}
			box_ptr.write(Vec::from_raw_parts(ptr.cast::<NbtElement>(), len, len));
			Self {
				elements: Box::from_raw(box_ptr),
				height: self.height,
				true_height: self.true_height,
				max_depth: self.max_depth,
				elements_bitset: self.elements_bitset,
				open: self.open,
			}
		}
	}
}

impl NbtList {
	pub const ID: u8 = 9;
	pub(in crate::elements) fn from_str0(mut s: &str) -> Result<(&str, Self), usize> {
		s = s.strip_prefix('[').ok_or(s.len())?.trim_start();
		let mut list = Self::new(vec![]);
		while !s.starts_with(']') {
			let (s2, mut element) = NbtElement::from_str0(s, NbtElement::parse_int)?;
			element = element.try_into_inner().unwrap_or_else(|element| element);
			list.insert(list.len(), element).map_err(|_| s.len())?;
			s = s2.trim_start();
			if let Some(s2) = s.strip_prefix(',') {
				s = s2.trim_start();
			} else if s.starts_with(']') {
				break;
			}
		}
		let s = s.strip_prefix(']').ok_or(s.len())?;
		list.elements.shrink_to_fit();
		list.recache_elements_bitset();
		Ok((s, list))
	}
	#[allow(clippy::cast_ptr_alignment)]
	#[inline]
	pub fn from_bytes<'a, D: Decoder<'a>>(decoder: &mut D) -> Option<Self> {
		unsafe {
			decoder.assert_len(5)?;
			let element = decoder.u8();
			let len = decoder.u32() as usize;
			let ptr = alloc(Layout::array::<NbtElement>(len).unwrap_unchecked()).cast::<NbtElement>();
			let mut true_height = 1;
			let mut extracted_inner_element = false;
			for n in 0..len {
				let mut element = NbtElement::from_bytes(element, decoder)?;
				element = match element.try_into_inner() {
					Ok(inner) => {
						extracted_inner_element = true;
						inner
					},
					Err(element) => element
				};
				true_height += element.true_height() as u32;
				ptr.add(n).write(element);
			}
			let box_ptr = alloc(Layout::new::<Vec<NbtElement>>()).cast::<Vec<NbtElement>>();
			box_ptr.write(Vec::from_raw_parts(ptr, len, len));
			let mut list = Self {
				elements: Box::from_raw(box_ptr),
				height: 1 + len as u32,
				true_height,
				max_depth: 0,
				elements_bitset: 1 << element,
				open: false,
			};
			if extracted_inner_element {
				list.recache_elements_bitset();
			}
			Some(list)
		}
	}

	#[inline]
	pub fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) {
		let heterogeneous = self.is_heterogeneous();
		writer.write(&[self.serialize_id()]);
		writer.write(&(self.len() as u32).to_be_bytes());
		for element in self.elements.iter() {
			if heterogeneous && element.id() != NbtCompound::ID {
				writer.write(&[element.id()]);
				element.to_be_bytes(writer);
				writer.write(&[0x00]);
			} else {
				element.to_be_bytes(writer);
			}
		}
	}

	#[inline]
	pub fn to_le_bytes(&self, writer: &mut UncheckedBufWriter) {
		let heterogeneous = self.is_heterogeneous();
		writer.write(&[self.serialize_id()]);
		writer.write(&(self.len() as u32).to_le_bytes());
		for element in self.elements.iter() {
			if heterogeneous && element.id() != NbtCompound::ID {
				writer.write(&[element.id()]);
				element.to_le_bytes(writer);
				writer.write(&[0x00]);
			} else {
				element.to_be_bytes(writer);
			}
		}
	}
}

impl NbtList {
	#[inline]
	pub fn new(elements: Vec<NbtElement>) -> Self {
		let mut this = Self {
			height: elements.iter().map(NbtElement::height).sum::<usize>() as u32 + 1,
			true_height: elements.iter().map(NbtElement::true_height).sum::<usize>() as u32 + 1,
			elements: Box::new(elements),
			open: false,
			elements_bitset: 0,
			max_depth: 0,
		};
		this.recache_elements_bitset();
		this
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
	#[must_use]
	pub const fn is_heterogeneous(&self) -> bool { !self.elements_bitset.is_power_of_two() }

	#[inline]
	#[must_use]
	pub const fn id(&self) -> u8 {
		if self.is_heterogeneous() {
			0
		} else {
			self.elements_bitset.trailing_zeros() as u8
		}
	}
	
	#[inline]
	#[must_use]
	pub const fn serialize_id(&self) -> u8 {
		if self.is_heterogeneous() {
			NbtCompound::ID
		} else {
			self.elements_bitset.trailing_zeros() as u8
		}
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
	pub const fn open(&self) -> bool { self.open }

	#[inline]
	#[must_use]
	pub fn len(&self) -> usize { self.elements.len() }

	#[inline]
	#[must_use]
	pub fn is_empty(&self) -> bool { self.elements.is_empty() }
	
	#[inline]
	#[must_use]
	pub fn can_insert(&self, value: &NbtElement) -> bool {
		value.id() != NbtChunk::ID
	}

	#[inline]
	pub fn insert(&mut self, idx: usize, value: NbtElement) -> Result<Option<NbtElement>, NbtElement> {
		if self.can_insert(&value) {
			self.increment(value.height(), value.true_height());
			unsafe {
				self.elements.try_reserve_exact(1).unwrap_unchecked();
			}
			self.elements.insert(idx, value);
			Ok(None)
		} else {
			Err(value)
		}
	}

	#[inline]
	#[must_use]
	pub fn remove(&mut self, idx: usize) -> NbtElement {
		let removed = self.elements.remove(idx);
		self.decrement(removed.height(), removed.true_height());
		self.elements.shrink_to_fit();
		removed
	}

	#[inline]
	pub fn replace(&mut self, idx: usize, value: NbtElement) -> Option<NbtElement> {
		if !self.can_insert(&value) || idx >= self.len() { return None; }
		self.increment(value.height(), value.true_height());
		let old = core::mem::replace(&mut self.elements[idx], value);
		self.decrement(old.height(), old.true_height());
		Some(old)
	}

	#[inline]
	#[must_use]
	pub fn get(&self, idx: usize) -> Option<&NbtElement> { self.elements.get(idx) }

	#[inline]
	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<&mut NbtElement> { self.elements.get_mut(idx) }

	#[inline]
	#[must_use]
	pub fn value(&self) -> CompactString {
		let (single, multiple) = id_to_string_name(self.id());
		format_compact!(
			"{} {}",
			self.len(),
			if self.len() == 1 { single } else { multiple }
		)
	}
}

impl Display for NbtList {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		let heterogeneous = self.is_heterogeneous();
		write!(f, "[")?;
		for (idx, element) in self.children().enumerate() {
			if heterogeneous && element.id() != NbtCompound::ID {
				write!(f, "{{'':{element}}}")?;
			} else {
				write!(f, "{element}")?;
			}
			if likely(idx < self.len() - 1) {
				write!(f, ",")?;
			}
		}
		write!(f, "]")
	}
}

impl NbtList {
	pub fn pretty_fmt(&self, f: &mut PrettyFormatter) {
		if self.is_empty() {
			f.write_str("[]")
		} else {
			let len = self.len();
			let heterogeneous = self.is_heterogeneous();
			f.write_str("[\n");
			f.increase();
			for (idx, element) in self.children().enumerate() {
				f.indent();
				if heterogeneous && element.id() != NbtCompound::ID {
					f.write_str("{ '': ");
					element.pretty_fmt(f);
					f.write_str(" }");
				} else {
					element.pretty_fmt(f);
				}
				if idx + 1 < len {
					f.write_str(",\n");
				} else {
					f.write_str("\n");
				}
			}
			f.decrease();
			f.indent();
			f.write_str("]");
		}
	}
}

impl NbtList {
	#[inline]
	pub fn render_root(&self, builder: &mut VertexBufferBuilder, str: &str, ctx: &mut RenderContext) {
		let mut remaining_scroll = builder.scroll() / 16;

		'head: {
			if remaining_scroll > 0 {
				remaining_scroll -= 1;
				ctx.skip_line_numbers(1);
				break 'head;
			}

			ctx.line_number();
			self.render_icon(ctx.pos(), BASE_Z, builder);
			if !self.is_empty() {
				ctx.draw_toggle(ctx.pos() - (16, 0), self.open, builder);
			}
			ctx.render_errors(ctx.pos(), builder);
			if ctx.forbid(ctx.pos()) {
				builder.settings(ctx.pos() + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "{str}: {}", self.value());
			}

			let pos = ctx.pos();
			if ctx.draw_held_entry_bar(ctx.pos() + (16, 16), builder, |x, y| pos + (16, 8) == (x, y), |x| self.can_insert(x)) {} else if self.height() == 1 && ctx.draw_held_entry_bar(ctx.pos() + (16, 16), builder, |x, y| pos + (16, 16) == (x, y), |x| self.can_insert(x)) {}

			ctx.offset_pos(0, 16);
		}

		if self.open {
			ctx.offset_pos(16, 0);

			for (idx, element) in self.children().enumerate() {
				if ctx.pos().y > builder.window_height() {
					break;
				}

				let height = element.height();
				if remaining_scroll >= height {
					remaining_scroll -= height;
					ctx.skip_line_numbers(element.true_height());
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
				ctx.check_for_key_duplicate(|_, _| false, false);
				element.render(
					&mut remaining_scroll,
					builder,
					None,
					idx == self.len() - 1,
					ctx,
				);

				let pos = ctx.pos();
				ctx.draw_held_entry_bar(pos, builder, |x, y| pos == (x, y + 8), |x| self.can_insert(x));
			}

			ctx.offset_pos(-16, 0);
		} else {
			ctx.skip_line_numbers(self.true_height() - 1);
		}
	}

	#[inline]
	pub fn render(&self, builder: &mut VertexBufferBuilder, name: Option<&str>, remaining_scroll: &mut usize, tail: bool, ctx: &mut RenderContext) {
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

			if ctx.draw_held_entry_bar(pos + (16, 16), builder, |x, y| pos + (16, 8) == (x, y), |x| self.can_insert(x)) {} else if self.height() == 1 && ctx.draw_held_entry_bar(pos + (16, 16), builder, |x, y| pos + (16, 16) == (x, y), |x| self.can_insert(x)) {}

			ctx.offset_pos(0, 16);
			y_before += 16;
		}

		let x_before = ctx.pos().x - 16;

		if self.open {
			ctx.offset_pos(16, 0);

			for (idx, element) in self.children().enumerate() {
				if ctx.pos().y > builder.window_height() {
					break;
				}

				let height = element.height();
				if *remaining_scroll >= height {
					*remaining_scroll -= height;
					ctx.skip_line_numbers(element.true_height());
					continue;
				}

				let pos = ctx.pos();
				ctx.draw_held_entry_bar(ctx.pos(), builder, |x, y| pos == (x, y), |x| self.can_insert(x));

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
				ctx.check_for_key_duplicate(|_, _| false, false);
				element.render(
					remaining_scroll,
					builder,
					None,
					tail && idx == self.len() - 1,
					ctx,
				);

				let pos = ctx.pos();
				ctx.draw_held_entry_bar(ctx.pos(), builder, |x, y| pos == (x, y + 8), |x| self.can_insert(x));
			}

			let difference = ctx.pos().y - y_before;
			if !tail {
				for i in 0..difference / 16 {
					builder.draw_texture((x_before, y_before + i * 16), CONNECTION_UV, (8, 16));
				}
			}

			ctx.offset_pos(-16, 0);
		} else {
			ctx.skip_line_numbers(self.true_height() - 1);
		}
	}

	#[inline]
	pub fn children(&self) -> Iter<'_, NbtElement> { self.elements.iter() }

	#[inline]
	pub fn children_mut(&mut self) -> IterMut<'_, NbtElement> { self.elements.iter_mut() }

	pub fn drop(&mut self, mut key: Option<CompactString>, mut element: NbtElement, y: &mut usize, depth: usize, target_depth: usize, mut line_number: usize, indices: &mut Vec<usize>) -> DropFn {
		let can_insert = self.can_insert(&element);
		if *y < 16 && *y >= 8 && depth == target_depth && can_insert {
			let before = (self.height(), self.true_height());
			indices.push(0);
			if let Err(element) = self.insert(0, element) { return DropFn::InvalidType((key, element)) }
			self.open = true;
			return DropFn::Dropped(
				self.height as usize - before.0,
				self.true_height as usize - before.1,
				None,
				line_number + 1,
				None,
			);
		} else if self.height() == 1 && *y < 24 && *y >= 16 && depth == target_depth && can_insert {
			let before = self.true_height();
			indices.push(self.len());
			if let Err(element) = self.insert(self.len(), element) {
				// indices are never used
				return DropFn::InvalidType((key, element));
			}
			self.open = true;
			return DropFn::Dropped(
				self.height as usize - 1,
				self.true_height as usize - before,
				None,
				line_number + before + 1,
				None,
			);
		}

		if *y < 16 {
			return DropFn::Missed((key, element));
		} else {
			*y -= 16;
		}

		if self.open && !self.is_empty() {
			indices.push(0);
			let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
			for (idx, value) in self.children_mut().enumerate() {
				*ptr = idx;
				let heights = (element.height(), element.true_height());
				if *y < 8 && depth == target_depth && can_insert {
					if let Err(element) = self.insert(idx, element) { return DropFn::InvalidType((key, element)) }
					return DropFn::Dropped(heights.0, heights.1, None, line_number + 1, None);
				} else if *y >= value.height() * 16 - 8 && *y < value.height() * 16 && depth == target_depth && can_insert {
					*ptr = idx + 1;
					let true_height = value.true_height();
					if let Err(element) = self.insert(idx + 1, element) { return DropFn::InvalidType((key, element)) }
					return DropFn::Dropped(heights.0, heights.1, None, line_number + true_height + 1, None);
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
					x @ DropFn::InvalidType(_) => return x,
					DropFn::Missed((k, e)) => {
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
		DropFn::Missed((key, element))
	}

	#[inline]
	pub fn shut(&mut self) {
		for element in self.children_mut() {
			if element.open() {
				element.shut();
			}
		}
		self.open = false;
		self.height = self.len() as u32 + 1;
	}

	#[inline]
	#[cfg(not(target_arch = "wasm32"))]
	pub fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		self.open = !self.is_empty();
		self.height = self.true_height;
		for element in self.children_mut() {
			element.expand(scope);
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
	pub fn render_icon(&self, pos: impl Into<(usize, usize)>, z: ZOffset, builder: &mut VertexBufferBuilder) { builder.draw_texture_z(pos, z, LIST_UV, (16, 16)); }

	#[inline]
	pub fn recache(&mut self) {
		let mut height = 1;
		let mut true_height = 1;
		for child in self.children() {
			height += child.height() as u32;
			true_height += child.true_height() as u32;
		}
		self.true_height = true_height;
		self.height = height;
		let mut max_depth = 0;
		if self.open() {
			for child in self.children() {
				max_depth = usize::max(max_depth, 16 + 4 + child.value_width());
				max_depth = usize::max(max_depth, 16 + child.max_depth());
			}
		}
		self.max_depth = max_depth as u32;
		self.recache_elements_bitset();
	}
	
	#[inline]
	pub fn recache_elements_bitset(&mut self) {
		let mut elements_bitset = 0_u16;
		for element in self.children() {
			elements_bitset |= 1 << element.id();
		}
		self.elements_bitset = elements_bitset;
	}

	#[inline]
	#[must_use]
	pub const fn max_depth(&self) -> usize { self.max_depth as usize }
}
