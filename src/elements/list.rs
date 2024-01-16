use compact_str::{format_compact, CompactString};
use std::alloc::{alloc, Layout};
use std::fmt::{Debug, Display, Formatter, Write};
use std::intrinsics::likely;
use std::slice::{Iter, IterMut};
use std::thread::Scope;

use crate::assets::{BASE_TEXT_Z, BASE_Z, CONNECTION_UV, LIST_UV};
use crate::decoder::Decoder;
use crate::elements::chunk::NbtChunk;
use crate::elements::element::{id_to_string_name, NbtElement};
use crate::encoder::UncheckedBufWriter;
use crate::{DropFn, OptionExt, RenderContext, StrExt, VertexBufferBuilder};

#[allow(clippy::module_name_repetitions)]
#[repr(C)]
pub struct NbtList {
	pub elements: Box<Vec<NbtElement>>,
	height: u32,
	true_height: u32,
	max_depth: u32,
	pub element: u8,
	open: bool,
}

impl Clone for NbtList {
	#[allow(clippy::cast_ptr_alignment)]
	#[inline]
	fn clone(&self) -> Self {
		unsafe {
			let len = self.elements.len();
			let ptr = alloc(Layout::array::<NbtElement>(len).unwrap_unchecked()).cast::<NbtElement>();
			let boxx = alloc(Layout::new::<Vec<NbtElement>>()).cast::<Vec<NbtElement>>();
			for n in 0..len {
				ptr.add(n).write(self.elements.get_unchecked(n).clone());
			}
			boxx.write(Vec::from_raw_parts(ptr.cast::<NbtElement>(), len, len));
			Self {
				elements: Box::from_raw(boxx),
				height: self.height,
				true_height: self.true_height,
				max_depth: self.max_depth,
				element: self.element,
				open: self.open,
			}
		}
	}
}

impl NbtList {
	pub const ID: u8 = 9;
	pub(in crate::elements) fn from_str0(mut s: &str) -> Option<(&str, Self)> {
		s = s.strip_prefix('[')?.trim_start();
		let mut list = Self::new(vec![], 0);
		while !s.starts_with(']') {
			let (s2, element) = NbtElement::from_str0(s)?;
			list.insert(list.len(), element).ok()?;
			s = s2.trim_start();
			if let Some(s2) = s.strip_prefix(',') {
				s = s2.trim_start();
			} else {
				break;
			}
		}
		let s = s.strip_prefix(']')?;
		list.elements.shrink_to_fit();
		Some((s, list))
	}
	#[allow(clippy::cast_ptr_alignment)]
	#[inline]
	pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
		unsafe {
			decoder.assert_len(5)?;
			let element = decoder.u8();
			let len = decoder.u32() as usize;
			let ptr = alloc(Layout::array::<NbtElement>(len).unwrap_unchecked()).cast::<NbtElement>();
			let mut true_height = 1;
			for n in 0..len {
				let element = NbtElement::from_bytes(element, decoder)?;
				true_height += element.true_height() as u32;
				ptr.add(n).write(element);
			}
			let boxx = alloc(Layout::new::<Vec<NbtElement>>()).cast::<Vec<NbtElement>>();
			boxx.write(Vec::from_raw_parts(ptr, len, len));
			Some(Self {
				elements: Box::from_raw(boxx),
				height: 1 + len as u32,
				true_height,
				max_depth: 0,
				element,
				open: false,
			})
		}
	}
	pub fn to_bytes(&self, writer: &mut UncheckedBufWriter) {
		writer.write(&[self.element]);
		writer.write(&(self.len() as u32).to_be_bytes());
		for element in self.elements.iter() {
			element.to_bytes(writer);
		}
	}
}

impl NbtList {
	#[inline]
	pub fn new(elements: Vec<NbtElement>, element: u8) -> Self {
		Self {
			height: elements.iter().map(NbtElement::height).sum::<usize>() as u32 + 1,
			true_height: elements.iter().map(NbtElement::true_height).sum::<usize>() as u32 + 1,
			elements: Box::new(elements),
			element,
			open: false,
			max_depth: 0,
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
	pub const fn true_height(&self) -> usize {
		self.true_height as usize
	}

	#[inline]
	#[must_use]
	pub const fn id(&self) -> u8 {
		self.element
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
		self.elements.len()
	}

	#[inline]
	#[must_use]
	pub fn is_empty(&self) -> bool {
		self.elements.is_empty()
	}

	/// # Errors
	///
	/// * `NbtElement::id` of `value` != `self.id()`
	#[inline]
	pub fn insert(&mut self, idx: usize, value: NbtElement) -> Result<(), NbtElement> {
		if self.element == value.id() || self.elements.is_empty() {
			self.element = value.id();
			self.increment(value.height(), value.true_height());
			unsafe {
				self.elements.try_reserve_exact(1).unwrap_unchecked();
			}
			self.elements.insert(idx, value);
			Ok(())
		} else {
			Err(value)
		}
	}

	#[inline]
	#[must_use]
	pub fn remove(&mut self, idx: usize) -> NbtElement {
		let removed = self.elements.remove(idx);
		self.elements.shrink_to_fit();
		removed
	}

	#[inline]
	#[must_use]
	pub fn get(&self, idx: usize) -> Option<&NbtElement> {
		self.elements.get(idx)
	}

	#[inline]
	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<&mut NbtElement> {
		self.elements.get_mut(idx)
	}

	#[inline]
	#[must_use]
	pub fn value(&self) -> CompactString {
		let (single, multiple) = id_to_string_name(self.element);
		format_compact!("{} {}", self.len(), if self.len() == 1 { single } else { multiple })
	}
}

impl Display for NbtList {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "[")?;
		for (idx, element) in self.children().enumerate() {
			write!(f, "{element}")?;
			if likely(idx < self.len() - 1) {
				write!(f, ",")?;
			}
		}
		write!(f, "]")
	}
}

impl Debug for NbtList {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.debug_list().entries(self.children()).finish()
	}
}

impl NbtList {
	#[inline]
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
				|id| (id != NbtChunk::ID) && (id == self.element || self.is_empty()),
			) {
				builder.draw_texture(ctx.pos() + (0, 16), CONNECTION_UV, (16, (self.height() != 1) as usize * 7 + 9));
				if !tail {
					builder.draw_texture(ctx.pos() - (16, 0) + (0, 16), CONNECTION_UV, (8, 16));
				}
				ctx.y_offset += 16;
			} else if self.height() == 1
				&& ctx.ghost(
					ctx.pos() + (16, 16),
					builder,
					|x, y| pos + (16, 16) == (x, y),
					|id| (id != NbtChunk::ID) && (id == self.element || self.is_empty()),
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

			for (idx, element) in self.children().enumerate() {
				if ctx.y_offset > builder.window_height() {
					break;
				}

				let height = element.height();
				if *remaining_scroll >= height {
					*remaining_scroll -= height;
					ctx.skip_line_numbers(element.true_height());
					continue;
				}

				let pos = ctx.pos();
				if ctx.ghost(ctx.pos(), builder, |x, y| pos == (x, y), |id| (id != NbtChunk::ID) && (id == self.element || self.is_empty())) {
					builder.draw_texture(ctx.pos() - (16, 0), CONNECTION_UV, (16, 16));
					ctx.y_offset += 16;
				}

				let ghost_tail_mod = if let Some((id, x, y, _)) = ctx.ghost && (id == self.element || self.is_empty()) && id != NbtChunk::ID && ctx.pos() + (0, height * 16 - *remaining_scroll * 16 - 8) == (x, y) {
					false
				} else {
					true
				};

				if *remaining_scroll == 0 {
					builder.draw_texture(ctx.pos() - (16, 0), CONNECTION_UV, (16, (!(idx == self.len() - 1 && ghost_tail_mod)) as usize * 7 + 9));
				}
				ctx.check_for_key_duplicate(|_, _| false, false);
				element.render(remaining_scroll, builder, None, tail && idx == self.len() - 1 && ghost_tail_mod, ctx);

				let pos = ctx.pos();
				if ctx.ghost(ctx.pos(), builder, |x, y| pos == (x, y + 8), |id| (id != NbtChunk::ID) && (id == self.element || self.is_empty())) {
					builder.draw_texture(ctx.pos() - (16, 0), CONNECTION_UV, (16, (idx != self.len() - 1) as usize * 7 + 9));
					ctx.y_offset += 16;
				}
			}

			let difference = ctx.y_offset - y_before;
			if !tail {
				for i in 0..difference / 16 {
					builder.draw_texture((x_before, y_before + i * 16), CONNECTION_UV, (8, 16));
				}
			}

			ctx.x_offset -= 16;
		} else {
			ctx.skip_line_numbers(self.true_height() - 1);
		}
	}

	#[inline]
	pub fn children(&self) -> ValueIterator {
		ValueIterator::Generic(self.elements.iter())
	}

	#[inline]
	pub fn children_mut(&mut self) -> ValueMutIterator {
		ValueMutIterator::Generic(self.elements.iter_mut())
	}

	pub fn drop(&mut self, mut key: Option<CompactString>, mut element: NbtElement, y: &mut usize, depth: usize, target_depth: usize, mut line_number: usize, indices: &mut Vec<usize>) -> DropFn {
		if *y < 16 && *y >= 8 && depth == target_depth {
			let before = (self.height(), self.true_height());
			indices.push(0);
			if let Err(element) = self.insert(0, element) {
				return DropFn::InvalidType(key, element);
			}
			self.open = true;
			return DropFn::Dropped(self.height as usize - before.0, self.true_height as usize - before.1, None, line_number + 1);
		} else if self.height() == 1 && *y < 24 && *y >= 16 && depth == target_depth {
			let before = self.true_height();
			indices.push(self.len());
			if let Err(element) = self.insert(self.len(), element) {
				// indices are never used
				return DropFn::InvalidType(key, element);
			}
			self.open = true;
			return DropFn::Dropped(self.height as usize - 1, self.true_height as usize - before, None, line_number + before + 1);
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
				if *y < 8 && depth == target_depth {
					if let Err(element) = self.insert(idx, element) {
						return DropFn::InvalidType(key, element);
					}
					return DropFn::Dropped(heights.0, heights.1, None, line_number + 1);
				} else if *y >= value.height() * 16 - 8 && *y < value.height() * 16 && depth == target_depth {
					*ptr = idx + 1;
					let true_height = value.true_height();
					if let Err(element) = self.insert(idx + 1, element) {
						return DropFn::InvalidType(key, element);
					}
					return DropFn::Dropped(heights.0, heights.1, None, line_number + true_height + 1);
				}

				match value.drop(key, element, y, depth + 1, target_depth, line_number + 1, indices) {
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
		for element in self.children_mut() {
			element.shut();
		}
		self.open = false;
		self.height = self.len() as u32 + 1;
	}

	#[inline]
	pub fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		self.open = !self.is_empty();
		self.height = self.true_height;
		for element in self.children_mut() {
			element.expand(scope);
		}
	}

	#[inline]
	pub fn render_icon(pos: impl Into<(usize, usize)>, z: u8, builder: &mut VertexBufferBuilder) {
		builder.draw_texture_z(pos, z, LIST_UV, (16, 16));
	}

	#[inline]
	pub fn recache_depth(&mut self) {
		let mut max_depth = 0;
		if self.open() {
			for child in self.children() {
				max_depth = usize::max(max_depth, 16 + 4 + child.value().0.width());
				max_depth = usize::max(max_depth, 16 + child.max_depth());
			}
		}
		self.max_depth = max_depth as u32;
	}

	#[inline]
	#[must_use]
	pub const fn max_depth(&self) -> usize {
		self.max_depth as usize
	}
}

#[must_use]
pub enum ValueIterator<'a> {
	Generic(Iter<'a, NbtElement>),
	Region(&'a [NbtElement; 32 * 32], Iter<'a, u16>),
}

impl<'a> Iterator for ValueIterator<'a> {
	type Item = &'a NbtElement;

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			Self::Generic(iter) => iter.next(),
			Self::Region(array, iter) => unsafe { iter.next().map(|&x| array.get(x as usize).panic_unchecked("Map index out of bounds")) },
		}
	}
}

impl<'a> ExactSizeIterator for ValueIterator<'a> {
	fn len(&self) -> usize {
		match self {
			Self::Generic(generic) => generic.len(),
			Self::Region(_, iter) => iter.len(),
		}
	}
}

impl<'a> DoubleEndedIterator for ValueIterator<'a> {
	fn next_back(&mut self) -> Option<Self::Item> {
		match self {
			Self::Generic(iter) => iter.next_back(),
			Self::Region(array, iter) => unsafe { iter.next_back().map(|&x| array.get(x as usize).panic_unchecked("Map index out of bounds")) },
		}
	}
}

#[must_use]
pub enum ValueMutIterator<'a> {
	Generic(IterMut<'a, NbtElement>),
	Region(&'a mut [NbtElement; 32 * 32], Iter<'a, u16>),
}

impl<'a> Iterator for ValueMutIterator<'a> {
	type Item = &'a mut NbtElement;

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			Self::Generic(iter) => iter.next(),
			// SAFETY: the only problem here is aliasing, which is assumed to not occur due to `map` indices not being identical, if they are identical it's UB, so all we need is to check if we have two pointers to the same data, which doesn't occur in mutation
			Self::Region(array, iter) => unsafe {
				let chunk = iter.next().map(|&x| array.get_mut(x as usize).panic_unchecked("Map index out of bounds"));
				let ptr = core::mem::transmute::<_, *mut NbtElement>(chunk);
				core::mem::transmute::<_, Option<&mut NbtElement>>(ptr)
			},
		}
	}
}

impl<'a> ExactSizeIterator for ValueMutIterator<'a> {
	fn len(&self) -> usize {
		match self {
			Self::Generic(generic) => generic.len(),
			Self::Region(_, iter) => iter.len(),
		}
	}
}

impl<'a> DoubleEndedIterator for ValueMutIterator<'a> {
	fn next_back(&mut self) -> Option<Self::Item> {
		match self {
			Self::Generic(iter) => iter.next_back(),
			// SAFETY: the only problem here is aliasing, which is assumed to not occur due to `map` indices not being identical, if they are identical it's UB, so all we need is to check if we have two pointers to the same data, which doesn't occur in mutation
			Self::Region(array, iter) => unsafe {
				let chunk = iter.next_back().map(|&x| array.get_mut(x as usize).panic_unchecked("Map index out of bounds"));
				let ptr = core::mem::transmute::<_, *mut NbtElement>(chunk);
				core::mem::transmute::<_, Option<&mut NbtElement>>(ptr)
			},
		}
	}
}
