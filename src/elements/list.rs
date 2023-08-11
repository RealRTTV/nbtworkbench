use std::fmt::{Debug, Display, Formatter, Write};
use std::intrinsics::likely;
use std::slice::{Iter, IterMut};

use crate::assets::LIST_UV;
use crate::decoder::Decoder;
use crate::elements::element_type::NbtElement;
use crate::{DropFn, RenderContext, StrExt, VertexBufferBuilder};

#[derive(Clone)]
#[allow(clippy::module_name_repetitions)]
pub struct NbtList {
	pub elements: Vec<NbtElement>,
	element: u8,
	open: bool,
	height: usize,
	true_height: usize,
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

	pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
		unsafe {
			decoder.assert_len(5)?;
			let element = decoder.u8();
			let len = decoder.u32() as usize;
			let mut elements = Vec::with_capacity(len);
			for _ in 0..len {
				elements.push(NbtElement::from_bytes(element, decoder)?);
			}
			Some(Self::new(elements, element))
		}
	}

	pub fn to_bytes<W: std::io::Write>(&self, writer: &mut W) {
		let _ = writer.write(&[self.element]);
		let _ = writer.write(&(self.len() as u32).to_be_bytes());
		for element in &self.elements {
			NbtElement::to_bytes(element, writer);
		}
	}
}

impl NbtList {
	#[inline]
	pub fn new(elements: Vec<NbtElement>, element: u8) -> Self {
		Self {
			height: elements.iter().map(NbtElement::height).sum::<usize>() + 1,
			true_height: elements.iter().map(NbtElement::true_height).sum::<usize>() + 1,
			elements,
			element,
			open: false,
		}
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
	#[must_use]
	pub const fn id(&self) -> u8 {
		self.element
	}

	#[inline]
	pub fn toggle(&mut self) -> Option<()> {
		self.open = !self.open && !self.elements.is_empty();
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
			self.elements.insert(idx, value);
			Ok(())
		} else {
			Err(value)
		}
	}

	#[inline]
	pub fn remove(&mut self, idx: usize) -> NbtElement {
		self.elements.remove(idx)
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
	pub fn value(&self) -> String {
		format!("{} {}", self.len(), if self.len() == 1 { "entry" } else { "entries" })
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
	pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut usize, y_offset: &mut usize, name: Option<&str>, remaining_scroll: &mut usize, tail: bool, ctx: &mut RenderContext) {
		let mut y_before = *y_offset;

		'head: {
			if *remaining_scroll > 0 {
				*remaining_scroll -= 1;
				ctx.line_number += 1;
				break 'head;
			}

			ctx.line_number(*y_offset, builder);
			Self::render_icon(*x_offset, *y_offset, builder);
			ctx.highlight((*x_offset, *y_offset), name.map_or(0, StrExt::width), builder);
			if !self.is_empty() {
				builder.draw_texture((*x_offset - 16, *y_offset), (96 + self.open as usize * 16, 16), (16, 16));
			}
			if ctx.forbid(*x_offset, *y_offset, builder) {
				builder.settings(*x_offset + 20, *y_offset, false);
				let _ = match name {
					Some(x) => write!(builder, "{x}: {}", self.value()),
					None => write!(builder, "{}", self.value()),
				};
			}

			if ctx.ghost.is_some_and(|(id, _, _)| id == self.element || self.is_empty()) && ctx.ghost(*x_offset + 16, *y_offset + 16, builder, |x, y| x == *x_offset + 16 && y == *y_offset + 8) {
				builder.draw_texture((*x_offset, *y_offset + 16), (80, 16), (16, (self.height() != 1) as usize * 7 + 9));
				if !tail {
					builder.draw_texture((*x_offset - 16, *y_offset + 16), (80, 16), (8, 16));
				}
				*y_offset += 16;
			} else if self.height() == 1
				&& ctx.ghost.is_some_and(|(id, _, _)| id == self.element || self.is_empty())
				&& ctx.ghost(*x_offset + 16, *y_offset + 16, builder, |x, y| x == *x_offset + 16 && y == *y_offset + 16)
			{
				builder.draw_texture((*x_offset, *y_offset + 16), (80, 16), (16, 9));
				if !tail {
					builder.draw_texture((*x_offset - 16, *y_offset + 16), (80, 16), (8, 16));
				}
				*y_offset += 16;
			}

			*y_offset += 16;
			y_before += 16;
		}

		let x_before = *x_offset - 16;

		if self.open {
			*x_offset += 16;

			for (idx, element) in self.children().enumerate() {
				if *y_offset > builder.window_height() {
					break;
				}

				let height = element.height();
				if *remaining_scroll >= height {
					*remaining_scroll -= height;
					ctx.line_number += element.true_height();
					continue;
				}

				if ctx.ghost.is_some_and(|(id, _, _)| id == self.element || self.is_empty()) && ctx.ghost(*x_offset, *y_offset, builder, |x, y| *x_offset == x && *y_offset == y) {
					builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, 16));
					*y_offset += 16;
				}

				let ghost_tail_mod = if let Some((id, x, y)) = ctx.ghost && (id == self.element || self.is_empty()) && x == *x_offset && y == *y_offset + height * 16 - *remaining_scroll * 16 - 8 {
					false
				} else {
					true
				};

				if *remaining_scroll == 0 {
					builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, (!(idx == self.len() - 1 && ghost_tail_mod)) as usize * 7 + 9));
				}
				ctx.check_key(|_| false);
				element.render(x_offset, y_offset, remaining_scroll, builder, None, tail && idx == self.len() - 1 && ghost_tail_mod, ctx);

				if ctx.ghost.is_some_and(|(id, _, _)| id == self.element || self.is_empty()) && ctx.ghost(*x_offset, *y_offset, builder, |x, y| *x_offset == x && *y_offset - 8 == y) {
					builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, (idx != self.len() - 1) as usize * 7 + 9));
					*y_offset += 16;
				}
			}

			let difference = *y_offset - y_before;
			if !tail {
				for i in 0..difference / 16 {
					builder.draw_texture((x_before, y_before + i * 16), (80, 16), (8, 16));
				}
			}

			*x_offset -= 16;
		} else {
			ctx.line_number += self.true_height - 1;
		}
	}

	#[inline]
	pub fn children(&self) -> Iter<'_, NbtElement> {
		self.elements.iter()
	}

	#[inline]
	pub fn children_mut(&mut self) -> IterMut<'_, NbtElement> {
		self.elements.iter_mut()
	}

	pub fn drop(&mut self, mut key: Option<Box<str>>, mut element: NbtElement, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> DropFn {
		if *y < 16 && *y >= 8 && depth == target_depth {
			let before = (self.height(), self.true_height());
			indices.push(0);
			if let Err(element) = self.insert(0, element) {
				return DropFn::InvalidType(key, element);
			}
			self.open = true;
			return DropFn::Dropped(self.height - before.0, self.true_height - before.1, None);
		} else if self.height() == 1 && *y < 24 && *y >= 16 && depth == target_depth {
			let before = self.true_height();
			indices.push(self.len());
			if let Err(element) = self.insert(self.len(), element) {
				// indices are never used
				return DropFn::InvalidType(key, element);
			}
			self.open = true;
			return DropFn::Dropped(self.height - 1, self.true_height - before, None);
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
					return DropFn::Dropped(heights.0, heights.1, None);
				} else if *y >= value.height() * 16 - 8 && *y < value.height() * 16 && depth == target_depth {
					*ptr = idx + 1;
					if let Err(element) = self.insert(idx + 1, element) {
						return DropFn::InvalidType(key, element);
					}
					return DropFn::Dropped(heights.0, heights.1, None);
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

	pub fn shut(&mut self) {
		for element in self.children_mut() {
			element.shut();
		}
		self.open = false;
		self.height = self.len() + 1;
	}

	#[inline]
	pub fn render_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
		builder.draw_texture((x, y), LIST_UV, (16, 16));
	}
}
