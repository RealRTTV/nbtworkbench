use std::borrow::Cow;
use std::fmt::{Debug, Display, Formatter, Write};
use std::intrinsics::likely;

use fxhash::FxBuildHasher;
use indexmap::map::{Iter, IterMut};
use indexmap::IndexMap;

use crate::assets::{COMPOUND_UV, HEADER_SIZE};
use crate::decoder::Decoder;
use crate::elements::element_type::NbtElement;
use crate::encoder::write_string;
use crate::{DropFn, OptionExt, RenderContext, StrExt, VertexBufferBuilder};

#[derive(Clone)]
#[allow(clippy::module_name_repetitions)]
pub struct NbtCompound {
	pub entries: Box<IndexMap<Box<str>, NbtElement, FxBuildHasher>>,
	open: bool,
	height: usize,
	true_height: usize,
}

impl NbtCompound {
	pub const ID: u8 = 10;

	pub(in crate::elements) fn from_str0(mut s: &str) -> Option<(&str, Self)> {
		s = s.strip_prefix('{')?.trim_start();
		let mut compound = Self::new();
		while !s.starts_with('}') {
			let (key, s2) = s.snbt_string_read()?;
			s = s2.trim_start().strip_prefix(':')?.trim_start();
			let (s2, value) = NbtElement::from_str0(s)?;
			compound.put(key.into_boxed_str(), value);
			s = s2.trim_start();
			if let Some(s2) = s.strip_prefix(',') {
				s = s2.trim_start();
			} else {
				break;
			}
		}
		let s = s.strip_prefix('}')?;
		compound.entries.shrink_to_fit();
		Some((s, compound))
	}

	pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
		let mut compound = Self::new();
		unsafe {
			decoder.assert_len(1)?;
			let mut current_element = decoder.u8();
			while current_element != 0 {
				decoder.assert_len(2)?;
				let key = decoder.string()?;
				let value = NbtElement::from_bytes(current_element, decoder)?;
				compound.put(key, value);
				match decoder.assert_len(1) {
					Some(()) => {}
					None => break, // wow mojang, saving one byte, so cool of you
				};
				current_element = decoder.u8();
			}
			Some(compound)
		}
	}

	pub fn to_bytes<W: std::io::Write>(&self, writer: &mut W) {
		for (key, value) in self.children() {
			let _ = writer.write(&[value.id()]);
			write_string(writer, key);
			NbtElement::to_bytes(value, writer);
		}
		let _ = writer.write(&[0x00]);
	}
}

impl Default for NbtCompound {
	#[inline]
	fn default() -> Self {
		Self {
			height: 1,
			entries: Box::new(IndexMap::with_hasher(FxBuildHasher::default())),
			open: false,
			true_height: 1,
		}
	}
}

impl NbtCompound {
	#[inline]
	#[must_use]
	pub fn new() -> Self {
		Self::default()
	}

	#[inline]
	pub fn insert_full(&mut self, idx: usize, mut str: String, value: NbtElement) {
		loop {
			if self.entries.contains_key(str.as_str()) {
				str += " - Copy";
			} else {
				self.height += value.height();
				self.true_height += value.true_height();
				let (new_idx, _) = self.entries.insert_full(str.into_boxed_str(), value);
				self.entries.move_index(new_idx, idx);
				return;
			}
		}
	}

	#[inline] // must only use for files, unless im stupid
	pub fn put(&mut self, str: Box<str>, element: NbtElement) {
		self.height += element.height();
		self.true_height += element.true_height();
		if let Some(element) = self.entries.insert(str, element) {
			self.height -= element.height();
			self.true_height -= element.true_height();
		}
	}

	#[inline]
	pub fn remove_idx(&mut self, idx: usize) -> Option<(Box<str>, NbtElement)> {
		self.entries.shift_remove_index(idx)
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
		self.open = !self.open && !self.entries.is_empty();
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

	pub fn update_key(&mut self, idx: usize, key: Box<str>) -> Option<Box<str>> {
		if !self.entries.contains_key(key.as_ref()) && let Some((old_key, value)) = self.entries.swap_remove_index(idx) {
			let (end_idx, _) = self.entries.insert_full(key, value);
			self.entries.swap_indices(end_idx, idx);
			Some(old_key)
		} else {
			None
		}
	}

	#[inline]
	#[must_use]
	pub fn len(&self) -> usize {
		self.entries.len()
	}

	#[inline]
	#[must_use]
	pub fn is_empty(&self) -> bool {
		self.entries.is_empty()
	}

	#[inline]
	#[must_use]
	pub fn get(&self, idx: usize) -> Option<(&str, &NbtElement)> {
		self.entries.get_index(idx).map(|(k, v)| (k.as_ref(), v))
	}

	#[inline]
	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<(&str, &mut NbtElement)> {
		self.entries.get_index_mut(idx).map(|(k, v)| (k.as_ref(), v))
	}

	#[inline]
	#[must_use]
	pub fn value(&self) -> String {
		format!("{} {}", self.len(), if self.len() == 1 { "entry" } else { "entries" })
	}

	#[inline]
	pub fn render_root(&self, builder: &mut VertexBufferBuilder, str: &str, ctx: &mut RenderContext) {
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
			builder.draw_texture_z((builder.text_coords.0 + 4, y_offset - 2), 1.0, (115, 32), (2, 2)); // fun hack for connection
			builder.draw_texture((x_offset, y_offset), (64, 16), (16, 16));
			ctx.highlight((x_offset, y_offset), str.width(), builder);
			builder.draw_texture((x_offset - 16, y_offset), (80, 16), (16, 9));
			if !self.is_empty() {
				builder.draw_texture((x_offset - 16, y_offset), (96 + self.open as usize * 16, 16), (16, 16));
			}
			if ctx.forbid(x_offset, y_offset, builder) {
				builder.settings(x_offset + 20, y_offset, false);
				let _ = write!(builder, "{} [{}]", str, self.value());
			}

			if ctx.ghost(x_offset + 16, y_offset + 16, builder, |x, y| x == x_offset + 16 && y == y_offset + 8) {
				builder.draw_texture((x_offset, y_offset + 16), (80, 16), (16, (self.height() != 1) as usize * 7 + 9));
				y_offset += 16;
			}

			if self.height() == 1 && ctx.ghost(x_offset + 16, y_offset + 16, builder, |x, y| x == x_offset + 16 && y == y_offset + 16) {
				builder.draw_texture((x_offset, y_offset + 16), (80, 16), (16, 9));
				y_offset += 16;
			}

			y_offset += 16;
		}

		x_offset += 16;

		if self.open {
			for (idx, (name, value)) in self.entries.iter().enumerate() {
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
					builder.draw_texture((x_offset - 16, y_offset), (80, 16), (16, 16));
					y_offset += 16;
				}

				let ghost_tail_mod = if let Some((_, x, y)) = ctx.ghost && x == x_offset && y == y_offset + height * 16 - remaining_scroll * 16 - 8 {
					false
				} else {
					true
				};

				if remaining_scroll == 0 {
					builder.draw_texture((x_offset - 16, y_offset), (80, 16), (16, (idx != self.len() - 1 || !ghost_tail_mod) as usize * 7 + 9));
				}
				ctx.check_key(|f| self.entries.contains_key(f) && name.as_ref() != f);
				value.render(
					&mut x_offset,
					&mut y_offset,
					&mut remaining_scroll,
					builder,
					Some(name.as_ref()),
					idx == self.len() - 1 && ghost_tail_mod,
					ctx,
				);

				if ctx.ghost(x_offset, y_offset, builder, |x, y| x_offset == x && y_offset - 8 == y) {
					builder.draw_texture((x_offset - 16, y_offset), (80, 16), (16, (idx != self.len() - 1) as usize * 7 + 9));
					y_offset += 16;
				}
			}
		} else {
			// unrequired tbh
			// line_number += self.true_height - 1;
		}
		// unrequired tbh
		// x_offset -= 16;
	}
}

impl Display for NbtCompound {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{{")?;
		for (idx, (key, value)) in self.entries.iter().enumerate() {
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
		let mut debug = f.debug_struct("");
		for (key, element) in self.children() {
			let key = if key.needs_escape() { Cow::Owned(format!("{key:?}")) } else { Cow::Borrowed(key.as_ref()) };
			debug.field(key.as_ref(), element);
		}
		debug.finish()
	}
}

impl NbtCompound {
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

			if ctx.ghost(*x_offset + 16, *y_offset + 16, builder, |x, y| x == *x_offset + 16 && y == *y_offset + 8) {
				builder.draw_texture((*x_offset, *y_offset + 16), (80, 16), (16, (self.height() != 1) as usize * 7 + 9));
				if !tail {
					builder.draw_texture((*x_offset - 16, *y_offset + 16), (80, 16), (8, 16));
				}
				*y_offset += 16;
			} else if self.height() == 1 && ctx.ghost(*x_offset + 16, *y_offset + 16, builder, |x, y| x == *x_offset + 16 && y == *y_offset + 16) {
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

			for (idx, (key, entry)) in self.entries.iter().enumerate() {
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
					builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, 16));
					*y_offset += 16;
				}

				let ghost_tail_mod = if let Some((_, x, y)) = ctx.ghost && x == *x_offset && y == *y_offset + height * 16 - *remaining_scroll * 16 - 8 {
					false
				} else {
					true
				};

				if *remaining_scroll == 0 {
					builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, (idx != self.len() - 1 || !ghost_tail_mod) as usize * 7 + 9));
				}
				ctx.check_key(|f| self.entries.contains_key(f) && key.as_ref() != f);
				entry.render(x_offset, y_offset, remaining_scroll, builder, Some(key.as_ref()), tail && idx == self.len() - 1 && ghost_tail_mod, ctx);

				if ctx.ghost(*x_offset, *y_offset, builder, |x, y| *x_offset == x && *y_offset - 8 == y) {
					builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, (idx != self.len() - 1) as usize * 7 + 9));
					*y_offset += 16;
				}
			}

			if !tail {
				let len = (*y_offset - y_before) / 16;
				for i in 0..len {
					builder.draw_texture((x_before, y_before + i * 16), (80, 16), (8, 16));
				}
			}

			*x_offset -= 16;
		} else {
			ctx.line_number += self.true_height - 1;
		}
	}

	#[inline]
	#[must_use]
	pub fn children(&self) -> Iter<'_, Box<str>, NbtElement> {
		self.entries.iter()
	}

	#[inline]
	#[must_use]
	pub fn children_mut(&mut self) -> IterMut<'_, Box<str>, NbtElement> {
		self.entries.iter_mut()
	}

	#[inline]
	pub fn drop(&mut self, mut key: Option<Box<str>>, mut element: NbtElement, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> DropFn {
		if *y < 16 && *y >= 8 && depth == target_depth {
			let before = (self.height(), self.true_height());
			self.open = true;
			self.insert_full(0, key.map_or_else(|| "_".to_owned(), str::into_string), element);
			indices.push(0);
			return DropFn::Dropped(self.height - before.0, self.true_height - before.1, unsafe {
				Some(self.get(0).panic_unchecked("We just added it").0.to_string().into_boxed_str())
			});
		}

		if self.height() == 1 && *y < 24 && *y >= 16 && depth == target_depth {
			let before = self.true_height();
			self.open = true;
			indices.push(self.len());
			self.insert_full(self.len(), key.map_or_else(|| "_".to_owned(), str::into_string), element);
			return DropFn::Dropped(self.height - 1, self.true_height - before, unsafe {
				Some(self.get(self.len() - 1).panic_unchecked("We just added it").0.to_string().into_boxed_str())
			});
		}

		if *y < 16 {
			return DropFn::Missed(key, element);
		} else {
			*y -= 16;
		}

		if self.open && !self.is_empty() {
			indices.push(0);
			let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
			for (idx, (_, value)) in self.entries.iter_mut().enumerate() {
				*ptr = idx;
				let heights = (element.height(), element.true_height());
				if *y < 8 && depth == target_depth {
					*y = 0;
					self.insert_full(idx, key.map_or_else(|| "_".to_owned(), str::into_string), element);
					return DropFn::Dropped(heights.0, heights.1, unsafe { Some(self.get(idx).panic_unchecked("We just added it").0.to_string().into_boxed_str()) });
				} else if *y >= value.height() * 16 - 8 && *y < value.height() * 16 && depth == target_depth {
					*y = 0;
					*ptr = idx + 1;
					self.insert_full(idx + 1, key.map_or_else(|| "_".to_owned(), str::into_string), element);
					return DropFn::Dropped(heights.0, heights.1, unsafe {
						Some(self.get(idx + 1).panic_unchecked("We just added it").0.to_string().into_boxed_str())
					});
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
		for (_, element) in self.children_mut() {
			element.shut();
		}
		self.open = false;
		self.height = self.len() + 1;
	}

	#[inline]
	pub fn render_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
		builder.draw_texture((x, y), COMPOUND_UV, (16, 16));
	}
}
