macro_rules! array {
	($element_field:ident, $name:ident, $t:ty, $my_id:literal, $id:literal, $char:literal, $uv:ident, $element_uv:ident, $default_snbt_integer:ident, $try_into_element:ident) => {
		#[repr(C)]
		pub struct $name {
			pub(in crate::elements) values: Box<Vec<NbtElement>>,
			max_depth: u32,
			open: bool,
		}

		impl $name {
			pub fn matches(&self, other: &Self) -> bool {
				if self.values.len() != other.values.len() {
					return false
				}

				for (a, b) in self.values.iter().zip(other.values.iter()) {
					if !a.matches(b) {
						return false
					}
				}

				true
			}
		}

		impl PartialEq for $name {
			fn eq(&self, other: &Self) -> bool {
				self.values.eq(&other.values)
			}
		}

		impl Clone for $name {
			#[allow(clippy::cast_ptr_alignment)]
			fn clone(&self) -> Self {
				unsafe {
					let len = self.values.len();
					let ptr = alloc(Layout::array::<NbtElement>(len).unwrap_unchecked()).cast::<NbtElement>();
					let boxx = alloc(Layout::new::<Vec<NbtElement>>()).cast::<Vec<NbtElement>>();
					Vec::as_ptr(&self.values).copy_to_nonoverlapping(ptr, len);
					boxx.write(Vec::from_raw_parts(ptr, len, len));
					Self {
						values: Box::from_raw(boxx),
						max_depth: self.max_depth,
						open: self.open,
					}
				}
			}
		}

		impl $name {
			#[must_use]
			pub fn new() -> Self {
				Self {
					values: Box::<Vec<NbtElement>>::default(),
					open: false,
					max_depth: 0,
				}
			}

			pub const ID: u8 = $my_id;

			pub(in $crate::elements) fn from_str0(mut s: &str) -> Result<(&str, Self), usize> {
				s = s
					.strip_prefix('[').ok_or(s.len())?
					.trim_start();
				s = s
					.strip_prefix(concat!($char, ";")).ok_or(s.len())?
					.trim_start();
				let mut array = Self::new();
				while !s.starts_with(']') {
					let (s2, element) = NbtElement::from_str0(s, NbtElement::$default_snbt_integer)?;
					let element = element.$try_into_element().ok_or(s.len())?;
					array.insert(array.len(), element).map_err(|_| s.len())?;
					s = s2.trim_start();
					if let Some(s2) = s.strip_prefix(',') {
						s = s2.trim_start();
					} else if s.starts_with(']') {
						break;
					}
				}
				array.values.shrink_to_fit();
				Ok((s.strip_prefix(']').ok_or(s.len())?, array))
			}

			pub fn from_bytes<'a, D: Decoder<'a>>(decoder: &mut D) -> NbtParseResult<Self> {
				use super::nbt_parse_result::*;

				unsafe {
					decoder.assert_len(4)?;
					let len = decoder.u32() as usize;
					decoder.assert_len(len * core::mem::size_of::<$t>())?;
					let vec = alloc(Layout::array::<NbtElement>(len).unwrap_unchecked()).cast::<NbtElement>();
					for idx in 0..len {
						let mut element = NbtElement {
							$element_field: core::mem::transmute(<$t>::from_ne_bytes(decoder.read_ne_bytes::<{ core::mem::size_of::<$t>() }>()))
						};
						element.set_id($id);
						vec.add(idx).write(element);
					}
					let boxx = alloc(Layout::new::<Vec<NbtElement>>()).cast::<Vec<NbtElement>>();
					boxx.write(Vec::from_raw_parts(vec, len, len));
					ok(Self {
						values: Box::from_raw(boxx),
						open: false,
						max_depth: 0,
					})
				}
			}

			pub fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) {
				writer.write(&(self.len() as u32).to_be_bytes());
				for entry in self.values.iter() {
					writer.write(&Self::transmute(entry).to_be_bytes());
				}
			}

			pub fn to_le_bytes(&self, writer: &mut UncheckedBufWriter) {
				writer.write(&(self.len() as u32).to_le_bytes());
				for entry in self.values.iter() {
					writer.write(&Self::transmute(entry).to_le_bytes());
				}
			}
		}

		impl $name {
			fn transmute(element: &NbtElement) -> $t { unsafe { element.$element_field.value } }

			pub fn increment(&mut self, _: usize, _: usize) {}

			pub fn decrement(&mut self, _: usize, _: usize) {}

			#[must_use]
			pub fn height(&self) -> usize {
				if self.open {
					self.len() + 1
				} else {
					1
				}
			}

			#[must_use]
			pub fn true_height(&self) -> usize { self.len() + 1 }

			pub fn toggle(&mut self) {
				self.open = !self.open && !self.is_empty();
			}

			#[must_use]
			pub const fn open(&self) -> bool { self.open }

			#[must_use]
			pub fn len(&self) -> usize { self.values.len() }

			#[must_use]
			pub fn is_empty(&self) -> bool { self.values.is_empty() }

			/// # Errors
			///
			/// * Element type was not supported for `$name`
			pub fn insert(&mut self, idx: usize, value: NbtElement) -> Result<Option<NbtElement>, NbtElement> {
				if value.id() == $id {
					// the time complexity is fine here
					unsafe {
						self.values.try_reserve_exact(1).unwrap_unchecked();
					}
					self.values.insert(idx, value);
					self.increment(1, 1);
					Ok(None)
				} else {
					Err(value)
				}
			}

			pub fn remove(&mut self, idx: usize) -> NbtElement {
				let removed = self.values.remove(idx);
				self.decrement(removed.height(), removed.true_height());
				self.values.shrink_to_fit();
				removed
			}

			pub fn replace(&mut self, idx: usize, value: NbtElement) -> Option<NbtElement> {
				if !self.can_insert(&value) || idx >= self.len() { return None; }
				self.increment(value.height(), value.true_height());
				let old = core::mem::replace(&mut self.values[idx], value);
				self.decrement(old.height(), old.true_height());
				Some(old)
			}

			pub fn render(&self, builder: &mut VertexBufferBuilder, key: Option<&str>, remaining_scroll: &mut usize, tail: bool, ctx: &mut RenderContext) {
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
						if let Some(key) = key {
							builder.color = TextColor::TreeKey.to_raw();
							let _ = write!(builder, "{key}: ");
						};

						builder.color = TextColor::TreeKey.to_raw();
						let _ = write!(builder, "{}", self.value());
					}

					if ctx.draw_held_entry_bar(pos + (16, 16), builder, |x, y| pos == (x - 16, y - 8), |x| self.can_insert(x)) {} else if self.height() == 1 && ctx.draw_held_entry_bar(pos + (16, 16), builder, |x, y| pos == (x - 16, y - 16), |x| self.can_insert(x)) {}

					ctx.offset_pos(0, 16);
				}

				if self.open {
					ctx.offset_pos(16, 0);

					for (idx, element) in self.children().enumerate() {
						let pos = ctx.pos();
						if pos.y > builder.window_height() {
							break;
						}

						if *remaining_scroll > 0 {
							*remaining_scroll -= 1;
							ctx.skip_line_numbers(1);
							continue;
						}

						ctx.draw_held_entry_bar(pos, builder, |x, y| pos == (x, y), |x| self.can_insert(x));

						builder.draw_texture(
							pos - (16, 0),
							CONNECTION_UV,
							(
								16,
								(idx != self.len() - 1) as usize * 7 + 9,
							),
						);
						if !tail {
							builder.draw_texture(pos - (32, 0), CONNECTION_UV, (8, 16));
						}

						ctx.line_number();
						Self::render_element_icon(pos, builder);
						ctx.check_for_invalid_value(|value| value.parse::<$t>().is_err());
						ctx.render_errors(pos, builder);
						let str = Self::transmute(element).to_compact_string();
						if ctx.forbid(pos) {
							builder.settings(pos + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
							builder.color = TextColor::TreePrimitive.to_raw();
							let _ = write!(builder, "{str}");
						}

						ctx.offset_pos(0, 16);

						let pos = ctx.pos();
						ctx.draw_held_entry_bar(pos, builder, |x, y| pos == (x, y + 8), |x| self.can_insert(x));
					}

					ctx.offset_pos(-16, 0);
				} else {
					ctx.skip_line_numbers(self.len());
				}
			}

			#[must_use]
			pub fn get(&self, idx: usize) -> Option<&NbtElement> { self.values.get(idx) }

			#[must_use]
			pub fn get_mut(&mut self, idx: usize) -> Option<&mut NbtElement> { self.values.get_mut(idx) }

			#[must_use]
			pub fn value(&self) -> CompactString {
				let (single, multiple) = id_to_string_name($id);
				format_compact!(
					"{} {}",
					self.len(),
					if self.len() == 1 { single } else { multiple }
				)
			}

			// ret type is #[must_use]
			pub fn children(&self) -> Iter<'_, NbtElement> { self.values.iter() }

			// ret type is #[must_use]
			pub fn children_mut(&mut self) -> IterMut<'_, NbtElement> { self.values.iter_mut() }

			pub fn shut(&mut self) { self.open = false; }

			pub fn expand(&mut self) { self.open = !self.is_empty(); }

			pub fn recache(&mut self) {
				let mut max_depth = 0;
				if self.open() {
					for child in self.children() {
						max_depth = usize::max(max_depth, 16 + 4 + child.value().0.width());
					}
				}
				self.max_depth = max_depth as u32;
			}

			#[must_use]
			pub const fn max_depth(&self) -> usize { self.max_depth as usize }

			pub fn render_icon(&self, pos: impl Into<(usize, usize)>, z: ZOffset, builder: &mut VertexBufferBuilder) { builder.draw_texture_z(pos, z, $uv, (16, 16)); }

			pub fn render_element_icon(pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder) { builder.draw_texture(pos, $element_uv, (16, 16)); }
			
			#[must_use]
            pub fn can_insert(&self, value: &NbtElement) -> bool {
				value.id() == $id
			}
		}

		impl Display for $name {
			fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
				write!(f, "[{};", $char)?;
				for (idx, element) in self.children().enumerate() {
					write!(f, "{element}")?;
					if likely(idx < self.len() - 1) {
						write!(f, ",")?;
					}
				}
				write!(f, "]")
			}
		}

		impl $name {
			pub fn pretty_fmt(&self, f: &mut PrettyFormatter) {
				if self.is_empty() {
					f.write_str(concat!("[", $char, ";]"))
				} else {
					let len = self.len();
					f.write_str(concat!("[", $char, ";\n"));
					f.increase();
					for (idx, element) in self.children().enumerate() {
						f.indent();
						element.pretty_fmt(f);
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
	};
}

use std::alloc::{alloc, Layout};
use std::fmt;
use std::fmt::{Display, Write};
use std::hint::likely;
use std::slice::{Iter, IterMut};

use compact_str::{format_compact, CompactString, ToCompactString};

use crate::assets::{ZOffset, BASE_Z, BYTE_ARRAY_UV, BYTE_UV, CONNECTION_UV, INT_ARRAY_UV, INT_UV, JUST_OVERLAPPING_BASE_TEXT_Z, LONG_ARRAY_UV, LONG_UV};
use crate::elements::{id_to_string_name, NbtElement};
use crate::elements::nbt_parse_result::NbtParseResult;
use crate::render::{RenderContext, TextColor, VertexBufferBuilder};
use crate::serialization::{Decoder, PrettyFormatter, UncheckedBufWriter};
use crate::util::StrExt;

array!(byte, NbtByteArray, i8, 7, 1, 'B', BYTE_ARRAY_UV, BYTE_UV, parse_byte, array_try_into_byte);
array!(int, NbtIntArray, i32, 11, 3, 'I', INT_ARRAY_UV, INT_UV, parse_int, array_try_into_int);
array!(long, NbtLongArray, i64, 12, 4, 'L', LONG_ARRAY_UV, LONG_UV, parse_long, array_try_into_long);
