macro_rules! array {
	($module:ident, $name:ident, $id:literal, $element:ty, $get_inner_unchecked:path, $constructor:path, $char:literal, $uv:path, $ghost_uv:path, $default_snbt_integer:path, $try_into_element:path) => {
		mod $module {
			#[cfg(not(target_arch = "wasm32"))] use ::std::thread::Scope;
			#[cfg(target_arch = "wasm32")] use $crate::wasm::FakeScope as Scope;
			use crate::elements::{NbtElementVariant, ComplexNbtElementVariant, NbtElement};

			#[repr(C)]
			pub struct $name {
				pub(in $crate::elements) values: Box<Vec<NbtElement>>,
				max_depth: u32,
				open: bool,
			}

			impl $crate::elements::Matches for $name {
				fn matches(&self, other: &Self) -> bool {
					self.eq(other)
				}
			}

			impl PartialEq for $name {
				fn eq(&self, other: &Self) -> bool {
					let a = unsafe { ::std::slice::from_raw_parts(self.values.as_ptr().cast::<u8>(), self.values.len() * ::std::mem::size_of::<NbtElement>()) };
					let b = unsafe { ::std::slice::from_raw_parts(other.values.as_ptr().cast::<u8>(), other.values.len() * ::std::mem::size_of::<NbtElement>()) };
					a == b
				}
			}

			impl Clone for $name {
				fn clone(&self) -> Self {
					let mut vec = unsafe { Vec::try_with_capacity(self.values.len()).unwrap_unchecked() };
					for src in self.values.iter() {
						unsafe {
							vec.push_within_capacity(src.clone())
								.unwrap_unchecked()
						};
					}
					Self {
						values: unsafe { Box::try_new(vec).unwrap_unchecked() },
						max_depth: self.max_depth,
						open: self.open,
					}
				}
			}

			impl Default for $name {
				fn default() -> Self {
					Self {
						values: Box::<Vec<NbtElement>>::default(),
						open: false,
						max_depth: 0,
					}
				}
			}

			impl ::std::fmt::Display for $name {
				fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
					write!(f, "[{};", $char)?;
					for (idx, element) in self.children().enumerate() {
						write!(f, "{element}")?;
						if ::std::hint::likely(idx < self.len() - 1) {
							write!(f, ",")?;
						}
					}
					write!(f, "]")
				}
			}

			impl $name {
				pub const CHILD_ID: u8 = <$element>::ID;

				pub type ChildType = $element;

				fn transmute(element: &NbtElement) -> <Self::ChildType as $crate::elements::PrimitiveNbtElementVariant>::InnerType { unsafe { $get_inner_unchecked(element).value } }
			}

			impl NbtElementVariant for $name {
				const ID: u8 = $id;

				const UV: $crate::util::Vec2u = $uv;

				const GHOST_UV: $crate::util::Vec2u = $ghost_uv;

				fn from_str0(mut s: &str) -> Result<(&str, Self), usize> {
					s = s.strip_prefix('[').ok_or(s.len())?.trim_start();
					s = s
						.strip_prefix(concat!($char, ";"))
						.ok_or(s.len())?
						.trim_start();
					let mut array = Self::default();
					while !s.starts_with(']') {
						let (s2, element) = NbtElement::from_str0(s, $default_snbt_integer)?;
						let element = $try_into_element(element).ok_or(s.len())?;
						// SAFETY: there is nothing to update
						unsafe {
							array
								.insert(array.len(), element)
								.map_err(|_| s.len())?;
						}
						s = s2.trim_start();
						if let Some(s2) = s.strip_prefix(',') {
							s = s2.trim_start();
						} else if s.starts_with(']') {
							break;
						}
					}
					array.values.shrink_to_fit();
					array.recache();
					Ok((s.strip_prefix(']').ok_or(s.len())?, array))
				}

				fn from_bytes<'a, D: $crate::serialization::Decoder<'a>>(decoder: &mut D) -> $crate::elements::result::NbtParseResult<Self> {
					use $crate::elements::result::*;

					decoder.assert_len(4)?;
					let len = unsafe { decoder.u32() } as usize;
					decoder.assert_len(len * core::mem::size_of::<<Self::ChildType as $crate::elements::PrimitiveNbtElementVariant>::InnerType>())?;
					let mut vec = from_opt(Vec::try_with_capacity(len).ok(), "Could not allocate enough memory for Vec")?;
					for _ in 0..len {
						let element = $constructor(unsafe { core::mem::transmute(<<Self::ChildType as $crate::elements::PrimitiveNbtElementVariant>::InnerType>::from_ne_bytes(decoder.read_ne_bytes::<{ core::mem::size_of::<<Self::ChildType as $crate::elements::PrimitiveNbtElementVariant>::InnerType>() }>())) });
						from_opt(vec.push_within_capacity(element).ok(), "Vec was longer than originally stated")?;
					}
					let mut array = Self {
						values: unsafe { Box::try_new(vec).unwrap_unchecked() },
						open: false,
						max_depth: 0,
					};
					array.recache();
					ok(array)
				}

				fn to_be_bytes(&self, writer: &mut $crate::serialization::UncheckedBufWriter) {
					writer.write(&(self.len() as u32).to_be_bytes());
					for entry in self.values.iter() {
						writer.write(&Self::transmute(entry).to_be_bytes());
					}
				}

				fn to_le_bytes(&self, writer: &mut $crate::serialization::UncheckedBufWriter) {
					writer.write(&(self.len() as u32).to_le_bytes());
					for entry in self.values.iter() {
						writer.write(&Self::transmute(entry).to_le_bytes());
					}
				}

				fn render(&self, builder: &mut $crate::render::VertexBufferBuilder, key: Option<&str>, remaining_scroll: &mut usize, tail: bool, ctx: &mut $crate::render::RenderContext) {
					use ::std::fmt::Write as _;

					'head: {
						if *remaining_scroll > 0 {
							*remaining_scroll -= 1;
							ctx.skip_line_numbers(1);
							break 'head;
						}

						let pos = ctx.pos();

						ctx.line_number();
						builder.draw_texture_z(pos, $crate::assets::BASE_Z, Self::UV, (16, 16));
						if !self.is_empty() {
							ctx.draw_toggle(pos - (16, 0), self.open, builder);
						}
						ctx.render_errors(pos, builder);
						if ctx.forbid(pos) {
							builder.settings(pos + (20, 0), false, $crate::assets::JUST_OVERLAPPING_BASE_TEXT_Z);
							if let Some(key) = key {
								builder.color = $crate::render::TextColor::TreeKey.to_raw();
								let _ = write!(builder, "{key}: ");
							};

							builder.color = $crate::render::TextColor::TreeKey.to_raw();
							let _ = write!(builder, "{}", self.value());
						}

						if ctx.draw_held_entry_bar(pos + (16, 16), builder, |x, y| pos == (x - 16, y - 8), |x| self.can_insert(x)) {
						} else if self.height() == 1 && ctx.draw_held_entry_bar(pos + (16, 16), builder, |x, y| pos == (x - 16, y - 16), |x| self.can_insert(x)) {
						}

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

							builder.draw_texture(pos - (16, 0), $crate::assets::CONNECTION_UV, (16, (idx != self.len() - 1) as usize * 7 + 9));
							if !tail {
								builder.draw_texture(pos - (32, 0), $crate::assets::CONNECTION_UV, (8, 16));
							}

							ctx.line_number();
							builder.draw_texture_z(pos, $crate::assets::BASE_Z, Self::ChildType::UV, (16, 16));
							ctx.check_for_invalid_value(|value| value.parse::<<Self::ChildType as $crate::elements::PrimitiveNbtElementVariant>::InnerType>().is_err());
							ctx.render_errors(pos, builder);
							let str = ::compact_str::format_compact!("{}", Self::transmute(element));
							if ctx.forbid(pos) {
								builder.settings(pos + (20, 0), false, $crate::assets::JUST_OVERLAPPING_BASE_TEXT_Z);
								builder.color = $crate::render::TextColor::TreePrimitive.to_raw();
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

				fn pretty_fmt(&self, f: &mut $crate::serialization::PrettyFormatter) {
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

				fn value(&self) -> ::std::borrow::Cow<'_, str> {
					let item = $crate::elements::id_to_string_name(Self::CHILD_ID, self.len());
					::std::borrow::Cow::Owned(format!("{} {item}", self.len()))
				}
			}

			impl ComplexNbtElementVariant for $name {
				type Entry = NbtElement;

				fn new(entries: Vec<Self::Entry>) -> Self
				where Self: Sized {
					Self {
                        values: Box::new(entries.into_iter().filter(|entry| entry.id() == Self::CHILD_ID).collect::<Vec<_>>()),
						max_depth: 0,
						open: false,
					}
				}

				fn height(&self) -> usize { if self.open { self.len() + 1 } else { 1 } }

				fn true_height(&self) -> usize { self.len() + 1 }

				fn len(&self) -> usize { self.values.len() }

				fn can_insert(&self, value: &NbtElement) -> bool { value.id() == Self::CHILD_ID }

				fn is_open(&self) -> bool { self.open }

				fn max_depth(&self) -> usize { self.max_depth as usize }

				unsafe fn toggle(&mut self) {
					self.open = !self.open && !self.is_empty();
				}

				unsafe fn insert(&mut self, idx: usize, entry: Self::Entry) -> Result<Option<Self::Entry>, Self::Entry> {
					if self.can_insert(&entry) {
						// the time complexity is fine here
						unsafe {
							self.values
								.try_reserve_exact(1)
								.unwrap_unchecked();
						}
						self.values.insert(idx, entry);
						Ok(None)
					} else {
						Err(entry)
					}
				}

				unsafe fn remove(&mut self, idx: usize) -> Option<Self::Entry> {
					if idx >= self.values.len() {
						return None
					}
					let removed = self.values.remove(idx);
					self.values.shrink_to_fit();
					Some(removed)
				}

				unsafe fn replace(&mut self, idx: usize, entry: Self::Entry) -> Result<Option<Self::Entry>, Self::Entry> {
					if !self.can_insert(&entry) || idx >= self.len() {
						return Err(entry)
					}
					Ok(Some(core::mem::replace(&mut self.values[idx], entry)))
				}

				unsafe fn swap(&mut self, a: usize, b: usize) {
					self.values.swap(a, b);
				}

				unsafe fn shut<'a, 'b>(&mut self, _scope: &'a Scope<'a, 'b>) { self.open = false; }

				unsafe fn expand<'a, 'b>(&mut self, _scope: &'a Scope<'a, 'b>) { self.open = !self.is_empty(); }

				fn recache(&mut self) {
					let mut max_depth = 0;
					if self.is_open() {
						for child in self.children() {
							max_depth = usize::max(max_depth, 16 + 4 + $crate::util::StrExt::width(child.value().0.as_ref()));
						}
					}
					self.max_depth = max_depth as u32;
				}

				fn children(&self) -> ::std::slice::Iter<'_, Self::Entry> { self.values.iter() }

				fn children_mut(&mut self) -> ::std::slice::IterMut<'_, Self::Entry> { self.values.iter_mut() }

				fn get(&self, idx: usize) -> Option<&Self::Entry> { self.values.get(idx) }

				fn get_mut(&mut self, idx: usize) -> Option<&mut Self::Entry> { self.values.get_mut(idx) }

				unsafe fn get_unchecked(&self, idx: usize) -> &Self::Entry { unsafe { self.values.get_unchecked(idx) } }

				unsafe fn get_unchecked_mut(&mut self, idx: usize) -> &mut Self::Entry { unsafe { self.values.get_unchecked_mut(idx) } }
			}
		}
		pub use $module::*;
	};
}

array!(
	byte_array,
	NbtByteArray,
	7,
	crate::elements::NbtByte,
	crate::elements::NbtElement::as_byte_unchecked,
	crate::elements::NbtElement::Byte,
	'B',
	crate::assets::BYTE_ARRAY_UV,
	crate::assets::BYTE_ARRAY_GHOST_UV,
	crate::elements::NbtElement::parse_byte,
	crate::elements::NbtElement::array_try_into_byte
);

array!(
	int_array,
	NbtIntArray,
	11,
	crate::elements::NbtInt,
	crate::elements::NbtElement::as_int_unchecked,
	crate::elements::NbtElement::Int,
	'I',
	crate::assets::INT_ARRAY_UV,
	crate::assets::INT_ARRAY_GHOST_UV,
	crate::elements::NbtElement::parse_int,
	crate::elements::NbtElement::array_try_into_int
);

array!(
	long_array,
	NbtLongArray,
	12,
	crate::elements::NbtLong,
	crate::elements::NbtElement::as_long_unchecked,
	crate::elements::NbtElement::Long,
	'L',
	crate::assets::LONG_ARRAY_UV,
	crate::assets::LONG_ARRAY_GHOST_UV,
	crate::elements::NbtElement::parse_long,
	crate::elements::NbtElement::array_try_into_long
);
