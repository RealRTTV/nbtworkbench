#[macro_export]
macro_rules! array {
	($element:ident, $element_inner:ident, $name:ident, $t:ty, $my_id:literal, $id:literal, $char:literal, ($u:literal $v:literal), ($u2:literal $v2:literal)) => {
		#[derive(Clone, Default)]
		pub struct $name {
			values: Vec<NbtElement>,
			open: bool,
			height: usize,
		}

		impl $name {
			#[inline]
			#[must_use]
			pub const fn new() -> Self {
				Self {
					values: vec![],
					open: false,
					height: 1,
				}
			}

			pub const ID: u8 = $my_id;

			#[inline]
			pub(in $crate::elements) fn from_str0(mut s: &str) -> Option<(&str, Self)> {
				s = s.strip_prefix('[')?.trim_start().strip_prefix(concat!($char, ";"))?.trim_start();
				let mut array = Self::new();
				while !s.starts_with(']') {
					let (s2, element) = NbtElement::from_str0(s)?;
					array.insert(array.len(), element).ok()?;
					s = s2.trim_start();
					if let Some(s2) = s.strip_prefix(',') {
						s = s2.trim_start();
					} else {
						break;
					}
				}
				array.values.shrink_to_fit();
				Some((s.strip_prefix(']')?, array))
			}

			#[inline]
			pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
				unsafe {
					decoder.assert_len(4)?;
					let len = decoder.u32() as usize;
					decoder.assert_len(len * core::mem::size_of::<$t>())?;
					let mut vec = Vec::<NbtElement>::with_capacity(len);
					for (idx, maybe) in vec.spare_capacity_mut().iter_mut().enumerate() {
						maybe.write(NbtElement::$element($element_inner {
							value: <$t>::from_be_bytes(decoder.data.add(idx * core::mem::size_of::<$t>()).cast::<[u8; { core::mem::size_of::<$t>() }]>().read()),
						}));
					}
					decoder.data = decoder.data.add(len * core::mem::size_of::<$t>());
					vec.set_len(len);
					Some(Self {
						values: vec,
						open: false,
						height: len + 1,
					})
				}
			}

			#[inline]
			pub fn to_bytes<W: std::io::Write>(&self, writer: &mut W) {
				let _ = writer.write(&(self.len() as u32).to_be_bytes());
				for entry in &self.values {
					let _ = writer.write(&Self::transmute(entry).to_be_bytes());
				}
			}
		}

		impl $name {
			#[inline(always)]
			fn transmute(element: &NbtElement) -> $t {
				match element {
					NbtElement::$element($element_inner { value }) => *value,
					_ => unsafe { panic_unchecked("Array contained invalid type") },
				}
			}

			#[inline]
			pub fn increment(&mut self, amount: usize, _: usize) {
				self.height = self.height.wrapping_add(amount);
			}

			#[inline]
			pub fn decrement(&mut self, amount: usize, _: usize) {
				self.height = self.height.wrapping_sub(amount);
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
				self.height
			}

			#[inline]
			pub fn toggle(&mut self) -> Option<()> {
				self.open = !self.open && !self.is_empty();
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
				self.values.len()
			}

			#[inline]
			#[must_use]
			pub fn is_empty(&self) -> bool {
				self.values.is_empty()
			}

			/// # Errors
			///
			/// * Element type was not supported for `$name`
			#[inline]
			pub fn insert(&mut self, idx: usize, value: NbtElement) -> Result<(), NbtElement> {
				if let x @ NbtElement::$element(_) = value {
					self.values.insert(idx, x);
					self.increment(1, 1);
					Ok(())
				} else {
					Err(value)
				}
			}

			#[inline]
			pub fn remove(&mut self, idx: usize) -> NbtElement {
				self.values.remove(idx)
			}

			#[inline]
			pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut usize, y_offset: &mut usize, key: Option<&str>, remaining_scroll: &mut usize, tail: bool, ctx: &mut RenderContext) {
				'head: {
					if *remaining_scroll > 0 {
						*remaining_scroll -= 1;
						break 'head;
					}

					ctx.line_number(*y_offset, builder);
					Self::render_icon(*x_offset, *y_offset, builder);
					ctx.highlight((*x_offset, *y_offset), key.map(StrExt::width).unwrap_or(0), builder);
					if !self.is_empty() {
						builder.draw_texture((*x_offset - 16, *y_offset), (96 + self.open as usize * 16, 16), (16, 16));
					}
					if ctx.forbid(*x_offset, *y_offset, builder) {
						builder.settings(*x_offset + 20, *y_offset, false);
						let _ = match key {
							Some(x) => write!(builder, "{x}: {}", self.value()),
							None => write!(builder, "{}", self.value()),
						};
					}

					if ctx.ghost.is_some_and(|(id, _, _)| id == $id) {
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
					}

					*y_offset += 16;
				}

				if self.open {
					*x_offset += 16;

					ctx.check_key(|_| false);
					for (idx, element) in self.children().enumerate() {
						if *y_offset > builder.window_height() {
							break;
						}

						if *remaining_scroll > 0 {
							*remaining_scroll -= 1;
							ctx.line_number += 1;
							continue;
						}

						if ctx.ghost.is_some_and(|(id, _, _)| id == $id) && ctx.ghost(*x_offset, *y_offset, builder, |x, y| *x_offset == x && *y_offset == y) {
							builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, 16));
							if !tail {
								builder.draw_texture((*x_offset - 32, *y_offset), (80, 16), (8, 16));
							}
							*y_offset += 16;
						}

						let ghost_tail_mod = if let Some((id, x, y)) = ctx.ghost && x == *x_offset && y == *y_offset + 16 - *remaining_scroll * 16 - 8 && id == $id {
																			false
																		} else {
																			true
																		};

						builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, (!(idx == self.len() - 1 && ghost_tail_mod)) as usize * 7 + 9));
						if !tail {
							builder.draw_texture((*x_offset - 32, *y_offset), (80, 16), (8, 16));
						}

						ctx.line_number(*y_offset, builder);
						Self::render_element_icon(*x_offset, *y_offset, builder);
						let str = Self::transmute(element).to_string();
						ctx.highlight((*x_offset, *y_offset), str.width(), builder);
						if ctx.forbid(*x_offset, *y_offset, builder) {
							builder.settings(*x_offset + 20, *y_offset, false);
							let _ = write!(builder, "{str}");
						}

						*y_offset += 16;

						if ctx.ghost.is_some_and(|(id, _, _)| id == $id) && ctx.ghost(*x_offset, *y_offset, builder, |x, y| *x_offset == x && *y_offset - 8 == y) {
							builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, (idx != self.len() - 1) as usize * 7 + 9));
							if !tail {
								builder.draw_texture((*x_offset - 32, *y_offset), (80, 16), (8, 16));
							}
							*y_offset += 16;
						}
					}

					*x_offset -= 16;
				} else {
					ctx.line_number += self.height - 1;
				}
			}

			#[inline]
			#[must_use]
			pub fn get(&self, idx: usize) -> Option<&NbtElement> {
				self.values.get(idx)
			}

			#[inline]
			#[must_use]
			pub fn get_mut(&mut self, idx: usize) -> Option<&mut NbtElement> {
				self.values.get_mut(idx)
			}

			#[inline]
			#[must_use]
			pub fn value(&self) -> String {
				format!("{} {}", self.len(), if self.len() == 1 { "entry" } else { "entries" })
			}

			#[inline] // ret type is #[must_use]
			pub fn children(&self) -> Iter<'_, NbtElement> {
				self.values.iter()
			}

			#[inline] // ret type is #[must_use]
			pub fn children_mut(&mut self) -> IterMut<'_, NbtElement> {
				self.values.iter_mut()
			}

			pub fn drop(&mut self, key: Option<Box<str>>, element: NbtElement, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> DropFn {
				if 8 <= *y && *y < 16 && depth == target_depth {
					let before = self.height();
					indices.push(0);
					if let Err(element) = self.insert(0, element) {
						return DropFn::InvalidType(key, element);
					}
					self.open = true;
					return DropFn::Dropped(self.height - before, 1, None);
				}

				if self.height() * 16 <= *y && *y < self.height() * 16 + 8 && depth == target_depth {
					let before = self.height();
					indices.push(self.len());
					if let Err(element) = self.insert(self.len(), element) {
						return DropFn::InvalidType(key, element);
					}
					self.open = true;
					return DropFn::Dropped(self.height - before, 1, None);
				}

				if *y < 16 {
					return DropFn::Missed(key, element);
				} else {
					*y -= 16;
				}

				if self.open && !self.is_empty() {
					if depth == target_depth {
						indices.push(0);
						let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
						let heights = (element.height(), element.true_height());
						for idx in 0..self.values.len() {
							*ptr = idx;
							if *y < 8 && depth == target_depth {
								if let Err(element) = self.insert(idx, element) {
									return DropFn::InvalidType(key, element);
								}
								return DropFn::Dropped(heights.0, heights.1, None);
							} else if *y < 16 && depth == target_depth {
								*ptr = idx + 1;
								if let Err(element) = self.insert(idx + 1, element) {
									return DropFn::InvalidType(key, element);
								}
								return DropFn::Dropped(heights.0, heights.1, None);
							}

							*y -= 16;
						}
						indices.pop();
					} else {
						*y = y.saturating_sub(self.height * 16);
					}
				}
				DropFn::Missed(key, element)
			}

			pub fn shut(&mut self) {
				self.open = false;
			}

			#[inline]
			pub fn render_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
				builder.draw_texture((x, y), ($u, $v), (16, 16));
			}

			#[inline]
			pub fn render_element_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
				builder.draw_texture((x, y), ($u2, $v2), (16, 16));
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

		impl Debug for $name {
			fn fmt<'b, 'a>(&self, f: &'a mut Formatter<'b>) -> fmt::Result {
				let mut debug = unsafe { core::mem::transmute::<_, DebugList<'static, 'static>>(f.debug_list()) };
				f.write_str(concat!($char, ";"))?;
				let result = debug.entries(self.children()).finish();
				let _ = unsafe { core::mem::transmute::<_, DebugList<'a, 'b>>(debug) };
				result
			}
		}
	};
}
