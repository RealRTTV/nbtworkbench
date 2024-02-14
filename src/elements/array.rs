#[macro_export]
macro_rules! array {
	($element_field:ident, $name:ident, $t:ty, $my_id:literal, $id:literal, $char:literal, $uv:ident, $element_uv:ident) => {
		#[derive(Default)]
		#[repr(C)]
		pub struct $name {
			values: Box<Vec<NbtElement>>,
			max_depth: u32,
			open: bool,
		}

		impl Clone for $name {
			#[allow(clippy::cast_ptr_alignment)]
			#[inline]
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
			#[inline]
			#[must_use]
			pub fn new() -> Self {
				Self {
					values: Box::<Vec<NbtElement>>::default(),
					open: false,
					max_depth: 0,
				}
			}

			pub const ID: u8 = $my_id;

			#[inline]
			pub(in $crate::elements) fn from_str0(mut s: &str) -> Option<(&str, Self)> {
				s = s
					.strip_prefix('[')?
					.trim_start()
					.strip_prefix(concat!($char, ";"))?
					.trim_start();
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
					let vec = alloc(Layout::array::<NbtElement>(len).unwrap_unchecked()).cast::<NbtElement>();
					for idx in 0..len {
						let mut element = NbtElement {
							$element_field: core::mem::transmute(<$t>::from_be_bytes(
								decoder
									.data
									.add(idx * core::mem::size_of::<$t>())
									.cast::<[u8; core::mem::size_of::<$t>()]>()
									.read(),
							)),
						};
						element.id.id = $id;
						vec.add(idx).write(element);
					}
					decoder.data = decoder.data.add(len * core::mem::size_of::<$t>());
					let boxx = alloc(Layout::new::<Vec<NbtElement>>()).cast::<Vec<NbtElement>>();
					boxx.write(Vec::from_raw_parts(vec, len, len));
					Some(Self {
						values: Box::from_raw(boxx),
						open: false,
						max_depth: 0,
					})
				}
			}

			#[inline]
			pub fn to_bytes(&self, writer: &mut UncheckedBufWriter) {
				writer.write(&(self.len() as u32).to_be_bytes());
				for entry in self.values.iter() {
					writer.write(&Self::transmute(entry).to_be_bytes());
				}
			}
		}

		impl $name {
			#[inline(always)]
			fn transmute(element: &NbtElement) -> $t { unsafe { element.deref().$element_field.deref().value } }

			#[inline]
			pub fn increment(&mut self, _: usize, _: usize) {}

			#[inline]
			pub fn decrement(&mut self, _: usize, _: usize) {}

			#[inline]
			#[must_use]
			pub fn height(&self) -> usize {
				if self.open {
					self.len() + 1
				} else {
					1
				}
			}

			#[inline]
			#[must_use]
			pub fn true_height(&self) -> usize { self.len() + 1 }

			#[inline]
			pub fn toggle(&mut self) -> Option<()> {
				self.open = !self.open && !self.is_empty();
				Some(())
			}

			#[inline]
			#[must_use]
			pub const fn open(&self) -> bool { self.open }

			#[inline]
			#[must_use]
			pub fn len(&self) -> usize { self.values.len() }

			#[inline]
			#[must_use]
			pub fn is_empty(&self) -> bool { self.values.is_empty() }

			/// # Errors
			///
			/// * Element type was not supported for `$name`
			#[inline]
			pub fn insert(&mut self, idx: usize, value: NbtElement) -> Result<(), NbtElement> {
				if value.id() == $id {
					// the time complexity is fine here
					unsafe {
						self.values.try_reserve_exact(1).unwrap_unchecked();
					}
					self.values.insert(idx, value);
					self.increment(1, 1);
					Ok(())
				} else {
					Err(value)
				}
			}

			#[inline]
			pub fn remove(&mut self, idx: usize) -> NbtElement {
				let removed = self.values.remove(idx);
				self.values.shrink_to_fit();
				removed
			}

			#[inline]
			pub fn render(&self, builder: &mut VertexBufferBuilder, key: Option<&str>, remaining_scroll: &mut usize, tail: bool, ctx: &mut RenderContext) {
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
						builder.settings(ctx.pos() + (20, 0), false, 1);
						let _ = match key {
							Some(x) => write!(builder, "{x}: {}", self.value()),
							None => write!(builder, "{}", self.value()),
						};
					}

					let pos = ctx.pos();
					if ctx.ghost(ctx.pos() + (16, 16), builder, |x, y| pos == (x - 16, y - 8), |id| id == $id) {} else if self.height() == 1 && ctx.ghost(ctx.pos() + (16, 16), builder, |x, y| pos == (x - 16, y - 16), |id| id == $id) {}

					ctx.y_offset += 16;
				}

				if self.open {
					ctx.x_offset += 16;

					for (idx, element) in self.children().enumerate() {
						if ctx.y_offset > builder.window_height() {
							break;
						}

						if *remaining_scroll > 0 {
							*remaining_scroll -= 1;
							ctx.skip_line_numbers(1);
							continue;
						}

						let pos = ctx.pos();
						ctx.ghost(ctx.pos(), builder, |x, y| pos == (x, y), |id| id == $id);

						builder.draw_texture(
							ctx.pos() - (16, 0),
							CONNECTION_UV,
							(
								16,
								(idx != self.len() - 1) as usize * 7 + 9,
							),
						);
						if !tail {
							builder.draw_texture(ctx.pos() - (32, 0), CONNECTION_UV, (8, 16));
						}

						ctx.line_number();
						Self::render_element_icon(ctx.pos(), builder);
						ctx.check_for_invalid_value(|value| value.parse::<$t>().is_err());
						ctx.render_errors(ctx.pos(), builder);
						let str = Self::transmute(element).to_compact_string();
						if ctx.forbid(ctx.pos()) {
							builder.settings(ctx.pos() + (20, 0), false, 1);
							let _ = write!(builder, "{str}");
						}

						ctx.y_offset += 16;

						let pos = ctx.pos();
						ctx.ghost(ctx.pos(), builder, |x, y| pos == (x, y + 8), |id| id == $id);
					}

					ctx.x_offset -= 16;
				} else {
					ctx.skip_line_numbers(self.len());
				}
			}

			#[inline]
			#[must_use]
			pub fn get(&self, idx: usize) -> Option<&NbtElement> { self.values.get(idx) }

			#[inline]
			#[must_use]
			pub fn get_mut(&mut self, idx: usize) -> Option<&mut NbtElement> { self.values.get_mut(idx) }

			#[inline]
			#[must_use]
			pub fn value(&self) -> CompactString {
				let (single, multiple) = id_to_string_name($id);
				format_compact!(
					"{} {}",
					self.len(),
					if self.len() == 1 { single } else { multiple }
				)
			}

			#[inline] // ret type is #[must_use]
			pub fn children(&self) -> ValueIterator { ValueIterator::Generic(self.values.iter()) }

			#[inline] // ret type is #[must_use]
			pub fn children_mut(&mut self) -> ValueMutIterator { ValueMutIterator::Generic(self.values.iter_mut()) }

			pub fn drop(&mut self, key: Option<CompactString>, element: NbtElement, y: &mut usize, depth: usize, target_depth: usize, line_number: usize, indices: &mut Vec<usize>) -> DropFn {
				if 8 <= *y && *y < 16 && depth == target_depth {
					indices.push(0);
					if let Err(element) = self.insert(0, element) { return DropFn::InvalidType(key, element) }
					self.open = true;
					return DropFn::Dropped(1, 1, None, line_number + 1);
				}

				if self.height() * 16 <= *y && *y < self.height() * 16 + 8 && depth == target_depth {
					indices.push(self.len());
					if let Err(element) = self.insert(self.len(), element) { return DropFn::InvalidType(key, element) }
					self.open = true;
					return DropFn::Dropped(1, 1, None, line_number + self.len());
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
						for idx in 0..self.values.len() {
							*ptr = idx;
							if *y < 8 && depth == target_depth {
								if let Err(element) = self.insert(idx, element) { return DropFn::InvalidType(key, element) }
								return DropFn::Dropped(1, 1, None, line_number + idx + 1);
							} else if *y < 16 && depth == target_depth {
								*ptr = idx + 1;
								if let Err(element) = self.insert(idx + 1, element) { return DropFn::InvalidType(key, element) }
								return DropFn::Dropped(1, 1, None, line_number + idx + 1 + 1);
							}

							*y -= 16;
						}
						indices.pop();
					} else {
						*y = y.saturating_sub((self.len() + 1) * 16);
					}
				}
				DropFn::Missed(key, element)
			}

			#[inline]
			pub fn shut(&mut self) { self.open = false; }

			#[inline]
			pub fn expand(&mut self) { self.open = !self.is_empty(); }

			#[inline]
			pub fn recache_depth(&mut self) {
				let mut max_depth = 0;
				if self.open() {
					for child in self.children() {
						max_depth = usize::max(max_depth, 16 + 4 + child.value().0.width());
					}
				}
				self.max_depth = max_depth as u32;
			}

			#[inline]
			#[must_use]
			pub const fn max_depth(&self) -> usize { self.max_depth as usize }

			#[inline]
			pub fn render_icon(pos: impl Into<(usize, usize)>, z: u8, builder: &mut VertexBufferBuilder) { builder.draw_texture_z(pos, z, $uv, (16, 16)); }

			#[inline]
			pub fn render_element_icon(pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder) { builder.draw_texture(pos, $element_uv, (16, 16)); }
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
