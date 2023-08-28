#[macro_export]
macro_rules! primitive {
	($uv:ident, $s:expr, $name:ident, $t:ty, $id:literal) => {
		#[derive(Clone, Default)]
		#[repr(transparent)]
		pub struct $name {
			pub value: $t,
		}

		impl $name {
			pub const ID: u8 = $id;

			#[inline]
			pub fn to_bytes(&self, writer: &mut UncheckedBufWriter) {
				writer.write(self.value.to_be_bytes().as_ref());
			}

			#[inline]
			#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
			pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
				unsafe {
					decoder.assert_len(core::mem::size_of::<$t>())?;
					Some(Self {
						value: <$t>::from_be_bytes(decoder.read_bytes::<{ core::mem::size_of::<$t>() }>()),
					})
				}
			}

			#[inline]
			pub fn set(&mut self, option: Option<$t>) {
				if let Some(t) = option {
					self.value = t;
				}
			}

			#[inline]
			pub fn render(&self, builder: &mut VertexBufferBuilder, name: Option<&str>, ctx: &mut RenderContext) {
				ctx.line_number();
				Self::render_icon(ctx.pos(), 0.0, builder);
				let str = self.value.to_string();
				ctx.highlight(
					ctx.pos(),
					name.map(StrExt::width).unwrap_or(0) + name.is_some() as usize * ": ".width() + str.width(),
					builder,
				);
				if ctx.forbid(ctx.pos(), builder) {
					builder.settings(ctx.pos() + (20, 0), false, 1);
					let _ = match name {
						Some(x) => write!(builder, "{x}: {str}"),
						None => write!(builder, "{str}"),
					};
				}

				ctx.y_offset += 16;
			}

			#[inline]
			pub fn render_icon(pos: impl Into<(usize, usize)>, z: f32, builder: &mut VertexBufferBuilder) {
				builder.draw_texture_z(pos, z, $uv, (16, 16));
			}
		}

		impl Display for $name {
			fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
				write!(f, "{}", self.value)?;
				if let Some(s) = $s {
					write!(f, "{s}")?;
				}
				Ok(())
			}
		}

		impl Debug for $name {
			fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
				Display::fmt(self, f)
			}
		}
	};
}
