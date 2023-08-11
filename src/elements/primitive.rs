#[macro_export]
macro_rules! primitive {
	(($u:literal $v:literal), $s:expr, $name:ident, $t:ty, $id:literal) => {
		#[derive(Clone, Default)]
		#[repr(transparent)]
		pub struct $name {
			pub value: $t,
		}

		impl $name {
			pub const ID: u8 = $id;

			#[inline]
			pub fn to_bytes<W: std::io::Write>(&self, writer: &mut W) {
				let _ = std::io::Write::write(writer, self.value.to_be_bytes().as_ref());
			}

			#[inline]
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
			pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut usize, y_offset: &mut usize, name: Option<&str>, ctx: &mut RenderContext) {
				ctx.line_number(*y_offset, builder);
				Self::render_icon(*x_offset, *y_offset, builder);
				let str = self.value.to_string();
				ctx.highlight(
					(*x_offset, *y_offset),
					name.map(StrExt::width).unwrap_or(0) + name.is_some() as usize * ": ".width() + str.width(),
					builder,
				);
				if ctx.forbid(*x_offset, *y_offset, builder) {
					builder.settings(*x_offset + 20, *y_offset, false);
					let _ = match name {
						Some(x) => write!(builder, "{x}: {str}"),
						None => write!(builder, "{str}"),
					};
				}

				*y_offset += 16;
			}

			#[inline]
			pub fn render_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
				builder.draw_texture((x, y), ($u, $v), (16, 16));
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
