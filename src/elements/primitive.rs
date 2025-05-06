macro_rules! primitive {
	($uv:ident, $s:expr, $name:ident, $t:ty, $id:literal) => {
		primitive!($uv, $s, $name, $t, $id, |x: $t| x.to_compact_string());
	};
	($uv:ident, $s:expr, $name:ident, $t:ty, $id:literal, $compact_format:expr) => {
		#[derive(Copy, Clone, Default, PartialEq)]
		#[repr(transparent)]
		pub struct $name {
			pub value: $t,
		}

		impl $name {
			pub const ID: u8 = $id;

			pub fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) { writer.write(self.value.to_be_bytes().as_ref()); }
			
			pub fn from_bytes<'a, D: Decoder<'a>>(decoder: &mut D) -> NbtParseResult<Self> {
				use super::nbt_parse_result::*;
				
				unsafe {
					decoder.assert_len(core::mem::size_of::<$t>())?;
					ok(Self {
						value: <$t>::from_ne_bytes(decoder.read_ne_bytes::<{ core::mem::size_of::<$t>() }>()),
					})
				}
			}

			pub fn to_le_bytes(&self, writer: &mut UncheckedBufWriter) { writer.write(self.value.to_le_bytes().as_ref()); }

			pub fn render(&self, builder: &mut VertexBufferBuilder, name: Option<&str>, ctx: &mut RenderContext) {
				ctx.line_number();
				self.render_icon(ctx.pos(), BASE_Z, builder);
				ctx.check_for_invalid_value(|value| value.parse::<$t>().is_err());
				ctx.render_errors(ctx.pos(), builder);
				let str = $compact_format(self.value);
				if ctx.forbid(ctx.pos()) {
					builder.settings(ctx.pos() + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
					if let Some(key) = name {
						builder.color = TextColor::TreeKey.to_raw();
						let _ = write!(builder, "{key}: ");
					};

					builder.color = TextColor::TreePrimitive.to_raw();
					let _ = write!(builder, "{str}");
				}

				ctx.offset_pos(0, 16);
			}

			pub fn render_icon(&self, pos: impl Into<(usize, usize)>, z: ZOffset, builder: &mut VertexBufferBuilder) { builder.draw_texture_z(pos, z, $uv, (16, 16)); }

			pub fn value(&self) -> CompactString { $compact_format(self.value) }
		}

		impl Display for $name {
			fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
				write!(f, "{}", $compact_format(self.value))?;
				if let Some(s) = $s {
					write!(f, "{s}")?;
				}
				Ok(())
			}
		}

		impl $name {
			pub fn pretty_fmt(&self, f: &mut PrettyFormatter) { f.write_str(&self.to_string()) }
		}

		impl $name {
			pub fn matches(&self, other: &Self) -> bool { self.value == other.value }
		}
	};
}

use compact_str::{CompactString, ToCompactString};
use std::fmt::{Display, Formatter, Write};

use crate::assets::{ZOffset, BASE_Z, BYTE_UV, DOUBLE_UV, FLOAT_UV, INT_UV, JUST_OVERLAPPING_BASE_TEXT_Z, LONG_UV, SHORT_UV};
use crate::render::{RenderContext, TextColor, VertexBufferBuilder};
use crate::serialization::{Decoder, PrettyFormatter, UncheckedBufWriter};
use crate::elements::nbt_parse_result::NbtParseResult;

primitive!(BYTE_UV, { Some('b') }, NbtByte, i8, 1);
primitive!(SHORT_UV, { Some('s') }, NbtShort, i16, 2);
primitive!(INT_UV, { None::<char> }, NbtInt, i32, 3);
primitive!(LONG_UV, { Some('L') }, NbtLong, i64, 4);
primitive!(FLOAT_UV, { Some('f') }, NbtFloat, f32, 5);
primitive!(DOUBLE_UV, { Some('d') }, NbtDouble, f64, 6);
