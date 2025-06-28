macro_rules! primitive {
	($module:ident, $name:ident, $t:ty, $id:literal, $s:expr, $uv:path, $ghost_uv:path) => {
		pub mod $module {
			#[derive(Copy, Clone, Default, PartialEq)]
			#[repr(transparent)]
			pub struct $name {
				pub value: $t,
			}

			impl $crate::elements::Matches for $name {
				fn matches(&self, other: &Self) -> bool { self.value == other.value }
			}

			impl ::std::fmt::Display for $name {
				fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
					write!(f, "{}", self.value)?;
					if let Some(s) = $s {
						write!(f, "{s}")?;
					}
					Ok(())
				}
			}

			impl $crate::serialization::formatter::PrettyDisplay for $name {
				fn pretty_fmt(&self, f: &mut $crate::serialization::formatter::PrettyFormatter) { f.write_str(&self.to_string()) }
			}

			impl $crate::elements::NbtElementVariant for $name {
				const ID: u8 = $id;

				const UV: $crate::util::Vec2u = $uv;

				const GHOST_UV: $crate::util::Vec2u = $ghost_uv;

				fn from_str0(s: &str) -> Result<(&str, Self), usize>
				where Self: Sized {
					Err(s.len())
				}

				fn from_bytes<'a, D: $crate::serialization::decoder::Decoder<'a>>(decoder: &mut D, _: Self::ExtraParseInfo) -> $crate::elements::result::NbtParseResult<Self> {
					use $crate::elements::result::*;

					unsafe {
						decoder.assert_len(core::mem::size_of::<<Self as $crate::elements::PrimitiveNbtElementVariant>::InnerType>())?;
						ok(Self {
							value: <Self as $crate::elements::PrimitiveNbtElementVariant>::InnerType::from_ne_bytes(decoder.read_ne_bytes::<{ core::mem::size_of::<<Self as $crate::elements::PrimitiveNbtElementVariant>::InnerType>() }>()),
						})
					}
				}

				fn to_be_bytes(&self, writer: &mut $crate::serialization::encoder::UncheckedBufWriter) { writer.write(self.value.to_be_bytes().as_ref()); }

				fn to_le_bytes(&self, writer: &mut $crate::serialization::encoder::UncheckedBufWriter) { writer.write(self.value.to_le_bytes().as_ref()); }

				fn render(&self, builder: &mut $crate::render::vertex_buffer_builder::VertexBufferBuilder, name: Option<&str>, _remaining_scroll: &mut usize, _tail: bool, ctx: &mut $crate::render::RenderContext) {
					use ::std::fmt::Write as _;

					ctx.line_number();
					builder.draw_texture(ctx.pos(), Self::UV, (16, 16));
					ctx.check_for_invalid_value(|value| value.parse::<<Self as $crate::elements::PrimitiveNbtElementVariant>::InnerType>().is_err());
					ctx.render_errors(ctx.pos(), builder);
					if ctx.forbid(ctx.pos()) {
						builder.settings(ctx.pos() + (20, 0), false, $crate::render::assets::JUST_OVERLAPPING_BASE_TEXT_Z);
						if let Some(key) = name {
							builder.color = $crate::render::color::TextColor::TreeKey.to_raw();
							let _ = write!(builder, "{key}: ");
						};

						builder.color = $crate::render::color::TextColor::TreePrimitive.to_raw();
						let _ = write!(builder, "{}", self.value);
					}

					ctx.offset_pos(0, 16);
				}

				fn value(&self) -> ::std::borrow::Cow<'_, str> { ::std::borrow::Cow::Owned(format!("{}", self.value)) }
			}

			impl $crate::elements::PrimitiveNbtElementVariant for $name {
				type InnerType = $t;

				fn new(value: Self::InnerType) -> Self
				where Self: Sized {
					Self { value }
				}
			}
		}
	};
}

primitive!(byte, NbtByte, i8, 1, Some('b'), crate::render::assets::BYTE_UV, crate::render::assets::BYTE_GHOST_UV);
primitive!(short, NbtShort, i16, 2, Some('s'), crate::render::assets::SHORT_UV, crate::render::assets::SHORT_GHOST_UV);
primitive!(int, NbtInt, i32, 3, None::<char>, crate::render::assets::INT_UV, crate::render::assets::INT_GHOST_UV);
primitive!(long, NbtLong, i64, 4, Some('L'), crate::render::assets::LONG_UV, crate::render::assets::LONG_GHOST_UV);
primitive!(float, NbtFloat, f32, 5, Some('f'), crate::render::assets::FLOAT_UV, crate::render::assets::FLOAT_GHOST_UV);
primitive!(double, NbtDouble, f64, 6, Some('d'), crate::render::assets::DOUBLE_UV, crate::render::assets::DOUBLE_GHOST_UV);
