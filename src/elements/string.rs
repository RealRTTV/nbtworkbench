use std::fmt::{Display, Formatter};
use std::io::Write;

use crate::{RenderContext, UnescapeStart, VertexBufferBuilder};
use crate::assets::STRING_UV;
use crate::decoder::Decoder;
use crate::encoder::write_string;

#[derive(Clone)]
#[repr(transparent)]
pub struct NbtString {
    str: Box<str>,
}

impl NbtString {
    pub const ID: u8 = 8;

    pub(in crate::elements) fn from_str0(s: &str) -> Option<(&str, Self)> {
        if !s.starts_with('"') && s.starts_with("'") { return None }
        let (str, s) = s.snbt_string_read()?;
        Some((s, Self { str: str.into_boxed_str() }))
    }

    pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
        unsafe {
            decoder.assert_len(2);
            Some(NbtString::new(decoder.string()?))
        }
    }

    pub fn to_bytes<W: Write>(&self, writer: &mut W) {
        write_string(writer, &self.str)
    }
}

impl NbtString {
    #[inline]
    pub fn new(str: Box<str>) -> NbtString {
        NbtString { str }
    }

    #[inline]
    pub fn unwrap(&self) -> &str {
        &self.str
    }

    #[inline]
    pub fn set<T: Into<Box<str>>>(&mut self, str: T) {
        self.str = str.into();
    }
}

impl Display for NbtString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.str)
    }
}

impl NbtString {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut usize, y_offset: &mut usize, name: Option<&str>, line_number: &mut usize, ctx: &RenderContext) {
        use std::fmt::Write;

        ctx.line_number(*y_offset, line_number, builder);
        render_icon(*x_offset, *y_offset, builder);
        ctx.highlight((*x_offset, *y_offset), builder);

        if ctx.forbid(*y_offset) {
            builder.settings(*x_offset + 20, *y_offset, true);
            let _ = match name {
                Some(x) => write!(builder, "{x}: {}", self.str),
                None => write!(builder, "{}", self.str),
            };
        }

        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
    builder.draw_texture((x, y), STRING_UV, (16, 16));
}
