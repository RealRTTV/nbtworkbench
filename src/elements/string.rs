use std::slice::Iter;
use crate::decoder::read_string;
use crate::encoder::write_string;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtString {
    str: String
}

impl NbtString {
    #[inline]
    pub fn from_bytes(iter: &mut Iter<u8>) -> Option<Self> {
        Some(NbtString::new(read_string(iter)?))
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_string(writer, &self.str)
    }
}

impl NbtString {
    #[inline]
    pub fn new(str: String) -> NbtString {
        NbtString { str }
    }
}

impl ToString for NbtString {
    fn to_string(&self) -> String {
        format!("\"{}\"", &*self.str.clone().replace('\\', "\\\\").replace('"', "\\\"").replace('\n', "\\\n").replace('\r', "\\\r"))
    }
}

impl NbtString {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>) {
        builder.draw_texture(*x_offset, *y_offset, 144, 0, 16, 16);
        builder.draw_text(*x_offset + 20, *y_offset + 4, &name.map(|x| format!("{}: {}", x, self.str)).unwrap_or_else(|| self.str.clone()), true);
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture(x, y, 144, 0, 16, 16);
}
