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
    
    #[inline]
    pub fn unwrap(&self) -> &str {
        &self.str
    }

    #[inline]
    pub fn set(&mut self, str: String) {
        self.str = str;
    }
}

impl ToString for NbtString {
    fn to_string(&self) -> String {
        format!("\"{}\"", &*self.str.clone().replace('\\', "\\\\").replace('"', "\\\"").replace('\n', "\\\n").replace('\r', "\\\r"))
    }
}

impl NbtString {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, forbidden_y: Option<u32>) {
        builder.draw_texture(*x_offset, *y_offset, 16, 16, 16, 16);
        if Some(*y_offset) != forbidden_y {
            builder.draw_text(*x_offset + 20, *y_offset, &name.map(|x| format!("{}: {}", x, self.str)).unwrap_or_else(|| self.str.clone()), true);
        }
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture(x, y, 16, 16, 16, 16);
}
