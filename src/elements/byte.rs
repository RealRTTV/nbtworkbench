use std::slice::Iter;

use crate::decoder::read_i8;
use crate::encoder::write_i8;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtByte {
    byte: i8
}

impl NbtByte {
    #[inline]
    pub fn from_bytes(iter: &mut Iter<u8>) -> Option<Self> {
         Some(NbtByte::new(read_i8(iter)?))
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_i8(writer, self.byte);
    }
}

impl NbtByte {
    #[inline]
    pub fn new(byte: i8) -> NbtByte {
        NbtByte { byte }
    }

    #[inline]
    pub fn unwrap(&self) -> &i8 {
        &self.byte
    }

    #[inline]
    pub fn set(&mut self, byte: i8) {
        self.byte = byte
    }
}

impl ToString for NbtByte {
    fn to_string(&self) -> String {
        self.byte.to_string()
    }
}

impl NbtByte {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, forbidden_y: Option<u32>) {
        builder.draw_texture(*x_offset, *y_offset, 0, 0, 16, 16);
        if Some(*y_offset) != forbidden_y {
            builder.draw_text(*x_offset + 20, *y_offset, &name.map(|x| format!("{}: {}", x, self.byte)).unwrap_or_else(|| self.byte.to_string()), true);
        }
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture(x, y, 0, 0, 16, 16);
}
