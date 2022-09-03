use std::slice::Iter;
use crate::decoder::read_i32;
use crate::encoder::write_i32;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtInt {
    int: i32
}

impl NbtInt {
    #[inline]
    pub fn from_bytes(iter: &mut Iter<u8>) -> Option<Self> {
        Some(NbtInt::new(read_i32(iter)?))
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_i32(writer, self.int);
    }
}

impl NbtInt {
    #[inline]
    pub fn new(int: i32) -> NbtInt {
        NbtInt { int }
    }

    #[inline]
    pub fn unwrap(&self) -> &i32 {
        &self.int
    }

    #[inline]
    pub fn unwrap_mut(&mut self) -> &mut i32 {
        &mut self.int
    }

    #[inline]
    pub fn set(&mut self, int: i32) {
        self.int = int
    }
}

impl ToString for NbtInt {
    fn to_string(&self) -> String {
        self.int.to_string()
    }
}

impl NbtInt {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>) {
        builder.draw_texture(*x_offset, *y_offset, 32, 0, 16, 16);
        builder.draw_text(*x_offset + 20, *y_offset + 4, &name.map(|x| format!("{}: {}", x, self.int)).unwrap_or_else(|| self.int.to_string()), true);
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture(x, y, 32, 0, 16, 16);
}
