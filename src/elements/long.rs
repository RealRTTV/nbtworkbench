use std::slice::Iter;
use crate::decoder::read_i64;
use crate::encoder::write_i64;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtLong {
    long: i64
}

impl NbtLong {
    #[inline]
    pub fn from_bytes(iter: &mut Iter<u8>) -> Option<Self> {
        Some(NbtLong::new(read_i64(iter)?))
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_i64(writer, self.long);
    }
}

impl NbtLong {
    #[inline]
    pub fn new(long: i64) -> NbtLong {
        NbtLong { long }
    }

    #[inline]
    pub fn unwrap(&self) -> &i64 {
        &self.long
    }

    #[inline]
    pub fn unwrap_mut(&mut self) -> &mut i64 {
        &mut self.long
    }

    #[inline]
    pub fn set(&mut self, long: i64) {
        self.long = long
    }
}

impl ToString for NbtLong {
    fn to_string(&self) -> String {
        self.long.to_string()
    }
}

impl NbtLong {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>) {
        builder.draw_texture(*x_offset, *y_offset, 48, 0, 16, 16);
        builder.draw_text(*x_offset + 20, *y_offset + 4, &name.map(|x| format!("{}: {}L", x, self.long)).unwrap_or_else(|| self.long.to_string()), true);
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture(x, y, 48, 0, 16, 16);
}
