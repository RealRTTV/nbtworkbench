use std::slice::Iter;
use crate::decoder::read_i16;
use crate::encoder::write_i16;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtShort {
    short: i16
}

impl NbtShort {
    #[inline]
    pub fn from_bytes(iter: &mut Iter<u8>) -> Option<Self> {
        Some(NbtShort::new(read_i16(iter)?))
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_i16(writer, self.short);
    }
}

impl NbtShort {
    #[inline]
    pub fn new(short: i16) -> NbtShort {
        NbtShort { short }
    }

    #[inline]
    pub fn unwrap(&self) -> &i16 {
        &self.short
    }

    #[inline]
    pub fn unwrap_mut(&mut self) -> &mut i16 {
        &mut self.short
    }

    #[inline]
    pub fn set(&mut self, short: i16) {
        self.short = short
    }
}

impl ToString for NbtShort {
    fn to_string(&self) -> String {
        self.short.to_string()
    }
}

impl NbtShort {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>) {
        builder.draw_texture(*x_offset, *y_offset, 16, 0, 16, 16);
        builder.draw_text(*x_offset + 20, *y_offset + 4, &name.map(|x| format!("{}: {}s", x, self.short)).unwrap_or_else(|| self.short.to_string()), true);
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture(x, y, 16, 0, 16, 16);
}
