use std::slice::Iter;
use crate::decoder::read_f32;
use crate::encoder::write_f32;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtFloat {
    float: f32
}

impl NbtFloat {
    #[inline]
    pub fn from_bytes(iter: &mut Iter<u8>) -> Option<Self> {
        Some(NbtFloat::new(read_f32(iter)?))
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_f32(writer, self.float);
    }
}

impl NbtFloat {
    #[inline]
    pub fn new(float: f32) -> NbtFloat {
        NbtFloat { float }
    }

    #[inline]
    pub fn unwrap(&self) -> &f32 {
        &self.float
    }
}

impl ToString for NbtFloat {
    fn to_string(&self) -> String {
        self.float.to_string()
    }
}

impl NbtFloat {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>) {
        builder.draw_texture(*x_offset, *y_offset, 64, 0, 16, 16);
        builder.draw_text(*x_offset + 20, *y_offset + 4, &name.map(|x| format!("{}: {}f", x, self.float)).unwrap_or_else(|| self.float.to_string()), true);
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture(x, y, 64, 0, 16, 16);
}
