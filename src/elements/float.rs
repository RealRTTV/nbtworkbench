use crate::assets::FLOAT_UV;
use crate::decoder::Decoder;
use crate::encoder::write_f32;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtFloat {
    float: f32
}

impl NbtFloat {
    #[inline]
    pub fn from_bytes(decoder: &mut Decoder) -> Self {
        unsafe {
            decoder.assert_len(4);
            NbtFloat::new(decoder.f32())
        }
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

    #[inline]
    pub fn set(&mut self, float: Option<f32>) {
        if let Some(float) = float {
            self.float = float;
        }
    }
}

impl ToString for NbtFloat {
    fn to_string(&self) -> String {
        self.float.to_string()
    }
}

impl NbtFloat {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, forbidden_y: Option<u32>) {
        render_icon(*x_offset, *y_offset, builder);
        if Some(*y_offset) != forbidden_y {
            builder.draw_text(*x_offset + 20, *y_offset, &name.map(|x| format!("{}: {}", x, self.float)).unwrap_or_else(|| self.float.to_string()), true);
        }
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture((x, y), FLOAT_UV, (16, 16));
}
