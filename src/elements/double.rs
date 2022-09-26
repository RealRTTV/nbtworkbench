use crate::assets::DOUBLE_UV;
use crate::decoder::Decoder;
use crate::encoder::write_f64;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtDouble {
    double: f64
}

impl NbtDouble {
    #[inline]
    pub fn from_bytes(decoder: &mut Decoder) -> Self {
        unsafe {
            decoder.assert_len(8);
            NbtDouble::new(decoder.f64())
        }
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_f64(writer, self.double);
    }
}

impl NbtDouble {
    #[inline]
    pub fn new(double: f64) -> NbtDouble {
        NbtDouble { double }
    }

    #[inline]
    pub fn unwrap(&self) -> &f64 {
        &self.double
    }


    #[inline]
    pub fn set(&mut self, double: Option<f64>) {
        if let Some(double) = double {
            self.double = double;
        }
    }
}

impl ToString for NbtDouble {
    fn to_string(&self) -> String {
        self.double.to_string()
    }
}

impl NbtDouble {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, forbidden_y: Option<u32>) {
        render_icon(*x_offset, *y_offset, builder);
        if Some(*y_offset) != forbidden_y {
            builder.draw_text(*x_offset + 20, *y_offset, &name.map(|x| format!("{}: {}", x, self.double)).unwrap_or_else(|| self.double.to_string()), true);
        }
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture((x, y), DOUBLE_UV, (16, 16));
}
