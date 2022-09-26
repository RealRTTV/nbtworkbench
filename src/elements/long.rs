use crate::assets::LONG_UV;
use crate::decoder::Decoder;
use crate::encoder::write_i64;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtLong {
    long: i64
}

impl NbtLong {
    #[inline]
    pub fn from_bytes(decoder: &mut Decoder) -> Self {
        unsafe {
            decoder.assert_len(8);
            NbtLong::new(decoder.i64())
        }
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
    pub fn set(&mut self, long: Option<i64>) {
        if let Some(long) = long {
            self.long = long;
        }
    }
}

impl ToString for NbtLong {
    fn to_string(&self) -> String {
        self.long.to_string()
    }
}

impl NbtLong {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, forbidden_y: Option<u32>) {
        render_icon(*x_offset, *y_offset, builder);
        if Some(*y_offset) != forbidden_y {
            builder.draw_text(*x_offset + 20, *y_offset, &name.map(|x| format!("{}: {}", x, self.long)).unwrap_or_else(|| self.long.to_string()), true);
        }
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture((x, y), LONG_UV, (16, 16));
}
