use crate::assets::INT_UV;
use crate::decoder::Decoder;
use crate::encoder::write_i32;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtInt {
    int: i32
}

impl NbtInt {
    #[inline]
    pub fn from_bytes(decoder: &mut Decoder) -> Self {
        unsafe {
            decoder.assert_len(4);
            NbtInt::new(decoder.i32())
        }
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
    pub fn set(&mut self, int: Option<i32>) {
        if let Some(int) = int {
            self.int = int
        }
    }
}

impl ToString for NbtInt {
    fn to_string(&self) -> String {
        self.int.to_string()
    }
}

impl NbtInt {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, forbidden_y: Option<u32>) {
        render_icon(*x_offset, *y_offset, builder);
        if Some(*y_offset) != forbidden_y {
            builder.draw_text(*x_offset + 20, *y_offset, &name.map(|x| format!("{}: {}", x, self.int)).unwrap_or_else(|| self.int.to_string()), true);
        }
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture((x, y), INT_UV, (16, 16));
}
