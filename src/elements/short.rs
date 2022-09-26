use crate::assets::SHORT_UV;
use crate::decoder::Decoder;
use crate::encoder::write_i16;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtShort {
    short: i16
}

impl NbtShort {
    #[inline]
    pub fn from_bytes(decoder: &mut Decoder) -> Self {
        unsafe {
            decoder.assert_len(2);
            NbtShort::new(decoder.i16())
        }
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
    pub fn set(&mut self, short: Option<i16>) {
        if let Some(short) = short {
            self.short = short
        }
    }
}

impl ToString for NbtShort {
    fn to_string(&self) -> String {
        self.short.to_string()
    }
}

impl NbtShort {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, forbidden_y: Option<u32>) {
        render_icon(*x_offset, *y_offset, builder);
        if Some(*y_offset) != forbidden_y {
            builder.draw_text(*x_offset + 20, *y_offset, &name.map(|x| format!("{}: {}", x, self.short)).unwrap_or_else(|| self.short.to_string()), true);
        }
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture((x, y), SHORT_UV, (16, 16));
}
