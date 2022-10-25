use crate::assets::STRING_UV;
use crate::decoder::Decoder;
use crate::encoder::write_string;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtString {
    str: String
}

impl NbtString {
    #[inline]
    pub fn from_bytes(decoder: &mut Decoder) -> Self {
        unsafe {
            decoder.assert_len(2);
            NbtString::new(decoder.string())
        }
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_string(writer, &self.str)
    }
}

impl NbtString {
    #[inline]
    pub fn new(str: String) -> NbtString {
        NbtString { str }
    }
    
    #[inline]
    pub fn unwrap(&self) -> &str {
        &self.str
    }

    #[inline]
    pub fn set(&mut self, str: String) {
        self.str = str;
    }
}

impl ToString for NbtString {
    fn to_string(&self) -> String {
        format!("{:#?}", self.str)
    }
}

impl NbtString {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, forbidden_y: Option<u32>) {
        render_icon(*x_offset, *y_offset, builder);
        if Some(*y_offset) != forbidden_y {
            builder.draw_text(*x_offset + 20, *y_offset, &name.map(|x| format!("{}: {}", x, self.str)).unwrap_or_else(|| self.str.clone()), true);
        }
        *y_offset += 16;
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture((x, y), STRING_UV, (16, 16));
}
