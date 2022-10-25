use std::fmt::Display;
use std::io::Write;
use std::str::FromStr;
use crate::VertexBufferBuilder;

#[repr(transparent)]
pub struct NbtPrimitive<T: FromStr + ToString + Display, const U: u32, const V: u32> {
    pub value: T
}

impl<T: FromStr + ToString + Display, const U: u32, const V: u32> NbtPrimitive<T, U, V> {
    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) where [(); std::mem::size_of::<T>()]: {
        unsafe {
            drop(writer.write(&*(&self.value as *const T as *const [u8; std::mem::size_of::<T>()])));
        }
    }

    #[inline]
    pub fn set(&mut self, option: Option<T>) {
        if let Some(t) = option {
            self.value = t
        }
    }

    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, forbidden_y: Option<u32>) {
        Self::render_icon(*x_offset, *y_offset, builder);
        if Some(*y_offset) != forbidden_y {
            builder.draw_text(*x_offset + 20, *y_offset, &name.map(|x| format!("{}: {}", x, self.value)).unwrap_or_else(|| self.value.to_string()), true);
        }
        *y_offset += 16;
    }

    #[inline]
    pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
        builder.draw_texture((x, y), (U, V), (16, 16));
    }
}

impl<T: FromStr + ToString + Display, const U: u32, const V: u32> ToString for NbtPrimitive<T, U, V> {
    fn to_string(&self) -> String {
        self.value.to_string()
    }
}
