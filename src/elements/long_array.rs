use std::slice::Iter;
use crate::decoder::{read_i64, read_u32};
use crate::elements::long::NbtLong;
use crate::encoder::{write_i64, write_u32};
use crate::{NbtElement, VertexBufferBuilder};
use crate::NbtElement::Long;

pub struct NbtLongArray {
    longs: Vec<NbtElement>,
    open: bool,
    height: u32
}

impl NbtLongArray {
    #[inline]
    pub fn from_bytes(iter: &mut Iter<u8>) -> Option<Self> {
        let length = read_u32(iter)? as usize;
        let mut vec = Vec::with_capacity(length);
        for _ in 0..length {
            vec.push(Long(NbtLong::new(read_i64(iter)?)));
        }
        Some(NbtLongArray::new(vec))
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_u32(writer, self.longs.len() as u32);
        for long in &self.longs {
            write_i64(writer, if let Long(long) = long { *long.unwrap() } else { panic!() })
        }
    }
}

impl NbtLongArray {
    #[inline]
    pub fn new(longs: Vec<NbtElement>) -> NbtLongArray {
        NbtLongArray { height: longs.len() as u32 + 1, longs, open: false }
    }

    #[inline]
    pub fn increment(&mut self, amount: u32) {
        self.height += amount;
    }

    #[inline]
    pub fn decrement(&mut self, amount: u32) {
        self.height -= amount;
    }

    #[inline]
    pub fn height(&self) -> u32 {
        if self.open {
            self.height
        } else {
            1
        }
    }

    #[inline]
    pub fn toggle(&mut self) -> bool {
        self.open = !self.open && !self.longs.is_empty();
        true
    }

    #[inline]
    pub fn open(&self) -> bool {
        self.open
    }
}

impl ToString for NbtLongArray {
    fn to_string(&self) -> String {
        let mut builder = String::with_capacity(self.longs.len() * 6);
        builder.push_str("[L;"); // todo, check
        for i in 0..self.longs.len() {
            builder.push_str(&self.longs[i].to_string());
            if i < self.longs.len() - 1 {
                builder.push_str(", ");
            }
        }
        builder.push(']');
        builder
    }
}

impl NbtLongArray {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, remaining_scroll: &mut u32, tail: bool) {
        if *remaining_scroll >= 16 {
            *remaining_scroll -= 16;
        } else {
            builder.draw_texture(*x_offset, *y_offset, 128, 0, 16, 16);
            if !self.longs.is_empty() {
                builder.draw_texture(*x_offset - 16, *y_offset, 224 + if self.open { 0 } else { 16 }, 0, 16, 16);
            }
            builder.draw_text(*x_offset + 20, *y_offset + 4, &format!("{}{} entr{}", name.map(|x| format!("{}: ", x)).unwrap_or_else(|| "".to_string()), self.longs.len(), if self.longs.len() == 1 { "y" } else { "ies" }), true);
            *y_offset += 16;
        }
        *x_offset += 16;
        if self.open {
            for (index, element) in self.longs.iter().enumerate() {
                if *y_offset > builder.window_height() {
                    break
                } else {
                    if *remaining_scroll >= 16 {
                        *remaining_scroll -= 16;
                        continue;
                    }

                    if *remaining_scroll < 16 {
                        builder.draw_texture(*x_offset - 16, *y_offset, 208, 0, 16, if index != self.longs.len() - 1 { 16 } else { 9 });
                        if !tail {
                            builder.draw_texture(*x_offset - 32, *y_offset, 208, 0, 8, 16);
                        }
                        element.render(x_offset, y_offset, remaining_scroll, builder, None, false);
                    }
                }
            }
        }
        *x_offset -= 16;
    }

    pub fn stack<F: FnMut(&mut NbtElement), G: FnMut(&mut NbtElement, u32, u32)>(wrapped: &mut NbtElement, y: &mut u32, depth: &mut u32, index: u32, parent: &mut F, tail: &mut G) -> bool {
        if *y == 0 {
            tail(wrapped, *depth, index);
            true
        } else {
            *y -= 1;
            let longs = if let NbtElement::LongArray(longs) = wrapped { longs } else { panic!() };
            if longs.open {
                for (index, long) in longs.longs.iter_mut().enumerate() {
                    if *y == 0 {
                        tail(long, *depth + 1, index as u32);
                        parent(wrapped);
                        return true;
                    } else {
                        *y -= 1;
                    }
                }
            }
            false
        }
    }

    #[inline]
    pub fn delete(&mut self, index: u32) {
        self.longs.remove(index as usize);
    }

    #[inline]
    pub fn drop(&mut self, other: NbtElement) -> bool {
        if let Long(_) = other {
            self.longs.push(other);
            self.increment(1);
            true
        } else {
            false
        }
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture(x, y, 128, 0, 16, 16);
}
