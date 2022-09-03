use std::slice::Iter;
use crate::decoder::{read_i32, read_u32};
use crate::elements::int::NbtInt;
use crate::encoder::{write_i32, write_u32};
use crate::{NbtElement, VertexBufferBuilder};
use crate::NbtElement::Int;

pub struct NbtIntArray {
    ints: Vec<NbtElement>,
    open: bool,
    height: u32
}

impl NbtIntArray {
    #[inline]
    pub fn from_bytes(iter: &mut Iter<u8>) -> Option<Self> {
        let length = read_u32(iter)? as usize;
        let mut vec = Vec::with_capacity(length);
        for _ in 0..length {
            vec.push(Int(NbtInt::new(read_i32(iter)?)))
        }
        Some(NbtIntArray::new(vec))
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_u32(writer, self.ints.len() as u32);
        for int in &self.ints {
            write_i32(writer, if let Int(int) = int { *int.unwrap() } else { panic!() })
        }
    }
}

impl NbtIntArray {
    #[inline]
    pub fn new(ints: Vec<NbtElement>) -> Self {
        NbtIntArray { height: ints.len() as u32 + 1, ints, open: false }
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
        self.open = !self.open && !self.ints.is_empty();
        true
    }

    #[inline]
    pub fn open(&self) -> bool {
        self.open
    }
}

impl ToString for NbtIntArray {
    fn to_string(&self) -> String {
        let mut builder = String::with_capacity(self.ints.len() * 4);
        builder.push_str("[I;");
        for i in 0..self.ints.len() {
            builder.push_str(&self.ints[i].to_string());
            if i < self.ints.len() - 1 {
                builder.push_str(", ");
            }
        }
        builder.push(']');
        builder
    }
}

impl NbtIntArray {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, remaining_scroll: &mut u32, tail: bool) {
        if *remaining_scroll >= 16 {
            *remaining_scroll -= 16;
        } else {
            builder.draw_texture(*x_offset, *y_offset, 112, 0, 16, 16);
            if !self.ints.is_empty() {
                builder.draw_texture(*x_offset - 16, *y_offset, 224 + if self.open { 0 } else { 16 }, 0, 16, 16);
            }
            builder.draw_text(*x_offset + 20, *y_offset + 4, &format!("{}{} entr{}", name.map(|x| format!("{}: ", x)).unwrap_or_else(|| "".to_string()), self.ints.len(), if self.ints.len() == 1 { "y" } else { "ies" }), true);
            *y_offset += 16;
        }
        *x_offset += 16;
        if self.open {
            for (index, element) in self.ints.iter().enumerate() {
                if *y_offset > builder.window_height() {
                    break
                } else {
                    if *remaining_scroll >= 16 {
                        *remaining_scroll -= 16;
                        continue;
                    }

                    if *remaining_scroll < 16 {
                        builder.draw_texture(*x_offset - 16, *y_offset, 208, 0, 16, if index != self.ints.len() - 1 { 16 } else { 9 });
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
            let ints = if let NbtElement::IntArray(ints) = wrapped { ints } else { panic!() };
            if ints.open {
                for (index, int) in ints.ints.iter_mut().enumerate() {
                    if *y == 0 {
                        tail(int, *depth + 1, index as u32);
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
        self.ints.remove(index as usize);
    }

    #[inline]
    pub fn drop(&mut self, other: NbtElement) -> bool {
        if let Int(_) = other {
            self.ints.push(other);
            self.increment(1);
            true
        } else {
            false
        }
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture(x, y, 112, 0, 16, 16);
}
