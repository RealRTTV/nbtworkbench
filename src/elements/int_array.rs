use crate::{NbtElement, VertexBufferBuilder};
use crate::assets::INT_ARRAY_UV;
use crate::decoder::Decoder;
use crate::elements::int::NbtInt;
use crate::encoder::write_u32;
use crate::NbtElement::Int;

pub struct NbtIntArray {
    ints: Vec<NbtElement>,
    open: bool,
    height: u32
}

impl NbtIntArray {
    #[inline]
    pub fn from_bytes(decoder: &mut Decoder) -> Self {
        unsafe {
            decoder.assert_len(4);
            let length = decoder.u32() as usize;
            decoder.assert_len(length * 4);
            let mut vec = Vec::with_capacity(length);
            for _ in 0..length {
                vec.push(Int(NbtInt::new(decoder.i32())))
            }
            NbtIntArray::new(vec)
        }
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_u32(writer, self.ints.len() as u32);
        for int in &self.ints {
            int.to_bytes(writer);
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

    #[inline]
    pub fn len(&self) -> usize {
        self.ints.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.ints.is_empty()
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
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, remaining_scroll: &mut u32, tail: bool, forbidden_y: Option<u32>) {
        if *remaining_scroll >= 16 {
            *remaining_scroll -= 16;
        } else {
            render_icon(*x_offset, *y_offset, builder);
            if !self.ints.is_empty() {
                builder.draw_texture((*x_offset - 16, *y_offset), (96 + if self.open { 0 } else { 16 }, 16), (16, 16));
            }
            if Some(*y_offset) != forbidden_y {
                builder.draw_text(*x_offset + 20, *y_offset, &format!("{}{} entr{}", name.map(|x| format!("{}: ", x)).unwrap_or_else(|| "".to_owned()), self.ints.len(), if self.ints.len() == 1 { "y" } else { "ies" }), true);
            }
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
                        builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, if index != self.ints.len() - 1 { 16 } else { 9 }));
                        if !tail {
                            builder.draw_texture((*x_offset - 32, *y_offset), (80, 16), (8, 16));
                        }
                        element.render(x_offset, y_offset, remaining_scroll, builder, None, false, forbidden_y);
                    }
                }
            }
        }
        *x_offset -= 16;
    }

    pub fn stack<F: FnMut(&mut NbtElement, u32), G: FnOnce(&mut NbtElement, u32, u32)>(wrapped: &mut NbtElement, y: &mut u32, depth: &mut u32, index: u32, parent: &mut F, tail: G) -> Option<G> {
        if *y == 0 {
            tail(wrapped, *depth, index);
            None
        } else {
            *y -= 1;
            let ints = if let NbtElement::IntArray(ints) = wrapped { ints } else { panic!() };
            if ints.open {
                for (index, int) in ints.ints.iter_mut().enumerate() {
                    if *y == 0 {
                        tail(int, *depth + 1, index as u32);
                        parent(wrapped, *y);
                        return None;
                    } else {
                        *y -= 1;
                    }
                }
            }
            Some(tail)
        }
    }

    #[inline]
    pub fn delete(&mut self, index: u32) -> Option<NbtElement> {
        Some(self.ints.remove(index as usize))
    }

    #[inline]
    pub fn child_height(&self, _: u32) -> u32 {
        1
    }

    #[inline]
    pub fn drop(&mut self, element: NbtElement, y: &mut u32, parent_y: u32) -> Result<NbtElement, (u32, Option<(u32, u32)>)> {
        if *y < 16 {
            *y = 0;
            Ok(element)
        } else {
            let mut child_y = parent_y + 1;
            *y -= 16;
            for (index, value) in self.ints.iter_mut().enumerate() {
                if *y < 16 {
                    let height = value.height();
                    return Err(match value.drop(element, y, child_y) {
                        Ok(element) => {
                            self.drop_index(index as u32, element);
                            (height, Some((index as u32, parent_y)))
                        },
                        Err((increment, index)) => {
                            self.increment(increment);
                            (increment, index)
                        }
                    })
                } else {
                    *y -= 16;
                    child_y += value.height();
                }
            }
            if *y < 8 {
                let height = element.height();
                self.drop_index(self.len() as u32, element);
                Err((height, Some((self.len() as u32 - 1, parent_y))))
            } else {
                Err((0, None))
            }
        }
    }

    #[inline]
    pub fn drop_index(&mut self, index: u32, other: NbtElement) -> bool {
        if let Int(_) = other {
            self.ints.insert(index as usize, other);
            self.increment(1);
            true
        } else {
            false
        }
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture((x, y), INT_ARRAY_UV, (16, 16));
}
