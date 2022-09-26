use crate::{NbtElement, VertexBufferBuilder};
use crate::assets::BYTE_ARRAY_UV;
use crate::decoder::Decoder;
use crate::elements::byte::NbtByte;
use crate::encoder::write_u32;
use crate::NbtElement::Byte;

pub struct NbtByteArray {
    bytes: Vec<NbtElement>,
    open: bool,
    height: u32
}

impl NbtByteArray {
    #[inline]
    pub fn from_bytes(decoder: &mut Decoder) -> Self {
        unsafe {
            let count = decoder.u32() as usize;
            let mut bytes = Vec::with_capacity(count);
            decoder.assert_len(count);
            for _ in 0..count {
                bytes.push(Byte(NbtByte::new(decoder.i8())))
            }
            NbtByteArray::new(bytes)
        }
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_u32(writer, self.bytes.len() as u32);
        for byte in self.bytes.iter().map(|byte| if let Byte(byte) = byte { byte } else { panic!() }) {
            byte.to_bytes(writer);
        }
    }
}

impl NbtByteArray {
    #[inline]
    pub fn new(bytes: Vec<NbtElement>) -> NbtByteArray {
        NbtByteArray { height: bytes.len() as u32 + 1, bytes, open: false }
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
        self.open = !self.open && !self.bytes.is_empty();
        true
    }

    #[inline]
    pub fn open(&self) -> bool {
        self.open
    }
    
    #[inline]
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }
}

impl ToString for NbtByteArray {
    fn to_string(&self) -> String {
        // we're assuming on average every byte takes up two characters, (two more per for the comma and space), and in total: two extra for the brackets, and two less for the removal of the final comma and space
        let mut builder = String::with_capacity(self.bytes.len() * 4);
        builder.push_str("[B;");
        for i in 0..self.bytes.len() {
            builder.push_str(&self.bytes[i].to_string());
            if i < self.bytes.len() - 1 {
                builder.push_str(", ");
            }
        }
        builder.push(']');
        builder
    }
}

impl NbtByteArray {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, remaining_scroll: &mut u32, tail: bool, forbidden_y: Option<u32>) {
        if *remaining_scroll >= 16 {
            *remaining_scroll -= 16;
        } else {
            render_icon(*x_offset, *y_offset, builder);
            if !self.bytes.is_empty() {
                builder.draw_texture((*x_offset - 16, *y_offset), (96 + if self.open { 0 } else { 16 }, 16), (16, 16));
            }
            if Some(*y_offset) != forbidden_y {
                builder.draw_text(*x_offset + 20, *y_offset, &format!("{}{} entr{}", name.map(|x| format!("{}: ", x)).unwrap_or_else(|| "".to_owned()), self.bytes.len(), if self.bytes.len() == 1 { "y" } else { "ies" }), true);
            }
            *y_offset += 16;
        }
        *x_offset += 16;
        if self.open {
            for (index, element) in self.bytes.iter().enumerate() {
                if *y_offset > builder.window_height() {
                    break
                } else {
                    if *remaining_scroll >= 16 {
                        *remaining_scroll -= 16;
                        continue;
                    }

                    if *remaining_scroll < 16 {
                        builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, if index != self.bytes.len() - 1 { 16 } else { 9 }));
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
            let bytes = if let NbtElement::ByteArray(bytes) = wrapped { bytes } else { panic!() };
            if bytes.open {
                for (index, byte) in bytes.bytes.iter_mut().enumerate() {
                    if *y == 0 {
                        tail(byte, *depth + 1, index as u32);
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
    pub fn delete(&mut self, index: u32) -> Option<NbtElement>  {
        Some(self.bytes.remove(index as usize))
    }

    #[inline]
    pub fn drop(&mut self, element: NbtElement, y: &mut u32, parent_y: u32) -> Result<NbtElement, (u32, Option<(u32, u32)>)> {
        if *y < 16 {
            *y = 0;
            Ok(element)
        } else {
            *y -= 16;
            let mut child_y = parent_y + 1;
            for (index, value) in self.bytes.iter_mut().enumerate() {
                if *y < 16 {
                    let height = element.height();
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
    pub fn child_height(&self, _: u32) -> u32 {
        1
    }

    #[inline]
    pub fn drop_index(&mut self, index: u32, other: NbtElement) -> bool {
        if let Byte(_) = other {
            self.bytes.insert(index as usize, other);
            self.increment(1);
            true
        } else {
            false
        }
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture((x, y), BYTE_ARRAY_UV, (16, 16));
}
