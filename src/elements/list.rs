use std::slice::Iter;
use crate::decoder::{read_u32, read_u8};
use crate::elements::element_type::NbtElement;
use crate::encoder::{write_u32, write_u8};
use crate::VertexBufferBuilder;

pub struct NbtList {
    elements: Vec<NbtElement>,
    element: u8,
    open: bool,
    height: u32
}

impl NbtList {
    #[inline]
    pub fn from_bytes(iter: &mut Iter<u8>) -> Option<Self> {
        let element = read_u8(iter)?;
        let length = read_u32(iter)? as usize;
        let mut elements = Vec::with_capacity(length);
        for _ in 0..length {
            elements.push(NbtElement::from_bytes(&element, iter)?);
        }
        Some(NbtList::new(elements, element))
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        write_u8(writer, self.element);
        write_u32(writer, self.elements.len() as u32);
        for element in &self.elements {
            NbtElement::to_bytes(element, writer)
        }
    }
}

impl NbtList {
    #[inline]
    pub fn new(elements: Vec<NbtElement>, element: u8) -> Self {
        NbtList { height: elements.iter().map(|x| x.height()).sum::<u32>() + 1, elements, element, open: false }
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
        self.open = !self.open && !self.elements.is_empty();
        if !self.open {
            for element in &mut self.elements {
                if element.open() {
                    element.toggle();
                }
            }
            self.height = self.elements.len() as u32 + 1;
        }
        true
    }

    #[inline]
    pub fn open(&self) -> bool {
        self.open
    }
}

impl ToString for NbtList {
    fn to_string(&self) -> String {
        let mut builder = String::with_capacity(self.elements.len() * 8); // i hope, i really hope
        builder.push('[');
        for i in 0..self.elements.len() {
            builder.push_str(&self.elements[i].to_string());
            if i < self.elements.len() - 1 {
                builder.push_str(", ");
            }
        }
        builder.push(']');
        builder
    }
}

impl NbtList {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, remaining_scroll: &mut u32, tail: bool) {
        let x_before = *x_offset - 16;
        let y_before;
        if *remaining_scroll >= 16 {
            *remaining_scroll -= 16;
            y_before = *y_offset + 16;
        } else {
            builder.draw_texture(*x_offset, *y_offset, 160, 0, 16, 16);
            if !self.elements.is_empty() {
                builder.draw_texture(*x_offset - 16, *y_offset, 224 + if self.open { 0 } else { 16 }, 0, 16, 16);
            }
            builder.draw_text(*x_offset + 20, *y_offset + 4, &format!("{}{} entr{}", name.map(|x| format!("{}: ", x)).unwrap_or_else(|| "".to_string()), self.elements.len(), if self.elements.len() == 1 { "y" } else { "ies" }), true);
            *y_offset += 16;
            y_before = *y_offset;
        }
        *x_offset += 16;
        if self.open {
            for (index, element) in self.elements.iter().enumerate() {
                if *y_offset > builder.window_height() {
                    break
                } else {
                    if *remaining_scroll >= element.height() * 16 {
                        *remaining_scroll -= element.height() * 16;
                        continue;
                    }

                    if *remaining_scroll < 16 {
                        builder.draw_texture(*x_offset - 16, *y_offset, 208, 0, 16, if index != self.elements.len() - 1 { 16 } else { 9 });
                    }

                    if element.height() == 1 && *remaining_scroll >= 16 {
                        *remaining_scroll -= 16;
                    } else {
                        element.render(x_offset, y_offset, remaining_scroll, builder, None, tail && index == self.elements.len() - 1);
                        let difference = *y_offset - y_before;
                        if difference >= 16 && !tail {
                            for i in 0..difference / 16 + 1 {
                                let y = y_before + i * 16 - 16;
                                builder.draw_texture(x_before, y, 112, 64, 8, 16);
                            }
                        }
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
            let list = if let NbtElement::List(list) = wrapped { list } else { panic!() };
            if list.open {
                *depth += 1;
                for (index, element) in list.elements.iter_mut().enumerate() {
                    if element.stack(y, depth, index as u32, parent, tail) {
                        parent(wrapped);
                        return true;
                    }
                }
                *depth -= 1;
            }
            false
        }
    }

    #[inline]
    pub fn delete(&mut self, index: u32) {
        self.elements.remove(index as usize);
    }

    #[inline]
    pub fn drop(&mut self, other: NbtElement) -> bool {
        if self.element == other.id() || self.elements.is_empty() {
            self.element = other.id();
            self.elements.push(other);
            self.increment(1);
            true
        } else {
            false
        }
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture(x, y, 160, 0, 16, 16);
}
