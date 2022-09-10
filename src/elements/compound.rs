use std::collections::HashMap;
use std::slice::Iter;
use crate::assets::HEADER_SIZE;

use crate::decoder::{read_string, read_u8};
use crate::elements::element_type::NbtElement;
use crate::encoder::{write_string, write_u8};
use crate::VertexBufferBuilder;

pub struct NbtCompound {
    entries: HashMap<String, NbtElement>,
    keys: Vec<String>,
    open: bool,
    height: u32
}

impl NbtCompound {
    #[inline]
    pub fn from_bytes(iter: &mut Iter<u8>) -> Option<Self> {
        let mut compound = NbtCompound::new();
        let mut current_element = read_u8(iter)?;
        while current_element != 0u8 {
            compound.put(read_string(iter)?, NbtElement::from_bytes(&current_element, iter)?);
            current_element = read_u8(iter)?;
        }
        Some(compound)
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        for key in &self.keys {
            let value = self.entries.get(key).unwrap();
            write_u8(writer, value.id());
            write_string(writer, key);
            NbtElement::to_bytes(value, writer);
        }
        write_u8(writer, 0);
    }
}

impl NbtCompound {
    #[inline]
    pub fn new() -> Self {
        Self { height: 1, entries: HashMap::new(), keys: Vec::new(), open: false }
    }

    #[inline]
    pub fn put(&mut self, str: String, element: NbtElement) {
        match self.entries.get(&*str) {
            None => self.keys.push(str.to_string()),
            Some(x) => self.decrement(x.height())
        }
        self.increment(element.height());
        self.entries.insert(str, element);
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
        self.open = !self.open && !self.keys.is_empty();
        if !self.open {
            for key in &self.keys {
                let value = self.entries.get_mut(key).unwrap();
                if value.open() {
                    value.toggle();
                }
            }
            self.height = self.keys.len() as u32 + 1;
        }
        true
    }

    #[inline]
    pub fn open(&self) -> bool {
        self.open
    }

    #[inline]
    pub fn update_key(&mut self, index: u32, name: String) {
        let key = self.keys.get_mut(index as usize);
        if let Some(key) = key {
            let value = self.entries.remove(key).unwrap();
            *key = name.clone();
            self.entries.insert(name, value);
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    #[inline]
    pub fn key(&self, index: u32) -> &str {
        self.keys.get(index as usize).unwrap()
    }

    #[inline]
    pub fn get(&self, index: u32) -> Option<&NbtElement> {
        self.entries.get(self.keys.get(index as usize)?)
    }

    #[inline]
    pub fn render_root(&self, builder: &mut VertexBufferBuilder, str: &str) {
        let x_offset = &mut 20;
        let y_offset = &mut HEADER_SIZE;
        let remaining_scroll = &mut builder.scroll();
        if *remaining_scroll >= 16 {
            *remaining_scroll -= 16;
        } else {
            builder.draw_texture(*x_offset, *y_offset, 192, 0, 16, 16);
            builder.draw_texture(*x_offset - 16, *y_offset, 208, 0, 16, 9);
            if !self.keys.is_empty() {
                builder.draw_texture(*x_offset - 16, *y_offset, 224 + if self.open { 0 } else { 16 }, 0, 16, 16);
            }
            builder.draw_text(*x_offset + 20, *y_offset + 4, &format!("{} [{} entr{}]", str, self.keys.len(), if self.keys.len() == 1 { "y" } else { "ies" }), true);
            *y_offset += 16;
        }
        *x_offset += 16;
        if self.open {
            for (index, name) in self.keys.iter().enumerate() {
                if *y_offset > builder.window_height() {
                    break
                } else {
                    let entry = self.entries.get(name).expect("has key, has value");
                    if *remaining_scroll >= entry.height() * 16 {
                        *remaining_scroll -= entry.height() * 16;
                        continue;
                    }

                    if *remaining_scroll < 16 {
                        builder.draw_texture(*x_offset - 16, *y_offset, 208, 0, 16, if index != self.keys.len() - 1 { 16 } else { 9 });
                    }

                    if entry.height() == 1 && *remaining_scroll >= 16 {
                        *remaining_scroll -= 16;
                    } else {
                        entry.render(x_offset, y_offset, remaining_scroll, builder, Some(name), index == self.keys.len() - 1);
                    }
                }
            }
        }
        *x_offset -= 16;
    }
}

impl ToString for NbtCompound {
    fn to_string(&self) -> String {
        let mut builder = String::with_capacity(self.keys.len() * 8);
        builder.push('{');
        for (i, key) in self.keys.iter().enumerate() {
            let value = &self.entries[key];
            builder.push_str(key);
            builder.push_str(": ");
            builder.push_str(&value.to_string());
            if i < self.keys.len() - 1 {
                builder.push_str(", ");
            }
        }
        builder.push('}');
        builder
    }
}

impl NbtCompound {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, remaining_scroll: &mut u32, tail: bool) {
        let x_before = *x_offset - 16;
        let y_before;
        if *remaining_scroll >= 16 {
            *remaining_scroll -= 16;
            y_before = *y_offset + 16;
        } else {
            builder.draw_texture(*x_offset, *y_offset, 176, 0, 16, 16);
            if !self.keys.is_empty() {
                builder.draw_texture(*x_offset - 16, *y_offset, 224 + if self.open { 0 } else { 16 }, 0, 16, 16);
            }
            builder.draw_text(*x_offset + 20, *y_offset + 4, &format!("{}{} entr{}", name.map(|x| format!("{}: ", x)).unwrap_or_else(|| "".to_string()), self.keys.len(), if self.keys.len() == 1 { "y" } else { "ies" }), true);
            *y_offset += 16;
            y_before = *y_offset;
        }
        *x_offset += 16;
        if self.open {
            for (index, name) in self.keys.iter().enumerate() {
                if *y_offset > builder.window_height() {
                    break
                } else {
                    let entry = self.entries.get(name).unwrap();
                    if *remaining_scroll >= entry.height() * 16 {
                        *remaining_scroll -= entry.height() * 16;
                        continue;
                    }

                    if *remaining_scroll < 16 {
                        builder.draw_texture(*x_offset - 16, *y_offset, 208, 0, 16, if index != self.keys.len() - 1 { 16 } else { 9 });
                    }

                    if entry.height() == 1 && *remaining_scroll >= 16 {
                        *remaining_scroll -= 16;
                    } else {
                        entry.render(x_offset, y_offset, remaining_scroll, builder, Some(name), tail && index == self.keys.len() - 1);
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
            let compound = if let NbtElement::Compound(compound) = wrapped { compound } else { panic!() };
            if compound.open {
                *depth += 1;
                for (index, key) in compound.keys.iter().enumerate() {
                    let value = compound.entries.get_mut(key).expect("has key, has value");
                    if value.stack(y, depth, index as u32, parent, tail) {
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
        let key = self.keys.remove(index as usize);
        self.entries.remove(&key);
    }

    #[inline]
    pub fn drop(&mut self, other: NbtElement) -> bool {
        if self.entries.contains_key("_") {
            false
        } else {
            self.entries.insert("_".to_string(), other);
            self.keys.push("_".to_string());
            self.increment(1);
            true
        }
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture(x, y, 176, 0, 16, 16);
}
