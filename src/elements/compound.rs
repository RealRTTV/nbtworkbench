use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::assets::{COMPOUND_UV, HEADER_SIZE};
use crate::decoder::Decoder;
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
    pub fn from_bytes(decoder: &mut Decoder) -> Self {
        let mut compound = NbtCompound::new();
        unsafe {
            decoder.assert_len(1);
            let mut current_element = decoder.u8();
            while current_element != 0 {
                decoder.assert_len(2);
                compound.put(decoder.string(), NbtElement::from_bytes(current_element, decoder));
                decoder.assert_len(1);
                current_element = decoder.u8();
            }
            compound
        }
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

impl Default for NbtCompound {
    #[inline]
    fn default() -> Self {
        Self { height: 1, entries: HashMap::new(), keys: Vec::new(), open: false }
    }
}

impl NbtCompound {
    #[inline]
    pub fn new() -> Self {
        Default::default()
    }

    #[inline]
    pub fn drop_index(&mut self, index: u32, mut str: String, element: NbtElement) -> bool {
        loop {
            if let Entry::Vacant(e) = self.entries.entry(str.clone()) {
                self.keys.insert(index as usize, e.key().to_owned());
                let height = element.height();
                e.insert(element);
                self.increment(height);
                return true;
            } else {
                str += " - Copy"
            }
        }
    }

    #[inline] // must only use for raw files
    pub unsafe fn put(&mut self, str: String, element: NbtElement) {
        if self.entries.get(&*str).is_some() {
            panic!("key cannot already exist, how did this happen")
        }
        self.keys.push(str.to_owned());
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
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
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
    #[allow(const_item_mutation)]
    pub fn render_root(&self, builder: &mut VertexBufferBuilder, str: &str, forbidden_y: Option<u32>) {
        let x_offset = &mut 20;
        let y_offset = &mut HEADER_SIZE;
        let remaining_scroll = &mut builder.scroll();
        if *remaining_scroll >= 16 {
            *remaining_scroll -= 16;
        } else {
            render_icon(*x_offset, *y_offset, builder);
            builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, 9));
            if !self.keys.is_empty() {
                builder.draw_texture((*x_offset - 16, *y_offset), (96 + if self.open { 0 } else { 16 }, 16), (16, 16));
            }
            if Some(*y_offset) != forbidden_y {
                builder.draw_text(*x_offset + 20, *y_offset, &format!("{} [{} entr{}]", str, self.keys.len(), if self.keys.len() == 1 { "y" } else { "ies" }), true);
            }
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
                        builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, if index != self.keys.len() - 1 { 16 } else { 9 }));
                    }

                    if entry.height() == 1 && *remaining_scroll >= 16 {
                        *remaining_scroll -= 16;
                    } else {
                        entry.render(x_offset, y_offset, remaining_scroll, builder, Some(name), index == self.keys.len() - 1, forbidden_y);
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
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, remaining_scroll: &mut u32, tail: bool, forbidden_y: Option<u32>) {
        let x_before = *x_offset - 16;
        if *remaining_scroll >= 16 {
            *remaining_scroll -= 16
        } else {
            render_icon(*x_offset, *y_offset, builder);
            if !self.keys.is_empty() {
                builder.draw_texture((*x_offset - 16, *y_offset), (96 + if self.open { 0 } else { 16 }, 16), (16, 16));
            }
            if Some(*y_offset) != forbidden_y {
                builder.draw_text(*x_offset + 20, *y_offset, &format!("{}{} entr{}", name.map(|x| format!("{}: ", x)).unwrap_or_else(|| "".to_owned()), self.keys.len(), if self.keys.len() == 1 { "y" } else { "ies" }), true);
            }
            *y_offset += 16
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
                        builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, if index != self.keys.len() - 1 { 16 } else { 9 }));
                    }

                    if entry.height() == 1 && *remaining_scroll >= 16 {
                        *remaining_scroll -= 16;
                    } else {
                        let y_before = *y_offset;
                        entry.render(x_offset, y_offset, remaining_scroll, builder, Some(name), tail && index == self.keys.len() - 1, forbidden_y);
                        let difference = *y_offset - y_before;
                        if !tail {
                            for i in 0..difference / 16 {
                                let y = y_before + i * 16;
                                builder.draw_texture((x_before, y), (80, 16), (8, 16));
                            }
                        }
                    }
                }
            }
        }
        *x_offset -= 16;
    }

    pub fn stack<F: FnMut(&mut NbtElement, u32), G: FnOnce(&mut NbtElement, u32, u32)>(wrapped: &mut NbtElement, y: &mut u32, depth: &mut u32, index: u32, parent: &mut F, mut tail: G) -> Option<G> {
        if *y == 0 {
            tail(wrapped, *depth, index);
            None
        } else {
            *y -= 1;
            let compound = if let NbtElement::Compound(compound) = wrapped { compound } else { panic!() };
            if compound.open {
                *depth += 1;
                for (index, key) in compound.keys.iter().enumerate() {
                    let value = compound.entries.get_mut(key).expect("has key, has value");
                    let x = value.stack(y, depth, index as u32, parent, tail);
                    if let Some(x) = x {
                        tail = x;
                    } else {
                        parent(wrapped, *y);
                        return None;
                    }
                }
                *depth -= 1;
            }
            Some(tail)
        }
    }

    #[inline]
    pub fn delete(&mut self, index: u32) -> Option<NbtElement> {
        let key = self.keys.remove(index as usize);
        self.entries.remove(&key)
    }

    #[inline]
    pub fn child_height(&self, index: u32) -> u32 {
        self.entries.get(self.key(index)).map(|x| x.height()).unwrap_or(0)
    }

    #[inline]
    pub fn drop(&mut self, element: NbtElement, y: &mut u32, parent_y: u32) -> Result<NbtElement, (u32, Option<(u32, u32)>)> {
        if *y < 16 {
            *y = 0;
            Ok(element)
        } else {
            *y -= 16;
            let mut child_y = parent_y + 1;
            for (index, key) in self.keys.iter().enumerate() {
                let value = self.entries.get_mut(key).expect("has key, has value");
                if *y < value.height() * 16 + if (value.open() || value.len().filter(|&x| x == 0).is_some()) && value.can_accept(element.id()) { 8 } else { 0 } {
                    let height = value.height();
                    return Err(match value.drop(element, y, child_y) {
                        Ok(element) => {
                            self.drop_index(index as u32, "_".to_owned(), element);
                            (height, Some((index as u32, parent_y)))
                        },
                        Err((increment, index)) => {
                            self.increment(increment);
                            (increment, index)
                        }
                    })
                } else {
                    *y -= value.height() * 16;
                    child_y += value.height();
                }
            }
            if *y < 8 {
                let height = element.height();
                self.drop_index(self.len() as u32, "_".to_owned(), element);
                Err((height, Some((self.len() as u32 - 1, parent_y))))
            } else {
                Err((0, None))
            }
        }
    }
}

#[inline]
pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
    builder.draw_texture((x, y), COMPOUND_UV, (16, 16));
}
