use std::slice::IterMut;
use crate::decoder::Decoder;
use crate::{DeleteFn, DropFn, LeftClickFn, NbtElement, VertexBufferBuilder};
use crate::encoder::write_u32;

pub struct NbtArray<const I: u8, const C: char, const U: u32, const V: u32, const L: usize> {
    values: Vec<NbtElement>,
    open: bool,
    height: u32
}

impl<const I: u8, const C: char, const U: u32, const V: u32, const L: usize> NbtArray<I, C, U, V, L> {
    #[inline]
    pub fn new() -> NbtArray<I, C, U, V, L> {
        NbtArray {
            values: Vec::new(),
            open: false,
            height: 1
        }
    }

    #[inline]
    pub fn from_bytes<F: Fn(&mut Decoder) -> NbtElement>(decoder: &mut Decoder, f: F) -> Self {
        unsafe {
            decoder.assert_len(4);
            let len = decoder.u32() as usize;
            decoder.assert_len(len * L);
            let mut vec = Vec::with_capacity(len);
            for i in 0..len {
                *(vec.as_mut_ptr() as *mut NbtElement).add(i) = f(decoder)
            }
            NbtArray {
                values: vec,
                open: false,
                height: len as u32 + 1
            }
        }
    }

    #[inline]
    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        drop(write_u32(writer, self.values.len() as u32));
        for element in &self.values {
            element.to_bytes(writer)
        }
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
        self.open = !self.open && !self.values.is_empty();
        true
    }

    #[inline]
    pub fn open(&self) -> bool {
        self.open
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.values.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut u32, y_offset: &mut u32, name: Option<&str>, remaining_scroll: &mut u32, tail: bool, forbidden_y: Option<u32>) {
        if *remaining_scroll >= 16 {
            *remaining_scroll -= 16;
        } else {
            Self::render_icon(*x_offset, *y_offset, builder);
            if !self.values.is_empty() {
                builder.draw_texture((*x_offset - 16, *y_offset), (96 + if self.open { 0 } else { 16 }, 16), (16, 16));
            }
            if Some(*y_offset) != forbidden_y {
                builder.draw_text(*x_offset + 20, *y_offset, &format!("{}{} entr{}", name.map(|x| format!("{}: ", x)).unwrap_or_else(|| "".to_owned()), self.values.len(), if self.values.len() == 1 { "y" } else { "ies" }), true);
            }
            *y_offset += 16;
        }
        *x_offset += 16;
        if self.open {
            for (index, element) in self.values.iter().enumerate() {
                if *y_offset > builder.window_height() {
                    break
                } else {
                    if *remaining_scroll >= 16 {
                        *remaining_scroll -= 16;
                        continue;
                    }

                    if *remaining_scroll < 16 {
                        builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, if index != self.values.len() - 1 { 16 } else { 9 }));
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

    pub fn iter_mut(&mut self) -> IterMut<'_, NbtElement> {
        self.values.iter_mut()
    }

    pub fn stack<F: FnMut(&mut NbtElement, u32), G: FnOnce(&mut NbtElement, u32, u32)>(wrapped: &mut NbtElement, y: &mut u32, depth: &mut u32, index: u32, parent: &mut F, tail: G) -> Option<G> {
        if *y == 0 {
            tail(wrapped, *depth, index);
            None
        } else {
            *y -= 1;
            if wrapped.open() {
                for (index, value) in unsafe { wrapped.array_iter_mut().unwrap_unchecked() }.enumerate() {
                    if *y == 0 {
                        tail(value, *depth + 1, index as u32);
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
    pub fn delete_index(&mut self, index: u32) -> Option<NbtElement> {
        Some(self.values.remove(index as usize))
    }

    #[inline]
    pub fn child_height(&self, _: u32) -> u32 {
        1
    }

    #[inline]
    pub fn drop(&mut self, element: NbtElement, y: &mut u32, parent_y: u32) -> DropFn {
        if *y < 16 {
            *y = 0;
            Ok(element)
        } else {
            let mut child_y = parent_y + 1;
            *y -= 16;
            for (index, value) in self.values.iter_mut().enumerate() {
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
    pub fn delete(&mut self, y: &mut u32, depth: u32) -> DeleteFn {
        if *y == 0 {
            Some(None)
        } else {
            let y_before = *y;
            *y -= 1;
            if self.open {
                for (index, element) in self.values.iter_mut().enumerate() {
                    match element.delete(y, depth + 1) {
                        None => {} // found nothing, not here, go to next element
                        Some(None) => { // found something, i am the parent
                            let height = element.height();
                            self.decrement(height);
                            return Some(self.delete_index(index as u32).map(|x| (x, y_before, None, index as u32, depth + 1)))
                        }
                        x => { // found something deeper, throw to caller fn
                            unsafe { self.decrement(x.as_ref().unwrap_unchecked().as_ref().unwrap_unchecked().0.height()); }
                            return x;
                        }
                    }
                }
            }
            None
        }
    }

    #[inline]
    pub fn drop_index(&mut self, index: u32, other: NbtElement) -> bool {
        if other.id() == I {
            self.values.insert(index as usize, other);
            self.increment(1);
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn render_icon(x: u32, y: u32, builder: &mut VertexBufferBuilder) {
        builder.draw_texture((x, y), (U, V), (16, 16));
    }

    #[inline]
    pub fn left_click(&mut self, y: &mut u32, depth: u32, mouse_x: u32, index: u32, other_value: Option<String>) -> LeftClickFn {
        if *y == 0 {
            if mouse_x / 16 == depth { // toggle button
                let before = self.height();
                self.toggle();
                let change = self.height() as i32 - before as i32;
                Some(Ok(change))
            } else if mouse_x / 16 >= depth + 1 { // ooh probably a text selection
                Some(Err((depth * 16 + 40, *y, index, None, mouse_x, other_value)))
            } else { // too small to do anything
                None
            }
        } else {
            *y -= 1;
            if self.open {
                for (index, element) in self.values.iter_mut().enumerate() {
                    match element.left_click(y, depth + 1, mouse_x, index as u32, None) {
                        None => {}, // next element
                        Some(Ok(change)) => {
                            if change < 0 {
                                self.decrement(-change as u32);
                            } else {
                                self.increment(change as u32);
                            }
                            return Some(Ok(change))
                        },
                        x => return x
                    }
                }
            }
            None
        }
    }
}

impl<const I: u8, const C: char, const U: u32, const V: u32, const L: usize> ToString for NbtArray<I, C, U, V, L> {
    fn to_string(&self) -> String {
        let mut builder = String::with_capacity(self.values.len() * 4); // just a guess
        builder.push('[');
        builder.push(C);
        builder.push(';');
        for i in 0..self.values.len() {
            builder.push_str(&unsafe { self.values.get_unchecked(i) }.to_string());
            if i < self.values.len() - 1 {
                builder.push_str(", ");
            }
        }
        builder.push(']');
        builder
    }
}
