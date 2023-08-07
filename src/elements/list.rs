use std::fmt::{Display, Formatter, Write};
use std::intrinsics::likely;

use crate::{DeleteFn, DropFn, RenderContext, StealFn, ToggleFn, TrySelectTextFn, VertexBufferBuilder};
use crate::assets::LIST_UV;
use crate::decoder::Decoder;
use crate::elements::element_type::NbtElement;

#[derive(Clone)]
pub struct NbtList {
    elements: Vec<NbtElement>,
    element: u8,
    open: bool,
    height: usize,
    true_height: usize,
}

impl NbtList {
    pub const ID: u8 = 9;

    pub(in crate::elements) fn from_str0(mut s: &str) -> Option<(&str, Self)> {
        s = s.strip_prefix('[')?.trim_start();
        let mut elements = vec![];
        let mut height = 1;
        let mut true_height = 1;
        let mut id = 0;
        'a: {
            if let Some(s2) = s.strip_prefix(']') {
                s = s2;
                break 'a
            }
            'b: loop {
                let (s2, element) = NbtElement::from_str0(s)?;
                if id == 0 {
                    id = element.id();
                } else if id != element.id() {
                    return None // this vec ain't big enough for the two of us
                }
                height += element.height();
                true_height += element.true_height();
                elements.push(element);
                s = s2.trim_start();
                match s.strip_prefix(',') {
                    Some(s2) => match s2.strip_prefix(']') {
                        Some(s2) => {
                            s = s2;
                            break 'b
                        },
                        None => s = s2.trim_start(),
                    },
                    None => match s.strip_prefix(']') {
                        Some(s2) => {
                            s = s2;
                            break 'b
                        }
                        None => {
                            return None // no comma, and no ], wtf
                        }
                    }
                }
            }
        }
        Some((s, Self {
            elements,
            element: id,
            open: false,
            height,
            true_height,
        }))
    }

    pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
        unsafe {
            decoder.assert_len(5)?;
            let element = decoder.u8();
            let len = decoder.u32() as usize;
            let mut elements = Vec::with_capacity(len);
            for _ in 0..len {
                elements.push(NbtElement::from_bytes(element, decoder)?);
            }
            Some(NbtList::new(elements, element))
        }
    }

    pub fn to_bytes<W: std::io::Write>(&self, writer: &mut W) {
        let _ = writer.write(&[self.element]);
        let _ = writer.write(&(self.len() as u32).to_be_bytes());
        for element in &self.elements {
            NbtElement::to_bytes(element, writer)
        }
    }
}

impl NbtList {
    #[inline]
    pub fn new(elements: Vec<NbtElement>, element: u8) -> Self {
        NbtList { height: elements.iter().map(NbtElement::height).sum::<usize>() + 1, true_height: elements.iter().map(NbtElement::true_height).sum::<usize>() + 1, elements, element, open: false }
    }

    #[inline]
    pub fn increment(&mut self, amount: usize, true_amount: usize) {
        self.height += amount;
        self.true_height += true_amount;
    }

    #[inline]
    pub fn decrement(&mut self, amount: usize, true_amount: usize) {
        self.height -= amount;
        self.true_height -= true_amount;
    }

    #[inline]
    pub fn height(&self) -> usize {
        if self.open {
            self.height
        } else {
            1
        }
    }

    #[inline]
    pub fn true_height(&self) -> usize {
        self.true_height
    }

    #[inline]
    pub fn id(&self) -> u8 {
        self.element
    }

    #[inline]
    pub fn toggle(&mut self) -> Option<()> {
        self.open = !self.open && !self.elements.is_empty();
        Some(())
    }

    #[inline]
    pub fn open(&self) -> bool {
        self.open
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.elements.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }

    #[inline]
    pub fn insert(&mut self, idx: usize, value: NbtElement) -> Option<NbtElement> {
        if self.element == value.id() || self.elements.is_empty() {
            self.element = value.id();
            self.increment(value.height(), value.true_height());
            self.elements.insert(idx, value);
            None
        } else {
            Some(value)
        }
    }

    #[inline]
    pub fn remove(&mut self, idx: usize) -> NbtElement {
        let e = self.elements.remove(idx);
        unsafe { std::intrinsics::assume(e.id() == self.element) }
        e
    }

    #[inline]
    pub fn get(&self, idx: usize) -> Option<&NbtElement> {
        let e = self.elements.get(idx);
        unsafe { std::intrinsics::assume(e.is_none() || e.as_ref().is_some_and(|x| x.id() == self.element)) }
        e
    }

    #[inline]
    pub fn get_mut(&mut self, idx: usize) -> Option<&mut NbtElement> {
        let e = self.elements.get_mut(idx);
        unsafe { std::intrinsics::assume(e.is_none() || e.as_ref().is_some_and(|x| x.id() == self.element)) }
        e
    }
}

impl Display for NbtList {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (idx, element) in self.elements.iter().enumerate() {
            write!(f, "{element}")?;
            if likely(idx < self.len() - 1) {
                write!(f, ",")?;
            }
        }
        write!(f, "]")
    }
}

impl NbtList {
    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut usize, y_offset: &mut usize, name: Option<&str>, remaining_scroll: &mut usize, tail: bool, line_number: &mut usize, ctx: &RenderContext) {
        let mut y_before = *y_offset;

        'head: {
            if *remaining_scroll > 0 {
                *remaining_scroll -= 1;
                *line_number += 1;
                break 'head
            }

            ctx.line_number(*y_offset, line_number, builder);
            render_icon(*x_offset, *y_offset, builder);
            ctx.highlight((*x_offset, *y_offset), builder);
            if !self.is_empty() { builder.draw_texture((*x_offset - 16, *y_offset), (96 + self.open as usize * 16, 16), (16, 16)); }
            if ctx.forbid(*y_offset) {
                builder.settings(*x_offset + 20, *y_offset, true);
                let _ = match name {
                    Some(x) => write!(builder, "{x}: {n} {suffix}", n = self.len(), suffix = { if self.len() == 1 { "entry" } else { "entries" } }),
                    None => write!(builder, "{n} {suffix}", n = self.len(), suffix = { if self.len() == 1 { "entry" } else { "entries" } }),
                };
            }

            if ctx.ghost(*x_offset + 16, *y_offset + 16, builder, |x, y| x == *x_offset + 16 && y == *y_offset + 8) {
                builder.draw_texture((*x_offset, *y_offset + 16), (80, 16), (16, (self.height() != 1) as usize * 7 + 9));
                if !tail { builder.draw_texture((*x_offset - 16, *y_offset + 16), (80, 16), (8, 16)); }
                *y_offset += 16;
            } else if self.height() == 1 && ctx.ghost(*x_offset + 16, *y_offset + 16, builder, |x, y| x == *x_offset + 16 && y == *y_offset + 16) {
                builder.draw_texture((*x_offset, *y_offset + 16), (80, 16), (16, 9));
                if !tail { builder.draw_texture((*x_offset - 16, *y_offset + 16), (80, 16), (8, 16)); }
                *y_offset += 16;
            }

            *y_offset += 16;
            y_before += 16;
        }

        let x_before = *x_offset - 16;

        if self.open {
            *x_offset += 16;

            for (idx, element) in self.elements.iter().enumerate() {
                if *y_offset > builder.window_height() { break }

                let height = element.height();
                if *remaining_scroll >= height {
                    *remaining_scroll -= height;
                    *line_number += element.true_height();
                    continue;
                }

                if ctx.ghost(*x_offset, *y_offset, builder, |x, y| *x_offset == x && *y_offset == y) {
                    builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, 16));
                    *y_offset += 16;
                }

                let ghost_tail_mod = if let Some((_, x, y)) = ctx.ghost && x == *x_offset && y == *y_offset + height * 16 - *remaining_scroll * 16 - 8 {
                    false
                } else {
                    true
                };

                if *remaining_scroll == 0 { builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, (!(idx == self.len() - 1 && ghost_tail_mod)) as usize * 7 + 9)); }
                element.render(x_offset, y_offset, remaining_scroll, builder, None, tail && idx == self.len() - 1 && ghost_tail_mod, line_number, ctx);

                if ctx.ghost(*x_offset, *y_offset, builder, |x, y| *x_offset == x && *y_offset - 8 == y) {
                    builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, (idx != self.len() - 1) as usize * 7 + 9));
                    *y_offset += 16;
                }
            }

            let difference = *y_offset - y_before;
            if !tail {
                for i in 0..difference / 16 {
                    builder.draw_texture((x_before, y_before + i * 16), (80, 16), (8, 16));
                }
            }

            *x_offset -= 16;
        } else {
            *line_number += self.true_height - 1;
        }
    }

    #[inline]
    pub fn child_height(&self, idx: usize) -> usize {
        self.elements.get(idx).map(NbtElement::height).unwrap_or(0)
    }

    pub fn drop(&mut self, mut key: Option<Box<str>>, mut element: NbtElement, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> DropFn {
        if *y < 16 && *y >= 8 && depth == target_depth {
            let open_before = self.open;
            let before = (self.height(), self.true_height());
            indices.push(0);
            if let Some(element) = self.insert(0, element) {
                return DropFn::InvalidType(key, element)
            }
            self.open = true;
            return DropFn::Dropped(self.height - before.0, self.true_height - before.1, !open_before)
        } else if self.height() == 1 && *y < 24 && *y >= 16 && depth == target_depth {
            let open_before = self.open;
            let before = self.true_height();
            indices.push(self.len());
            if let Some(element) = self.insert(self.len(), element) {
                // indices are never used
                return DropFn::InvalidType(key, element)
            }
            self.open = true;
            return DropFn::Dropped(self.height - 1, self.true_height - before, !open_before)
        }

        if *y < 16 {
            return DropFn::Missed(key, element)
        } else {
            *y -= 16;
        }

        if self.open && !self.is_empty() {
            indices.push(0);
            let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
            for (idx, value) in self.elements.iter_mut().enumerate() {
                *ptr = idx;
                let heights = (element.height(), element.true_height());
                if *y < 8 && depth == target_depth {
                    if let Some(element) = self.insert(idx, element) {
                        return DropFn::InvalidType(key, element)
                    }
                    return DropFn::Dropped(heights.0, heights.1, false)
                } else if *y >= value.height() * 16 - 8 && *y < value.height() * 16 && depth == target_depth {
                    *ptr = idx + 1;
                    if let Some(element) = self.insert(idx + 1, element) {
                        return DropFn::InvalidType(key, element)
                    }
                    return DropFn::Dropped(heights.0, heights.1, false)
                }

                match value.drop(key, element, y, depth + 1, target_depth, indices) {
                    x @ DropFn::InvalidType(_, _) => return x,
                    DropFn::Missed(k, e) => {
                        key = k;
                        element = e;
                    },
                    DropFn::Dropped(increment, true_increment, opened) => {
                        self.increment(increment, true_increment);
                        return DropFn::Dropped(increment, true_increment, opened)
                    }
                }
            }
            indices.pop();
        }
        DropFn::Missed(key, element)
    }

    pub unsafe fn duplicate(&mut self, y: &mut usize, indices: &mut Vec<usize>) {
        *y -= 1;
        indices.push(0);
        let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
        for (idx, element) in self.elements.iter_mut().enumerate() {
            *ptr = idx;
            let heights = (element.height(), element.true_height());
            if *y == 0 {
                let element = element.clone();
                self.elements.insert(idx + 1, element);
                self.increment(heights.0, heights.1);
                return
            } else if *y < heights.0 {
                element.duplicate(y, indices);
                let heights2 = (element.height(), element.true_height());
                self.increment(heights2.0 - heights.0, heights2.1 - heights.1);
                return
            } else {
                *y -= heights.0;
            }
        }
        core::hint::unreachable_unchecked()
    }

    pub unsafe fn drop_simple(&mut self, y: &mut usize, key: Option<Box<str>>, element: NbtElement, idx: usize, indices: &mut Vec<usize>) -> usize {
        let heights = (element.height(), element.true_height());
        if *y == 0 {
            indices.push(idx);
            self.increment(heights.0, heights.1);
            self.insert(idx, element);
            return self.open as usize * heights.0;
        }
        *y -= 1;
        indices.push(0);
        let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
        for (jdx, value) in self.elements.iter_mut().enumerate() {
            *ptr = jdx;
            let vh = value.height();
            if *y < vh {
                let res = value.drop_simple(y, key, element, idx, indices);
                self.increment(res, heights.1);
                return self.open as usize * res
            } else {
                *y -= vh;
            }
        }
        core::hint::unreachable_unchecked()
    }

    // no need for open checks
    pub fn copy(&self, y: &mut usize, parent: Option<String>) {
        if *y == 0 {
            let _ = cli_clipboard::set_contents(format!("{}{self}", parent.map(|x| x + ":").unwrap_or(String::new())));
        } else if *y < self.height() {
            *y -= 1;
            for element in &self.elements {
                let height = element.height();
                if *y < height {
                    element.copy(y, None);
                    break
                } else {
                    *y -= height;
                }
            }
        }
    }

    pub fn toggle_fn(&mut self, y: &mut usize, target_depth: usize, depth: usize, indices: &mut Vec<usize>) -> ToggleFn {
        if *y == 0 {
            if depth == target_depth {
                self.toggle();
                Ok((self.height - 1).wrapping_mul((self.open as usize * 2).wrapping_sub(1)))
            } else {
                Err(())
            }
        } else if self.open {
            *y -= 1;
            let depth = depth + 1;
            indices.push(0);
            let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
            for (idx, value) in self.elements.iter_mut().enumerate() {
                *ptr = idx;

                let height = value.height();
                if *y >= height {
                    *y -= height;
                    continue
                } else if depth <= target_depth {
                    return match value.toggle_fn(y, target_depth, depth, indices) {
                        Ok(change) => {
                            self.height = self.height.wrapping_add(change);
                            Ok(change)
                        }
                        Err(()) => Err(()),
                    }
                } else {
                    return Err(())
                }
            }
            Err(())
        } else {
            Err(())
        }
    }

    pub unsafe fn delete(&mut self, y: &mut usize, indices: &mut Vec<usize>) -> DeleteFn {
        *y -= 1;
        indices.push(0);
        let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
        for (idx, value) in self.elements.iter_mut().enumerate() {
            *ptr = idx;
            let heights = (value.height(), value.true_height());
            if *y >= heights.0 {
                *y -= heights.0;
                continue
            } else if *y == 0 {
                self.decrement(heights.0, heights.1);
                return (None, self.elements.remove(idx), (heights.0 * self.open as usize, heights.1 * self.open as usize))
            } else {
                let mut res = value.delete(y, indices);
                self.decrement(res.2.0, res.2.1);
                res.2 = (res.2.0 * self.open as usize, res.2.1 * self.open as usize);
                return res
            }
        }
        core::hint::unreachable_unchecked()
    }

    pub fn steal(&mut self, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> StealFn {
        *y -= 1;
        if self.open {
            indices.push(0);
            let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
            for (idx, value) in self.elements.iter_mut().enumerate() {
                *ptr = idx;

                let heights = (value.height(), value.true_height());
                if *y >= heights.0 {
                    *y -= heights.0;
                    continue
                } else if *y == 0 {
                    if depth + 1 == target_depth {
                        self.decrement(heights.0, heights.1);
                        return Ok((None, self.elements.remove(idx), heights))
                    } else {
                        return Err(())
                    }
                } else if let Ok((key, element, (decrement, true_decrement))) = value.steal(y, depth + 1, target_depth, indices) {
                    self.decrement(decrement, true_decrement);
                    return Ok((key, element, (decrement, true_decrement)))
                } else {
                    break
                }
            }
            indices.pop();
        }
        Err(())
    }

    pub unsafe fn try_select_text(&self, y: &mut usize, indices: &mut Vec<usize>) -> TrySelectTextFn {
        *y -= 1;
        indices.push(0);
        let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
        for (idx, value) in self.elements.iter().enumerate() {
            *ptr = idx;

            let height = value.height();
            if *y >= height {
                *y -= height;
                continue
            } else if *y == 0 {
                return (None, Some(value.value()))
            } else {
                return value.try_select_text(y, indices)
            }
        }
        core::hint::unreachable_unchecked()
    }
}

#[inline]
pub fn render_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
    builder.draw_texture((x, y), LIST_UV, (16, 16));
}
