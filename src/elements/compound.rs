use std::fmt::{Display, Formatter, Write};
use std::intrinsics::likely;
use std::ops::Deref;

use fxhash::FxBuildHasher;
use indexmap::IndexMap;

use crate::{DeleteFn, DropFn, RenderContext, StealFn, ToggleFn, TrySelectTextFn, UnescapeStart, VertexBufferBuilder};
use crate::assets::{COMPOUND_UV, HEADER_SIZE};
use crate::decoder::Decoder;
use crate::elements::element_type::NbtElement;
use crate::encoder::write_string;

#[derive(Clone)]
pub struct NbtCompound {
    entries: Box<IndexMap<Box<str>, NbtElement, FxBuildHasher>>,
    open: bool,
    height: usize,
    true_height: usize,
}

impl NbtCompound {
    pub const ID: u8 = 10;

    pub(in crate::elements) fn from_str0(mut s: &str) -> Option<(&str, NbtCompound)> {
        s = s.strip_prefix("{")?.trim_start();
        let mut compound = NbtCompound::new();
        'a: while !s.is_empty() && !s.starts_with('}') {
            let (key, s2) = s.snbt_string_read()?;
            s = s2.trim_start().strip_prefix(':')?.trim_start();
            let (s2, value) = NbtElement::from_str0(s)?;
            compound.put(key.into_boxed_str(), value);
            s = s2.trim_start();
            match s.strip_prefix(',') {
                Some(s2) => match s2.trim_start().strip_prefix('}') {
                    Some(_) => break 'a,
                    None => {
                        s = s2;
                        continue
                    }
                }
                None => match s.strip_prefix('}') {
                    Some(_) => break 'a,
                    None => return None,
                }
            }
        }
        s = s.trim_start().strip_prefix('}')?;
        Some((s, compound))
    }

    pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
        let mut compound = NbtCompound::new();
        unsafe {
            decoder.assert_len(1)?;
            let mut current_element = decoder.u8();
            while current_element != 0 {
                decoder.assert_len(2)?;
                let key = decoder.string()?;
                let value = NbtElement::from_bytes(current_element, decoder)?;
                compound.put(key, value);
                match decoder.assert_len(1) {
                    Some(()) => {},
                    None => break, // wow mojang, saving one byte, so cool of you
                };
                current_element = decoder.u8();
            }
            Some(compound)
        }
    }

    pub fn to_bytes<W: std::io::Write>(&self, writer: &mut W) {
        for (key, value) in self.entries.deref() {
            let _ = writer.write(&[value.id()]);
            write_string(writer, key);
            NbtElement::to_bytes(value, writer);
        }
        let _ = writer.write(&[0x00]);
    }
}

impl Default for NbtCompound {
    #[inline]
    fn default() -> Self {
        Self { height: 1, entries: Box::new(IndexMap::with_hasher(Default::default())), open: false, true_height: 1 }
    }
}

impl NbtCompound {
    #[inline]
    pub fn new() -> Self {
        Default::default()
    }

    #[inline]
    pub fn insert_full(&mut self, idx: usize, mut str: String, value: NbtElement) -> Option<NbtElement> {
        loop {
            if !self.entries.contains_key(str.as_str()) {
                self.height += value.height();
                self.true_height += value.true_height();
                let (new_idx, _) = self.entries.insert_full(str.into_boxed_str(), value);
                self.entries.move_index(new_idx, idx);
                return None
            } else {
                str += " - Copy"
            }
        }
    }

    #[inline] // must only use for files, unless im stupid
    pub fn put(&mut self, str: Box<str>, element: NbtElement) {
        self.height += element.height();
        self.true_height += element.true_height();
        if let Some(element) = self.entries.insert(str, element) {
            self.height -= element.height();
            self.true_height -= element.true_height();
        }
    }

    #[inline]
    pub fn remove_idx(&mut self, idx: usize) -> Option<(Box<str>, NbtElement)> {
        self.entries.shift_remove_index(idx)
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
    pub fn toggle(&mut self) -> Option<()> {
        self.open = !self.open && !self.entries.is_empty();
        Some(())
    }

    #[inline]
    pub fn open(&self) -> bool {
        self.open
    }

    pub fn update_key(&mut self, idx: usize, key: Box<str>) -> Option<Box<str>> {
        if !self.entries.contains_key(key.as_ref()) && let Some((old_key, value)) = self.entries.swap_remove_index(idx)  {
            let (end_idx, _) = self.entries.insert_full(key, value);
            self.entries.swap_indices(end_idx, idx);
            Some(old_key)
        } else {
            None
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
    pub fn get(&self, idx: usize) -> Option<(&Box<str>, &NbtElement)> {
        self.entries.get_index(idx)
    }

    #[inline]
    pub fn get_mut(&mut self, idx: usize) -> Option<(&Box<str>, &mut NbtElement)> {
        self.entries.get_index_mut(idx)
    }

    pub fn render_root(&self, builder: &mut VertexBufferBuilder, str: &str, ctx: &RenderContext) {
        let mut x_offset = 16 + ctx.left_margin;
        let mut y_offset = HEADER_SIZE;
        let mut remaining_scroll = builder.scroll() / 16;
        let mut line_number = 1;
        'head: {
            if remaining_scroll > 0 {
                remaining_scroll -= 1;
                line_number += 1;
                break 'head
            }

            ctx.line_number(y_offset, &mut line_number, builder);
            builder.draw_texture((x_offset, y_offset), (64, 16), (16, 16));
            ctx.highlight((x_offset, y_offset), builder);
            builder.draw_texture((x_offset - 16, y_offset), (80, 16), (16, 9));
            if !self.is_empty() { builder.draw_texture((x_offset - 16, y_offset), (96 + self.open as usize * 16, 16), (16, 16)); }
            if ctx.forbid(y_offset) {
                builder.settings(x_offset + 20, y_offset, true);
                let _ = write!(builder, "{} [{n} {suffix}]", str, n = self.len(), suffix = { if self.len() == 1 { "entry" } else { "entries" } });
            }

            if ctx.ghost(x_offset + 16, y_offset + 16, builder, |x, y| x == x_offset + 16 && y == y_offset + 8) {
                builder.draw_texture((x_offset, y_offset + 16), (80, 16), (16, (self.height() != 1) as usize * 7 + 9));
                y_offset += 16;
            }

            if self.height() == 1 && ctx.ghost(x_offset + 16, y_offset + 16, builder, |x, y| x == x_offset + 16 && y == y_offset + 16) {
                builder.draw_texture((x_offset, y_offset + 16), (80, 16), (16, 9));
                y_offset += 16;
            }

            y_offset += 16;
        }

        x_offset += 16;

        if self.open {
            for (idx, (name, value)) in self.entries.iter().enumerate() {
                if y_offset > builder.window_height() { break }

                let height = value.height();
                if remaining_scroll >= height {
                    remaining_scroll -= height;
                    line_number += value.true_height();
                    continue;
                }

                if ctx.ghost(x_offset, y_offset, builder, |x, y| x_offset == x && y_offset == y) {
                    builder.draw_texture((x_offset - 16, y_offset), (80, 16), (16, 16));
                    y_offset += 16;
                }

                let ghost_tail_mod = if let Some((_, x, y)) = ctx.ghost && x == x_offset && y == y_offset + height * 16 - remaining_scroll * 16 - 8 {
                    false
                } else {
                    true
                };

                if remaining_scroll == 0 { builder.draw_texture((x_offset - 16, y_offset), (80, 16), (16, (idx != self.len() - 1 || !ghost_tail_mod) as usize * 7 + 9)); }
                value.render(&mut x_offset, &mut y_offset, &mut remaining_scroll, builder, Some(name.as_ref()), idx == self.len() - 1 && ghost_tail_mod, &mut line_number, ctx);

                if ctx.ghost(x_offset, y_offset, builder, |x, y| x_offset == x && y_offset - 8 == y) {
                    builder.draw_texture((x_offset - 16, y_offset), (80, 16), (16, (idx != self.len() - 1) as usize * 7 + 9));
                    y_offset += 16;
                }
            }
        } else {
            // unrequired tbh
            // line_number += self.true_height - 1;
        }
        // unrequired tbh
        // x_offset -= 16;
    }
}

impl Display for NbtCompound {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (idx, (key, value)) in self.entries.iter().enumerate() {
            write!(f, "{key:?}:{value}")?;
            if likely(idx < self.len() - 1) {
                write!(f, ",")?;
            }
        }
        write!(f, "}}")
    }
}

impl NbtCompound {
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

            for (idx, (key, entry)) in self.entries.iter().enumerate() {
                if *y_offset > builder.window_height() { break }

                let height = entry.height();
                if *remaining_scroll >= height {
                    *remaining_scroll -= height;
                    *line_number += entry.true_height();
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

                if *remaining_scroll == 0 { builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, (idx != self.len() - 1 || !ghost_tail_mod) as usize * 7 + 9)); }
                entry.render(x_offset, y_offset, remaining_scroll, builder, Some(key.as_ref()), idx == self.len() - 1 && ghost_tail_mod, line_number, ctx);

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
        self.entries.get_index(idx).map(|(_, x)| x.height()).unwrap_or(0)
    }

    #[inline]
    pub fn drop(&mut self, mut key: Option<Box<str>>, mut element: NbtElement, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> DropFn {
        if *y < 16 && *y >= 8 && depth == target_depth {
            let open_before = self.open;
            let before = (self.height(), self.true_height());
            self.open = true;
            self.insert_full(0, key.map(str::into_string).unwrap_or_else(|| "_".to_owned()), element);
            indices.push(0);
            return DropFn::Dropped(self.height - before.0, self.true_height - before.1, !open_before)
        }

        if self.height() == 1 && *y < 24 && *y >= 16 && depth == target_depth {
            let open_before = self.open;
            let before = self.true_height();
            self.open = true;
            indices.push(self.len());
            self.insert_full(self.len(), key.map(str::into_string).unwrap_or_else(|| "_".to_owned()), element);
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
            for (idx, (_, value)) in self.entries.iter_mut().enumerate() {
                *ptr = idx;
                let heights = (element.height(), element.true_height());
                if *y < 8 && depth == target_depth {
                    *y = 0;
                    self.insert_full(idx, key.map(str::into_string).unwrap_or_else(|| "_".to_owned()), element);
                    return DropFn::Dropped(heights.0, heights.1, false)
                } else if *y >= value.height() * 16 - 8 && *y < value.height() * 16 && depth == target_depth {
                    *y = 0;
                    *ptr = idx + 1;
                    self.insert_full(idx + 1, key.map(str::into_string).unwrap_or_else(|| "_".to_owned()), element);
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
        for (idx, (key, element)) in self.entries.iter_mut().enumerate() {
            *ptr = idx;
            let heights = (element.height(), element.true_height());
            if *y == 0 {
                let element = element.clone();
                let key = key.clone().into_string();
                self.insert_full(idx + 1, key, element);
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
            let before = self.height();
            self.insert_full(idx, key.unwrap_unchecked().into_string(), element);
            return self.height - before
        }

        *y -= 1;
        indices.push(0);
        let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
        for (jdx, (_, value)) in self.entries.iter_mut().enumerate() {
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

    pub fn copy(&self, y: &mut usize, parent: Option<String>) {
        if *y == 0 {
            let _ = cli_clipboard::set_contents(format!("{}{self}", parent.map(|x| x + ":").unwrap_or(String::new())));
        } else if *y < self.height() {
            *y -= 1;
            for (key, element) in self.entries.iter() {
                let height = element.height();
                if *y < height {
                    element.copy(y, Some(format!("{key:?}")));
                    break
                } else {
                    *y -= height;
                }
            }
            unsafe { core::hint::unreachable_unchecked() }
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
        } else {
            *y -= 1;
            let depth = depth + 1;
            indices.push(0);
            let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
            for (idx, (_, value)) in self.entries.iter_mut().enumerate() {
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
            unsafe { core::hint::unreachable_unchecked() }
        }
    }

    pub unsafe fn try_select_text(&self, y: &mut usize, indices: &mut Vec<usize>) -> TrySelectTextFn {
        *y -= 1;
        indices.push(0);
        let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
        for (idx, (key, value)) in self.entries.iter().enumerate() {
            *ptr = idx;

            let height = value.height();
            if *y >= height {
                *y -= height;
                continue
            } else if *y == 0 {
                return (Some((key.clone(), true)), Some(value.value()))
            } else {
                return value.try_select_text(y, indices)
            }
        }
        core::hint::unreachable_unchecked()
    }

    pub unsafe fn delete(&mut self, y: &mut usize, indices: &mut Vec<usize>) -> DeleteFn {
        *y -= 1;
        indices.push(0);
        let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
        for (idx, (_, value)) in self.entries.iter_mut().enumerate() {
            *ptr = idx;
            let heights = (value.height(), value.true_height());
            if *y >= heights.0 {
                *y -= heights.0;
                continue
            } else if *y == 0 {
                // SAFETY: lets stop playin' around, we know it exists
                let (key, value) = unsafe { self.entries.shift_remove_index(idx).unwrap_unchecked() };
                self.decrement(heights.0, heights.1);
                return (Some(key), value, (heights.0 * self.open as usize, heights.1 * self.open as usize))
            } else {
                let mut res = value.delete(y, indices);
                self.decrement(res.2.0, res.2.1);
                res.2 = (res.2.0 * self.open as usize, res.2.1 * self.open as usize);
                return res
            }
        }
        unsafe { core::hint::unreachable_unchecked() }
    }

    pub fn steal(&mut self, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> StealFn {
        *y -= 1;
        if self.open {
            indices.push(0);
            let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
            for (idx, (_, value)) in self.entries.iter_mut().enumerate() {
                *ptr = idx;

                let heights = (value.height(), value.true_height());
                if *y >= heights.0 {
                    *y -= heights.0;
                    continue
                } else if *y == 0 {
                    return if depth + 1 == target_depth {
                        // SAFETY: lets stop playin' around, we know it exists
                        let (key, value) = unsafe { self.entries.shift_remove_index(idx).unwrap_unchecked() };
                        self.decrement(heights.0, heights.1);
                        Ok((Some(key), value, heights))
                    } else {
                        Err(())
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
}

#[inline]
pub fn render_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
    builder.draw_texture((x, y), COMPOUND_UV, (16, 16));
}
