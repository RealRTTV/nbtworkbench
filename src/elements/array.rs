#[macro_export]
macro_rules! array {
    ($element:ident, $element_inner:ident, $name:ident, $t:ty, $my_id:literal, $id:literal, $char:literal, ($u:literal $v:literal), ($u2:literal $v2:literal)) => {
        #[derive(Clone)]
        pub struct $name {
            values: Vec<$t>,
            open: bool,
            height: usize,
        }

        impl $name {
            #[inline]
            pub fn new() -> Self {
                Self {
                    values: vec![],
                    open: false,
                    height: 1,
                }
            }

            pub const ID: u8 = $my_id;

            #[inline]
            pub(in crate::elements) fn from_str0(mut s: &str) -> Option<(&str, Self)> {
                s = s.strip_prefix(concat!("[", $char, ";"))?.trim_start();
                let mut vec = Vec::<$t>::new();
                'a: {
                    if let Some(s2) = s.strip_prefix(']') {
                        s = s2;
                        break 'a
                    }
                    'b: loop {
                        let num_len = {
                            let mut num_len = 0;
                            let mut s = s;
                            if s.starts_with('+') | s.starts_with('-') {
                                s = &s[1..];
                                num_len += 1;
                            }
                            let digits = s.bytes().take_while(u8::is_ascii_digit).count();
                            if digits == 0 {
                                return None
                            }
                            num_len + digits
                        };
                        vec.push((&s[..num_len]).parse().ok()?);
                        s = (&s[num_len..]).trim_start();
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
                vec.shrink_to_fit();
                Some((s, $name {
                    height: vec.len() + 1,
                    values: vec,
                    open: false,
                }))
            }

            #[inline]
            pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
                unsafe {
                    decoder.assert_len(4)?;
                    let len = decoder.u32() as usize;
                    decoder.assert_len(len * core::mem::size_of::<$t>())?;
                    let mut vec = Vec::<$t>::with_capacity(len);
                    for (idx, maybe) in vec.spare_capacity_mut().iter_mut().enumerate() {
                        maybe.write(<$t>::from_be_bytes(decoder.data.add(idx * core::mem::size_of::<$t>()).cast::<[u8; { core::mem::size_of::<$t>() }]>().read()));
                    }
                    decoder.data = decoder.data.add(len * core::mem::size_of::<$t>());
                    vec.set_len(len);
                    Some($name {
                        values: vec,
                        open: false,
                        height: len + 1,
                    })
                }
            }

            #[inline]
            pub fn to_bytes<W: std::io::Write>(&self, writer: &mut W) {
                let _ = writer.write(&(self.len() as u32).to_be_bytes());
                for entry in &self.values {
                    let _ = writer.write(&entry.to_be_bytes());
                }
            }
        }

        impl $name {
            #[inline]
            pub fn increment(&mut self, amount: usize, _: usize) {
                self.height += amount;
            }

            #[inline]
            pub fn decrement(&mut self, amount: usize, _: usize) {
                self.height -= amount;
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
                self.height
            }

            #[inline]
            pub fn toggle(&mut self) -> Option<()> {
                self.open = !self.open && !self.values.is_empty();
                Some(())
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
            pub fn insert(&mut self, idx: usize, value: NbtElement) -> Option<NbtElement> {
                if let NbtElement::$element($element_inner { value }) = value {
                    self.values.insert(idx, value);
                    self.increment(1, 1);
                    None
                } else {
                    Some(value)
                }
            }

            #[inline]
            pub fn remove(&mut self, idx: usize) -> NbtElement {
                NbtElement::$element($element_inner { value: self.values.remove(idx) })
            }

            #[inline]
            pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut usize, y_offset: &mut usize, key: Option<&str>, remaining_scroll: &mut usize, tail: bool, line_number: &mut usize, ctx: &RenderContext) {
                'head: {
                    if *remaining_scroll > 0 {
                        *remaining_scroll -= 1;
                        break 'head
                    }

                    ctx.line_number(*y_offset, line_number, builder);
                    Self::render_icon(*x_offset, *y_offset, builder);
                    ctx.highlight((*x_offset, *y_offset), builder);
                    if !self.is_empty() { builder.draw_texture((*x_offset - 16, *y_offset), (96 + self.open as usize * 16, 16), (16, 16)); }
                    if ctx.forbid(*y_offset) {
                        builder.settings(*x_offset + 20, *y_offset, true);
                        let _ = match key {
                            Some(x) => write!(builder, "{x}: {n} {suffix}", n = self.len(), suffix = { if self.len() == 1 { "entry" } else { "entries" } }),
                            None => write!(builder, "{n} {suffix}", n = self.len(), suffix = { if self.len() == 1 { "entry" } else { "entries" } }),
                        };
                    }

                    if ctx.ghost.is_some_and(|(id, _, _)| id == $id)  {
                        if ctx.ghost(*x_offset + 16, *y_offset + 16, builder, |x, y| x == *x_offset + 16 && y == *y_offset + 8) {
                            builder.draw_texture((*x_offset, *y_offset + 16), (80, 16), (16, (self.height() != 1) as usize * 7 + 9));
                            if !tail { builder.draw_texture((*x_offset - 16, *y_offset + 16), (80, 16), (8, 16)); }
                            *y_offset += 16;
                        } else if self.height() == 1 && ctx.ghost(*x_offset + 16, *y_offset + 16, builder, |x, y| x == *x_offset + 16 && y == *y_offset + 16) {
                            builder.draw_texture((*x_offset, *y_offset + 16), (80, 16), (16, 9));
                            if !tail { builder.draw_texture((*x_offset - 16, *y_offset + 16), (80, 16), (8, 16)); }
                            *y_offset += 16;
                        }
                    }

                    *y_offset += 16;
                }

                if self.open {
                    *x_offset += 16;

                    for (idx, element) in self.values.iter().enumerate() {
                        if *y_offset > builder.window_height() { break }

                        if *remaining_scroll > 0 {
                            *remaining_scroll -= 1;
                            *line_number += 1;
                            continue;
                        }

                        if ctx.ghost.is_some_and(|(id, _, _)| id == $id) && ctx.ghost(*x_offset, *y_offset, builder, |x, y| *x_offset == x && *y_offset == y) {
                            builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, 16));
                            if !tail { builder.draw_texture((*x_offset - 32, *y_offset), (80, 16), (8, 16)); }
                            *y_offset += 16;
                        }

                        let ghost_tail_mod = if let Some((id, x, y)) = ctx.ghost && x == *x_offset && y == *y_offset + 16 - *remaining_scroll * 16 - 8 && id == $id {
                            false
                        } else {
                            true
                        };

                        builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, (!(idx == self.len() - 1 && ghost_tail_mod)) as usize * 7 + 9));
                        if !tail { builder.draw_texture((*x_offset - 32, *y_offset), (80, 16), (8, 16)); }

                        ctx.line_number(*y_offset, line_number, builder);
                        Self::render_element_icon(*x_offset, *y_offset, builder);
                        ctx.highlight((*x_offset, *y_offset), builder);
                        if ctx.forbid(*y_offset) {
                            builder.settings(*x_offset + 20, *y_offset, true);
                            let _ = write!(builder, "{element}");
                        }

                        *y_offset += 16;

                        if ctx.ghost.is_some_and(|(id, _, _)| id == $id) && ctx.ghost(*x_offset, *y_offset, builder, |x, y| *x_offset == x && *y_offset - 8 == y) {
                            builder.draw_texture((*x_offset - 16, *y_offset), (80, 16), (16, (idx != self.len() - 1) as usize * 7 + 9));
                            if !tail { builder.draw_texture((*x_offset - 32, *y_offset), (80, 16), (8, 16)); }
                            *y_offset += 16;
                        }
                    }

                    *x_offset -= 16;
                } else {
                    *line_number += self.height - 1;
                }
            }

            pub fn get(&self, idx: usize) -> Option<&$t> {
                self.values.get(idx)
            }

            pub fn get_mut(&mut self, idx: usize) -> Option<&mut $t> {
                self.values.get_mut(idx)
            }

            #[inline]
            pub fn child_height(&self, _: usize) -> usize {
                1
            }

            pub fn drop(&mut self, key: Option<Box<str>>, element: NbtElement, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> DropFn {
                if 8 <= *y && *y < 16 && depth == target_depth {
                    let open_before = self.open;
                    let before = self.height();
                    indices.push(0);
                    if let Some(element) = self.insert(0, element) {
                        return DropFn::InvalidType(key, element)
                    }
                    self.open = true;
                    return DropFn::Dropped(self.height - before, 1, !open_before)
                }

                if self.height() * 16 <= *y && *y < self.height() * 16 + 8 && depth == target_depth {
                    let open_before = self.open;
                    let before = self.height();
                    indices.push(self.len());
                    if let Some(element) = self.insert(self.len(), element) {
                        return DropFn::InvalidType(key, element)
                    }
                    self.open = true;
                    return DropFn::Dropped(self.height - before, 1, !open_before)
                }

                if *y < 16 {
                    return DropFn::Missed(key, element)
                } else {
                    *y -= 16;
                }

                if self.open && !self.is_empty() {
                    if depth == target_depth {
                        indices.push(0);
                        let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
                        let heights = (element.height(), element.true_height());
                        for idx in 0..self.values.len() {
                            *ptr = idx;
                            if *y < 8 && depth == target_depth {
                                if let Some(element) = self.insert(idx, element) {
                                    return DropFn::InvalidType(key, element)
                                }
                                return DropFn::Dropped(heights.0, heights.1, false);
                            } else if *y < 16 && depth == target_depth {
                                *ptr = idx + 1;
                                if let Some(element) = self.insert(idx + 1, element) {
                                    return DropFn::InvalidType(key, element)
                                }
                                return DropFn::Dropped(heights.0, heights.1, false);
                            }

                            *y -= 16;
                        }
                        indices.pop();
                    } else {
                        if *y < self.height * 16 {
                            *y = 0;
                        } else {
                            *y -= self.height * 16;
                        }
                    }
                }
                DropFn::Missed(key, element)
            }

            pub unsafe fn duplicate(&mut self, y: &mut usize, indices: &mut Vec<usize>) {
                *y -= 1;
                self.height += 1;
                indices.push(*y);
                self.values.insert(*y, self.values[*y]);
            }

            pub unsafe fn drop_simple(&mut self, _: &mut usize, _: Option<Box<str>>, value: NbtElement, idx: usize, indices: &mut Vec<usize>) -> usize {
                indices.push(idx);
                self.values.insert(idx, match value {
                    NbtElement::$element($element_inner { value }) => value,
                    _ => core::hint::unreachable_unchecked(),
                });
                let before = self.height();
                self.height += 1;
                self.open = true;
                self.height - before
            }

            pub fn copy(&self, y: &mut usize, parent: Option<String>) {
                if *y == 0 {
                    let _ = cli_clipboard::set_contents(format!("{}{self}", parent.map(|x| x + ":").unwrap_or(String::new())));
                } else if *y <= self.len() {
                    let _ = cli_clipboard::set_contents(format!("{}", self.values[*y - 1]));
                }
            }

            pub fn toggle_fn(&mut self, y: &mut usize, target_depth: usize, depth: usize, _: &mut Vec<usize>) -> ToggleFn {
                if *y == 0 && depth == target_depth {
                    self.toggle();
                    Ok((self.height - 1).wrapping_mul((self.open as usize * 2).wrapping_sub(1)))
                } else {
                    Err(())
                }
            }

            pub unsafe fn try_select_text(&self, y: &mut usize, indices: &mut Vec<usize>) -> TrySelectTextFn {
                *y -= 1;
                indices.push(*y);
                return (None, Some((self.values[*y].to_string().into_boxed_str(), true)))
            }

            pub unsafe fn delete(&mut self, y: &mut usize, indices: &mut Vec<usize>) -> DeleteFn {
                *y -= 1;
                indices.push(*y);
                self.height -= 1;
                return (None, NbtElement::$element($element_inner { value: self.values.remove(*y) }), (1, 1))
            }

            pub fn steal(&mut self, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> StealFn {
                *y -= 1;
                if self.open {
                    indices.push(0);
                    let ptr = unsafe { &mut *indices.as_mut_ptr().add(indices.len() - 1) };
                    for idx in 0..self.len() {
                        *ptr = idx;

                        if *y >= 1 {
                            *y -= 1;
                            continue
                        } else if depth + 1 == target_depth {
                            self.height -= 1;
                            return Ok((None, NbtElement::$element($element_inner { value: self.values.remove(idx) }), (1, 1)))
                        } else {
                            break
                        }
                    }
                    indices.pop();
                }
                Err(())
            }

            #[inline]
            pub fn render_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
                builder.draw_texture((x, y), ($u, $v), (16, 16));
            }

            #[inline]
            pub fn render_element_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
                builder.draw_texture((x, y), ($u2, $v2), (16, 16));
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "[{};", $char)?;
                for (idx, element) in self.values.iter().enumerate() {
                    write!(f, "{element}")?;
                    if likely(idx < self.len() - 1) {
                        write!(f, ",")?;
                    }
                }
                write!(f, "]")
            }
        }
    };
}
