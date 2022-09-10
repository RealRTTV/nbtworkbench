#![feature(unchecked_math)]
#![feature(linked_list_remove)]
#![windows_subsystem = "windows"]

use std::collections::{HashSet, LinkedList};
use std::ffi::OsString;
use std::fs::read;
use std::io::Read;
use std::path::PathBuf;
use std::string::String;
use std::time;

use flate2::read::GzDecoder;
use winit::dpi::PhysicalPosition;
use winit::event::{ElementState, KeyboardInput, MouseButton, MouseScrollDelta, VirtualKeyCode};

use elements::element_type::NbtElement;
use vertex_buffer_builder::VertexBufferBuilder;

use crate::assets::HEADER_SIZE;
use crate::elements::byte::NbtByte;
use crate::elements::byte_array::NbtByteArray;
use crate::elements::compound::NbtCompound;
use crate::elements::int::NbtInt;
use crate::elements::int_array::NbtIntArray;
use crate::elements::long::NbtLong;
use crate::elements::long_array::NbtLongArray;
use crate::NbtElement::*;

mod decoder;
mod encoder;
mod elements;
mod window;
mod assets;
mod vertex_buffer_builder;

fn main() {
    pollster::block_on(window::run());
    // todo, finish program
    {
        // todo, screen updating
        // todo, saving

        // todo, copy-paste
        // todo, scrollbar

        // todo, windows icon
        // todo, web assembly ver
    }
}

#[repr(C)]
pub struct NbtWorkbench {
    tabs: LinkedList<FileEntry>,
    tab: usize,
    scroll: u32,
    mouse_x: u32,
    mouse_y: u32,
    window_height: u32,
    held_mouse_keys: HashSet<MouseButton>,
    held_keys: HashSet<VirtualKeyCode>,
    held_entry: Option<u8>,
    selected_text: Option<SelectedText>
}

pub struct SelectedText { // expires on click, no need to mutate the y value
    x: u32,
    y: u32,
    index: Option<(u32, std::string::String)>, // index and value name (if parent is a compound)
    cursor: u32,
    value: String,
    original_width: u32,
    selection: i32 // todo, (selection relative)
}

impl SelectedText {
    #[inline]
    pub fn new(x: u32, y: u32, index: Option<(u32, String)>, value: String) -> SelectedText {
        SelectedText { x, y, index, cursor: value.len() as u32, original_width: VertexBufferBuilder::width(&value), value, selection: 0 }
    }

    #[inline]
    pub fn on_key_press(&mut self, key: VirtualKeyCode, char: Option<char>, ctrl: bool) -> bool {
        if ctrl {
            if key == VirtualKeyCode::Back {
                let cursor = self.cursor as usize;
                let pos = self.value.char_indices().take(cursor).filter(|&(_, x)| is_jump_point_char(x)).map(|(x, _)| x).collect::<Vec<_>>().pop().unwrap_or(0);
                let (left, _) = self.value.split_at(pos);
                let (_, right) = self.value.split_at(cursor);
                self.value = format!("{}{}", left, right);
                self.cursor = pos as u32;
                return false;
            } else if key == VirtualKeyCode::Delete {
                let cursor = self.cursor as usize;
                let pos = if self.value.chars().skip(cursor).next().filter(|&x| is_jump_point_char(x)).is_some() { cursor + 1 } else { self.value.char_indices().skip(cursor).filter(|&(_, x)| is_jump_point_char(x)).next().map_or(0, |(x, _)| x) };
                let (left, _) = self.value.split_at(cursor);
                let (_, right) = self.value.split_at(pos);
                self.value = format!("{}{}", left, right);
                return false;
            } else if key == VirtualKeyCode::Left {
                let cursor = self.cursor as usize;
                self.cursor = self.value.char_indices().take(cursor).filter(|&(_, x)| is_jump_point_char(x)).map(|(x, _)| x as u32).collect::<Vec<_>>().pop().unwrap_or(0);
                return false;
            } else if key == VirtualKeyCode::Right {
                let cursor = self.cursor as usize;
                self.cursor = if self.value.chars().skip(cursor).next().filter(|&x| is_jump_point_char(x)).is_some() { (cursor + 1) as u32 } else { self.value.char_indices().skip(cursor).filter(|&(_, x)| is_jump_point_char(x)).next().map_or(0, |(x, _)| x as u32) };
                return false;
            }
        }

        if key == VirtualKeyCode::Escape || key == VirtualKeyCode::Return || key == VirtualKeyCode::NumpadEnter {
            return true;
        } else if key == VirtualKeyCode::Back && self.cursor > 0 {
            let cursor = self.cursor as usize;
            let (left, right) = self.value.split_at(cursor);
            let left = left.split_at(left.len() - 1).0;
            self.value = format!("{}{}", left, right);
            self.cursor -= 1;
        } else if key == VirtualKeyCode::Delete && self.cursor < self.value.len() as u32 {
            let cursor = self.cursor as usize;
            let (left, right) = self.value.split_at(cursor);
            let right = right.split_at(1).1;
            self.value = format!("{}{}", left, right);
        } else if let Some(char) = char {
            let cursor = self.cursor as usize;
            let (left, right) = self.value.split_at(cursor);
            self.value = format!("{}{}{}", left, char, right);
            self.cursor += 1;
        } else if key == VirtualKeyCode::Left && self.cursor != 0 {
            self.cursor = if ctrl { 0 } else { self.cursor - 1 };
        } else if key == VirtualKeyCode::Right && self.cursor != self.value.len() as u32 {
            self.cursor = if ctrl { self.value.len() as u32 } else { self.cursor + 1 };
        }
        false
    }

    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder) {
        let x = self.x;
        let y = self.y;
        let width = VertexBufferBuilder::width(&self.value).max(1); // I can cache this if i really want
        {
            let mut offset = x;
            let mut remaining_width = self.original_width + self.index.as_ref().map(|(_, x)| VertexBufferBuilder::width(x)).unwrap_or(0);
            while remaining_width > 0 {
                builder.draw_texture(offset, y, 112, 16, remaining_width.min(16), 16);
                remaining_width = if remaining_width <= 16 { 0 } else { remaining_width - 16 };
                offset += 16
            }
        }
        {
            builder.draw_texture(x, y + 13, 0, 16, 1, 3);
            let mut offset = x + 1;
            let mut remaining_width = width - 1;
            while remaining_width > 0 {
                builder.draw_texture(offset, y + 13, 1, 16, remaining_width.min(14), 3);
                remaining_width = if remaining_width <= 14 {
                    builder.draw_texture(offset + remaining_width, y + 13, 15, 16, 1, 3);
                    0
                } else {
                    remaining_width - 14
                };
                offset += 14;
            }
        }
        if let Some(compound_value) = self.index.as_ref().map(|x| &x.1) {
            builder.draw_text(x + VertexBufferBuilder::width(&self.value), y + 4, compound_value, true);
        }
        builder.draw_text(x, y + 4, &self.value, true);
    }
}

#[inline]
pub fn is_jump_point_char(char: char) -> bool {
    char == ' '
}

impl Default for NbtWorkbench {
    fn default() -> Self {
        let mut workbench = NbtWorkbench {
            tabs: LinkedList::new(),
            tab: 0,
            scroll: 0,
            mouse_x: 0,
            mouse_y: 0,
            window_height: 0,
            held_mouse_keys: HashSet::new(),
            held_keys: HashSet::new(),
            held_entry: None,
            selected_text: None
        };
        let mut compound = NbtCompound::new();
        compound.put("bytes§4".to_string(), ByteArray(NbtByteArray::new(vec![1, 2, 3, 4, 5, 6, 7, 8, 9].iter().map(|&x| Byte(NbtByte::new(x))).collect())));
        compound.put("ints".to_string(), IntArray(NbtIntArray::new(vec![1, 2, 3, 4, 5, 6, 7, 8, 9].iter().map(|&x| Int(NbtInt::new(x))).collect())));
        compound.put("longs".to_string(), LongArray(NbtLongArray::new(vec![1, 2, 3, 4, 5, 6, 7, 8, 9].iter().map(|&x| Long(NbtLong::new(x))).collect())));
        let shell = Compound(compound);
        workbench.tabs.push_front(FileEntry {
            value: shell,
            name: "in.nbt".to_string(),
            path: None,
            compressed: false
        });
        workbench
    }
}

impl NbtWorkbench {
    #[inline]
    pub fn new() -> Self {
        Default::default()
    }

    #[inline]
    pub fn on_open_file(&mut self, path: &PathBuf) -> bool {
        if let Ok(t) = read(path) {
            let (file, compressed) = {
                if t.first() == Some(&0x1F) && t.get(1) == Some(&0x8B) {
                    let mut decoder = GzDecoder::new(t.as_slice());
                    let mut file = Vec::new();
                    let _ = decoder.read_to_end(&mut file);
                    (file, true)
                } else {
                    (t, false)
                }
            };
            match FileEntry::new(&file, &path, compressed) {
                None => false,
                Some(entry) => {
                    self.tabs.push_back(entry);
                    self.tab = self.tabs.len() - 1;
                    true
                }
            }
        } else {
            false
        }
    }

    #[inline]
    pub fn on_scroll(&mut self, scroll: &MouseScrollDelta) -> bool {
        match scroll {
            MouseScrollDelta::LineDelta(_, v) => {
                let value = -*v;
                if value.is_sign_negative() && self.scroll < (-value * 48.0) as u32 {
                    self.scroll = 0;
                } else if value.is_sign_negative() {
                    self.scroll -= (-value * 48.0) as u32;
                } else {
                    self.scroll += (value * 48.0) as u32;
                }
            }
            MouseScrollDelta::PixelDelta(_) => {}
        }
        true
    }

    #[inline]
    pub fn on_mouse_input(&mut self, state: &ElementState, button: &MouseButton) -> bool {
        if *state == ElementState::Released {
            self.held_mouse_keys.remove(button);
            let x = self.mouse_x;
            let y = self.mouse_y;
            if y < 19 && x > 2 && y > 3 {
                self.click_tab(button);
            } else if y > HEADER_SIZE {
                if let Some(x) = &self.held_entry {
                    self.drop(*x)
                } else if self.held_keys.contains(&VirtualKeyCode::LAlt) || self.held_keys.contains(&VirtualKeyCode::RAlt) {
                    self.delete(button);
                } else {
                    self.toggle(button)
                }
            }
            self.held_entry = None;
        } else {
            self.held_mouse_keys.insert(*button);
            if self.selected_text.is_some() {
                self.close_selected_text();
            }
            self.selected_text = None;
            if self.mouse_y >= 23 && self.mouse_y < 39 {
                self.hold_entry(button);
            }
        }
        true
    }

    #[inline]
    fn delete(&mut self, button: &MouseButton) {
        let start = time::Instant::now();
        if *button == MouseButton::Right {
            let x = self.mouse_x / 16;
            let y = (self.mouse_y - HEADER_SIZE) / 16;
            let scroll = self.scroll() / 16;
            if let Some(entry) = self.tab_mut() {
                let mut first_parent = false;
                let first_parent_mut = &mut first_parent as *mut bool;
                let mut height = 0;
                let height_mut = &mut height as *mut u32;
                let mut index = 0;
                let index_mut = &mut index as *mut u32;
                entry.value.stack(&mut (y + scroll), &mut 0, 0, &mut |parent| unsafe {
                    if first_parent {
                        parent.delete(index);
                        *first_parent_mut = false;
                    }
                    parent.decrement(height);
                }, &mut |tail, depth, index| unsafe {
                    if depth == x || x == depth + 1 {
                        *height_mut = tail.height();
                        *index_mut = index;
                        *first_parent_mut = true;
                    }
                });
            }
        }
        println!("delete: {}ms", time::Instant::now().duration_since(start).as_nanos() as f64 / 1_000_000.0)
    }

    #[inline]
    fn drop(&mut self, id: u8) {
        let start = time::Instant::now();
        let x = self.mouse_x / 16;
        let y = (self.mouse_y - HEADER_SIZE) / 16;
        let scroll = self.scroll() / 16;
        if let Some(entry) = self.tab_mut() {
            let success = false;
            let success_mut = &success as *const bool as *mut bool;
            let mut depth = 0;
            let depth_mut = &mut depth as *mut u32;
            let mut toggle = false;
            let toggle_mut = &mut toggle as *mut bool;
            entry.value.stack(&mut (y + scroll), &mut 0, 0, &mut |parent| if success {
                parent.increment(1);
            }, &mut |tail, depth, _| unsafe { // assert this runs on the same thread
                if x == depth || depth == x + 1 {
                    *success_mut = tail.drop(NbtElement::from_id(id));
                    *depth_mut = depth * 16;
                    *toggle_mut = tail.height() == 1;
                }
            });
            if toggle {
                let x_before = self.mouse_x;
                self.mouse_x = depth;
                self.toggle(&MouseButton::Left);
                self.mouse_x = x_before;
            }
        }
        println!("drop: {}ms", time::Instant::now().duration_since(start).as_nanos() as f64 / 1_000_000.0)
    }

    #[inline]
    fn hold_entry(&mut self, button: &MouseButton) {
        if *button == MouseButton::Left {
            self.held_entry = match self.mouse_x / 16 {
                0 => Some(1),
                1 => Some(2),
                2 => Some(3),
                3 => Some(4),
                4 => Some(5),
                5 => Some(6),
                6 => Some(7),
                7 => Some(11),
                8 => Some(12),
                9 => Some(8),
                10 => Some(9),
                11 => Some(10),
                _ => None
            };
        }
    }

    #[inline]
    fn click_tab(&mut self, button: &MouseButton) {
        let mut x = self.mouse_x - 2;
        for (index, tab) in self.tabs.iter().enumerate() {
            let width = VertexBufferBuilder::width(&tab.name) + 5;
            if x <= width {
                match button {
                    MouseButton::Left => self.tab = index,
                    MouseButton::Middle | MouseButton::Right => {
                        self.tabs.remove(index);
                        self.tab = if index == 0 { 0 } else { index - 1 };
                    },
                    _ => {}
                }
                break
            } else {
                x -= width;
                if x < 4 {
                    break
                } else {
                    x -= 4;
                }
            }
        }
    }

    #[inline]
    fn toggle(&mut self, button: &MouseButton) {
        let start = time::Instant::now();
        if *button == MouseButton::Left || *button == MouseButton::Right {
            let x = self.mouse_x / 16;
            let y = (self.mouse_y - HEADER_SIZE) / 16;
            let scroll = self.scroll() / 16;
            let selected_text_mut = &mut self.selected_text as *mut Option<SelectedText>;
            if let Some(entry) = self.tab_mut() {
                let mut change = 0;
                let change_mut = &mut change as *mut i32;
                let mut depth = 0;
                let depth_mut = &mut depth as *mut u32;
                let mut index = 0;
                let index_mut = &mut index as *mut u32;
                let mut first_parent = false;
                let first_parent_mut = &mut first_parent as *mut bool;
                let mut value: Option<std::string::String> = None;
                let value_mut = &mut value as *mut Option<std::string::String>;
                let _ = entry.value.stack(&mut (y + scroll), &mut 0, 0, &mut |parent| {
                    if change < 0 {
                        parent.decrement(-change as u32);
                    } else {
                        parent.increment(change as u32);
                    }

                    if first_parent {
                        unsafe {
                            *first_parent_mut = false;
                            *selected_text_mut = if *button == MouseButton::Left { if let Compound(compound) = parent {
                                Some(SelectedText::new(depth + 16 + 39, y * 16 + HEADER_SIZE, Some((index, compound.get(index).unwrap().value_render())), compound.key(index).to_string()))
                            }else{None}} else if *button == MouseButton::Right { if let Some(value) = &value {
                                Some(SelectedText::new(depth * 16 + 40 + if let Compound(compound) = parent { VertexBufferBuilder::width(compound.key(index)) + VertexBufferBuilder::width(": ") } else { 0 }, y * 16 + HEADER_SIZE, None, value.to_string()))
                            }else{None}} else {
                                None
                            };
                        }
                    }
                }, &mut |tail, depth, index| {
                    let before = tail.height();
                    if (depth == x || x == depth + 1) && tail.toggle() {
                        unsafe {
                            *change_mut = tail.height() as i32 - before as i32;
                        }
                    } else if x > depth {
                        unsafe {
                            *value_mut = tail.value();
                            *depth_mut = depth;
                            *index_mut = index;
                            *first_parent_mut = true;
                        }
                    }
                });
            }
        }
        println!("toggle: {}ms", time::Instant::now().duration_since(start).as_nanos() as f64 / 1_000_000.0)
    }

    #[inline]
    pub fn close_selected_text(&mut self) {
        let values = if self.selected_text.is_some() {
            let selected_text = self.selected_text.as_ref().unwrap();
            Some(((selected_text.y - HEADER_SIZE) / 16, selected_text.value.clone(), selected_text.index.as_ref().map(|x| *(&x.0, &x.1).0)))
        } else {
            None
        };
        if let Some((y, value, index)) = values {
            let scroll = self.scroll() / 16;
            let mut first_parent = index.is_some();
            let first_parent_mut = &mut first_parent as *mut bool;
            self.tab_mut().unwrap().value.stack(&mut if scroll > y { 0 } else { y - scroll }, &mut 0, 0, &mut |parent| if first_parent {
                unsafe { *first_parent_mut = false; }
                if let Compound(compound) = parent {
                    if let Some(index) = index {
                        compound.update_key(index, value.clone());
                    }
                }
            },
            &mut |tail, _, _| if index.is_none() { tail.set_value(&value) });
        }
    }

    #[inline]
    pub fn on_key_input(&mut self, key: &KeyboardInput) -> bool {
        match key.state {
            ElementState::Pressed => {
                if let Some(x) = key.virtual_keycode {
                    self.held_keys.insert(x);
                    let char = self.char_from_key(x);
                    if let Some(selected_text) = &mut self.selected_text {
                        if selected_text.on_key_press(x, char, self.held_keys.contains(&VirtualKeyCode::LControl) || self.held_keys.contains(&VirtualKeyCode::RControl)) {
                            self.close_selected_text();
                            self.selected_text = None
                        }
                    }
                }
            }
            ElementState::Released => {
                if let Some(x) = key.virtual_keycode {
                    self.held_keys.remove(&x);
                }
            }
        }
        true
    }

    #[inline]
    pub fn on_cursor_move(&mut self, pos: &PhysicalPosition<f64>) -> bool {
        self.mouse_x = pos.x as u32;
        self.mouse_y = pos.y as u32;
        true
    }

    #[inline]
    pub fn window_height(&mut self, window_height: u32) {
        self.window_height = window_height;
    }

    #[inline]
    pub fn scroll(&mut self) -> u32 {
        let height = self.tab().map(|x| x.value.height() * 16 + 16).unwrap_or(0);
        let window_height = self.window_height - HEADER_SIZE;
        let max_scroll = if window_height > height { 0 } else { height - window_height };
        self.scroll = self.scroll.min(max_scroll);
        self.scroll
    }

    #[inline]
    fn tab(&self) -> Option<&FileEntry> {
        let offset_from_end = self.tabs.len() - self.tab - 1;
        let mut iter = self.tabs.iter();
        if self.tab <= offset_from_end {
            for _ in 0..self.tab {
                iter.next();
            }
            iter.next()
        } else {
            for _ in 0..offset_from_end {
                iter.next_back();
            }
            iter.next_back()
        }
    }

    #[inline]
    fn tab_mut(&mut self) -> Option<&mut FileEntry> {
        let offset_from_end = self.tabs.len() - self.tab - 1;
        let mut iter = self.tabs.iter_mut();
        if self.tab <= offset_from_end {
            for _ in 0..self.tab {
                iter.next();
            }
            iter.next()
        } else {
            for _ in 0..offset_from_end {
                iter.next_back();
            }
            iter.next_back()
        }
    }

    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder) {
        self.render_tabs(builder);
        self.render_icons(builder);
        if let Some(entry) = self.tab() {
            entry.render(builder);
            if let Some(selected_text) = &self.selected_text {
                selected_text.render(builder);
            }
        }
        self.render_held_entry(builder);
    }

    #[inline]
    fn render_held_entry(&self, builder: &mut VertexBufferBuilder) {
        if let Some(x) = &self.held_entry {
            NbtElement::render_icon(*x, self.mouse_x, self.mouse_y, builder);
        }
    }

    #[inline]
    fn render_icons(&self, builder: &mut VertexBufferBuilder) {
        builder.draw_texture(0, 26, 0, 0, 16, 16);
        builder.draw_texture(16, 26, 16, 0, 16, 16);
        builder.draw_texture(32, 26, 32, 0, 16, 16);
        builder.draw_texture(48, 26, 48, 0, 16, 16);
        builder.draw_texture(64, 26, 64, 0, 16, 16);
        builder.draw_texture(80, 26, 80, 0, 16, 16);
        builder.draw_texture(96, 26, 96, 0, 16, 16);
        builder.draw_texture(112, 26, 112, 0, 16, 16);
        builder.draw_texture(128, 26, 128, 0, 16, 16);
        builder.draw_texture(144, 26, 144, 0, 16, 16);
        builder.draw_texture(160, 26, 160, 0, 16, 16);
        builder.draw_texture(176, 26, 176, 0, 16, 16);

        if self.mouse_x < 192 && self.mouse_y >= 23 && self.mouse_y < 39 {
            builder.draw_texture(self.mouse_x & 0xFFFFFFF0, 26, 0, 16, 16, 16);
        }
    }

    #[inline]
    fn render_tabs(&self, builder: &mut VertexBufferBuilder) {
        let mut offset = 3;
        for (index, entry) in self.tabs.iter().enumerate() {
            let u = if index == self.tab { 79 } else { 31 };
            let color = if entry.path.is_none() { 0xFF55FF_FF } else { 0xFFFFFF_FF };
            builder.draw_texture(offset, 3, u, 16, 1, 16);
            let mut width = VertexBufferBuilder::width(&entry.name) + 4;
            let mut middle_offset = offset;
            while width > 0 {
                builder.draw_texture(middle_offset, 3, u + 1, 16, width.min(16), 16);
                width = if width < 16 { 0 } else { width - 16 };
                middle_offset += 16
            }
            builder.draw_text_color(offset + 2, 7, &entry.name, true, color);
            offset += width;
            builder.draw_texture(offset, 3, u + 17, 16, 1, 16);
            offset += 2;
        }
        for i in 0..=builder.window_width() >> 4 {
            builder.draw_texture(i << 4, 21, 0, 64, 16, 2);
            builder.draw_texture(i << 4, 45, 0, 64, 16, 2);
        }
    }

    #[inline]
    fn char_from_key(&self, key: VirtualKeyCode) -> Option<char> {
        let shift = self.held_keys.contains(&VirtualKeyCode::LShift) || self.held_keys.contains(&VirtualKeyCode::RShift);
        Some(match key {
            VirtualKeyCode::Key1 => if shift { '!' } else { '1' },
            VirtualKeyCode::Key2 => if shift { '@' } else { '2' },
            VirtualKeyCode::Key3 => if shift { '#' } else { '3' },
            VirtualKeyCode::Key4 => if shift { '$' } else { '4' },
            VirtualKeyCode::Key5 => if shift { '%' } else { '5' },
            VirtualKeyCode::Key6 => if shift { '^' } else { '6' },
            VirtualKeyCode::Key7 => if shift { '&' } else { '7' },
            VirtualKeyCode::Key8 => if shift { '*' } else { '8' },
            VirtualKeyCode::Key9 => if shift { '(' } else { '9' },
            VirtualKeyCode::Key0 => if shift { ')' } else { '0' },
            VirtualKeyCode::A => if shift { 'A' } else { 'a' },
            VirtualKeyCode::B => if shift { 'B' } else { 'b' },
            VirtualKeyCode::C => if shift { 'C' } else { 'c' },
            VirtualKeyCode::D => if shift { 'D' } else { 'd' },
            VirtualKeyCode::E => if shift { 'E' } else { 'e' },
            VirtualKeyCode::F => if shift { 'F' } else { 'f' },
            VirtualKeyCode::G => if shift { 'G' } else { 'g' },
            VirtualKeyCode::H => if shift { 'H' } else { 'h' },
            VirtualKeyCode::I => if shift { 'I' } else { 'i' },
            VirtualKeyCode::J => if shift { 'J' } else { 'j' },
            VirtualKeyCode::K => if shift { 'K' } else { 'k' },
            VirtualKeyCode::L => if shift { 'L' } else { 'l' },
            VirtualKeyCode::M => if shift { 'M' } else { 'm' },
            VirtualKeyCode::N => if shift { 'N' } else { 'n' },
            VirtualKeyCode::O => if shift { 'O' } else { 'o' },
            VirtualKeyCode::P => if shift { 'P' } else { 'p' },
            VirtualKeyCode::Q => if shift { 'Q' } else { 'q' },
            VirtualKeyCode::R => if shift { 'R' } else { 'r' },
            VirtualKeyCode::S => if shift { 'S' } else { 's' },
            VirtualKeyCode::T => if shift { 'T' } else { 't' },
            VirtualKeyCode::U => if shift { 'U' } else { 'u' },
            VirtualKeyCode::V => if shift { 'V' } else { 'v' },
            VirtualKeyCode::W => if shift { 'W' } else { 'w' },
            VirtualKeyCode::X => if shift { 'X' } else { 'x' },
            VirtualKeyCode::Y => if shift { 'Y' } else { 'y' },
            VirtualKeyCode::Z => if shift { 'Z' } else { 'z' },
            VirtualKeyCode::Space => ' ',
            VirtualKeyCode::Caret => '^',
            VirtualKeyCode::Numpad0 => '0',
            VirtualKeyCode::Numpad1 => '1',
            VirtualKeyCode::Numpad2 => '2',
            VirtualKeyCode::Numpad3 => '3',
            VirtualKeyCode::Numpad4 => '4',
            VirtualKeyCode::Numpad5 => '5',
            VirtualKeyCode::Numpad6 => '6',
            VirtualKeyCode::Numpad7 => '7',
            VirtualKeyCode::Numpad8 => '8',
            VirtualKeyCode::Numpad9 => '9',
            VirtualKeyCode::NumpadAdd => '+',
            VirtualKeyCode::NumpadDivide => '/',
            VirtualKeyCode::NumpadDecimal => '.',
            VirtualKeyCode::NumpadComma => ',',
            VirtualKeyCode::NumpadEquals => '=',
            VirtualKeyCode::NumpadMultiply => '*',
            VirtualKeyCode::NumpadSubtract => '-',
            VirtualKeyCode::Apostrophe => if shift { '"' } else { '\'' },
            VirtualKeyCode::Asterisk => '*',
            VirtualKeyCode::Backslash => if shift { '|' } else { '\\' },
            VirtualKeyCode::Colon => if shift { ':' } else { ';' },
            VirtualKeyCode::Comma => if shift { '<' } else { ',' },
            VirtualKeyCode::Equals => if shift { '+' } else { '=' },
            VirtualKeyCode::Grave => if shift { '~' } else { '`' },
            VirtualKeyCode::LBracket => if shift { '{' } else { '[' },
            VirtualKeyCode::Minus => if shift { '_' } else { '-' },
            VirtualKeyCode::Period => if shift { '>' } else { '.' },
            VirtualKeyCode::Plus => '+',
            VirtualKeyCode::RBracket => if shift { '}' } else { ']' },
            VirtualKeyCode::Semicolon => ';',
            VirtualKeyCode::Slash => if shift { '?' } else { '/' },
            VirtualKeyCode::Tab => '\t',
            _ => return None
        })
    }
}

#[repr(C)]
pub struct FileEntry {
    value: NbtElement,
    name: std::string::String,
    path: Option<OsString>,
    compressed: bool
}

impl FileEntry {
    pub fn new(file: &[u8], path: &&PathBuf, compressed: bool) -> Option<FileEntry> {
        Some(FileEntry {
            value: NbtElement::from_file(file)?,
            name: path.file_name()?.to_str()?.to_string(),
            path: Some(path.as_os_str().to_os_string()),
            compressed
        })
    }

    pub fn render(&self, builder: &mut VertexBufferBuilder) {
        if let Compound(x) = &self.value {
            x.render_root(builder, &self.name)
        }
    }
}
