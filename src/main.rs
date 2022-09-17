#![feature(unchecked_math)]
#![feature(linked_list_remove)]
#![feature(inline_const)]
#![windows_subsystem = "windows"]

use std::collections::{HashSet, LinkedList};
use std::ffi::OsString;
use std::fs::{read, write};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::string::String;

use flate2::read::GzDecoder;
use unicode_segmentation::UnicodeSegmentation;
use winit::dpi::PhysicalPosition;
use winit::event::{ElementState, KeyboardInput, MouseButton, MouseScrollDelta, VirtualKeyCode};

use elements::element_type::NbtElement;
use vertex_buffer_builder::VertexBufferBuilder;

use crate::assets::HEADER_SIZE;
use crate::elements::compound::NbtCompound;
use crate::NbtElement::*;
use crate::workbench_action::WorkbenchAction;

mod decoder;
mod encoder;
mod window;
mod assets;
mod vertex_buffer_builder;
mod workbench_action;

fn main() {
    pollster::block_on(window::run());
    { // todo, other
        // todo, drag and drop between two elements
        // todo, up down arrows, you know what they'll do

        // todo, scrollbar
        // todo, windows icon
        // todo, web assembly ver
        // todo, smart screen
    }
    { // todo, key binds
        // todo, ctrl + c to copy hovered element, or selected text
        // todo, ctrl + v or drag it into existance from the void slot
        // todo, double click to expand
        // todo, ctrl + up/down, to change indexes on things
        // todo, ctrl + x, to cut hovered element, or selected text
        // todo, ctrl + r, reload current page / currently hovered entry
        // todo, ctrl + s, save, or you can click the icon
        // todo, ctrl + z, keep a log of things that have been done
        // todo, ctrl + y
        // todo, ctrl + d, duplicate hovered element below thyself
        // todo, ctrl + h, open a playground nbt file to test in
    }
}

#[repr(C)]
pub struct NbtWorkbench {
    tabs: LinkedList<FileEntry>,
    tab: usize,
    scroll: u32, // todo, make it tab based
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
    cursor: usize, // grapheme index
    value: String,
    key_string: Option<std::string::String>,
    selection: Option<usize>
}

impl SelectedText {
    #[inline]
    pub fn new(x: u32, y: u32, index: Option<(u32, String)>, value: String, offset: u32, key_string: Option<std::string::String>) -> Option<SelectedText> {
        if x > offset + 4 {
            None
        } else if x > offset {
            Some(SelectedText { x, y, index, cursor: 0, value, key_string, selection: None })
        } else {
            let mut cursor = 0;
            let mut c = 0;
            for grapheme in value.graphemes(true) {
                if c < offset - x {
                    c += VertexBufferBuilder::width(grapheme);
                    cursor += 1;
                }
                if c >= offset - x {
                    return Some(SelectedText { x, y, index, cursor, value, key_string, selection: None })
                }
            }
            None
        }
    }

    #[inline]
    pub fn on_key_press(&mut self, key: VirtualKeyCode, char: Option<char>, ctrl: bool, shift: bool) -> bool {
        if key == VirtualKeyCode::Escape || key == VirtualKeyCode::Return || key == VirtualKeyCode::NumpadEnter {
            return true;
        }
        let mut cursor_before = self.cursor;
        if ctrl {
            if key == VirtualKeyCode::A {
                self.cursor = 0;
                self.selection = Some(self.value.graphemes(true).count());
            } else if key == VirtualKeyCode::Back {
                let (left, right, cursor) = if let Some(selection) = self.selection {
                    let (left, right) = self.value.split_at(grapheme_to_byte_index(&self.value, self.cursor));
                    let cursor = if self.cursor > 0 { self.cursor - 1 } else { 0 };
                    if selection < self.cursor {
                        (left.split_at(grapheme_to_byte_index(left, selection)).0, right, cursor)
                    } else {
                        (left, right.split_at(grapheme_to_byte_index(right, selection - self.cursor)).1, cursor)
                    }
                } else {
                    let pos = self.value.graphemes(true).enumerate().take(self.cursor).filter(|&(_, x)| is_jump_point_str(x)).map(|(x, _)| x).collect::<Vec<_>>().pop().unwrap_or(0);
                    (self.value.split_at(grapheme_to_byte_index(&self.value, pos)).0, self.value.split_at(grapheme_to_byte_index(&self.value, self.cursor)).1, pos)
                };
                self.value = format!("{}{}", left, right);
                self.selection = None;
                self.cursor = cursor;
            } else if key == VirtualKeyCode::Delete {
                let pos = if let Some(selection) = self.selection { selection } else if self.value.graphemes(true).nth(self.cursor).filter(|&x| is_jump_point_str(x)).is_some() { self.cursor + 1 } else { self.value.graphemes(true).enumerate().skip(self.cursor).find(|&(_, x)| is_jump_point_str(x)).map_or(self.value.graphemes(true).count(), |(x, _)| x) };
                let (left, _) = self.value.split_at(grapheme_to_byte_index(&self.value, self.cursor));
                let (_, right) = self.value.split_at(grapheme_to_byte_index(&self.value, pos));
                self.value = format!("{}{}", left, right);
                self.selection = None;
            } else if key == VirtualKeyCode::Left {
                self.cursor = self.value.graphemes(true).enumerate().take(self.cursor).filter(|&(_, x)| is_jump_point_str(x)).map(|(x, _)| x).collect::<Vec<_>>().pop().unwrap_or(0);
                if !shift {
                    self.selection = None;
                }
            } else if key == VirtualKeyCode::Right {
                self.cursor = if self.value.graphemes(true).nth(self.cursor).filter(|&x| is_jump_point_str(x)).is_some() { self.cursor + 1 } else { self.value.graphemes(true).enumerate().skip(self.cursor).find(|&(_, x)| is_jump_point_str(x)).map_or(self.value.graphemes(true).count(), |(x, _)| x) };
                if !shift {
                    self.selection = None;
                }
            }
            if shift {
                self.selection = Some(self.selection.unwrap_or(cursor_before));
            }
            false
        } else {
            if key == VirtualKeyCode::Back {
                let (mut left, mut right) = self.value.split_at(grapheme_to_byte_index(&self.value, self.cursor));
                if let Some(selection) = self.selection {
                    if selection < self.cursor {
                        left = left.split_at(grapheme_to_byte_index(left, selection)).0;
                    } else {
                        right = right.split_at(grapheme_to_byte_index(right, selection - self.cursor)).1;
                    }
                } else if left.graphemes(true).count() > 0 {
                    left = left.split_at(grapheme_to_byte_index(left, left.graphemes(true).count() - 1)).0
                }
                self.value = format!("{}{}", left, right);
                self.cursor = if self.cursor > 0 { self.cursor - 1 } else { 0 };
                self.selection = None;
            } else if key == VirtualKeyCode::Delete {
                let (mut left, mut right) = self.value.split_at(grapheme_to_byte_index(&self.value, self.cursor));
                if let Some(selection) = self.selection {
                    if selection < self.cursor {
                        left = left.split_at(grapheme_to_byte_index(left, selection)).0;
                    } else {
                        right = right.split_at(grapheme_to_byte_index(right, selection - self.cursor)).1;
                    }
                } else {
                    right = right.split_at(grapheme_to_byte_index(right, grapheme_to_byte_index(right, 1))).1;
                }
                self.value = format!("{}{}", left, right);
                self.selection = None;
            } else if let Some(char) = char {
                let (mut left, mut right) = self.value.split_at(grapheme_to_byte_index(&self.value, self.cursor));
                if let Some(selection) = self.selection {
                    if selection < self.cursor {
                        left = left.split_at(grapheme_to_byte_index(left, selection)).0;
                    } else {
                        right = right.split_at(grapheme_to_byte_index(right, selection - self.cursor)).1;
                    }
                }
                self.value = format!("{}{}{}", left, char, right);
                self.cursor += 1; // todo, graphemes are complex, maybe check this later
                cursor_before = self.cursor;
                self.selection = None;
            } else if key == VirtualKeyCode::Left {
                if self.cursor != 0 {
                    self.cursor -= 1;
                }
                if !shift {
                    self.selection = None;
                }
            } else if key == VirtualKeyCode::Right {
                if self.cursor != self.value.graphemes(true).count() {
                    self.cursor += 1;
                }
                if !shift {
                    self.selection = None;
                }
            } else if key == VirtualKeyCode::Home {
                self.cursor = 0;
                if !shift {
                    self.selection = None;
                }
            } else if key == VirtualKeyCode::End {
                self.cursor = self.value.graphemes(true).count();
                if !shift {
                    self.selection = None;
                }
            }
            if shift {
                self.selection = Some(self.selection.unwrap_or(cursor_before));
            }
            false
        }
    }

    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder) {
        let x = self.x;
        let y = self.y;
        builder.draw_texture(x + VertexBufferBuilder::width(self.value.split_at(grapheme_to_byte_index(&self.value, self.cursor)).0), y, 0, 32, 1, 16);
        if let Some(compound_value) = self.index.as_ref().map(|x| &x.1) {
            builder.draw_text(x + VertexBufferBuilder::width(&self.value), y, compound_value, true);
        } else if let Some(key_string) = &self.key_string {
            builder.draw_text(x - VertexBufferBuilder::width(key_string), y, key_string, true);
        }
        builder.draw_text(x, y, &self.value, true);
        if let Some(selection) = self.selection {
            let (start, end) = if self.cursor > selection { (selection, self.cursor) } else { (self.cursor, selection) };
            let mut start_x = x + VertexBufferBuilder::width(self.value.split_at(grapheme_to_byte_index(&self.value, start)).0);
            let end_x = x + VertexBufferBuilder::width(self.value.split_at(grapheme_to_byte_index(&self.value, end)).0);
            let mut remaining_width = end_x - start_x;
            while remaining_width > 0 {
                builder.draw_texture(start_x, y, 1, 32, remaining_width.min(14), 16);
                remaining_width = if remaining_width <= 14 { 0 } else { remaining_width - 14 };
                start_x += 14;
            }
        }
    }
}

#[inline]
pub fn is_jump_point_str(str: &str) -> bool {
    str == " "
}

#[inline]
pub fn grapheme_to_byte_index(str: &str, index: usize) -> usize {
    str.grapheme_indices(true).nth(index).map_or(str.len(), |x| x.0)
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
        if let Some(Ok(x)) = &std::env::args().nth(1).map(|x| PathBuf::from_str(&x)) {
            workbench.on_open_file(x);
        } else {
            let mut compound = NbtCompound::new();
            let mut a = NbtCompound::new();
            a.put("a".to_string(), NbtElement::from_id(1));
            a.put("b".to_string(), NbtElement::from_id(1));
            a.put("c".to_string(), NbtElement::from_id(1));
            a.put("d".to_string(), NbtElement::from_id(1));
            a.put("e".to_string(), NbtElement::from_id(1));
            a.put("f".to_string(), NbtElement::from_id(1));
            a.put("g".to_string(), NbtElement::from_id(1));
            a.put("h".to_string(), NbtElement::from_id(1));
            a.put("i".to_string(), NbtElement::from_id(1));
            a.put("j".to_string(), NbtElement::from_id(1));
            a.put("k".to_string(), NbtElement::from_id(1));
            a.put("l".to_string(), NbtElement::from_id(1));
            a.put("m".to_string(), NbtElement::from_id(1));
            a.put("n".to_string(), NbtElement::from_id(1));
            a.put("o".to_string(), NbtElement::from_id(1));
            a.put("p".to_string(), NbtElement::from_id(1));
            a.put("q".to_string(), NbtElement::from_id(1));
            a.put("r".to_string(), NbtElement::from_id(1));
            a.put("s".to_string(), NbtElement::from_id(1));
            compound.put("§ection fucking §ign".to_string(), Compound(a));
            compound.put("b".to_string(), Compound(NbtCompound::new()));
            workbench.tabs.push_front(FileEntry {
                value: Compound(compound),
                name: "debug.nbt".to_string(),
                path: None,
                compressed: false,
                history: LinkedList::new(),
                history_changed: false
            });
        };
        workbench
    }
}

impl NbtWorkbench {
    #[inline]
    pub fn new() -> Self {
        Default::default()
    }

    #[inline]
    pub fn forbidden_y(&self) -> Option<u32> {
        self.selected_text.as_ref().map(|x| x.y)
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
        self.close_selected_text();
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
                if (y - HEADER_SIZE) / 16 == 0 && x > 36 {
                    self.rename(x);
                } else if let Some(x) = &self.held_entry {
                    self.drop(*x)
                } else {
                    self.toggle(button)
                }
            }
            self.held_entry = None;
        } else {
            self.held_mouse_keys.insert(*button);
            self.close_selected_text();
            if self.mouse_y >= 23 && self.mouse_y < 39 {
                self.hold_entry(button);
            }
        }
        true
    }

    #[inline]
    fn rename(&mut self, offset: u32) {
        self.selected_text = SelectedText::new(40, HEADER_SIZE, None, self.tab().expect("tab exists").path.as_ref().and_then(|x| x.to_str().map(|x| x.to_string())).unwrap_or_else(|| self.tab().expect("tab exists").name.to_string()), offset, None);
    }

    #[inline]
    fn delete(&mut self) {
        let x = self.mouse_x / 16;
        let y = (self.mouse_y - HEADER_SIZE) / 16;
        let scroll = self.scroll() / 16;
        if let Some(tab) = self.tab_mut() {
            let mut first_parent = false;
            let first_parent_mut = &mut first_parent as *mut bool;
            let mut height = 0;
            let height_mut = &mut height as *mut u32;
            let mut index = 0;
            let index_mut = &mut index as *mut u32;
            tab.value.stack(&mut (y + scroll), &mut 0, 0, &mut |parent, remaining_y| {
                if first_parent {
                    let key = if let Compound(compound) = parent { Some(compound.key(index).to_owned()) } else { None };
                    let element = parent.delete(index).expect("expected the parent entry to hold other elements");
                    tab.history.push_back(WorkbenchAction::Remove {
                        y: (y + scroll) - remaining_y - 1,
                        element,
                        key,
                        index,
                    });
                    tab.history_changed = true;
                    first_parent = false;
                }
                parent.decrement(height);
            }, |tail, depth, index| unsafe {
                if depth == x || x == depth + 1 {
                    *height_mut = tail.height();
                    *index_mut = index;
                    *first_parent_mut = true;
                }
            });
        }
    }

    #[inline]
    fn drop(&mut self, id: u8) {
        let x = self.mouse_x / 16;
        let y = (self.mouse_y - HEADER_SIZE) / 16;
        let scroll = self.scroll() / 16;
        if let Some(tab) = self.tab_mut() {
            let mut success = false;
            let success_mut = &mut success as *mut bool;
            let mut depth = 0;
            let depth_mut = &mut depth as *mut u32;
            let mut toggle = false;
            let toggle_mut = &mut toggle as *mut bool;
            let mut action = None;
            let action_mut = &mut action as *mut Option<WorkbenchAction>;
            tab.value.stack(&mut (y + scroll), &mut 0, 0, &mut |parent, _| if success {
                parent.increment(1);
            }, |tail, depth, _| unsafe { // assert this runs on the same thread
                if x == depth || depth + 1 == x {
                    *success_mut = tail.drop(NbtElement::from_id(id));
                    *depth_mut = depth * 16;
                    *toggle_mut = !tail.open();
                    *action_mut = Some(WorkbenchAction::Add {
                        y: y + scroll,
                        index: tail.len().unwrap() - 1
                    })
                }
            });
            if let Some(action) = action {
                tab.history.push_back(action);
            }
            if toggle {
                let x_before = self.mouse_x;
                self.mouse_x = depth;
                self.toggle(&MouseButton::Left);
                self.mouse_x = x_before;
            }
        }
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
        if *button == MouseButton::Left || *button == MouseButton::Right {
            let x = self.mouse_x / 16;
            let y = (self.mouse_y - HEADER_SIZE) / 16;
            let mouse_x = self.mouse_x;
            let scroll = self.scroll() / 16;
            let selected_text_mut = &mut self.selected_text as *mut Option<SelectedText>;
            if let Some(tab) = self.tab_mut() {
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
                let mut action = None;
                let action_mut = &mut action as *mut Option<WorkbenchAction>;
                let _ = tab.value.stack(&mut (y + scroll), &mut 0, 0, &mut |parent, _| {
                    if change < 0 {
                        parent.decrement(-change as u32);
                    } else {
                        parent.increment(change as u32);
                    }

                    if first_parent {
                        unsafe {
                            first_parent = false;
                            let key = if let Compound(compound) = parent { SelectedText::new(depth * 16 + 40, y * 16 + HEADER_SIZE, Some((index, compound.get(index).unwrap().value_render())), compound.key(index).to_string(), mouse_x, None) } else { None };
                            if key.is_some() {
                                *selected_text_mut = key
                            } else {
                                *selected_text_mut = value.as_ref().and_then(|value| { SelectedText::new(depth * 16 + 40 + if let Compound(compound) = parent { VertexBufferBuilder::width(compound.key(index)) + VertexBufferBuilder::width(": ") } else { 0 }, y * 16 + HEADER_SIZE, None, value.to_string(), mouse_x, if let Compound(compound) = parent { Some(compound.key(index).to_owned() + ": ") } else { None }) })
                            }
                        }
                    }
                }, |tail, depth, index| {
                    let before = tail.height();
                    if (depth == x || x == depth + 1) && tail.toggle() {
                        unsafe {
                            *change_mut = tail.height() as i32 - before as i32;
                            *action_mut = Some(WorkbenchAction::Toggle { y: y + scroll })
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
                if let Some(action) = action {
                    tab.history.push_back(action);
                }
            }
        }
    }

    #[inline]
    pub fn close_selected_text(&mut self) {
        if let Some(selected_text) = &self.selected_text {
            if selected_text.y == HEADER_SIZE {
                let value = &selected_text.value.clone();
                let string = OsString::from(value);
                let tab = self.tab_mut().expect("tab exists");
                tab.name = Path::new(&string).file_name().unwrap_or(&string).to_str().unwrap_or(value).to_string();
                tab.path = Some(string);
                self.selected_text = None;
                return
            }
        }
        let values = if self.selected_text.is_some() {
            let selected_text = self.selected_text.as_ref().unwrap();
            Some(((selected_text.y - HEADER_SIZE) / 16, selected_text.value.clone(), selected_text.index.as_ref().map(|&(x, _)| x)))
        } else {
            None
        };
        if let Some((y, value, index)) = values {
            let scroll = self.scroll() / 16;
            let mut first_parent = index.is_some();
            let tab = self.tab_mut().unwrap();
            let mut action = None;
            let action_mut = &mut action as *mut Option<WorkbenchAction>;
            tab.value.stack(&mut (y + scroll), &mut 0, 0, &mut |parent, _| if first_parent {
                first_parent = false;
                if let Compound(compound) = parent {
                    if let Some(index) = index {
                        let key = compound.key(index).to_owned();
                        unsafe {
                            *action_mut = Some(WorkbenchAction::Rename {
                                y: y + scroll,
                                before: key,
                                key: true
                            })
                        }
                        compound.update_key(index, value.clone());
                    }
                }
            },
            |tail, _, _| if index.is_none() {
                if let Some(before) = tail.set_value(&value) {
                    unsafe {
                        *action_mut = Some(WorkbenchAction::Rename {
                            y: y + scroll,
                            before,
                            key: false
                        })
                    }
                }
            });
            if let Some(action) = action {
                tab.history.push_back(action);
            }
            tab.history_changed = true;
        }
        self.selected_text = None
    }

    #[inline]
    pub fn on_key_input(&mut self, key: &KeyboardInput) -> bool {
        match key.state {
            ElementState::Pressed => {
                if let Some(key) = key.virtual_keycode {
                    self.held_keys.insert(key);
                    let char = self.char_from_key(key);
                    let ctrl = self.held_keys.contains(&VirtualKeyCode::LControl) || self.held_keys.contains(&VirtualKeyCode::RControl);
                    if let Some(selected_text) = &mut self.selected_text {
                        if selected_text.on_key_press(key, char, ctrl, self.held_keys.contains(&VirtualKeyCode::LShift) || self.held_keys.contains(&VirtualKeyCode::RShift)) {
                            self.close_selected_text();
                        }
                    } else if ctrl && key == VirtualKeyCode::N {
                        self.close_selected_text();
                        self.tabs.push_back(FileEntry {
                            value: Compound(NbtCompound::new()),
                            name: "new.nbt".to_string(),
                            path: None,
                            compressed: false,
                            history: LinkedList::new(),
                            history_changed: false
                        });
                        self.tab = self.tabs.len() - 1;
                    } else if ctrl && key == VirtualKeyCode::S {
                        self.close_selected_text();
                        if let Some(tab) = self.tab_mut() {
                            if let Some(dir) = &tab.path {
                                let _ = write(dir, tab.value.to_file()).is_err();
                                tab.history_changed = false;
                            }
                        }
                    } else if ctrl && key == VirtualKeyCode::Z {
                        self.close_selected_text();
                        if let Some(tab) = self.tab_mut() {
                            if let Some(action) = tab.history.pop_back() {
                                action.undo(&mut tab.value);
                            }
                        }
                    } else if key == VirtualKeyCode::Back || key == VirtualKeyCode::Delete {
                        self.delete();
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
        if self.tabs.is_empty() {
            None
        } else {
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
    }

    #[inline]
    fn tab_mut(&mut self) -> Option<&mut FileEntry> {
        if self.tabs.is_empty() {
            None
        } else {
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
    }

    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder) {
        self.render_tabs(builder);
        self.render_icons(builder);
        if let Some(entry) = self.tab() {
            entry.render(builder, self.forbidden_y());
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
        builder.draw_texture(128, 26, 0, 16, 16, 16);
        builder.draw_texture(144, 26, 16, 16, 16, 16);
        builder.draw_texture(160, 26, 32, 16, 16, 16);
        builder.draw_texture(176, 26, 48, 16, 16, 16);

        if self.mouse_x < 192 && self.mouse_y >= 23 && self.mouse_y < 39 {
            builder.draw_texture(self.mouse_x & 0xFFFFFFF0, 26, 0, 32, 16, 16);
        }
    }

    #[inline]
    fn render_tabs(&self, builder: &mut VertexBufferBuilder) {
        let mut offset = 3;
        for (index, tab) in self.tabs.iter().enumerate() {
            let u = if index == self.tab { 32 } else { 16 };
            builder.draw_texture(offset, 3, u, 32, 1, 16);
            let width = VertexBufferBuilder::width(&tab.name) + 4 + 16;
            let mut remaining_width = width;
            let mut middle_offset = offset;
            while remaining_width > 0 {
                builder.draw_texture(middle_offset, 3, u + 1, 32, remaining_width.min(14), 16);
                remaining_width = if remaining_width < 14 { 0 } else { remaining_width - 14 };
                middle_offset += 14
            }
            builder.draw_text(offset + 2, 3, &tab.name, true);
            offset += width;
            builder.draw_texture(offset - 16, 3, 80 + if tab.history_changed && !tab.history.is_empty() { 0 } else { 16 }, 32, 16, 16);
            builder.draw_texture(offset, 3, u + 15, 32, 1, 16);
            offset += 2;
        }
        for i in 0..=builder.window_width() >> 4 {
            builder.draw_texture(i << 4, 21, 48, 32, 16, 2);
            builder.draw_texture(i << 4, 45, 48, 32, 16, 2);
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
            VirtualKeyCode::Colon => ':',
            VirtualKeyCode::Comma => if shift { '<' } else { ',' },
            VirtualKeyCode::Equals => if shift { '+' } else { '=' },
            VirtualKeyCode::Grave => if shift { '~' } else { '`' },
            VirtualKeyCode::LBracket => if shift { '{' } else { '[' },
            VirtualKeyCode::Minus => if shift { '_' } else { '-' },
            VirtualKeyCode::Period => if shift { '>' } else { '.' },
            VirtualKeyCode::Plus => '+',
            VirtualKeyCode::RBracket => if shift { '}' } else { ']' },
            VirtualKeyCode::Semicolon => if shift { ':' } else { ';' },
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
    compressed: bool,
    history: LinkedList<WorkbenchAction>,
    history_changed: bool
}

impl FileEntry {
    pub fn new(file: &[u8], path: &&PathBuf, compressed: bool) -> Option<FileEntry> {
        Some(FileEntry {
            value: NbtElement::from_file(file)?,
            name: path.file_name()?.to_str()?.to_string(),
            path: Some(path.as_os_str().to_os_string()),
            compressed,
            history: LinkedList::new(),
            history_changed: false
        })
    }

    pub fn render(&self, builder: &mut VertexBufferBuilder, forbidden_y: Option<u32>) {
        if let Compound(x) = &self.value {
            x.render_root(builder, &self.name, forbidden_y)
        }
    }
}

pub mod elements {
    pub mod element_type;
    pub mod byte;
    pub mod short;
    pub mod int;
    pub mod long;
    pub mod float;
    pub mod double;
    pub mod byte_array;
    pub mod string;
    pub mod list;
    pub mod compound;
    pub mod int_array;
    pub mod long_array;
}
