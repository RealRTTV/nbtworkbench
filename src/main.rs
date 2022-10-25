#![feature(unchecked_math)]
#![feature(linked_list_remove)]
#![feature(inline_const)]
#![feature(generic_const_exprs)]
#![windows_subsystem = "windows"]

use std::collections::{HashSet, LinkedList};
use std::ffi::OsString;
use std::fs::{read, write};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::string::String;

use flate2::read::GzDecoder;
use pollster::block_on;
use unicode_segmentation::UnicodeSegmentation;
use winit::dpi::PhysicalPosition;
use winit::event::{ElementState, KeyboardInput, MouseButton, MouseScrollDelta, VirtualKeyCode};

use elements::element_type::NbtElement;
use vertex_buffer_builder::VertexBufferBuilder;

use crate::assets::HEADER_SIZE;
use crate::elements::compound::NbtCompound;
use crate::elements::primitive::NbtPrimitive;
use crate::KeyResult::*;
use crate::NbtElement::*;
use crate::workbench_action::WorkbenchAction;

mod decoder;
mod encoder;
mod window;
mod assets;
mod vertex_buffer_builder;
mod workbench_action;

pub type DropFn = Result<NbtElement, (u32, Option<(u32, u32)>)>;
pub type DeleteFn = Option<Option<(NbtElement, u32, Option<std::string::String>, u32, u32)>>;
pub type LeftClickFn = Option<Result<i32, (u32, u32, u32, Option<std::string::String>, u32, Option<std::string::String>)>>;

fn main() {
    block_on(window::run());
    {
        // todo, up down arrows, you know what they'll do
        // todo, pull element out

        // todo, windows icon
        // todo, web assembly ver
        // todo, smart screen
    }
    {
        // todo, ctrl + h, open a playground nbt file to help with user interaction
        // todo, ctrl + s, click the icon
        // todo, ctrl + r, reload current page / currently hovered entry
        // todo, ctrl + c to copy hovered element, or selected text
        // todo, ctrl + d, duplicate hovered element below thyself
        // todo, ctrl + v or drag it into existance from **the** void/portal slot, or just text paste
        // todo, ctrl + x, to cut hovered element, or selected text

        // todo, ctrl + y, haha, no
    }
}

pub struct NbtWorkbench {
    tabs: LinkedList<FileEntry>,
    tab: usize,
    mouse_x: u32,
    mouse_y: u32,
    window_height: u32,
    held_mouse_keys: HashSet<MouseButton>,
    held_keys: HashSet<VirtualKeyCode>,
    held_entry: Option<(NbtElement, bool)>,
    selected_text: Option<SelectedText>
}

pub struct SelectedText { // expires on click, no need to mutate the y value
    x: u32,
    y: u32,
    index: u32,
    cursor: usize, // grapheme index
    value: String, // value we're modifying
    other_value: Option<(std::string::String, bool)>,
    selection: Option<usize>
}

#[repr(u8)]
pub enum KeyResult {
    Nothing,
    Revert,
    Finish
}

impl SelectedText {
    #[inline]
    pub fn new(x: u32, y: u32, index: u32, optional_value: Option<std::string::String>, mut mouse_x: u32, mut other_value: Option<(std::string::String, bool)>) -> Option<SelectedText> {
        let value = if let Some(value) = optional_value {
            value
        } else if let Some((str, true)) = other_value.take() {
            str
        } else {
            return None
        };

        if let Some((str, bool)) = &other_value {
            if *bool { // if this contains the key
                mouse_x += VertexBufferBuilder::width(str) + VertexBufferBuilder::width(": ");
            }
        }

        if x > mouse_x + 4 {
            None
        } else if x > mouse_x {
            Some(SelectedText { x, y, index, cursor: 0, value, other_value, selection: None })
        } else {
            let mut cursor = 0;
            let mut c = 0;
            for grapheme in value.graphemes(true) {
                if c < mouse_x - x {
                    c += VertexBufferBuilder::width(grapheme);
                    cursor += 1;
                }
                if c >= mouse_x - x {
                    return Some(SelectedText { x, y, index, cursor, value, other_value, selection: None })
                }
            }
            None
        }
    }

    #[inline]
    pub fn on_key_press(&mut self, key: VirtualKeyCode, char: Option<char>, ctrl: bool, shift: bool) -> KeyResult {
        if key == VirtualKeyCode::Escape {
            return Revert;
        }
        if key == VirtualKeyCode::Return || key == VirtualKeyCode::NumpadEnter {
            return Finish;
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
            Nothing
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
            Nothing
        }
    }

    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder) {
        let x = self.x;
        let y = if builder.scroll() & !0b111 > self.y { return } else { self.y - (builder.scroll() & !0b111) };
        builder.draw_texture((x + VertexBufferBuilder::width(self.value.split_at(grapheme_to_byte_index(&self.value, self.cursor)).0) , y), (0, 32), (1, 16));
        if let Some((str, bool)) = &self.other_value {
            if *bool { // contains the key
                let mut width = builder.draw_text(x, y, str, true);
                width += builder.draw_text(x + width, y, ": ", true);
                builder.draw_text(x + width, y, &self.value, true);
            } else {
                let mut width = builder.draw_text(x, y, &self.value, true);
                width += builder.draw_text(x + width, y, ": ", true);
                builder.draw_text(x + width, y, str, true);
            }
        } else {
            builder.draw_text(x, y, &self.value, true);
        }
        if let Some(selection) = self.selection {
            let (start, end) = if self.cursor > selection { (selection, self.cursor) } else { (self.cursor, selection) };
            let mut start_x = x + VertexBufferBuilder::width(self.value.split_at(grapheme_to_byte_index(&self.value, start)).0);
            let end_x = x + VertexBufferBuilder::width(self.value.split_at(grapheme_to_byte_index(&self.value, end)).0);
            let mut remaining_width = end_x - start_x;
            while remaining_width > 0 {
                builder.draw_texture((start_x, y), (1, 32), (remaining_width.min(14), 16));
                remaining_width = if remaining_width <= 14 { 0 } else { remaining_width - 14 };
                start_x += 14;
            }
        }
    }
}

#[inline]
pub fn is_jump_point_str(str: &str) -> bool {
    str.contains(' ')
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
            unsafe {
                a.put("a".to_owned(), NbtElement::from_id(1));
                a.put("b".to_owned(), NbtElement::from_id(2));
                a.put("c".to_owned(), NbtElement::from_id(3));
                a.put("d".to_owned(), NbtElement::from_id(4));
                a.put("e".to_owned(), NbtElement::from_id(5));
                a.put("f".to_owned(), NbtElement::from_id(6));
                a.put("g".to_owned(), NbtElement::from_id(7));
                a.put("h".to_owned(), NbtElement::from_id(8));
                a.put("i".to_owned(), NbtElement::from_id(9));
                a.put("j".to_owned(), NbtElement::from_id(10));
                a.put("k".to_owned(), NbtElement::from_id(11));
                a.put("l".to_owned(), NbtElement::from_id(12));
                a.put("m".to_owned(), NbtElement::from_id(1));
                a.put("n".to_owned(), NbtElement::from_id(2));
                a.put("o".to_owned(), NbtElement::from_id(3));
                a.put("p".to_owned(), NbtElement::from_id(4));
                a.put("q".to_owned(), NbtElement::from_id(5));
                a.put("r".to_owned(), NbtElement::from_id(6));
                a.put("s".to_owned(), NbtElement::from_id(7));
                a.put("t".to_owned(), NbtElement::from_id(8));
                a.put("u".to_owned(), NbtElement::from_id(9));
                a.put("v".to_owned(), NbtElement::from_id(10));
                a.put("w".to_owned(), NbtElement::from_id(11));
                a.put("x".to_owned(), NbtElement::from_id(12));
                compound.put("§ection fucking §ign".to_owned(), Compound(a));
                compound.put("b".to_owned(), Compound(NbtCompound::new()));
            }
            workbench.tabs.push_front(FileEntry {
                value: Compound(compound),
                name: "debug.nbt".to_owned(),
                path: None,
                compressed: false,
                history: LinkedList::new(),
                history_changed: false,
                scroll: 0
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
    pub fn forbidden_y(&self, scroll: u32) -> Option<u32> {
        self.selected_text.as_ref().map(|x| x.y).and_then(|x| if scroll > x { None } else { Some(x - scroll) })
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
                if let Some(tab) = self.tab_mut() {
                    tab.set_scroll(value);
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
                if (y - HEADER_SIZE) / 16 == 0 && x > 36 {
                    self.rename(x);
                } else if let Some((x, _)) = self.held_entry.take() {
                    self.drop(x);
                } else if *button == MouseButton::Left {
                    self.left_click()
                }
            }
            if *button == MouseButton::Left {
                self.held_entry = None;
            }
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
        self.selected_text = SelectedText::new(40, HEADER_SIZE + (self.scroll() / 16) * 16, 0, Some(self.tab().expect("tab exists").path.as_ref().and_then(|x| x.to_str().map(|x| x.to_owned())).unwrap_or_else(|| self.tab().expect("tab exists").name.to_owned())), offset, None);
    }

    #[inline]
    fn delete(&mut self) {
        let x = self.mouse_x / 16;
        let y = (self.mouse_y - HEADER_SIZE) / 16;
        let scroll = self.scroll() / 16;
        if let Some(tab) = self.tab_mut() {
            if let Some(Some((element, remaining_y, key, index, depth))) = tab.value.delete(&mut (y + scroll), 1) {
                if depth == x {
                    tab.history.push_back(WorkbenchAction::Remove {
                        y: (y + scroll) - remaining_y,
                        element,
                        key,
                        index
                    })
                }
            }
        }
    }

    #[inline]
    fn drop(&mut self, element: NbtElement) {
        let y = self.mouse_y - HEADER_SIZE;
        let scroll = self.scroll();
        if let Some(tab) = self.tab_mut() {
            if let Err((_, Some((index, parent_y)))) = tab.value.drop(element, &mut (y + scroll + 8), 0) {
                tab.history.push_back(WorkbenchAction::Add {
                    y: parent_y,
                    index
                });
            }
        }
    }

    #[inline]
    fn hold_entry(&mut self, button: &MouseButton) {
        if *button == MouseButton::Left {
            self.held_entry = match self.mouse_x / 16 {
                0 => Some((NbtElement::from_id(1), true)),
                1 => Some((NbtElement::from_id(2), true)),
                2 => Some((NbtElement::from_id(3), true)),
                3 => Some((NbtElement::from_id(4), true)),
                4 => Some((NbtElement::from_id(5), true)),
                5 => Some((NbtElement::from_id(6), true)),
                6 => Some((NbtElement::from_id(7), true)),
                7 => Some((NbtElement::from_id(11), true)),
                8 => Some((NbtElement::from_id(12), true)),
                9 => Some((NbtElement::from_id(8), true)),
                10 => Some((NbtElement::from_id(9), true)),
                11 => Some((NbtElement::from_id(10), true)),
                _ => None
            }
        }
    }

    #[inline]
    fn click_tab(&mut self, button: &MouseButton) {
        let mut x = self.mouse_x - 2;
        for (index, tab) in self.tabs.iter().enumerate() {
            let width = VertexBufferBuilder::width(&tab.name) + 21;
            if x <= width {
                match button {
                    MouseButton::Left => self.tab = index,
                    MouseButton::Middle => {
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
    fn left_click(&mut self) {
        let mouse_x = self.mouse_x;
        let mouse_y = self.mouse_y - HEADER_SIZE;
        let y = mouse_y / 16;
        let scroll = self.scroll() / 16;
        if let Some(tab) = self.tab_mut() {
            match tab.value.left_click(&mut (y + scroll), 0, mouse_x, 0, None) {
                None => {} // no click
                Some(Ok(_)) => {
                    tab.history.push_back(WorkbenchAction::Toggle {
                        y: y + scroll
                    });
                } // height changed
                Some(Err((x, _, index, value, mouse_x, other_value))) => {
                    self.selected_text = SelectedText::new(x, (y + scroll) * 16 + HEADER_SIZE, index, value, mouse_x, other_value.map(|x| (x, true))); // todo, change bool value to fix bugs magically
                }
            }
        }
    }

    #[inline]
    pub fn close_selected_text(&mut self) {
        if let Some(SelectedText { y, index, value, other_value, .. }) = self.selected_text.take() {
            if y == HEADER_SIZE {
                let string = OsString::from(value.clone());
                let tab = self.tab_mut().expect("tab exists");
                tab.name = Path::new(&string).file_name().unwrap_or(&string).to_str().unwrap_or(&value).to_owned();
                tab.path = Some(string);
                self.selected_text = None;
            } else if let Some(tab) = self.tab_mut() {
                let mut first_parent = other_value.as_ref().map(|&(_, x)| x).unwrap_or(false);
                let first_parent_mut = &mut first_parent as *mut bool;
                let mut action = None;
                let action_mut = &mut action as *mut Option<WorkbenchAction>;
                tab.value.stack(&mut ((y - HEADER_SIZE) / 16), &mut 0, 0, &mut |parent, _| if first_parent {
                    unsafe { *first_parent_mut = false; }
                    if let Compound(compound) = parent {
                        unsafe { *action_mut = Some(WorkbenchAction::Rename {
                            y,
                            before: compound.key(index).to_owned(),
                            key: true
                        }) };
                        compound.update_key(index, value.clone());
                    }
                }, &mut |tail: &mut NbtElement, _, _| if !first_parent {
                    if let Some(before) = tail.set_value(&value).filter(|x| x != &value) {
                        unsafe { *action_mut = Some(WorkbenchAction::Rename {
                            y,
                            before,
                            key: false
                        }) };
                    }
                });
                if let Some(action) = action {
                    tab.history.push_back(action);
                }
                tab.history_changed = true;
            }
        }
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
                            match selected_text.on_key_press(key, char, ctrl, self.held_keys.contains(&VirtualKeyCode::LShift) || self.held_keys.contains(&VirtualKeyCode::RShift)) {
                                Nothing => {},
                                Revert => self.selected_text = None,
                                Finish => self.close_selected_text()
                            }
                        } else if ctrl && key == VirtualKeyCode::N {
                            self.close_selected_text();
                            self.tabs.push_back(FileEntry {
                                value: Compound(NbtCompound::new()),
                                name: "new.nbt".to_owned(),
                                path: None,
                                compressed: false,
                                history: LinkedList::new(),
                                history_changed: false,
                                scroll: 0
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
                        } else if (key == VirtualKeyCode::Back || key == VirtualKeyCode::Delete) && self.mouse_y > HEADER_SIZE {
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
        let height = self.window_height;
        self.tab_mut().map(|x| x.scroll(height)).unwrap_or(0)
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
            entry.render(builder, self.forbidden_y(builder.scroll() / 16 * 16));
            if let Some(selected_text) = &self.selected_text {
                selected_text.render(builder);
            }
        }
        self.render_held_entry(builder);
    }

    #[inline]
    fn render_held_entry(&self, builder: &mut VertexBufferBuilder) {
        if let Some((x, _)) = &self.held_entry {
            NbtElement::render_icon(x.id(), self.mouse_x, self.mouse_y, builder);
        }
    }

    #[inline]
    fn render_icons(&self, builder: &mut VertexBufferBuilder) {
        NbtPrimitive::<u8, 0, 0>::render_icon(0, 26, builder);
        NbtPrimitive::<u8, 16, 0>::render_icon(16, 26, builder);
        NbtPrimitive::<u8, 32, 0>::render_icon(32, 26, builder);
        NbtPrimitive::<u8, 48, 0>::render_icon(48, 26, builder);
        NbtPrimitive::<u8, 64, 0>::render_icon(64, 26, builder);
        NbtPrimitive::<u8, 80, 0>::render_icon(80, 26, builder);
        NbtPrimitive::<u8, 96, 0>::render_icon(96, 26, builder);
        NbtPrimitive::<u8, 112, 0>::render_icon(112, 26, builder);
        NbtPrimitive::<u8, 0, 16>::render_icon(128, 26, builder);
        elements::string::render_icon(144, 26, builder);
        elements::list::render_icon(160, 26, builder);
        elements::compound::render_icon(176, 26, builder);

        if self.mouse_x < 192 && self.mouse_y >= 23 && self.mouse_y < 39 {
            builder.draw_texture((self.mouse_x & 0xFFFFFFF0, 26), (0, 32), (16, 16));
        }
    }

    #[inline]
    fn render_tabs(&self, builder: &mut VertexBufferBuilder) {
        let mut offset = 3;
        for (index, tab) in self.tabs.iter().enumerate() {
            let u = if index == self.tab { 32 } else { 16 };
            builder.draw_texture((offset, 3), (u, 32), (1, 16));
            let width = VertexBufferBuilder::width(&tab.name) + 4 + 16;
            let mut remaining_width = width;
            let mut middle_offset = offset;
            while remaining_width > 0 {
                builder.draw_texture((middle_offset, 3), (u + 1, 32), (remaining_width.min(14), 16));
                remaining_width = if remaining_width < 14 { 0 } else { remaining_width - 14 };
                middle_offset += 14
            }
            builder.draw_text(offset + 2, 3, &tab.name, true);
            offset += width;
            builder.draw_texture((offset - 16, 3), (80 + if tab.history_changed && tab.history.iter().any(|x| x.mutation()) { 0 } else { 16 }, 32), (16, 16));
            builder.draw_texture((offset, 3), (u + 15, 32), (1, 16));
            offset += 2;
        }
        for i in 0..=builder.window_width() >> 4 {
            builder.draw_texture((i << 4, 21), (48, 32), (16, 2));
            builder.draw_texture((i << 4, 45), (48, 32), (16, 2));
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
    history_changed: bool,
    scroll: u32
}

impl FileEntry {
    pub fn new(file: &[u8], path: &&PathBuf, compressed: bool) -> Option<FileEntry> {
        Some(FileEntry {
            value: NbtElement::from_file(file)?,
            name: path.file_name()?.to_str()?.to_owned(),
            path: Some(path.as_os_str().to_os_string()),
            compressed,
            history: LinkedList::new(),
            history_changed: false,
            scroll: 0
        })
    }

    pub fn render(&self, builder: &mut VertexBufferBuilder, forbidden_y: Option<u32>) {
        if let Compound(x) = &self.value {
            x.render_root(builder, &self.name, forbidden_y);
            if builder.window_height() >= HEADER_SIZE {
                let height = x.height() * 16;
                let total = builder.window_height() - HEADER_SIZE;
                if height > total {
                    let offset = (total as f64 / (height as f64 / (self.scroll & !15) as f64)) as u32 + HEADER_SIZE; // safe to grab scroll
                    let height = (total * total) / height;
                    if height > 1 {
                        builder.draw_texture((builder.window_width() - 4, offset), (112, 32), (3, 1));
                        let mut remaining = height - 1;
                        while remaining > 0 {
                            builder.draw_texture((builder.window_width() - 4, offset + (height - remaining)), (112, 33), (3, (remaining).min(14)));
                            remaining = if remaining <= 14 { 0 } else { remaining - 14 };
                        }
                        builder.draw_texture((builder.window_width() - 4, offset + height), (112, 47), (3, 1));
                    }
                }
            }
        }
    }

    pub fn scroll(&mut self, window_height: u32) -> u32 {
        let height = self.value.height() * 16;
        if window_height >= HEADER_SIZE {
            let window_height = window_height - HEADER_SIZE;
            let max_scroll = if window_height > height { 0 } else { height - window_height };
            self.scroll = self.scroll.min(max_scroll);
        } else {
            self.scroll = 0;
        }
        self.scroll
    }

    pub fn set_scroll(&mut self, scroll: f32) {
        if scroll.is_sign_negative() && self.scroll < (-scroll * 48.0) as u32 {
            self.scroll = 0;
        } else if scroll.is_sign_negative() {
            self.scroll -= (-scroll * 48.0) as u32;
        } else {
            self.scroll += (scroll * 48.0) as u32;
        }
    }
}

pub mod elements {
    pub mod element_type;
    pub mod string;
    pub mod list;
    pub mod compound;
    pub mod primitive;
    pub mod array;
}
