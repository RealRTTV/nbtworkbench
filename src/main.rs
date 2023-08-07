#![feature(unchecked_math)]
#![feature(let_chains)]
#![feature(core_intrinsics)]
#![feature(stmt_expr_attributes)]
#![feature(slice_first_last_chunk)]
#![feature(prelude_2024)]
#![cfg_attr(all(windows, not(debug_assertions)), windows_subsystem = "windows")]

use std::collections::LinkedList;
use std::fmt::Write;
use std::fs::{read, write};
use std::io::Read;
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;
use std::str::FromStr;
use std::string::String;

use flate2::read::{GzDecoder, ZlibDecoder};
use flate2::read::{GzEncoder, ZlibEncoder};
use fxhash::{FxBuildHasher, FxHashSet};
use winit::dpi::PhysicalPosition;
use winit::event::{ElementState, KeyboardInput, MouseButton, MouseScrollDelta, VirtualKeyCode};

use elements::element_type::NbtElement;
use vertex_buffer_builder::VertexBufferBuilder;

use crate::assets::HEADER_SIZE;
use crate::elements::compound::NbtCompound;
use crate::elements::element_type::{NbtByte, NbtByteArray, NbtDouble, NbtFloat, NbtInt, NbtIntArray, NbtLong, NbtLongArray, NbtShort};
use crate::KeyResult::*;
use crate::window::WINDOW_HEIGHT;
use crate::workbench_action::WorkbenchAction;

mod decoder;
mod encoder;
mod window;
mod assets;
mod vertex_buffer_builder;
mod workbench_action;

#[macro_export]
macro_rules! flags {
    () => { 0b000_u8 };
    (Ctrl) => { 0b001_u8 };
    (Shift) => { 0b010_u8 };
    (Ctrl + Shift) => { 0b011_u8 };
    (Alt) => { 0b100_u8 };
    (Ctrl + Alt) => { 0b101_u8 };
    (Shift + Alt) => { 0b110_u8 };
    (Ctrl + Shift + Alt) => { 0b111_u8 };
}

pub enum DropFn {
    Dropped(usize, usize, bool),
    Missed(Option<Box<str>>, NbtElement),
    InvalidType(Option<Box<str>>, NbtElement),
}

pub type DeleteFn = (Option<Box<str>>, NbtElement, (usize, usize));

pub type ToggleFn = Result<usize, ()>;

pub type StealFn = Result<(Option<Box<str>>, NbtElement, (usize, usize)), ()>;

pub type TrySelectTextFn = (Option<(Box<str>, bool)>, Option<(Box<str>, bool)>);

fn main() {
    pollster::block_on(window::run());
    {
        // todo, ctrl + shift + up/down moves the index around, (i love it)
        // todo, toggle closes **all** child toggles, **recursively**

        // todo, windows icon
        // todo, web assembly ver
        // todo, smart screen
        // todo, 24x24 for bar icons
        // todo, horizontal scrolling
    }
    {
        // todo, ctrl + h, open a playground nbt file to help with user interaction
        // todo, ctrl + r, reload current page / currently hovered entry
        // todo, snbt save and load
        // todo, mca

        // todo, icons for each workbench action, and a 5 most recent actions for undo and redo respectively
    }
}

pub struct NbtWorkbench {
    tabs: Vec<Tab>,
    tab: usize,
    mouse_x: usize,
    mouse_y: usize,
    window_height: usize,
    held_mouse_keys: FxHashSet<MouseButton>,
    held_keys: FxHashSet<VirtualKeyCode>,
    held_entry: HeldEntry,
    selected_text: Option<SelectedText>
}

pub enum HeldEntry {
    Empty,
    FromAether(Result<(Box<str>, NbtElement), NbtElement>),
    FromKnown(Result<(Box<str>, NbtElement), NbtElement>, Box<[usize]>),
}

impl HeldEntry {
    pub fn element(&self) -> Option<&NbtElement> {
        match self {
            HeldEntry::Empty => None,
            HeldEntry::FromAether(element) | HeldEntry::FromKnown(element, _) => Some(element.as_ref().map_or_else(|x| x, |(_, x)| x)),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, HeldEntry::Empty)
    }
}

pub fn sum_indices<I: Iterator<Item=usize>>(indices: I, mut root: &NbtElement) -> Option<usize> {
    let mut total = 0;
    let mut indices = indices.peekable();
    while let Some(idx) = indices.next() {
        root = match root {
            NbtElement::ByteArray(array) => if idx >= array.len() {
                return None
            } else {
                total += 1 + idx;
                break
            },
            NbtElement::List(list) => if idx >= list.len() {
                return None
            } else {
                total += 1;
                for jdx in 0..idx {
                    // SAFETY: n < len (is valid bounds) implies n - m (where m is a positive integer <= n) < len (is valid bounds)
                    total += unsafe { list.get(jdx).unwrap_unchecked() }.height();
                }
                // SAFETY: asserted beforehand
                unsafe { list.get(idx).unwrap_unchecked() }
            },
            NbtElement::Compound(compound) => if idx >= compound.len() {
                return None
            } else {
                total += 1;
                for jdx in 0..idx {
                    // SAFETY: n < len (is valid bounds) implies n - m (where m is a positive integer <= n) < len (is valid bounds)
                    total += unsafe { compound.get(jdx).unwrap_unchecked() }.1.height();
                }
                // SAFETY: asserted beforehand
                unsafe { compound.get(idx).unwrap_unchecked() }.1
            },
            NbtElement::IntArray(array) => if idx >= array.len() {
                return None
            } else {
                total += 1 + idx;
                break
            },
            NbtElement::LongArray(array) => if idx >= array.len() {
                return None
            } else {
                total += 1 + idx;
                break
            },
            x => {
                total += x.height();
                if indices.peek().is_some() {
                    return None
                } else {
                    break
                }
            }
        }
    }
    Some(total)
}

#[derive(Clone)]
pub struct SelectedText { // expires on click, no need to mutate the y value
x: usize,
    y: usize,
    indices: Box<[usize]>,
    cursor: usize, // byte index
    value: String, // value we're modifying
    selection: Option<usize>,
    keyfix: Option<String>,
    prefix: String,
    suffix: String,
    valuefix: Option<String>,
}

#[repr(u8)]
pub enum KeyResult {
    Failed,
    NothingSpecial,
    Revert,
    Finish,
    Keyfix,
    Valuefix,
    Up,
    Down,
}

impl SelectedText {
    #[inline]
    pub fn new(target_x: usize, mouse_x: usize, y: usize, key: Option<(Box<str>, bool)>, value: Option<(Box<str>, bool)>, indices: Vec<usize>) -> Option<SelectedText> {
        let key_width = if let Some((key, true)) = key.clone() {
            let key_width = VertexBufferBuilder::width(key.as_ref());

            if mouse_x + 4 >= target_x {
                let (suffix, valuefix) = if let Some((v, b)) = &value {
                    if *b {
                        (": ".to_owned(), Some(v.clone().into_string()))
                    } else {
                        (format!(": {v}"), None)
                    }
                } else {
                    (String::new(), None)
                };

                if mouse_x <= target_x {
                    return Some(SelectedText {
                        x: target_x,
                        y,
                        indices: indices.into_boxed_slice(),
                        cursor: 0,
                        value: key.into_string(),
                        selection: None,
                        keyfix: None,
                        prefix: String::new(),
                        suffix,
                        valuefix,
                    })
                }

                if target_x + key_width < mouse_x + key.chars().last().map(|char| VertexBufferBuilder::furthest_pixel(char as u16)).unwrap_or(0) && mouse_x < target_x + key_width + 7 {
                    return Some(SelectedText {
                        x: target_x,
                        y,
                        indices: indices.into_boxed_slice(),
                        cursor: key.len(),
                        value: key.into_string(),
                        selection: None,
                        keyfix: None,
                        prefix: String::new(),
                        suffix,
                        valuefix,
                    })
                }

                let mut cursor = 0;
                let mut x = (mouse_x - target_x) as isize;
                let key_width = key_width as isize;

                for char in key.chars() {
                    let width = VertexBufferBuilder::furthest_pixel(char as u16) as isize;
                    if x * 2 >= width { // algebra, to understand, divide both sides by two
                        cursor += char.len_utf8();
                        x -= width;
                    } else if x < key_width {
                        return Some(SelectedText {
                            x: target_x,
                            y,
                            indices: indices.into_boxed_slice(),
                            cursor,
                            value: key.into_string(),
                            selection: None,
                            keyfix: None,
                            prefix: String::new(),
                            suffix,
                            valuefix,
                        })
                    }
                }
            }
            key_width + VertexBufferBuilder::width(": ")
        } else {
            0
        };

        if let Some((value, true)) = value {
            let value_x = target_x + key_width;
            if mouse_x + 4 >= value_x {
                let (keyfix, prefix) = if let Some((k, b)) = &key {
                    if *b {
                        (Some(k.clone().into_string()), ": ".to_owned())
                    } else {
                        (None, format!(": {k}"))
                    }
                } else {
                    (None, String::new())
                };

                if mouse_x <= value_x {
                    return Some(SelectedText {
                        x: value_x - key_width,
                        y,
                        indices: indices.into_boxed_slice(),
                        cursor: 0,
                        value: value.into_string(),
                        selection: None,
                        keyfix,
                        prefix,
                        suffix: String::new(),
                        valuefix: None,
                    })
                }

                let value_width = VertexBufferBuilder::width(value.as_ref());

                if value_x + value_width < mouse_x + value.chars().last().map(|char| VertexBufferBuilder::furthest_pixel(char as u16)).unwrap_or(0) && mouse_x < value_x + value_width + 5 {
                    return Some(SelectedText {
                        x: value_x - key_width,
                        y,
                        indices: indices.into_boxed_slice(),
                        cursor: value.len(),
                        value: value.into_string(),
                        selection: None,
                        keyfix,
                        prefix,
                        suffix: String::new(),
                        valuefix: None,
                    });
                }

                let value_width = value_width as isize;
                let mut x = (mouse_x - value_x) as isize;
                let mut cursor = 0;

                for char in value.chars() {
                    let width = VertexBufferBuilder::furthest_pixel(char as u16) as isize;
                    if x * 2 >= width { // algebra, to understand, divide both sides by two
                        cursor += char.len_utf8();
                        x -= width;
                    } else if x < value_width {
                        return Some(SelectedText {
                            x: value_x - key_width,
                            y,
                            indices: indices.into_boxed_slice(),
                            cursor,
                            value: value.into_string(),
                            selection: None,
                            keyfix,
                            prefix,
                            suffix: String::new(),
                            valuefix: None,
                        })
                    }
                }
            }
        }

        None
    }

    #[inline]
    pub fn on_key_press(&mut self, key: VirtualKeyCode, mut char: Option<char>, flags: u8) -> KeyResult {
        if key == VirtualKeyCode::Escape && flags == flags!() {
            return Revert;
        }
        if let VirtualKeyCode::Return | VirtualKeyCode::NumpadEnter = key && flags == flags!() {
            return Finish;
        }

        if key == VirtualKeyCode::A && flags == flags!(Ctrl) {
            self.cursor = 0;
            self.selection = Some(self.value.len());
            return NothingSpecial
        }

        if let Some(selection) = self.selection {
            if let VirtualKeyCode::Back | VirtualKeyCode::Delete = key {
                let (low_selection, high_selection) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
                let (left, right) = self.value.split_at(low_selection);
                let (_, right) = right.split_at(high_selection - low_selection);
                self.value = format!("{left}{right}");
                self.selection = None;
                self.cursor = low_selection;
                return NothingSpecial
            }
        }

        if key == VirtualKeyCode::X && flags == flags!(Ctrl) {
            if let Some(selection) = self.selection {
                let (start, end) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
                let (low, right) = self.value.split_at(start);
                let (cut, high) = right.split_at(end - start);
                if cli_clipboard::set_contents(cut.to_owned()).is_ok() {
                    self.value = format!("{low}{high}");
                    self.selection = None;
                }
                return NothingSpecial
            }
        }

        if key == VirtualKeyCode::C && flags == flags!(Ctrl) {
            if let Some(selection) = self.selection {
                let (start, end) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
                let (_, right) = self.value.split_at(start);
                let (cut, _) = right.split_at(end - start);
                let _ = cli_clipboard::set_contents(cut.to_owned()).is_ok();
                return NothingSpecial
            }
        }

        if key == VirtualKeyCode::V && flags == flags!(Ctrl) {
            if let Ok(clipboard) = cli_clipboard::get_contents() {
                if let Some(selection) = self.selection.take() {
                    let (start, end) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
                    let (left, right) = self.value.split_at(start);
                    let (_, right) = right.split_at(end - start);
                    self.value = format!("{left}{clipboard}{right}");
                    self.cursor = end + clipboard.len();
                    return NothingSpecial
                } else {
                    let (left, right) = self.value.split_at(self.cursor);
                    self.value = format!("{left}{clipboard}{right}");
                    self.cursor += clipboard.len();
                    return NothingSpecial
                }
            }
        }

        if key == VirtualKeyCode::Back && flags < 2 {
            if flags & flags!(Ctrl) > 0 {
                self.value = self.value.split_off(self.cursor);
                self.cursor = 0;
                return NothingSpecial
            } else {
                let (left, right) = self.value.split_at(self.cursor);

                if !left.is_empty() {
                    let mut end = left.len() - 1;
                    while end > 0 {
                        if is_utf8_char_boundary(left.as_bytes()[end]) {
                            break
                        }
                        end -= 1;
                    }
                    let (left, _) = left.split_at(end);
                    self.cursor = left.len();
                    self.value = format!("{left}{right}");
                }

                return NothingSpecial
            }
        }

        if key == VirtualKeyCode::Delete {
            return if flags & flags!(Ctrl) > 0 {
                self.value.truncate(self.cursor);
                self.cursor = self.value.len();
                NothingSpecial
            } else {
                let (left, right) = self.value.split_at(self.cursor);

                if !right.is_empty() {
                    let mut start = 1;
                    while start < right.len() {
                        if is_utf8_char_boundary(right.as_bytes()[start]) {
                            break
                        }
                        start += 1;
                    }
                    let (_, right) = right.split_at(start);
                    self.cursor = left.len();
                    self.value = format!("{left}{right}");
                }

                NothingSpecial
            }
        }

        if key == VirtualKeyCode::Up && flags == flags!() {
            return Up
        }

        if key == VirtualKeyCode::Down && flags == flags!() {
            return Down
        }

        if key == VirtualKeyCode::Left {
            if flags & flags!(Shift) == 0 && self.selection.is_none() && self.cursor == 0 && self.keyfix.is_some() {
                return Keyfix
            }

            if flags & flags!(Ctrl) > 0 {
                if flags & flags!(Shift) > 0 {
                    self.selection = Some(0);
                } else {
                    self.cursor = 0;
                    self.selection = None;
                }
            } else {
                let mut new = if let Some(selection) = self.selection && flags & flags!(Shift) > 0 { selection } else { self.cursor };
                if new > 0 {
                    new -= 1;
                    while new > 0 {
                        if is_utf8_char_boundary(self.value.as_bytes()[new]) {
                            break
                        }

                        new -= 1;
                    }
                }

                if flags & flags!(Shift) > 0 {
                    self.selection = Some(new);
                } else {
                    if let Some(selection) = self.selection.take() {
                        self.cursor = self.cursor.min(selection);
                    } else {
                        self.cursor = new;
                    }
                }
            }

            if self.selection.is_some_and(|x| x == self.cursor) {
                self.selection = None;
            }

            return NothingSpecial
        }

        if key == VirtualKeyCode::Right {
            if flags & flags!(Shift) == 0 && self.selection.is_none() && self.cursor == self.value.len() && self.valuefix.is_some() {
                return Valuefix
            }

            if flags & flags!(Ctrl) > 0 {
                if flags & flags!(Shift) > 0 {
                    self.selection = Some(self.value.len());
                } else {
                    self.cursor = self.value.len();
                    self.selection = None;
                }
            } else {
                let mut new = if let Some(selection) = self.selection && flags == flags!(Shift) { selection } else { self.cursor };
                if new < self.value.len() {
                    new += 1;
                    while new < self.value.len() {
                        if is_utf8_char_boundary(self.value.as_bytes()[new]) {
                            break
                        }

                        new += 1;
                    }
                }

                if flags & flags!(Shift) > 0 {
                    self.selection = Some(new);
                } else {
                    if let Some(selection) = self.selection.take() {
                        self.cursor = self.cursor.max(selection);
                    } else {
                        self.cursor = new;
                    }
                }
            }

            if self.selection.is_some_and(|x| x == self.cursor) {
                self.selection = None;
            }

            return NothingSpecial
        }

        if let VirtualKeyCode::Return |VirtualKeyCode::NumpadEnter = key && flags == flags!(Ctrl) {
            char = Some('\n')
        }

        if let Some(char) = char {
            if let Some(selection) = self.selection {
                let (low_selection, high_selection) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
                let (left, right) = self.value.split_at(low_selection);
                let (_, right) = right.split_at(high_selection - low_selection);
                self.value = format!("{left}{char}{right}");
                self.selection = None;
                self.cursor = low_selection + char.len_utf8();
            } else {
                let (left, right) = self.value.split_at(self.cursor);
                self.value = format!("{left}{char}{right}");
                self.cursor += char.len_utf8();
            }

            return NothingSpecial
        }

        Failed
    }

    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder) {
        let x = self.x;
        let y = if builder.scroll() > self.y { return } else { self.y - builder.scroll() };

        if y < HEADER_SIZE {
            return
        }

        let prefix_width = VertexBufferBuilder::width(self.prefix.as_str()) + self.keyfix.as_ref().map(|x| VertexBufferBuilder::width(&x)).unwrap_or(0);

        builder.draw_texture((x + VertexBufferBuilder::width(self.value.split_at(self.cursor).0) + prefix_width, y), (0, 32), (1, 16));

        builder.settings(x, y, false);
        let _ = write!(builder, "{}{}{}{}{}", self.keyfix.as_ref().unwrap_or(&String::new()), self.prefix, self.value, self.suffix, self.valuefix.as_ref().unwrap_or(&String::new()));

        if let Some(selection) = self.selection {
            let (start, end) = if self.cursor > selection { (selection, self.cursor) } else { (self.cursor, selection) };
            let mut start_x = x + VertexBufferBuilder::width(self.value.split_at(start).0);
            let end_x = x + VertexBufferBuilder::width(self.value.split_at(end).0);
            let mut remaining_width = end_x - start_x;
            while remaining_width > 0 {
                builder.draw_texture((start_x + prefix_width, y), (1, 32), (remaining_width.min(14), 16));
                remaining_width = if remaining_width <= 14 { 0 } else { remaining_width - 14 };
                start_x += 14;
            }
        }
    }
}

#[inline]
pub fn is_utf8_char_boundary(x: u8) -> bool {
    (x as i8) >= -0x40
}

#[derive(Copy, Clone)]
pub enum FileFormat {
    Nbt = 0,
    Gzip = 1,
    Zlib = 2,
    Snbt = 3,
}

impl FileFormat {
    pub fn cycle(self) -> Self {
        match self {
            Self::Nbt => Self::Gzip,
            Self::Gzip => Self::Zlib,
            Self::Zlib => Self::Snbt,
            Self::Snbt => Self::Nbt,
        }
    }

    pub fn encode(self, data: &NbtElement) -> Vec<u8> {
        let mut buf = vec![];
        match self {
            FileFormat::Nbt => data.to_file().unwrap_or(vec![]),
            FileFormat::Gzip => { let data = data.to_file().unwrap_or(vec![]); let _ = GzEncoder::new(&*data, flate2::Compression::best()).read_to_end(&mut buf); buf },
            FileFormat::Zlib => { let data = data.to_file().unwrap_or(vec![]); let _ = ZlibEncoder::new(&*data, flate2::Compression::best()).read_to_end(&mut buf); buf },
            FileFormat::Snbt => data.to_string().into_bytes(),
        }
    }
}

impl Default for NbtWorkbench {
    fn default() -> Self {
        let mut workbench = NbtWorkbench {
            tabs: vec![],
            tab: 0,
            mouse_x: 0,
            mouse_y: 0,
            window_height: WINDOW_HEIGHT,
            held_mouse_keys: FxHashSet::with_hasher(FxBuildHasher::default()),
            held_keys: FxHashSet::with_hasher(FxBuildHasher::default()),
            held_entry: HeldEntry::Empty,
            selected_text: None
        };
        if let Some(x) = &std::env::args().nth(1).and_then(|x| PathBuf::from_str(&x).ok()) {
            workbench.on_open_file(x);
        } else {
            workbench.tabs.insert(0, Tab {
                #[cfg(debug_assertions)]
                value: Box::new(NbtElement::from_file(include_bytes!("assets/test.nbt")).unwrap()),
                #[cfg(debug_assertions)]
                name: "debug.nbt".into(),
                #[cfg(not(debug_assertions))]
                value: Box::new(NbtElement::Compound(NbtCompound::new())),
                #[cfg(not(debug_assertions))]
                name: "new.nbt".into(),
                path: None,
                compression: FileFormat::Nbt,
                undos: LinkedList::new(),
                redos: LinkedList::new(),
                history_changed: false,
                scroll: 0,
                window_height: WINDOW_HEIGHT,
            });
        }
        workbench
    }
}

impl NbtWorkbench {
    #[inline]
    pub fn new() -> Self {
        Default::default()
    }

    #[inline]
    pub fn on_open_file(&mut self, path: &PathBuf) -> Option<()> {
        if let Ok(t) = read(path) {
            let (nbt, compressed) = {
                if let Some(0x1F8B) = t.first_chunk::<2>().cloned().map(u16::from_be_bytes) {
                    let mut decoder = GzDecoder::new(t.as_slice());
                    let mut file = vec![];
                    let _ = decoder.read_to_end(&mut file);
                    (NbtElement::from_file(&file)?, FileFormat::Gzip)
                } else if let Some(0x7801 | 0x789C | 0x78DA) = t.first_chunk::<2>().cloned().map(u16::from_be_bytes) {
                    let mut decoder = ZlibDecoder::new(t.as_slice());
                    let mut file = vec![];
                    let _ = decoder.read_to_end(&mut file);
                    (NbtElement::from_file(&file)?, FileFormat::Gzip)
                } else if let Some(b'{') = t.first().cloned() {
                    (match NbtElement::from_str(core::str::from_utf8(&t).ok()?)? { Ok((_, x)) | Err(x) => x, }, FileFormat::Snbt)
                } else {
                    (NbtElement::from_file(&t)?, FileFormat::Nbt)
                }
            };
            match Tab::new(nbt, &path, compressed, self.window_height) {
                None => None,
                Some(entry) => {
                    self.tabs.push(entry);
                    self.tab = self.tabs.len() - 1;
                    Some(())
                }
            }
        } else {
            None
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
                'a: {
                    match core::mem::replace(&mut self.held_entry, HeldEntry::Empty) {
                        HeldEntry::Empty => {}
                        HeldEntry::FromAether(x) => {
                            self.drop(x, None);
                            break 'a
                        }
                        HeldEntry::FromKnown(x, indices) => {
                            self.drop(x, Some(indices));
                            break 'a
                        }
                    }

                    if (y - HEADER_SIZE) < 16 && x > 32 + self.left_margin() {
                        if self.rename(x) {
                            break 'a
                        }
                    }

                    if let MouseButton::Left | MouseButton::Right = button {
                        if self.toggle() {
                            break 'a
                        }
                        if self.try_select_text() {
                            break 'a
                        }
                    }
                }
            }
        } else {
            self.close_selected_text();
            self.held_mouse_keys.insert(*button);
            'a: {
                if self.held_entry.is_empty() && self.mouse_y >= 23 && self.mouse_y < 39 {
                    self.hold_entry(button);
                    break 'a
                }
                if self.held_entry.is_empty() && self.mouse_y >= HEADER_SIZE + 16 && self.mouse_x >= self.left_margin() + 16 {
                    self.steal();
                    break 'a
                }
            }
        }
        true
    }

    #[inline]
    fn steal(&mut self) {
        let mut y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
        let target_depth = (self.mouse_x - self.left_margin() - 16) / 16;
        if let Some(tab) = self.tab_mut() {
            let mut indices = vec![];
            if let Ok((key, value, _)) = tab.value.steal(&mut y, 0, target_depth, &mut indices) {
                self.held_entry = HeldEntry::FromKnown(match key {
                    Some(key) => Ok((key, value)),
                    None => Err(value),
                }, indices.into_boxed_slice());
            }
        }
    }

    #[inline]
    fn rename(&mut self, offset: usize) -> bool {
        if let Some(tab) = self.tab() {
            let name: Box<str> = tab.path.as_ref().and_then(|x| x.to_str()).map(Into::into).unwrap_or_else(|| tab.name.clone());
            self.selected_text = SelectedText::new(36 + self.left_margin(), offset, HEADER_SIZE, Some((name, true)), None, vec![]);
            self.selected_text.is_some()
        } else {
            false
        }
    }

    #[inline]
    fn duplicate(&mut self) -> bool {
        if self.mouse_y > HEADER_SIZE { return false }

        let mut y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
        if let Some(tab) = self.tab_mut() {
            let before = tab.value.true_height();
            let mut indices = vec![];
            unsafe { tab.value.duplicate(&mut y, &mut indices) };
            tab.value.true_height() != before
        } else {
            false
        }
    }

    #[inline]
    fn copy(&self) -> bool {
        if self.mouse_y < HEADER_SIZE { return false }
        let mut y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
        if let Some(tab) = self.tab() {
            let before = cli_clipboard::get_contents().ok();
            tab.value.copy(&mut y, None);
            let after = cli_clipboard::get_contents().ok();
            before != after
        } else {
            false
        }
    }

    #[inline]
    fn delete(&mut self, clipboard: bool) -> bool {
        if self.mouse_y < HEADER_SIZE { return false };

        let mut y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
        if let Some(tab) = self.tab_mut() && y > 0 && y < tab.value.height() {
            let mut indices = vec![];
            let (key, value, _) = unsafe { tab.value.delete(&mut y, &mut indices) };
            if clipboard {
                // well shit, lets hope it don't error, i guess todo
                let _ = cli_clipboard::set_contents(format!("{}{value}", key.as_ref().map(|x| x.to_string() + ":").unwrap_or(String::new())));
            }
            tab.undos.push_back(WorkbenchAction::Remove {
                element: match key {
                    Some(x) => Ok((x, value)),
                    None => Err(value),
                },
                indices: indices.into_boxed_slice(),
            });
            true
        } else {
            false
        }
    }

    #[inline]
    fn drop(&mut self, element: Result<(Box<str>, NbtElement), NbtElement>, from_indices: Option<Box<[usize]>>) -> bool {
        let left_margin = self.left_margin();
        if self.mouse_y <= HEADER_SIZE || self.mouse_x < left_margin { return false }

        let y = self.mouse_y - HEADER_SIZE + self.scroll();
        let x = (self.mouse_x - left_margin) / 16;
        if let Some(tab) = self.tab_mut() {
            let mut indices = vec![];

            let (key, element) = element.map(|(x, y)| (Some(x), y)).unwrap_or_else(|element| (None, element));

            match NbtElement::drop(tab.value.as_mut(), key.clone(), element, &mut y.clone(), 2, x, &mut indices) {
                DropFn::InvalidType(key, element) | DropFn::Missed(key, element) => {
                    if let Some(from_indices) = from_indices {
                        tab.undos.push_back(WorkbenchAction::Remove { indices: from_indices, element: match key { Some(key) => Ok((key, element)), None => Err(element), } });
                        tab.history_changed = true;
                    }
                }
                DropFn::Dropped(_, _, opened) => {
                    if let Some(from_indices) = from_indices {
                        tab.undos.push_back(WorkbenchAction::Move { from: from_indices, to: indices.clone().into_boxed_slice(), original_key: key });
                        tab.history_changed = true;
                    } else {
                        tab.undos.push_back(WorkbenchAction::Add { indices: indices.clone().into_boxed_slice() });
                        tab.history_changed = true;
                    }

                    if opened {
                        // let _ = indices.pop();
                        // tab.undos.push_back(WorkbenchAction::Toggle { indices: indices.into_boxed_slice() });
                        // tab.history_changed = true;
                    }
                }
            }
            true
        } else {
            false
        }
    }

    #[inline]
    fn hold_entry(&mut self, button: &MouseButton) {
        if *button == MouseButton::Left {
            if self.mouse_x & !0b1111 == 192 && let Some(element) = cli_clipboard::get_contents().ok().and_then(|x| NbtElement::from_str(&x)) {
                self.held_entry = HeldEntry::FromAether(element);
            } else {
                self.held_entry = HeldEntry::FromAether(Err(NbtElement::from_id(match self.mouse_x / 16 {
                    0 => 1,
                    1 => 2,
                    2 => 3,
                    3 => 4,
                    4 => 5,
                    5 => 6,
                    6 => 7,
                    7 => 11,
                    8 => 12,
                    9 => 8,
                    10 => 9,
                    11 => 10,
                    _ => return
                })))
            }
        }
    }

    #[inline]
    fn click_tab(&mut self, button: &MouseButton) {
        if self.mouse_x < 2 { return }

        let mut x = self.mouse_x - 2;
        for (idx, tab) in self.tabs.iter_mut().enumerate() {
            let width = VertexBufferBuilder::width(&tab.name) + 37;

            if x <= width {
                if idx == self.tab && x >= width - 16 - 1 && x <= width - 1 {
                    tab.compression = tab.compression.cycle();
                } else if idx == self.tab && x >= width - 32 - 1 && x <= width - 16 - 1 {
                    tab.save();
                } else {
                    match button {
                        MouseButton::Left => self.tab = idx,
                        MouseButton::Middle => {
                            self.tabs.remove(idx);
                            self.tab = if idx == 0 { 0 } else { idx - 1 };
                        },
                        _ => {}
                    }
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
    fn left_margin(&self) -> usize {
        self.tab().map(|x| (x.value.true_height().ilog10() as usize + 1) * 8 + 8).unwrap_or(4)
    }

    #[inline]
    fn toggle(&mut self) -> bool {
        let left_margin = self.left_margin();
        if self.mouse_x < left_margin { return false }
        if self.mouse_y < HEADER_SIZE { return false }

        let x = (self.mouse_x - left_margin) / 16;
        let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
        let mut indices = vec![];
        if let Some(tab) = self.tab_mut() {
            if y >= tab.value.height() { return false }
            match tab.value.toggle_fn(&mut y.clone(), x, 0, &mut indices) {
                Ok(_) => {
                    // tab.undos.push_back(WorkbenchAction::Toggle { indices: indices.into_boxed_slice() });
                    tab.scroll = tab.scroll();
                    true
                }
                Err(()) => false
            }
        } else {
            false
        }
    }

    #[inline]
    fn try_select_text(&mut self) -> bool {
        let left_margin = self.left_margin();
        if self.mouse_x < left_margin { return false }
        if self.mouse_y < HEADER_SIZE + 16 { return false }

        let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
        let mut indices = vec![];
        if let Some(tab) = self.tab_mut() {
            if y >= tab.value.height() { return false }
            // SAFETY: upheld at self.mouse_y guard clause
            let (key, value) = unsafe { tab.value.try_select_text(&mut y.clone(), &mut indices) };
            self.selected_text = SelectedText::new(indices.len() * 16 + 32 + 4 + left_margin, self.mouse_x, y * 16 + HEADER_SIZE, key, value, indices);
            self.selected_text.is_some()
        } else {
            false
        }
    }

    #[inline]
    pub fn keyfix(&mut self) {
        if let Some(SelectedText { x, y, indices, cursor, value, selection, keyfix, prefix, suffix, valuefix }) = self.selected_text.clone() && let Some(keyfix) = keyfix && valuefix.is_none() && suffix.is_empty() && cursor == 0 {
            self.close_selected_text(); // we'll still render the new one, even if its invalid text. cool right?
            self.selected_text = Some(SelectedText {
                x,
                y,
                indices,
                cursor: keyfix.len(),
                selection,
                keyfix: None,
                prefix: String::new(),
                suffix: prefix,
                valuefix: Some(value),
                value: keyfix,
            });
        }
    }

    #[inline]
    pub fn valuefix(&mut self) {
        if let Some(SelectedText { x, y, indices, cursor, value, selection, keyfix, prefix, suffix, valuefix }) = self.selected_text.clone() && let Some(valuefix) = valuefix && keyfix.is_none() && prefix.is_empty() && cursor == value.len() {
            self.close_selected_text(); // we'll still render the new one, even if its invalid text. cool right?
            self.selected_text = Some(SelectedText {
                x,
                y,
                indices,
                cursor: 0,
                selection,
                keyfix: Some(value),
                prefix: suffix,
                suffix: String::new(),
                valuefix: None,
                value: valuefix,
            });
        }
    }

    #[inline]
    pub fn selected_text_up(&mut self) {
        let left_margin = self.left_margin();
        if let Some(SelectedText { y, indices, cursor, keyfix, prefix, value: str_value, .. }) = self.selected_text.clone() {
            if indices.is_empty() { return }

            self.close_selected_text();
            if let Some(tab) = self.tab_mut() {
                let old_depth = indices.len();
                let mouse_x = old_depth * 16 + 32 + 4 + left_margin + keyfix.as_ref().map(|x| VertexBufferBuilder::width(x)).unwrap_or(0) + VertexBufferBuilder::width(&prefix) + VertexBufferBuilder::width(str_value.split_at(cursor).0);

                if y == HEADER_SIZE + 16 {
                    let width = VertexBufferBuilder::width(tab.path.as_ref().and_then(|x| x.to_str()).map(Into::into).unwrap_or_else(|| tab.name.clone()).as_ref());
                    self.rename(mouse_x.min(width + 32 + 4 + self.left_margin()));
                    return
                }

                let mut total = sum_indices(indices.iter().copied(), tab.value.deref()).unwrap();
                total -= 1; // move up
                let mut indices = vec![];
                // SAFETY: total is -1'd meaning that it's original range of 1..root.height() is now 0..root.height() - 1, which is in range
                let (k, v) = unsafe { tab.value.try_select_text(&mut total, &mut indices) };
                let low = indices.len() * 16 + 32 + 4 + left_margin;
                let high = low + k.as_ref().map(|(x, _)| VertexBufferBuilder::width(x.as_ref()) + VertexBufferBuilder::width(": ")).unwrap_or(0) + v.as_ref().map(|(x, _)| VertexBufferBuilder::width(x)).unwrap_or(0);
                self.selected_text = SelectedText::new(
                    low,
                    mouse_x.clamp(low, high),
                    y - 16,
                    k,
                    v,
                    indices
                );
            }
        }
    }

    #[inline]
    pub fn selected_text_down(&mut self) {
        let left_margin = self.left_margin();
        if let Some(SelectedText { y, indices, cursor, keyfix, prefix, value: str_value, .. }) = self.selected_text.clone() {
            let mut total = if let Some(tab) = self.tab() {
                let total = sum_indices(indices.iter().copied(), tab.value.deref()).unwrap();
                if total + 1 >= tab.value.height() { return }
                total
            } else { return };
            self.close_selected_text();
            if let Some(tab) = self.tab_mut() {
                total += 1; // move down
                let old_depth = indices.len();
                let mut indices = vec![];
                // SAFETY: upheld above with the `return` check
                let (k, v) = unsafe { tab.value.try_select_text(&mut total, &mut indices) };
                let low = indices.len() * 16 + 32 + 4 + left_margin;
                let high = low + k.as_ref().map(|(x, _)| VertexBufferBuilder::width(x.as_ref()) + VertexBufferBuilder::width(": ")).unwrap_or(0) + v.as_ref().map(|(x, _)| VertexBufferBuilder::width(x)).unwrap_or(0);
                let mouse_x = old_depth * 16 + 32 + 4 + left_margin + keyfix.as_ref().map(|x| VertexBufferBuilder::width(x)).unwrap_or(0) + VertexBufferBuilder::width(&prefix) + VertexBufferBuilder::width(str_value.split_at(cursor).0);
                self.selected_text = SelectedText::new(
                    low,
                    mouse_x.clamp(low, high),
                    y + 16,
                    k,
                    v,
                    indices
                );
            }
        }
    }

    #[inline]
    pub fn close_selected_text(&mut self) {
        if let Some(SelectedText { indices, value, prefix, suffix, .. }) = self.selected_text.take() {
            if let Some(tab) = self.tab_mut() {
                if indices.is_empty() {
                    let buf = PathBuf::from(value);
                    if let Some(name) = buf.file_name().and_then(|x| x.to_str()) {
                        tab.name = name.into();
                        tab.path = Some(buf);
                    }
                } else {
                    let value = value.into_boxed_str();
                    let key = prefix.is_empty() && !suffix.is_empty();
                    let mut element = tab.value.deref_mut();
                    let end = indices.len() - key as usize;
                    let previous = 'a: {
                        for (indices_idx, &idx) in indices.iter().enumerate() {
                            if indices_idx == end {
                                break
                            }

                            element = match element {
                                NbtElement::List(list) => list.get_mut(idx),
                                NbtElement::Compound(compound) => compound.get_mut(idx).map(|(_, x)| x),
                                NbtElement::ByteArray(array) => {
                                    let Ok(parse) = value.as_ref().parse() else { break 'a None };
                                    break 'a Some(match array.get_mut(idx) {
                                        Some(element) => core::mem::replace(element, parse),
                                        None => break 'a None
                                    }.to_string().into())
                                },
                                NbtElement::IntArray(array) => {
                                    let Ok(parse) = value.as_ref().parse() else { break 'a None };
                                    break 'a Some(match array.get_mut(idx) {
                                        Some(element) => core::mem::replace(element, parse),
                                        None => break 'a None
                                    }.to_string().into())
                                },
                                NbtElement::LongArray(array) => {
                                    let Ok(parse) = value.as_ref().parse() else { break 'a None };
                                    break 'a Some(match array.get_mut(idx) {
                                        Some(element) => core::mem::replace(element, parse),
                                        None => break 'a None
                                    }.to_string().into())
                                },
                                _ => panic!("Unsupported element type for indices chain")
                            }.expect("Valid index in indices chain");
                        }

                        if key {
                            match element {
                                NbtElement::Compound(compound) => compound.update_key(*indices.last().unwrap(), value),
                                _ => panic!("Key was given for non-compound element")
                            }
                        } else {
                            element.set_value(value)
                        }
                    };

                    let Some(previous) = previous else { return };

                    tab.undos.push_back(WorkbenchAction::Rename {
                        indices,
                        key,
                        previous,
                    });

                    tab.history_changed = true;
                }
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
                    let flags = (self.held_keys.contains(&VirtualKeyCode::LControl) as u8 | self.held_keys.contains(&VirtualKeyCode::RControl) as u8) | ((self.held_keys.contains(&VirtualKeyCode::LShift) as u8 | self.held_keys.contains(&VirtualKeyCode::RShift) as u8) << 1) | ((self.held_keys.contains(&VirtualKeyCode::LAlt) as u8 | self.held_keys.contains(&VirtualKeyCode::RAlt) as u8) << 2);
                    if let Some(selected_text) = &mut self.selected_text {
                        match selected_text.on_key_press(key, char, flags) {
                            NothingSpecial => return true,
                            Revert => { self.selected_text = None; return true },
                            Finish => { self.close_selected_text(); return true },
                            Keyfix => { self.keyfix(); return true },
                            Valuefix => { self.valuefix(); return true },
                            Up => {
                                self.selected_text_up();
                                return true
                            },
                            Down => {
                                self.selected_text_down();
                                return true
                            }
                            Failed => {}, // next thing pls
                        }
                    }
                    if !self.held_entry.is_empty() && key == VirtualKeyCode::Escape && flags == flags!() {
                        self.held_entry = HeldEntry::Empty;
                        return true
                    }
                    if key == VirtualKeyCode::N && flags == flags!(Ctrl) {
                        self.tabs.push(Tab {
                            value: Box::new(NbtElement::Compound(NbtCompound::new())),
                            name: "new.nbt".into(),
                            path: None,
                            compression: FileFormat::Nbt,
                            undos: LinkedList::new(),
                            redos: LinkedList::new(),
                            history_changed: false,
                            scroll: 0,
                            window_height: WINDOW_HEIGHT,
                        });
                        self.tab = self.tabs.len() - 1;
                        self.selected_text = None;
                        return true
                    }
                    if key == VirtualKeyCode::S && flags == flags!(Ctrl) {
                        if let Some(tab) = self.tab_mut() {
                            if tab.save() {
                                self.selected_text = None;
                                return true
                            }
                        }
                    }
                    if key == VirtualKeyCode::W && flags == flags!(Ctrl) {
                        if let Some(tab) = self.tab() && !tab.history_changed {
                            self.tabs.remove(self.tab);
                            if self.tab >= self.tabs.len() && self.tab > 0 {
                                self.tab -= 1;
                            }
                            self.selected_text = None;
                            return true
                        }
                    }
                    if key == VirtualKeyCode::Z && flags == flags!(Ctrl) {
                        if let Some(tab) = self.tab_mut() {
                            if let Some(action) = tab.undos.pop_back() {
                                tab.redos.push_back(action.undo(&mut tab.value));
                                self.selected_text = None;
                                return true
                            }
                        }
                    }
                    if key == VirtualKeyCode::Y && flags == flags!(Ctrl) {
                        if let Some(tab) = self.tab_mut() {
                            if let Some(action) = tab.redos.pop_back() {
                                tab.undos.push_back(action.undo(&mut tab.value));
                                self.selected_text = None;
                                return true
                            }
                        }
                    }
                    if ((key == VirtualKeyCode::Back || key == VirtualKeyCode::Delete) && flags == flags!()) || (key == VirtualKeyCode::X && flags == flags!(Ctrl)) {
                        if self.delete(flags & flags!(Ctrl) > 0) {
                            self.selected_text = None;
                            return true
                        }
                    }
                    if key == VirtualKeyCode::D && flags == flags!(Ctrl) {
                        if self.duplicate() {
                            self.selected_text = None;
                            return true
                        }
                    }
                    if key == VirtualKeyCode::C && flags == flags!(Ctrl) {
                        if self.copy() {
                            self.selected_text = None;
                            return true
                        }
                    }
                    if let Some(element) = cli_clipboard::get_contents().ok().and_then(|x| NbtElement::from_str(&x)) && key == VirtualKeyCode::V && flags == flags!(Ctrl) {
                        if self.drop(element, None) {
                            self.selected_text = None;
                            return true
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

        false
    }

    #[inline]
    pub fn on_cursor_move(&mut self, pos: &PhysicalPosition<f64>) -> bool {
        self.mouse_x = pos.x as usize;
        self.mouse_y = pos.y as usize;
        true
    }

    #[inline]
    pub fn window_height(&mut self, window_height: usize) {
        self.window_height = window_height;
    }

    #[inline]
    pub fn scroll(&self) -> usize {
        self.tab().map(Tab::scroll).unwrap_or(0)
    }

    #[inline]
    fn tab(&self) -> Option<&Tab> {
        self.tabs.get(self.tab)
    }

    #[inline]
    fn tab_mut(&mut self) -> Option<&mut Tab> {
        self.tabs.get_mut(self.tab)
    }

    #[inline]
    pub fn render(&self, builder: &mut VertexBufferBuilder) {
        self.render_tabs(builder);
        self.render_icons(builder);
        if let Some(tab) = self.tab() {
            let line_width = tab.value.true_height().ilog10() as usize + 1;
            let left_margin = self.left_margin();
            let mut highlight_y = if self.mouse_y < HEADER_SIZE { 0 } else { ((self.mouse_y - HEADER_SIZE) & !0b1111) + HEADER_SIZE };
            let ghost = if self.mouse_x >= left_margin && self.mouse_y >= HEADER_SIZE { self.held_entry.element().map(|x| (x.id(), ((self.mouse_x - left_margin) & !0b1111) + left_margin, ((self.mouse_y - HEADER_SIZE) & !0b0111) + HEADER_SIZE)) } else { None };
            if self.selected_text.is_some() { highlight_y = 0 }
            if !self.held_entry.is_empty() { highlight_y = 0 }
            let forbidden_y = self.selected_text.as_ref().map(|x| x.y).and_then(|x| x.checked_sub(builder.scroll())).unwrap_or(0);
            let ctx = RenderContext::new(forbidden_y, highlight_y, ghost, line_width, left_margin);
            tab.render(builder, &ctx);
            if let Some(selected_text) = &self.selected_text {
                selected_text.render(builder);
            }
        }
        self.render_held_entry(builder);
    }

    #[inline]
    fn render_held_entry(&self, builder: &mut VertexBufferBuilder) {
        if let Some(element) = self.held_entry.element() {
            NbtElement::render_icon(element.id(), self.mouse_x.checked_sub(8).unwrap_or(0), self.mouse_y.checked_sub(8).unwrap_or(0), builder);
        }
    }

    #[inline]
    fn render_icons(&self, builder: &mut VertexBufferBuilder) {
        NbtByte::render_icon(0, 26, builder);
        NbtShort::render_icon(16, 26, builder);
        NbtInt::render_icon(32, 26, builder);
        NbtLong::render_icon(48, 26, builder);
        NbtFloat::render_icon(64, 26, builder);
        NbtDouble::render_icon(80, 26, builder);
        NbtByteArray::render_icon(96, 26, builder);
        NbtIntArray::render_icon(112, 26, builder);
        NbtLongArray::render_icon(128, 26, builder);
        elements::string::render_icon(144, 26, builder);
        elements::list::render_icon(160, 26, builder);
        elements::compound::render_icon(176, 26, builder);
        builder.draw_texture((192, 26), (0, 48), (16, 16));

        if self.mouse_x < 208 && self.mouse_y >= 23 && self.mouse_y < 39 {
            builder.draw_texture((self.mouse_x & !0b1111, 26), (0, 32), (16, 16));
        }
    }

    #[inline]
    fn render_tabs(&self, builder: &mut VertexBufferBuilder) {
        let mut offset = 3;
        for (idx, tab) in self.tabs.iter().enumerate() {
            let u = (idx == self.tab) as usize * 16 + 16;
            builder.draw_texture((offset, 3), (u, 32), (1, 16));
            let width = VertexBufferBuilder::width(&tab.name) + 36;
            let mut remaining_width = width;
            let mut middle_offset = offset;
            while remaining_width > 0 {
                builder.draw_texture((middle_offset, 3), (u + 1, 32), (remaining_width.min(14), 16));
                remaining_width = if remaining_width < 14 { 0 } else { remaining_width - 14 };
                middle_offset += 14
            }
            builder.settings(offset + 2, 3, true);
            let _ = write!(builder, "{}", tab.name);
            offset += width;
            builder.draw_texture((offset - 32, 3), (96 - tab.history_changed as usize * 16, 32), (16, 16));
            builder.draw_texture((offset - 16, 3), (16 + tab.compression as usize * 16, 48), (16, 16));
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
        if self.held_keys.contains(&VirtualKeyCode::LControl) || self.held_keys.contains(&VirtualKeyCode::RControl) { return None }
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

pub struct RenderContext {
    forbidden_y: usize,
    highlight_y: usize,
    ghost: Option<(u8, usize, usize)>,
    line_width: usize,
    left_margin: usize,
}

impl RenderContext {
    pub fn new(forbidden_y: usize, highlight_y: usize, ghost: Option<(u8, usize, usize)>, line_width: usize, left_margin: usize) -> Self {
        Self {
            forbidden_y,
            highlight_y,
            ghost,
            line_width,
            left_margin,
        }
    }

    pub fn highlight(&self, pos: (usize, usize), builder: &mut VertexBufferBuilder) -> bool {
        if pos.1 == self.highlight_y {
            builder.draw_texture(pos, (0, 32), (16, 16));
            true
        } else {
            false
        }
    }

    pub fn forbid(&self, y: usize) -> bool {
        y != self.forbidden_y
    }

    pub fn line_number(&self, y: usize, line_number: &mut usize, builder: &mut VertexBufferBuilder) {
        builder.settings(4, y, false);
        let _ = write!(builder, "{:0n$}", *line_number, n = self.line_width);
        *line_number += 1;
    }

    pub fn ghost<F: FnOnce(usize, usize) -> bool>(&self, x_offset: usize, y_offset: usize, builder: &mut VertexBufferBuilder, f: F) -> bool {
        if let Some((id, x, y)) = self.ghost && f(x, y) {
            builder.draw_texture((x_offset, y_offset), match id {
                1 => (0, 64),
                2 => (16, 64),
                3 => (32, 64),
                4 => (48, 64),
                5 => (64, 64),
                6 => (80, 64),
                7 => (96, 64),
                8 => (16, 80),
                9 => (32, 80),
                10 => (48, 80),
                11 => (112, 64),
                12 => (0, 80),
                _ => panic!("Invalid element id"),
            }, (16, 16));
            true
        } else {
            false
        }
    }
}

pub struct Tab {
    value: Box<NbtElement>,
    name: Box<str>,
    path: Option<PathBuf>,
    compression: FileFormat,
    undos: LinkedList<WorkbenchAction>,
    redos: LinkedList<WorkbenchAction>,
    history_changed: bool,
    scroll: usize,
    window_height: usize,
}

impl Tab {
    pub fn new(nbt: NbtElement, path: &&PathBuf, compression: FileFormat, window_height: usize) -> Option<Tab> {
        if nbt.id() != NbtCompound::ID { return None }

        Some(Tab {
            value: Box::new(nbt),
            name: path.file_name()?.to_str()?.into(),
            path: Some(path.to_path_buf()),
            compression,
            undos: LinkedList::new(),
            redos: LinkedList::new(),
            history_changed: false,
            scroll: 0,
            window_height,
        })
    }

    pub fn save(&mut self) -> bool {
        if let Some(dir) = &self.path {
            let _ = write(dir, self.compression.encode(&self.value));
            self.history_changed = false;
            true
        } else {
            false
        }
    }

    pub fn render(&self, builder: &mut VertexBufferBuilder, ctx: &RenderContext) {
        if let NbtElement::Compound(compound) = self.value.deref() {
            compound.render_root(builder, &self.name, ctx);
            if builder.window_height() >= HEADER_SIZE {
                let height = compound.height() * 16 + 48;
                let total = builder.window_height() - HEADER_SIZE;
                if height > total {
                    let height = height;
                    let offset = (total as f64 / (height as f64 / (self.scroll() & !0b1111) as f64)) as usize + HEADER_SIZE; // safe to grab scroll
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

    pub fn scroll(&self) -> usize {
        let height = self.value.height() * 16 + 48;
        let scroll = self.scroll;
        let mut max = height + HEADER_SIZE; // 48 is an extra bonus bit
        let mut window_height = self.window_height;
        if cfg!(windows) { // todo
            window_height -= 15;
        }
        if max < window_height {
            max = 0;
        } else {
            max -= window_height;
        }
        scroll.min(max) & !0b1111
    }

    pub fn set_scroll(&mut self, scroll: f32) {
        const SCROLL_MUTLIPLIER: f32 = 48.0;

        if scroll.is_sign_negative() && self.scroll < (scroll * -SCROLL_MUTLIPLIER) as usize {
            self.scroll = 0;
        } else if scroll.is_sign_negative() {
            self.scroll -= (scroll * -SCROLL_MUTLIPLIER) as usize;
        } else {
            self.scroll += (scroll * SCROLL_MUTLIPLIER) as usize;
        }
        self.scroll = self.scroll();
    }
}

pub trait UnescapeStart {
    fn snbt_string_read(&self) -> Option<(String, &str)>;
}

pub fn valid_unescaped_char(byte: u8) -> bool {
    matches!(byte, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'#'..=b'&' | b' ' | b'\t' | b','..=b'/' | b';'..=b'@' | b'^'..=b'`' | b'~')
}

impl UnescapeStart for str {
    fn snbt_string_read(mut self: &Self) -> Option<(String, &str)> {
        if !self.starts_with('"') && !self.starts_with("'") {
            let colon_idx = self.char_indices().find(|(_, c)| *c == ':').map(|(idx, _)| idx)?;
            let (s, s2) = self.split_at(colon_idx);
            let mut s3 = String::with_capacity(s.len());
            for byte in s.bytes() {
                if valid_unescaped_char(byte) {
                    s3.push(byte as char);
                } else {
                    return None
                }
            }
            return Some((s3, s2))
        } else {
            let enclosing = self.chars().nth(0)?;
            self = &self[1..];
            let mut buf = String::new();
            let mut backslash = false;
            let end = 'a: {
                for (idx, mut char) in self.char_indices() {
                    if char == '\\' {
                        if backslash {
                            backslash = false;
                        } else {
                            backslash = true;
                            continue
                        }
                    }

                    if char == enclosing {
                        if backslash {
                            backslash = false;
                        } else {
                            break 'a idx
                        }
                    }

                    if char == 'n' {
                        if backslash {
                            backslash = false;
                            char = '\n';
                        }
                    }

                    if char == 'r' {
                        if backslash {
                            backslash = false;
                            char = '\r';
                        }
                    }

                    if char == '0' {
                        if backslash {
                            backslash = false;
                            char = '\0';
                        }
                    }

                    if backslash {
                        return None
                    }

                    buf.push(char);
                }
                return None
            };
            Some((buf, &self[(end + 1)..]))
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
