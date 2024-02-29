use std::intrinsics::{likely, unlikely};
use std::ops::{Deref, DerefMut};
use std::time::Duration;

use winit::keyboard::KeyCode;

use crate::{flags, get_clipboard, is_jump_char_boundary, is_utf8_char_boundary, LinkedQueue, OptionExt, set_clipboard, since_epoch, StrExt};
use crate::assets::{BASE_TEXT_Z, SELECTED_TEXT_Z, SELECTION_UV};
use crate::color::TextColor;
use crate::text::KeyResult::{Failed, Finish, NothingSpecial, Revert};
use crate::vertex_buffer_builder::{Vec2u, VertexBufferBuilder};

#[repr(u8)]
pub enum SelectedTextKeyResult {
    Failed,
    NothingSpecial,
    Revert,
    Finish,
    Keyfix,
    Valuefix,
    Up(bool),
    Down(bool),
    ForceClose,
    ForceOpen,
    ShiftUp,
    ShiftDown,
}

#[repr(u8)]
pub enum KeyResult {
    Failed,
    NothingSpecial,
    Revert,
    Finish,
}

impl From<KeyResult> for SelectedTextKeyResult {
    fn from(value: KeyResult) -> Self {
        match value {
            Failed => Self::Failed,
            NothingSpecial => Self::NothingSpecial,
            Revert => Self::Revert,
            Finish => Self::Finish,
        }
    }
}

pub trait Cachelike<Additional: Clone>: PartialEq + Clone {
    fn new(text: &Text<Additional, Self>) -> Self where Self: Sized;

    fn revert(self, text: &mut Text<Additional, Self>) where Self: Sized;
}

#[derive(Clone)]
pub struct Text<Additional: Clone, Cache: Cachelike<Additional>> {
    pub value: String,
    pub cursor: usize,
    pub selection: Option<usize>,
    pub editable: bool,
    pub additional: Additional,
    last_interaction: Duration,
    undos: LinkedQueue<Cache>,
    redos: LinkedQueue<Cache>,
}

impl<Additional: Clone, Cache: Cachelike<Additional>> Deref for Text<Additional, Cache> {
    type Target = Additional;

    fn deref(&self) -> &Self::Target {
        &self.additional
    }
}

impl<Additional: Clone, Cache: Cachelike<Additional>> DerefMut for Text<Additional, Cache> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.additional
    }
}

impl<Additional: Clone, Cache: Cachelike<Additional>> Text<Additional, Cache> {
    pub fn new(value: String, cursor: usize, editable: bool, additional: Additional) -> Self {
        let mut this = Self {
            value,
            cursor,
            selection: None,
            last_interaction: since_epoch(),
            editable,
            undos: LinkedQueue::new(),
            redos: LinkedQueue::new(),
            additional,
        };
        this.save_state_in_history();
        this
    }

    pub const fn uninit() -> Self {
        Self {
            value: String::new(),
            cursor: 0,
            selection: None,
            last_interaction: Duration::ZERO,
            editable: true,
            undos: LinkedQueue::new(),
            redos: LinkedQueue::new(),
            additional: unsafe { core::mem::MaybeUninit::zeroed().assume_init() },
        }
    }

    #[inline]
    pub fn interact(&mut self) {
        self.last_interaction = since_epoch();
    }

    #[must_use]
    pub fn on_key_press(&mut self, key: KeyCode, mut char: Option<char>, flags: u8) -> KeyResult {
        if key == KeyCode::Escape && flags == flags!() { return Revert }

        if let KeyCode::Enter | KeyCode::NumpadEnter = key && flags == flags!() {
            return Finish;
        }

        if key == KeyCode::KeyZ && flags == flags!(Ctrl) && self.editable {
            let current = Cache::new(&self);

            let cache = 'a: {
                while let Some(cache) = self.undos.pop() {
                    if unlikely(cache.ne(&current)) {
                        self.undos.push(cache.clone());
                        break 'a cache;
                    }
                }
                return Failed;
            };

            self.redos.push(current);
            self.last_interaction = Duration::ZERO;
            cache.revert(self);
            return NothingSpecial;
        }

        if (key == KeyCode::KeyY && flags == flags!(Ctrl) || key == KeyCode::KeyZ && flags == flags!(Ctrl + Shift)) && self.editable {
            let current = Cache::new(&self);

            let cache = 'a: {
                while let Some(cache) = self.redos.pop() {
                    if likely(cache.ne(&current)) {
                        self.redos.push(cache.clone());
                        break 'a cache;
                    }
                }
                return Failed;
            };

            self.undos.push(current);
            self.last_interaction = Duration::ZERO;
            cache.revert(self);
            return NothingSpecial;
        }

        if key == KeyCode::KeyA && flags == flags!(Ctrl) && self.editable {
            self.cursor = 0;
            self.selection = Some(self.value.len());
            return NothingSpecial;
        }

        if let Some(selection) = self.selection
            && flags == flags!()
        {
            if let KeyCode::Backspace | KeyCode::Delete = key
                && self.editable
            {
                let (low_selection, high_selection) = if self.cursor < selection {
                    (self.cursor, selection)
                } else {
                    (selection, self.cursor)
                };
                let (left, right) = self.value.split_at(low_selection);
                let (_, right) = right.split_at(high_selection - low_selection);
                self.value = format!("{left}{right}");
                self.selection = None;
                self.cursor = low_selection;
                return NothingSpecial;
            }
        }

        if key == KeyCode::KeyX && flags == flags!(Ctrl) && self.editable {
            if let Some(selection) = self.selection {
                let (start, end) = if self.cursor < selection {
                    (self.cursor, selection)
                } else {
                    (selection, self.cursor)
                };
                let (low, right) = self.value.split_at(start);
                let (cut, high) = right.split_at(end - start);
                if set_clipboard(cut.to_owned()) {
                    self.value = format!("{low}{high}");
                    self.selection = None;
                }
                return NothingSpecial;
            }
        }

        if key == KeyCode::KeyC && flags == flags!(Ctrl) && self.editable {
            if let Some(selection) = self.selection {
                let (start, end) = if self.cursor < selection {
                    (self.cursor, selection)
                } else {
                    (selection, self.cursor)
                };
                let (_, right) = self.value.split_at(start);
                let (cut, _) = right.split_at(end - start);
                set_clipboard(cut.to_owned());
                return NothingSpecial;
            }
        }

        if key == KeyCode::KeyV && flags == flags!(Ctrl) && self.editable {
            if let Some(clipboard) = get_clipboard() {
                if let Some(selection) = self.selection.take() {
                    let (start, end) = if self.cursor < selection {
                        (self.cursor, selection)
                    } else {
                        (selection, self.cursor)
                    };
                    let (left, right) = self.value.split_at(start);
                    self.cursor = left.len() + clipboard.len();
                    let (_, right) = right.split_at(end - start);
                    self.value = format!("{left}{clipboard}{right}");
                } else {
                    let (left, right) = self.value.split_at(self.cursor);
                    self.value = format!("{left}{clipboard}{right}");
                    self.cursor += clipboard.len();
                }
                return NothingSpecial;
            }
        }

        if key == KeyCode::Home && flags & !flags!(Shift) == 0 && self.editable {
            if flags == flags!(Shift) {
                let new = self.selection.map_or(self.cursor, |x| x.min(self.cursor));
                self.selection = if new == 0 { None } else { Some(new) };
            } else {
                self.selection = None;
            }
            self.cursor = 0;
            return NothingSpecial;
        }

        if key == KeyCode::End && flags & !flags!(Shift) == 0 && self.editable {
            if flags == flags!(Shift) {
                let new = self.selection.map_or(self.cursor, |x| x.max(self.cursor));
                self.selection = if new == self.value.len() {
                    None
                } else {
                    Some(new)
                };
            } else {
                self.selection = None;
            }
            self.cursor = self.value.len();
            return NothingSpecial;
        }

        if key == KeyCode::Backspace && flags < 2 && self.editable {
            let (left, right) = self.value.split_at(self.cursor);
            if flags & flags!(Ctrl) > 0 {
                if !left.is_empty() {
                    let mut end = left.len() - 1;
                    while end > 0 {
                        if left.as_bytes()[end].is_ascii_whitespace() {
                            end -= 1;
                        } else {
                            break;
                        }
                    }
                    let last_byte = left.as_bytes()[end];
                    let last_jump_char_boundary = is_jump_char_boundary(last_byte);
                    while end > 0 {
                        let byte = left.as_bytes()[end];
                        if is_utf8_char_boundary(byte) && (!last_jump_char_boundary && is_jump_char_boundary(byte) || last_jump_char_boundary && byte != last_byte) {
                            // this is to fix "string   |" [CTRL + BACKSPACE] => "strin" instead of "string"
                            end += 1;
                            break;
                        }
                        end -= 1;
                    }
                    let (left, _) = left.split_at(end);
                    self.value = format!("{left}{right}");
                    self.cursor = end;
                }
            } else {
                if !left.is_empty() {
                    let mut end = left.len() - 1;
                    while end > 0 {
                        if is_utf8_char_boundary(left.as_bytes()[end]) {
                            break;
                        }
                        end -= 1;
                    }
                    let (left, _) = left.split_at(end);
                    self.cursor = left.len();
                    self.value = format!("{left}{right}");
                }
            }

            return NothingSpecial;
        }

        if key == KeyCode::Delete && self.editable {
            let (left, right) = self.value.split_at(self.cursor);
            if flags & flags!(Ctrl) > 0 {
                if !right.is_empty() {
                    let mut start = 1;
                    while start < right.len() {
                        if right.as_bytes()[start].is_ascii_whitespace() {
                            start += 1;
                        } else {
                            break;
                        }
                    }
                    let first_byte = right.as_bytes()[start];
                    let first_jump_char_boundary = is_jump_char_boundary(first_byte);
                    while start < right.len() {
                        let byte = right.as_bytes()[start];
                        if is_utf8_char_boundary(byte) && (!first_jump_char_boundary && is_jump_char_boundary(byte) || first_jump_char_boundary && byte != first_byte) {
                            start -= 1;
                            break;
                        }
                        start += 1;
                    }
                    let (_, right) = right.split_at(start);
                    self.cursor = left.len();
                    self.value = format!("{left}{right}");
                }
            } else {
                if !right.is_empty() {
                    let mut start = 1;
                    while start < right.len() {
                        if is_utf8_char_boundary(right.as_bytes()[start]) {
                            break;
                        }
                        start += 1;
                    }
                    let (_, right) = right.split_at(start);
                    self.cursor = left.len();
                    self.value = format!("{left}{right}");
                }
            }
            return NothingSpecial;
        }

        if key == KeyCode::ArrowLeft {
            if self.editable {
                if flags & flags!(Shift) == 0
                    && let Some(selection) = self.selection.take()
                {
                    self.cursor = selection.min(self.cursor);
                    return NothingSpecial;
                }

                let mut new = self.cursor;
                if flags & flags!(Ctrl) > 0 {
                    if new > 0 {
                        new -= 1;
                        while new > 0 {
                            if self.value.as_bytes()[new].is_ascii_whitespace() {
                                new -= 1;
                            } else {
                                break;
                            }
                        }
                        let last_byte = self.value.as_bytes()[new];
                        let last_jump_char_boundary = is_jump_char_boundary(last_byte);
                        while new > 0 {
                            let byte = self.value.as_bytes()[new];
                            if is_utf8_char_boundary(byte) && (!last_jump_char_boundary && is_jump_char_boundary(byte) || last_jump_char_boundary && byte != last_byte) {
                                // this is to fix "string   |" [CTRL + BACKSPACE] => "strin" instead of "string"
                                new += 1;
                                break;
                            }
                            new -= 1;
                        }
                    }
                } else {
                    if new > 0 {
                        new -= 1;
                        while new > 0 {
                            if is_utf8_char_boundary(self.value.as_bytes()[new]) {
                                break;
                            }

                            new -= 1;
                        }
                    }
                }

                if flags & flags!(Shift) > 0 {
                    if self.selection.is_none() {
                        self.selection = Some(self.cursor);
                    }
                } else {
                    self.selection = None;
                }

                self.cursor = new;

                if self.selection.is_some_and(|x| x == self.cursor) {
                    self.selection = None;
                }
            }
            return NothingSpecial;
        }

        if key == KeyCode::ArrowRight {
            if self.editable {
                if flags & flags!(Shift) == 0
                    && let Some(selection) = self.selection.take()
                {
                    self.cursor = selection.max(self.cursor);
                    return NothingSpecial;
                }

                let mut new = self.cursor;
                if flags & flags!(Ctrl) > 0 {
                    if new < self.value.len() {
                        new += 1;
                        if new < self.value.len() {
                            while new < self.value.len() {
                                if self.value.as_bytes()[new].is_ascii_whitespace() {
                                    new += 1;
                                } else {
                                    break;
                                }
                            }
                            let first_byte = self.value.as_bytes()[new];
                            let first_jump_char_boundary = is_jump_char_boundary(first_byte);
                            while new < self.value.len() {
                                let byte = self.value.as_bytes()[new];
                                if is_utf8_char_boundary(byte) && (!first_jump_char_boundary && is_jump_char_boundary(byte) || first_jump_char_boundary && byte != first_byte) {
                                    break;
                                }
                                new += 1;
                            }
                        }
                    }
                } else {
                    if new < self.value.len() {
                        new += 1;
                        while new < self.value.len() {
                            if is_utf8_char_boundary(self.value.as_bytes()[new]) {
                                break;
                            }

                            new += 1;
                        }
                    }
                }

                if flags & flags!(Shift) > 0 {
                    if self.selection.is_none() {
                        self.selection = Some(self.cursor);
                    }
                } else {
                    self.selection = None;
                }
                self.cursor = new;

                if self.selection.is_some_and(|x| x == self.cursor) {
                    self.selection = None;
                }
            }
            return NothingSpecial;
        }

        if let KeyCode::Enter | KeyCode::NumpadEnter = key
            && flags == flags!(Shift)
            && self.editable
        {
            char = Some('\n');
        }

        if let Some(char) = char && self.editable {
            if let Some(selection) = self.selection {
                let (low_selection, high_selection) = if self.cursor < selection {
                    (self.cursor, selection)
                } else {
                    (selection, self.cursor)
                };
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

            return NothingSpecial;
        }

        Failed
    }

    pub fn render(&self, builder: &mut VertexBufferBuilder, color: TextColor, pos: Vec2u, z: u8) {
        use std::fmt::Write;

        let (x, y) = pos.into();

        builder.settings((x, y), false, z);

        builder.color = color.to_raw();
        let _ = write!(builder, "{}", self.value);

        if self.editable {
            let cursor_prefixing = self.value.split_at(self.cursor).0;
            let duration_from_last_interaction = since_epoch() - self.last_interaction;
            if let Some(selection) = self.selection && self.editable {
                let (start, end) = if self.cursor > selection {
                    (selection, self.cursor)
                } else {
                    (self.cursor, selection)
                };
                let start = self.value.split_at(start).0.width();
                let end = self.value.split_at(end).0.width();
                builder.draw_texture_region_z(
                    (start + x, y),
                    z + 1,
                    SELECTION_UV + (1, 1),
                    (end - start - 1, 16),
                    (14, 14),
                );
                if duration_from_last_interaction < Duration::from_millis(500) || duration_from_last_interaction.subsec_millis() < 500 {
                    builder.draw_texture_region_z(
                        (x + cursor_prefixing.width() - 1, y),
                        z + 1,
                        SELECTION_UV,
                        (2, 16),
                        (1, 16),
                    );
                }
            } else {
                if duration_from_last_interaction < Duration::from_millis(500) || duration_from_last_interaction.subsec_millis() < 500 {
                    builder.draw_texture_region_z(
                        (x + cursor_prefixing.width(), y),
                        z + 1,
                        SELECTION_UV,
                        (2, 16),
                        (1, 16),
                    );
                }
            }
        }
    }

    #[inline]
    pub fn save_state_in_history(&mut self) {
        self.undos.push(Cache::new(&self));
    }

    #[inline]
    pub fn post_input(&mut self) {
        let current = Cache::new(self);

        let should_cache = core::mem::replace(&mut self.last_interaction, since_epoch()).as_millis() >= 1_500;
        if should_cache && self.editable && self.undos.get().is_none_or(|x| x.ne(&current)) {
            if self.redos.pop().is_none_or(|x| x.ne(&current)) {
                self.redos = LinkedQueue::new();
            }

            self.save_state_in_history();
        }

        if self.undos.get().is_some_and(|x| x.eq(&current)) && let Some(undo) = self.undos.get_mut() {
            *undo = current;
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.value.clear();
        self.cursor = 0;
        self.redos.clear();
        self.undos.clear();
        self.selection = None;
        self.last_interaction = since_epoch();
    }
}
