use std::fmt::Display;
use std::ops::{Deref, DerefMut};
use std::time::Duration;
use compact_str::CompactString;
use regex::Regex;
use winit::event::MouseButton;

use winit::keyboard::KeyCode;
use crate::assets::{ADD_SEARCH_BOOKMARKS_UV, BASE_Z, BOOKMARK_UV, DARK_STRIPE_UV, HIDDEN_BOOKMARK_UV, HOVERED_WIDGET_UV, REGEX_SEARCH_MODE_UV, REMOVE_SEARCH_BOOKMARKS_UV, SEARCH_KEYS_UV, SEARCH_KEYS_AND_VALUES_UV, SEARCH_VALUES_UV, SNBT_SEARCH_MODE_UV, STRING_SEARCH_MODE_UV, UNSELECTED_WIDGET_UV, SEARCH_BOX_Z, SEARCH_BOX_SELECTION_Z};

use crate::color::TextColor;
use crate::{combined_two_sorted, create_regex, flags, since_epoch, SortAlgorithm, StrExt};
use crate::elements::element::NbtElement;
use crate::text::{Cachelike, SearchBoxKeyResult, Text};
use crate::vertex_buffer_builder::{Vec2u, VertexBufferBuilder};
use crate::marked_line::{MarkedLine, MarkedLines};

pub struct SearchPredicate {
    pub search_flags: u8,
    pub inner: SearchPredicateInner,
}

pub enum SearchPredicateInner {
    String(String),
    Regex(Regex),
    Snbt(Option<String>, NbtElement),
}

#[derive(Copy, Clone)]
pub enum SearchMode {
    String,
    Regex,
    Snbt,
}

impl Display for SearchMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::String => "String",
            Self::Regex => "Regex",
            Self::Snbt => "SNBT",
        })
    }
}

impl SearchMode {
    pub fn cycle(self) -> Self {
        match self {
            Self::String => Self::Regex,
            Self::Regex => Self::Snbt,
            Self::Snbt => Self::String,
        }
    }

    pub fn rev_cycle(self) -> Self {
        match self {
            Self::String => Self::Snbt,
            Self::Regex => Self::String,
            Self::Snbt => Self::Regex,
        }
    }

    pub fn uv(self) -> Vec2u {
        match self {
            Self::String => STRING_SEARCH_MODE_UV,
            Self::Regex => REGEX_SEARCH_MODE_UV,
            Self::Snbt => SNBT_SEARCH_MODE_UV
        }
    }

    pub fn into_predicate(self, value: String, search_flags: u8) -> Option<SearchPredicate> {
        Some(match self {
            Self::String => SearchPredicate { inner: SearchPredicateInner::String(value), search_flags },
            Self::Regex => if let Some(regex) = create_regex(value) { SearchPredicate { inner: SearchPredicateInner::Regex(regex), search_flags } } else { return None },
            Self::Snbt => if let Some((key, value)) = NbtElement::from_str(&value, SortAlgorithm::None) { SearchPredicate { inner: SearchPredicateInner::Snbt(key.map(CompactString::into_string), value), search_flags } } else { return None },
        })
    }
}

impl SearchPredicate {
    fn matches(&self, key: Option<&str>, value: &NbtElement) -> bool {
        match &self.inner {
            SearchPredicateInner::String(str) => {
                let (value, color) = value.value();
                ((self.search_flags & 0b01) > 0 && color != TextColor::TreeKey && value.contains(str)) || ((self.search_flags & 0b10) > 0 && key.is_some_and(|k| k.contains(str)))
            }
            SearchPredicateInner::Regex(regex) => {
                let (value, color) = value.value();
                ((self.search_flags & 0b01) > 0 && color != TextColor::TreeKey && regex.is_match(&value)) || ((self.search_flags & 0b10) > 0 && key.is_some_and(|k| regex.is_match(k)))
            }
            SearchPredicateInner::Snbt(k, element) => {
                // cmp order does matter
                let a = element.matches(value);
                let b = k.as_deref() == key;
                ((self.search_flags == 0b11) & a & b) | ((self.search_flags == 0b01) & a) | ((self.search_flags == 0b10) & b)
            },
        }
    }
}

#[derive(Clone, Eq)]
pub struct SearchBoxCache {
    value: String,
    cursor: usize,
    selection: Option<usize>,
    hits: Option<(usize, Duration)>,
}

impl PartialEq for SearchBoxCache {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Cachelike<SearchBoxAdditional> for SearchBoxCache {
    fn new(text: &Text<SearchBoxAdditional, Self>) -> Self where Self: Sized {
        Self {
            value: text.value.clone(),
            cursor: text.cursor,
            selection: text.selection,
            hits: text.hits,
        }
    }

    fn revert(self, text: &mut Text<SearchBoxAdditional, Self>) where Self: Sized {
        text.value = self.value;
        text.cursor = self.cursor;
        text.selection = self.selection;
        text.hits = self.hits;
    }
}

#[derive(Clone)]
pub struct SearchBoxAdditional {
    selected: bool,
    horizontal_scroll: usize,
    pub hits: Option<(usize, Duration)>,
    pub flags: u8,
    pub mode: SearchMode,
}

pub struct SearchBox(Text<SearchBoxAdditional, SearchBoxCache>);

impl Deref for SearchBox {
    type Target = Text<SearchBoxAdditional, SearchBoxCache>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for SearchBox {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl SearchBox {
    pub fn new() -> Self {
        Self(Text::new(String::new(), 0, true, SearchBoxAdditional { selected: false, horizontal_scroll: 0, hits: None, flags: 0b01, mode: SearchMode::String }))
    }

    pub const fn uninit() -> Self {
        Self(Text::uninit())
    }

    pub fn render(&self, builder: &mut VertexBufferBuilder, shift: bool, mouse: (usize, usize)) {
        use std::fmt::Write;

        let pos = Vec2u::new(316, 23);
        let (mouse_x, mouse_y) = mouse;

        builder.draw_texture_region_z(
            pos,
            SEARCH_BOX_Z,
            DARK_STRIPE_UV,
            (builder.window_width() - 215 - pos.x, 22),
            (16, 16),
        );

        let hover = (pos.x..builder.window_width() - 215 - 17 - 16 - 16).contains(&mouse_x) && (23..46).contains(&mouse_y);

        builder.horizontal_scroll = self.horizontal_scroll;

        if self.value.is_empty() {
            builder.settings(pos + (0, 3), false, SEARCH_BOX_Z);
            builder.color = TextColor::Gray.to_raw();
            let _ = write!(builder, "{}", match self.mode {
                SearchMode::String => r#"Search..."#,
                SearchMode::Regex => r#"/[Ss]earch\.*/g"#,
                SearchMode::Snbt => r#"{dialog: "search", ...}"#,
            });
        }
        if self.is_selected() {
            self.0.render(builder, TextColor::Default, pos + (0, 3), SEARCH_BOX_Z, SEARCH_BOX_SELECTION_Z);
        } else {
            builder.settings(pos + (0, 3), false, SEARCH_BOX_Z);
            builder.color = TextColor::Default.to_raw();
            let _ = write!(builder, "{}", self.value);
        }

        if let Some((hits, stat)) = self.hits && (self.is_selected() || hover) {
            builder.draw_tooltip(&[&format!("{hits} hit{s} for \"{arg}\" ({ms}ms)", s = if hits == 1 { "" } else { "s" }, arg = self.value, ms = stat.as_millis())], if !self.is_selected() && hover { mouse } else { (316, 30) }, true);
        }

        builder.horizontal_scroll = 0;

        {
            let bookmark_uv = if shift { REMOVE_SEARCH_BOOKMARKS_UV } else { ADD_SEARCH_BOOKMARKS_UV };
            let widget_uv = if (builder.window_width() - 215 - 17 - 16 - 16..builder.window_width() - 215 - 1 - 16 - 16).contains(&mouse_x) && (26..42).contains(&mouse_y) {
                builder.draw_tooltip(&[if shift { "Remove all bookmarks (Shift + Enter)" } else { "Add search bookmarks (Enter)" }], mouse, false);
                HOVERED_WIDGET_UV
            } else {
                UNSELECTED_WIDGET_UV
            };

            builder.draw_texture_z((builder.window_width() - 215 - 17 - 16 - 16, 26), BASE_Z, widget_uv, (16, 16));
            builder.draw_texture_z((builder.window_width() - 215 - 17 - 16 - 16, 26), BASE_Z, bookmark_uv, (16, 16));
        }

        {
            let search_uv = match self.flags { 0b01 => SEARCH_VALUES_UV, 0b10 => SEARCH_KEYS_UV, _ => SEARCH_KEYS_AND_VALUES_UV };
            let widget_uv = if (builder.window_width() - 215 - 17 - 16..builder.window_width() - 215 - 1 - 16).contains(&mouse_x) && (26..42).contains(&mouse_y) {
                builder.draw_tooltip(&[match self.flags { 0b01 => "Values only", 0b10 => "Keys only", _ => "Keys + Values" }], mouse, false);
                HOVERED_WIDGET_UV
            } else {
                UNSELECTED_WIDGET_UV
            };

            builder.draw_texture_z((builder.window_width() - 215 - 17 - 16, 26), BASE_Z, widget_uv, (16, 16));
            builder.draw_texture_z((builder.window_width() - 215 - 17 - 16, 26), BASE_Z, search_uv, (16, 16));
        }

        {
            let mode_uv = self.mode.uv();
            let widget_uv = if (builder.window_width() - 215 - 17..builder.window_width() - 215 - 1).contains(&mouse_x) && (26..42).contains(&mouse_y) {
                builder.draw_tooltip(&[&format!("{mode} Mode", mode = self.mode)], mouse, false);
                HOVERED_WIDGET_UV
            } else {
                UNSELECTED_WIDGET_UV
            };

            builder.draw_texture_z((builder.window_width() - 215 - 17, 26), BASE_Z, widget_uv, (16, 16));
            builder.draw_texture_z((builder.window_width() - 215 - 17, 26), BASE_Z, mode_uv, (16, 16));
        }
    }

    #[inline]
    pub fn deselect(&mut self) {
        self.selected = false;
        self.cursor = 0;
        self.selection = None;
    }

    #[inline]
    pub fn select(&mut self, x: usize, button: MouseButton) {
        if button == MouseButton::Right {
            self.value.clear();
            self.cursor = 0;
            self.selection = None;
            self.hits = None;
            self.horizontal_scroll = 0;
            self.0.post_input();
        } else {
            let x = x + self.horizontal_scroll;
            self.cursor = 'a: {
                let mut current_x = 0;
                for (idx, char) in self.value.char_indices() {
                    let width = if (x as u32) < 56832 { VertexBufferBuilder::CHAR_WIDTH[char as usize] as usize } else { 0 };
                    if current_x + width / 2 >= x {
                        break 'a idx;
                    }
                    current_x += width;
                }
                self.value.len()
            };
        }
        self.selected = true;
        self.interact();
    }

    #[inline]
    pub fn on_bookmark_widget(&mut self, shift: bool, bookmarks: &mut MarkedLines, root: &mut NbtElement) {
        if shift {
            bookmarks.clear();
        } else {
            self.search(bookmarks, root, false);
        }
    }

    #[inline]
    pub fn on_search_widget(&mut self, shift: bool) {
        self.flags = ((self.flags as i8 - 1).wrapping_add((!shift) as i8 * 2 - 1).rem_euclid(3) + 1) as u8;
    }

    #[inline]
    pub fn on_mode_widget(&mut self, shift: bool) {
        self.mode = if shift { self.mode.rev_cycle() } else { self.mode.cycle() };
    }

    #[inline]
    pub fn search(&mut self, bookmarks: &mut MarkedLines, root: &NbtElement, count_only: bool) {
        if self.value.is_empty() {
            return;
        }

        let Some(predicate) = self.mode.into_predicate(self.value.clone(), self.flags) else { return };
        let start = since_epoch();
        let new_bookmarks = Self::search0(root, &predicate);
        self.hits = Some((new_bookmarks.len(), since_epoch() - start));
        if !count_only {
            let old_bookmarks = core::mem::replace(bookmarks, MarkedLines::new());
            *bookmarks = unsafe { MarkedLines::from_raw(combined_two_sorted(new_bookmarks.into_raw(), old_bookmarks.into_raw())) };
        }
    }

    pub fn search0(root: &NbtElement, predicate: &SearchPredicate) -> MarkedLines {
        let mut new_bookmarks = Vec::new();
        let mut queue = Vec::new();
        queue.push((None, &*root, true));
        let mut true_line_number = 1;
        let mut line_number = 0;
        while let Some((key, element, parent_open)) = queue.pop() {
            if predicate.matches(key, element) {
                new_bookmarks.push(MarkedLine::with_uv(true_line_number, line_number, if parent_open { BOOKMARK_UV } else { HIDDEN_BOOKMARK_UV }));
            }

            match element.children() {
                Some(Ok(iter)) => for value in iter.rev() {
                    queue.push((None, value, element.open()))
                }
                Some(Err(iter)) => for (key, value) in iter.rev() {
                    queue.push((Some(key), value, element.open()))
                }
                None => {}
            }

            true_line_number += 1;
            if parent_open {
                line_number += 1;
            }
        }
        unsafe { MarkedLines::from_raw(new_bookmarks) }
    }

    #[inline]
    #[must_use]
    pub fn is_selected(&self) -> bool {
        self.selected
    }

    #[inline]
    pub fn post_input(&mut self, window_dims: (usize, usize)) {
        let (window_width, _) = window_dims;
        self.0.post_input();
        let field_width = window_width - 215 - 316 - 17 - 16 - 16;
        let precursor_width = self.value.split_at(self.cursor).0.width();
        // 8px space just to look cleaner
        let horizontal_scroll = (precursor_width + 8).saturating_sub(field_width);
        self.horizontal_scroll = horizontal_scroll;
    }

    #[must_use]
    pub fn on_key_press(&mut self, key: KeyCode, char: Option<char>, flags: u8) -> SearchBoxKeyResult {
        let before = self.value.clone();
        let result = 'a: {
            if let KeyCode::Enter | KeyCode::NumpadEnter = key && flags == flags!(Shift) {
                break 'a SearchBoxKeyResult::ClearAllBookmarks
            }

            if let KeyCode::Enter | KeyCode::NumpadEnter = key && flags == flags!(Alt) {
                break 'a SearchBoxKeyResult::FinishCountOnly
            }

            self.0.on_key_press(key, char, flags).into()
        };
        if self.value != before && !((key == KeyCode::KeyZ || key == KeyCode::KeyY) && flags == flags!(Ctrl)) {
            self.hits = None;
        }
        result
    }
}
