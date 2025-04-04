use std::fmt::Display;
use std::ops::{Deref, DerefMut};
use std::time::Duration;

use regex::Regex;
use winit::event::MouseButton;
use winit::keyboard::KeyCode;
use winit::window::Theme;

use crate::assets::{BASE_Z, BOOKMARK_UV, DARK_STRIPE_UV, EXACT_MATCH_OFF_UV, EXACT_MATCH_ON_UV, HIDDEN_BOOKMARK_UV, HOVERED_WIDGET_UV, REGEX_SEARCH_MODE_UV, SEARCH_APPEND_BOOKMARKS_UV, SEARCH_BOOKMARKS_UV, SEARCH_BOX_SELECTION_Z, SEARCH_BOX_Z, SEARCH_KEYS_AND_VALUES_UV, SEARCH_KEYS_UV, SEARCH_VALUES_UV, SELECTED_WIDGET_UV, SNBT_SEARCH_MODE_UV, STRING_SEARCH_MODE_UV, UNSELECTED_WIDGET_UV};
use crate::{config, flags};
use crate::elements::{NbtElement, NbtElementAndKey};
use crate::render::{TextColor, Vec2u, VertexBufferBuilder};
use crate::util::{create_regex, now, StrExt};
use crate::widget::{get_cursor_idx, Cachelike, SearchBoxKeyResult, Text};
use crate::workbench::{MarkedLine, MarkedLines, SortAlgorithm};

pub const SEARCH_BOX_START_X: usize = 332;
pub const SEARCH_BOX_END_X: usize = 2;

pub struct SearchPredicate {
    pub search_flags: SearchFlags,
    pub inner: SearchPredicateInner,
}

pub enum SearchPredicateInner {
    String(String),
    StringCaseInsensitive(String),
    Regex(Regex),
    Snbt(NbtElementAndKey),
    SnbtExactMatch(NbtElementAndKey),
}

#[repr(u8)]
#[derive(Copy, Clone)]
pub enum SearchFlags {
    Values = 0,
    Keys = 1,
    KeysValues = 2,
}

impl Display for SearchFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Values => "Values only",
            Self::Keys => "Keys only",
            Self::KeysValues => "Keys or Values",
        })
    }
}

impl SearchFlags {
    pub fn cycle(self) -> Self {
        match self {
            Self::Values => Self::Keys,
            Self::Keys => Self::KeysValues,
            Self::KeysValues => Self::Values,
        }
    }

    pub fn rev_cycle(self) -> Self {
        match self {
            Self::Values => Self::KeysValues,
            Self::Keys => Self::Values,
            Self::KeysValues => Self::Keys,
        }
    }

    pub fn uv(self) -> Vec2u {
        match self {
            Self::Values => SEARCH_VALUES_UV,
            Self::Keys => SEARCH_KEYS_UV,
            Self::KeysValues => SEARCH_KEYS_AND_VALUES_UV
        }
    }
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
    #[inline]
    pub fn cycle(self) -> Self {
        match self {
            Self::String => Self::Regex,
            Self::Regex => Self::Snbt,
            Self::Snbt => Self::String,
        }
    }

    #[inline]
    pub fn rev_cycle(self) -> Self {
        match self {
            Self::String => Self::Snbt,
            Self::Regex => Self::String,
            Self::Snbt => Self::Regex,
        }
    }

    #[inline]
    pub fn uv(self) -> Vec2u {
        match self {
            Self::String => STRING_SEARCH_MODE_UV,
            Self::Regex => REGEX_SEARCH_MODE_UV,
            Self::Snbt => SNBT_SEARCH_MODE_UV
        }
    }

    pub fn into_predicate(self, value: String, exact_match: bool) -> Option<SearchPredicate> {
        let search_flags = config::get_search_flags();
        Some(match self {
            Self::String => SearchPredicate { inner: if exact_match { SearchPredicateInner::String(value) } else { SearchPredicateInner::StringCaseInsensitive(value.to_lowercase()) }, search_flags },
            Self::Regex => if let Some(regex) = create_regex(value, exact_match) { SearchPredicate { inner: SearchPredicateInner::Regex(regex), search_flags } } else { return None },
            Self::Snbt => if let Ok((key, value)) = {
                let sort = config::set_sort_algorithm(SortAlgorithm::None);
                let result = NbtElement::from_str(&value);
                config::set_sort_algorithm(sort);
                result
            } { SearchPredicate { inner: if exact_match { SearchPredicateInner::SnbtExactMatch((key, value)) } else { SearchPredicateInner::Snbt((key, value)) }, search_flags } } else { return None },
        })
    }

    #[inline]
    pub fn has_exact_match_mode(&self) -> bool {
        matches!(self, Self::String | Self::Regex | Self::Snbt)
    }

    #[inline]
    pub fn get_exact_search_on_name(&self) -> &str {
        match self {
            Self::String | Self::Regex => "Case Sensitive Mode",
            Self::Snbt => "Exact Match Mode",
        }
    }

    #[inline]
    pub fn get_exact_search_off_name(&self) -> &str {
        match self {
            Self::String | Self::Regex => "Case Insensitive Mode",
            Self::Snbt => "Contains Mode",
        }
    }
}

impl SearchPredicate {
    fn matches(&self, key: Option<&str>, value: &NbtElement) -> bool {
        let flags = self.search_flags as u8 + 1;
        match &self.inner {
            SearchPredicateInner::String(matcher) => {
                let (value, color) = value.value();
                ((flags & 0b01) > 0 && color != TextColor::TreeKey && value.contains(matcher)) || ((flags & 0b10) > 0 && key.is_some_and(|k| k.contains(matcher)))
            }
            SearchPredicateInner::StringCaseInsensitive(matcher) => {
                let (value, color) = value.value();
                let (key, value) = (key.map(|s| s.to_lowercase()), value.to_lowercase());
                ((flags & 0b01) > 0 && color != TextColor::TreeKey && value.contains(matcher)) || ((flags & 0b10) > 0 && key.is_some_and(|k| k.contains(matcher)))
            }
            SearchPredicateInner::Regex(regex) => {
                let (value, color) = value.value();
                ((flags & 0b01) > 0 && color != TextColor::TreeKey && regex.is_match(&value)) || ((flags & 0b10) > 0 && key.is_some_and(|k| regex.is_match(k)))
            }
            SearchPredicateInner::Snbt((k, element)) => {
                (!((flags & 0b01) > 0 && !element.matches(value))) && (!((flags & 0b10) > 0 && k.as_ref().map(|k| k.as_str()) != key))
            }
            SearchPredicateInner::SnbtExactMatch((k, element)) => {
                (!((flags & 0b01) > 0 && !element.eq(value))) && (!((flags & 0b10) > 0 && k.as_ref().map(|k| k.as_str()) != key))
            }
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
    pub horizontal_scroll: usize,
    pub hits: Option<(usize, Duration)>,
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
        Self(Text::new(String::new(), 0, true, SearchBoxAdditional { selected: false, horizontal_scroll: 0, hits: None }))
    }

    pub const fn uninit() -> Self {
        Self(Text::uninit())
    }

    pub fn render(&self, builder: &mut VertexBufferBuilder, shift: bool, mouse: (usize, usize)) {
        use std::fmt::Write;

        let search_mode = config::get_search_mode();
        let pos = Vec2u::new(SEARCH_BOX_START_X, 23);
        let (mouse_x, mouse_y) = mouse;

        builder.draw_texture_region_z(
            pos,
            SEARCH_BOX_Z,
            DARK_STRIPE_UV,
            (builder.window_width() - SEARCH_BOX_END_X - pos.x, 22),
            (16, 16),
        );

        let hover = (pos.x..builder.window_width() - SEARCH_BOX_END_X - 17 - 16 - 16).contains(&mouse_x) && (23..46).contains(&mouse_y);

        builder.horizontal_scroll = self.horizontal_scroll;

        if self.value.is_empty() {
            builder.settings(pos + (0, 3), false, SEARCH_BOX_Z);
            builder.color = TextColor::Gray.to_raw();
            let _ = write!(builder, "{}", match search_mode {
                SearchMode::String => r#"Search..."#,
                SearchMode::Regex => r#"/[Ss]earch\.*/g"#,
                SearchMode::Snbt => r#"{dialog: "search", ...}"#,
            });
        }
        let color = match config::get_theme() { Theme::Light => TextColor::Black, Theme::Dark => TextColor::White };
        if self.is_selected() {
            self.0.render(builder, color, pos + (0, 3), SEARCH_BOX_Z, SEARCH_BOX_SELECTION_Z);
        } else {
            builder.settings(pos + (0, 3), false, SEARCH_BOX_Z);
            builder.color = color.to_raw();
            let _ = write!(builder, "{}", self.value);
        }

        if let Some((hits, stat)) = self.hits && (self.is_selected() || hover) {
            builder.draw_tooltip(&[&format!("{hits} hit{s} for \"{arg}\" ({ms}ms)", s = if hits == 1 { "" } else { "s" }, arg = self.value, ms = stat.as_millis())], if !self.is_selected() && hover { mouse } else { (SEARCH_BOX_START_X, 30) }, true);
        }

        builder.horizontal_scroll = 0;

        {
            let bookmark_uv = if shift { SEARCH_APPEND_BOOKMARKS_UV } else { SEARCH_BOOKMARKS_UV };
            let widget_uv = if (builder.window_width() - SEARCH_BOX_END_X - 17 - 16 - 16 - 16..builder.window_width() - SEARCH_BOX_END_X - 1 - 16 - 16 - 16).contains(&mouse_x) && (26..42).contains(&mouse_y) {
                builder.draw_tooltip(&[if shift { "Append to search (Shift + Enter)"} else { "Search (Enter)" }], mouse, false);
                HOVERED_WIDGET_UV
            } else {
                SELECTED_WIDGET_UV
            };

            builder.draw_texture_z((builder.window_width() - SEARCH_BOX_END_X - 17 - 16 - 16 - 16, 26), BASE_Z, widget_uv, (16, 16));
            builder.draw_texture_z((builder.window_width() - SEARCH_BOX_END_X - 17 - 16 - 16 - 16, 26), BASE_Z, bookmark_uv, (16, 16));
        }

        {
            let search_uv = config::get_search_flags().uv();
            let widget_uv = if (builder.window_width() - SEARCH_BOX_END_X - 17 - 16 - 16..builder.window_width() - SEARCH_BOX_END_X - 1 - 16 - 16).contains(&mouse_x) && (26..42).contains(&mouse_y) {
                builder.draw_tooltip(&[&config::get_search_flags().to_string()], mouse, false);
                HOVERED_WIDGET_UV
            } else {
                SELECTED_WIDGET_UV
            };

            builder.draw_texture_z((builder.window_width() - SEARCH_BOX_END_X - 17 - 16 - 16, 26), BASE_Z, widget_uv, (16, 16));
            builder.draw_texture_z((builder.window_width() - SEARCH_BOX_END_X - 17 - 16 - 16, 26), BASE_Z, search_uv, (16, 16));
        }

        {
            let mode_uv = search_mode.uv();
            let widget_uv = if (builder.window_width() - SEARCH_BOX_END_X - 17 - 16..builder.window_width() - SEARCH_BOX_END_X - 1 - 16).contains(&mouse_x) && (26..42).contains(&mouse_y) {
                builder.draw_tooltip(&[&format!("{search_mode} Mode")], mouse, false);
                HOVERED_WIDGET_UV
            } else {
                SELECTED_WIDGET_UV
            };

            builder.draw_texture_z((builder.window_width() - SEARCH_BOX_END_X - 17 - 16, 26), BASE_Z, widget_uv, (16, 16));
            builder.draw_texture_z((builder.window_width() - SEARCH_BOX_END_X - 17 - 16, 26), BASE_Z, mode_uv, (16, 16));
        }

        {
            let has_exact_search = search_mode.has_exact_match_mode();
            let within_widget_bounds = (builder.window_width() - SEARCH_BOX_END_X - 17..builder.window_width() - SEARCH_BOX_END_X - 1).contains(&mouse_x) && (26..42).contains(&mouse_y);
            let exact_match = config::get_search_exact_match();
            let exact_match_uv = if exact_match || !has_exact_search { EXACT_MATCH_ON_UV } else { EXACT_MATCH_OFF_UV };
            if within_widget_bounds {
                builder.draw_tooltip(&[if exact_match || !has_exact_search { search_mode.get_exact_search_on_name() } else { search_mode.get_exact_search_off_name() }], mouse, false);
            }
            let widget_uv = if has_exact_search {
                if within_widget_bounds {
                    HOVERED_WIDGET_UV
                } else {
                    SELECTED_WIDGET_UV
                }
            } else {
                UNSELECTED_WIDGET_UV
            };

            builder.draw_texture_z((builder.window_width() - SEARCH_BOX_END_X - 17, 26), BASE_Z, widget_uv, (16, 16));
            builder.draw_texture_z((builder.window_width() - SEARCH_BOX_END_X - 17, 26), BASE_Z, exact_match_uv, (16, 16));
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
            self.cursor = get_cursor_idx(&self.value, (x + self.horizontal_scroll) as isize);
            self.selection = None;
        }
        self.selected = true;
        self.interact();
    }

    #[inline]
    pub fn on_bookmark_widget(&mut self, shift: bool, bookmarks: &mut MarkedLines, root: &mut NbtElement) {
        if !shift {
            bookmarks.clear();
        }

        self.search(bookmarks, root, false);
    }

    #[inline]
    pub fn on_search_widget(&mut self, shift: bool) {
        config::set_search_flags(if shift { config::get_search_flags().rev_cycle() } else { config::get_search_flags().cycle() });
    }

    #[inline]
    pub fn on_mode_widget(&mut self, shift: bool) {
        config::set_search_mode(if shift { config::get_search_mode().rev_cycle() } else { config::get_search_mode().cycle() });
    }

    #[inline]
    pub fn on_exact_match_widget(&mut self, _shift: bool) {
        if config::get_search_mode().has_exact_match_mode() {
            config::set_search_exact_match(!config::get_search_exact_match());
        }
    }

    #[inline]
    pub fn search(&mut self, bookmarks: &mut MarkedLines, root: &NbtElement, count_only: bool) {
        if self.value.is_empty() {
            return;
        }

        let start = now();
        let new_bookmarks = if self.value.is_empty() {
            MarkedLines::new()
        } else {
            let Some(predicate) = config::get_search_mode().into_predicate(self.value.clone(), config::get_search_exact_match()) else { return };
            Self::search0(root, &predicate)
        };
        self.hits = Some((new_bookmarks.len(), now() - start));
        if !count_only && !new_bookmarks.is_empty() {
            bookmarks.add_bookmarks(new_bookmarks);
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
        let field_width = window_width - SEARCH_BOX_END_X - SEARCH_BOX_START_X - 17 - 16 - 16;
        let precursor_width = self.value.split_at(self.cursor).0.width();
        // 8px space just to look cleaner
        let horizontal_scroll = (precursor_width + 8).saturating_sub(field_width);
        self.horizontal_scroll = horizontal_scroll;
    }

    #[must_use]
    pub fn on_key_press(&mut self, key: KeyCode, char: Option<char>, flags: u8) -> SearchBoxKeyResult {
        let before = self.value.clone();
        let result = 'a: {
            if let KeyCode::Enter | KeyCode::NumpadEnter = key && flags == flags!() {
                break 'a SearchBoxKeyResult::ClearAndSearch
            }

            if let KeyCode::Enter | KeyCode::NumpadEnter = key && flags == flags!(Shift) {
                break 'a SearchBoxKeyResult::Search
            }

            if let KeyCode::Enter | KeyCode::NumpadEnter = key && flags == flags!(Alt) {
                break 'a SearchBoxKeyResult::SearchCountOnly
            }

            self.0.on_key_press(key, char, flags).into()
        };
        if self.value != before && !((key == KeyCode::KeyZ || key == KeyCode::KeyY) && flags == flags!(Ctrl)) {
            self.hits = None;
        }
        result
    }
}
