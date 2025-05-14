use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};
use std::time::Duration;

use regex::Regex;
use winit::event::MouseButton;
use winit::keyboard::KeyCode;
use winit::window::Theme;

use crate::assets::{AND_SELECTION_OPERATION_UV, BOOKMARK_UV, DARK_STRIPE_UV, HIDDEN_BOOKMARK_UV, OR_SELECTION_OPERATION_UV, REGEX_SEARCH_MODE_UV, REPLACE_SELECTION_OPERATION_UV, SEARCH_BOX_SELECTION_Z, SEARCH_BOX_Z, SEARCH_KEYS_AND_VALUES_UV, SEARCH_KEYS_UV, SEARCH_VALUES_UV, SNBT_SEARCH_MODE_UV, STRING_SEARCH_MODE_UV, XOR_SELECTION_OPERATION_UV};
use crate::elements::{NbtElement, NbtElementAndKey, NbtElementAndKeyRef};
use crate::render::widget::text::get_cursor_idx;
use crate::render::{TextColor, VertexBufferBuilder};
use crate::util::{create_regex, now, StrExt, Vec2u};
use crate::widget::{Cachelike, Notification, NotificationKind, SearchBoxKeyResult, Text};
use crate::workbench::{MarkedLine, MarkedLines, SortAlgorithm};
use crate::{config, flags};

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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Values => "Values only",
            Self::Keys => "Keys only",
            Self::KeysValues => "Keys or Values",
        })
    }
}

impl SearchFlags {
    #[must_use]
    pub fn cycle(self) -> Self {
        match self {
            Self::Values => Self::Keys,
            Self::Keys => Self::KeysValues,
            Self::KeysValues => Self::Values,
        }
    }

    #[must_use]
    pub fn rev_cycle(self) -> Self {
        match self {
            Self::Values => Self::KeysValues,
            Self::Keys => Self::Values,
            Self::KeysValues => Self::Keys,
        }
    }

    #[must_use]
    pub fn uv(self) -> Vec2u {
        match self {
            Self::Values => SEARCH_VALUES_UV,
            Self::Keys => SEARCH_KEYS_UV,
            Self::KeysValues => SEARCH_KEYS_AND_VALUES_UV
        }
    }
    
    #[must_use]
    pub fn has_key(self) -> bool {
        matches!(self, Self::Keys | Self::KeysValues)
    }

    #[must_use]
    pub fn has_value(self) -> bool {
        matches!(self, Self::Values | Self::KeysValues)
    }
}


#[derive(Copy, Clone)]
pub enum SearchOperation {
    And,
    Or,
    Xor,
    B,
}

impl Display for SearchOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::And => "And (intersection)",
            Self::Or => "Or (union)",
            Self::Xor => "Xor (symmetric difference)",
            Self::B => "B (replace) [default behaviour]"
        })
    }
}
impl SearchOperation {
    #[must_use]
    pub fn cycle(self) -> Self {
        match self {
            Self::And => Self::Or,
            Self::Or => Self::Xor,
            Self::Xor => Self::B,
            Self::B => Self::And,
        }
    }


    #[must_use]
    pub fn rev_cycle(self) -> Self {
        match self {
            Self::And => Self::B,
            Self::Or => Self::And,
            Self::Xor => Self::Or,
            Self::B => Self::Xor,
        }
    }
    
    #[must_use]
    pub fn uv(self) -> Vec2u {
        match self {
            Self::And => AND_SELECTION_OPERATION_UV,
            Self::Or => OR_SELECTION_OPERATION_UV,
            Self::Xor => XOR_SELECTION_OPERATION_UV,
            Self::B => REPLACE_SELECTION_OPERATION_UV,
        }
    }
    
    pub fn apply(self, lhs: &mut MarkedLines, rhs: MarkedLines) {
        match self {
            Self::And => *lhs &= rhs,
            Self::Or => *lhs |= rhs,
            Self::Xor => *lhs ^= rhs,
            Self::B => *lhs = rhs,
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::String => "String",
            Self::Regex => "Regex",
            Self::Snbt => "SNBT",
        })
    }
}

impl SearchMode {
    #[must_use]
    pub fn cycle(self) -> Self {
        match self {
            Self::String => Self::Regex,
            Self::Regex => Self::Snbt,
            Self::Snbt => Self::String,
        }
    }

    #[must_use]
    pub fn rev_cycle(self) -> Self {
        match self {
            Self::String => Self::Snbt,
            Self::Regex => Self::String,
            Self::Snbt => Self::Regex,
        }
    }

    #[must_use]
    pub fn uv(self) -> Vec2u {
        match self {
            Self::String => STRING_SEARCH_MODE_UV,
            Self::Regex => REGEX_SEARCH_MODE_UV,
            Self::Snbt => SNBT_SEARCH_MODE_UV
        }
    }

    #[must_use]
    pub fn has_exact_match_mode(&self) -> bool {
        matches!(self, Self::String | Self::Regex | Self::Snbt)
    }

    #[must_use]
    pub fn get_exact_search_on_name(&self) -> &str {
        match self {
            Self::String | Self::Regex => "Case Sensitive Mode",
            Self::Snbt => "Exact Match Mode",
        }
    }

    #[must_use]
    pub fn get_exact_search_off_name(&self) -> &str {
        match self {
            Self::String | Self::Regex => "Case Insensitive Mode",
            Self::Snbt => "Contains Mode",
        }
    }
}

impl SearchPredicate {
    #[must_use]
    fn new(value: String) -> Option<Self> {
        let search_mode = config::get_search_mode();
        let search_flags = config::get_search_flags();
        let exact_match = config::get_search_exact_match();
        Some(match search_mode {
            SearchMode::String => Self { inner: if exact_match { SearchPredicateInner::String(value) } else { SearchPredicateInner::StringCaseInsensitive(value.to_lowercase()) }, search_flags },
            SearchMode::Regex => if let Some(regex) = create_regex(value, exact_match) { Self { inner: SearchPredicateInner::Regex(regex), search_flags } } else { return None },
            SearchMode::Snbt => if let Ok((key, value)) = {
                let sort = config::set_sort_algorithm(SortAlgorithm::None);
                let result = NbtElement::from_str(&value);
                config::set_sort_algorithm(sort);
                result
            } { Self { inner: if exact_match { SearchPredicateInner::SnbtExactMatch((key, value)) } else { SearchPredicateInner::Snbt((key, value)) }, search_flags } } else { return None },
        })
    }

    #[must_use]
    fn matches(&self, kv: NbtElementAndKeyRef) -> bool {
        let flags = self.search_flags as u8 + 1;
        match &self.inner {
            SearchPredicateInner::String(matcher) => {
                let (value, color) = kv.1.value();
                ((flags & 0b01) > 0 && color != TextColor::TreeKey && value.contains(matcher)) || ((flags & 0b10) > 0 && kv.0.is_some_and(|k| k.contains(matcher)))
            }
            SearchPredicateInner::StringCaseInsensitive(matcher) => {
                let (value, color) = kv.1.value();
                ((flags & 0b01) > 0 && color != TextColor::TreeKey && value.contains_ignore_ascii_case(matcher)) || ((flags & 0b10) > 0 && kv.0.is_some_and(|k| k.contains_ignore_ascii_case(matcher)))
            }
            SearchPredicateInner::Regex(regex) => {
                let (value, color) = kv.1.value();
                ((flags & 0b01) > 0 && color != TextColor::TreeKey && regex.is_match(&value)) || ((flags & 0b10) > 0 && kv.0.is_some_and(|k| regex.is_match(k)))
            }
            SearchPredicateInner::Snbt((k, element)) => {
                ((flags & 0b01) == 0 || element.matches(kv.1)) && ((flags & 0b10) == 0 || k.as_ref().map(|k| k.as_str()) == kv.0)
            }
            SearchPredicateInner::SnbtExactMatch((k, element)) => {
                ((flags & 0b01) == 0 || element.eq(kv.1)) && ((flags & 0b10) == 0 || k.as_ref().map(|k| k.as_str()) == kv.0)
            }
        }
    }
}

#[derive(Clone, Eq)]
pub struct SearchBoxCache {
    value: String,
    cursor: usize,
    selection: Option<usize>,
}

impl PartialEq for SearchBoxCache {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Cachelike<SearchBoxAdditional> for SearchBoxCache {
    fn new(text: &Text<SearchBoxAdditional, Self>) -> Self
    where
        Self: Sized,
    {
        Self {
            value: text.value.clone(),
            cursor: text.cursor,
            selection: text.selection,
        }
    }

    fn revert(self, text: &mut Text<SearchBoxAdditional, Self>)
    where
        Self: Sized,
    {
        text.value = self.value;
        text.cursor = self.cursor;
        text.selection = self.selection;
    }
}

#[derive(Clone)]
pub struct SearchBoxAdditional {
    selected: bool,
    pub horizontal_scroll: usize,
    pub last_interaction: (usize, Duration),
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
        Self(Text::new(String::new(), 0, true, SearchBoxAdditional { selected: false, horizontal_scroll: 0, last_interaction: (0, Duration::ZERO) }))
    }

    pub const fn uninit() -> Self {
        Self(Text::uninit())
    }

    pub fn render(&self, builder: &mut VertexBufferBuilder) {
        use std::fmt::Write;

        let search_mode = config::get_search_mode();
        let pos = Vec2u::new(SEARCH_BOX_START_X, 23);

        builder.draw_texture_region_z(
            pos,
            SEARCH_BOX_Z,
            DARK_STRIPE_UV,
            (builder.window_width() - SEARCH_BOX_END_X - pos.x, 22),
            (16, 16),
        );

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
        let color = match config::get_theme() {
            Theme::Light => TextColor::Black,
            Theme::Dark => TextColor::White
        };
        if self.is_selected() {
            self.0.render(builder, color, pos + (0, 3), SEARCH_BOX_Z, SEARCH_BOX_SELECTION_Z);
        } else {
            builder.settings(pos + (0, 3), false, SEARCH_BOX_Z);
            builder.color = color.to_raw();
            let _ = write!(builder, "{}", self.value);
        }

        builder.horizontal_scroll = 0;
    }

    #[must_use]
    pub fn is_within_bounds(mouse: (usize, usize), window_width: usize) -> bool {
        let (mouse_x, mouse_y) = mouse;
        let pos = Vec2u::new(SEARCH_BOX_START_X, 23);

        (pos.x..window_width - SEARCH_BOX_END_X).contains(&mouse_x) && (23..45).contains(&mouse_y)
    }

    pub fn deselect(&mut self) {
        self.selected = false;
        self.cursor = 0;
        self.selection = None;
    }

    pub fn select(&mut self, x: usize, button: MouseButton) {
        if button == MouseButton::Right {
            self.value.clear();
            self.cursor = 0;
            self.selection = None;
            self.horizontal_scroll = 0;
            self.0.post_input();
        } else {
            self.cursor = get_cursor_idx(&self.value, (x + self.horizontal_scroll) as isize);
            self.selection = None;
        }
        self.selected = true;
        self.interact();
    }

    #[must_use]
    pub fn search(&self, bookmarks: &mut MarkedLines, root: &NbtElement, count_only: bool) -> Notification {
        let start = now();
        let new_bookmarks = if self.value.is_empty() {
            MarkedLines::new()
        } else {
            let Some(predicate) = SearchPredicate::new(self.value.clone()) else { return Notification::new(format!("Invalid search syntax ({})", self.value), TextColor::Red, NotificationKind::Find) };
            Self::search0(root, &predicate)
        };
        let ms = now() - start;
        
        let hits = new_bookmarks.len();
        if !count_only {
            config::get_search_operation().apply(bookmarks, new_bookmarks);
        }
        Notification::new(format!("{hits} hit{s} for \"{arg}\" ({num_bookmarks} total bookmark{s2}) ({ms}ms)", s = if hits == 1 { "" } else { "s" }, arg = self.value, ms = ms.as_millis(), num_bookmarks = bookmarks.len(), s2 = if bookmarks.len() == 1 { "" } else { "s" }), TextColor::White, NotificationKind::Find)
    }

    pub fn search0(root: &NbtElement, predicate: &SearchPredicate) -> MarkedLines {
        let mut new_bookmarks = Vec::new();
        let mut queue = Vec::new();
        queue.push(((None, &*root), true));
        let mut true_line_number = 1;
        let mut line_number = 0;
        while let Some(((key, value), parent_open)) = queue.pop() {
            if predicate.matches((key, value)) {
                new_bookmarks.push(MarkedLine::with_uv(true_line_number, line_number, if parent_open { BOOKMARK_UV } else { HIDDEN_BOOKMARK_UV }));
            }
            
            match value.children() {
                Some(Ok(iter)) => for child in iter.rev() {
                    queue.push(((None, child), value.is_open()))
                }
                Some(Err(iter)) => for (key, child) in iter.rev() {
                    queue.push(((Some(key), child), value.is_open()))
                }
                None => {}
            }

            true_line_number += 1;
            if parent_open {
                line_number += 1;
            }
        }
        unsafe { MarkedLines::from_unchecked(new_bookmarks) }
    }

    #[must_use]
    pub fn is_selected(&self) -> bool {
        self.selected
    }

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
        if let KeyCode::ArrowDown | KeyCode::Tab = key && flags == flags!() {
            return SearchBoxKeyResult::MoveToReplaceBox;
        }

        if let KeyCode::Enter | KeyCode::NumpadEnter = key && flags == flags!(Shift) {
            return SearchBoxKeyResult::Search;
        }

        if let KeyCode::Enter | KeyCode::NumpadEnter = key && flags == flags!(Alt) {
            return SearchBoxKeyResult::SearchCountOnly;
        }

        self.0.on_key_press(key, char, flags).into()
    }
}
