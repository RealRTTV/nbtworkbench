use std::ops::{Deref, DerefMut};
use std::time::Duration;

use winit::keyboard::KeyCode;
use crate::assets::{ADD_SEARCH_BOOKMARKS, BOOKMARK_UV, DARK_STRIPE_UV, HIDDEN_BOOKMARK_UV, HOVERED_WIDGET_UV, REMOVE_SEARCH_BOOKMARKS, UNSELECTED_WIDGET_UV};

use crate::color::TextColor;
use crate::{Bookmark, combined_two_sorted, flags, since_epoch, StrExt};
use crate::elements::element::NbtElement;
use crate::text::{Cachelike, SearchBoxKeyResult, Text};
use crate::vertex_buffer_builder::{Vec2u, VertexBufferBuilder};

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
    hits: Option<(usize, Duration)>,
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

        let pos = Vec2u::new(284, 23);
        let (mouse_x, mouse_y) = mouse;

        builder.draw_texture_region_z(
            pos,
            0,
            DARK_STRIPE_UV,
            (builder.window_width() - 215 - pos.x - 17, 22),
            (16, 16),
        );

        let hover = (pos.x..builder.window_width() - 215 - 17).contains(&mouse_x) && (23..45).contains(&mouse_y);

        builder.horizontal_scroll = self.horizontal_scroll;

        if self.value.is_empty() {
            builder.settings(pos + (0, 3), false, 0);
            builder.color = TextColor::Gray.to_raw();
            let _ = write!(builder, "Search...");
        }
        if self.is_selected() {
            self.0.render(builder, TextColor::White, pos + (0, 3), 0);
        } else {
            builder.settings(pos + (0, 3), false, 0);
            builder.color = TextColor::White.to_raw();
            let _ = write!(builder, "{}", self.value);
        }

        if let Some((hits, stat)) = self.hits && (self.is_selected() || hover) {
            builder.draw_tooltip(&[&format!("{hits} hits for \"{arg}\" ({ms}ms)", arg = self.value, ms = stat.as_millis())], if !self.is_selected() && hover { mouse } else { (284, 30) });
        }

        builder.horizontal_scroll = 0;

        let bookmark_uv = if shift { REMOVE_SEARCH_BOOKMARKS } else { ADD_SEARCH_BOOKMARKS };
        let widget_uv = if (builder.window_width() - 215 - 17..builder.window_width() - 215 - 1).contains(&mouse_x) && (26..42).contains(&mouse_y) {
            builder.draw_tooltip(&[if shift { "Remove all bookmarks" } else { "Add search bookmarks" }], mouse);
            HOVERED_WIDGET_UV
        } else {
            UNSELECTED_WIDGET_UV
        };

        builder.draw_texture_z((builder.window_width() - 215 - 17, 26), 0, widget_uv, (16, 16));
        builder.draw_texture_z((builder.window_width() - 215 - 17, 26), 0, bookmark_uv, (16, 16));
    }

    #[inline]
    pub fn deselect(&mut self) {
        self.selected = false;
        self.cursor = 0;
        self.selection = None;
    }

    #[inline]
    pub fn select(&mut self, x: usize) {
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
        self.selected = true;
        self.interact();
    }

    #[inline]
    pub fn on_widget(&mut self, shift: bool, bookmarks: &mut Vec<Bookmark>, root: &mut NbtElement) {
        if shift {
            bookmarks.clear();
        } else {
            self.search(bookmarks, root, false);
        }
    }

    #[inline]
    pub fn search(&mut self, bookmarks: &mut Vec<Bookmark>, root: &mut NbtElement, count_only: bool) {
        if self.value.is_empty() {
            return;
        }

        let start = since_epoch();
        let mut new_bookmarks = Vec::new();
        let mut queue = Vec::new();
        queue.push((None, &*root, true));
        let mut true_line_number = 1;
        let mut line_number = 0;
        while let Some((key, element, parent_open)) = queue.pop() {
            let (value, color) = element.value();
            let value = if color == TextColor::TreeKey {
                None
            } else {
                Some(value)
            };

            if self.matches(key, value.as_deref()) {
                let mut bookmark = Bookmark::new(true_line_number, line_number);
                bookmark.uv = if parent_open { BOOKMARK_UV } else { HIDDEN_BOOKMARK_UV };
                new_bookmarks.push(bookmark);
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

        self.hits = Some((new_bookmarks.len(), since_epoch() - start));
        if !count_only {
            let old_bookmarks = core::mem::replace(bookmarks, vec![]);
            *bookmarks = combined_two_sorted(new_bookmarks.into_boxed_slice(), old_bookmarks.into_boxed_slice());
        }
    }

    #[inline]
    pub fn matches(&self, key: Option<&str>, value: Option<&str>) -> bool {
        key.is_some_and(|key| key.contains(&self.value)) || value.is_some_and(|value| value.contains(&self.value))
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
        let field_width = window_width - 215 - 284 - 17;
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
