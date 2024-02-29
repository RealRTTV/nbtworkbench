use std::ops::{Deref, DerefMut};

use winit::keyboard::KeyCode;
use crate::assets::{ADD_SEARCH_BOOKMARKS, DARK_STRIPE_UV, HOVERED_WIDGET_UV, REMOVE_SEARCH_BOOKMARKS, SELECTED_WIDGET_UV, UNSELECTED_WIDGET_UV};

use crate::color::TextColor;
use crate::{Bookmark, RenderContext, StrExt};
use crate::elements::element::NbtElement;
use crate::text::{Cachelike, KeyResult, Text};
use crate::vertex_buffer_builder::{Vec2u, VertexBufferBuilder};

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
    fn new(text: &Text<SearchBoxAdditional, Self>) -> Self where Self: Sized {
        Self {
            value: text.value.clone(),
            cursor: text.cursor,
            selection: text.selection,
        }
    }

    fn revert(self, text: &mut Text<SearchBoxAdditional, Self>) where Self: Sized {
        text.value = self.value;
        text.cursor = self.cursor;
        text.selection = self.selection;
    }
}

#[derive(Clone)]
pub struct SearchBoxAdditional {
    selected: bool,
    horizontal_scroll: usize,
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
        Self(Text::new(String::new(), 0, true, SearchBoxAdditional { selected: false, horizontal_scroll: 0 }))
    }

    pub const fn uninit() -> Self {
        Self(Text::uninit())
    }

    pub fn render(&self, builder: &mut VertexBufferBuilder, shift: bool, pos: (usize, usize)) {
        use std::fmt::Write;

        let (mouse_x, mouse_y) = pos;
        let pos = Vec2u::new(284, 23);

        builder.draw_texture_region_z(
            pos,
            0,
            DARK_STRIPE_UV,
            (builder.window_width() - 215 - pos.x - 16, 22),
            (16, 16),
        );

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
        builder.horizontal_scroll = 0;

        let bookmark_uv = if shift { REMOVE_SEARCH_BOOKMARKS } else { ADD_SEARCH_BOOKMARKS };
        let widget_uv = if (builder.window_width() - 215 - 16..builder.window_width() - 215).contains(&mouse_x) && (26..42).contains(&mouse_y) { HOVERED_WIDGET_UV } else { UNSELECTED_WIDGET_UV };

        builder.draw_texture_z((builder.window_width() - 215 - 16, 26), 0, widget_uv, (16, 16));
        builder.draw_texture_z((builder.window_width() - 215 - 16, 26), 0, bookmark_uv, (16, 16));
    }

    #[inline]
    pub fn deselect(&mut self) {
        self.selected = false;
        self.cursor = 0;
        self.selection = None;
        // better ui this way
        // self.horizontal_scroll = 0;
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
    pub fn search(&mut self, root: &NbtElement) -> Vec<Bookmark> {
        let mut bookmarks = Vec::new();
        let target = &*self.value;
        root.search(|k, v| k.is_some_and(|k| k == target) || v.is_some_and(|v| v == target), 1, 0, &mut bookmarks);
        bookmarks
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
        let field_width = window_width - 215 - 284;
        let precursor_width = self.value.split_at(self.cursor).0.width();
        // 8px space just to look cleaner
        let horizontal_scroll = (precursor_width + 8).saturating_sub(field_width);
        self.horizontal_scroll = horizontal_scroll;
    }

    #[must_use]
    pub fn on_key_press(&mut self, key: KeyCode, char: Option<char>, flags: u8) -> KeyResult {
        self.0.on_key_press(key, char, flags)
    }
}
