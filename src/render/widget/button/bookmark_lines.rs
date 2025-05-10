use fxhash::FxHashSet;
use winit::event::MouseButton;
use crate::assets::{BASE_Z, HOVERED_WIDGET_UV, SEARCH_APPEND_BOOKMARKS_UV, SEARCH_BOOKMARKS_UV};
use crate::render::{TextColor, VertexBufferBuilder};
use crate::util::{AxisAlignedBoundingBox, Vec2u};
use crate::widget::{ButtonWidget, ButtonWidgetContext, ButtonWidgetContextMut, SEARCH_BOX_END_X};

pub struct BookmarkLinesButton;

impl ButtonWidget for BookmarkLinesButton {
    fn new() -> Self
    where
        Self: Sized
    {
        Self
    }

    fn bounds(window_dims: Vec2u) -> AxisAlignedBoundingBox {
        AxisAlignedBoundingBox::new(
            window_dims.x - SEARCH_BOX_END_X - 17 - 16 - 16 - 16,
            window_dims.x - SEARCH_BOX_END_X - 1 - 16 - 16 - 16,
            26,
            42
        )
    }

    fn is_valid_mouse_button(button: MouseButton) -> bool {
        matches!(button, MouseButton::Left | MouseButton::Right)
    }

    fn on_mouse_up(&mut self, _button: MouseButton, _ctx: &mut ButtonWidgetContextMut) -> bool { false }

    fn on_mouse_down(&mut self, _button: MouseButton, ctx: &mut ButtonWidgetContextMut) -> bool {
        if !ctx.shift {
            ctx.tab.bookmarks.clear();
        }

        let notification = ctx.search_box.search(&mut ctx.tab.bookmarks, &mut ctx.tab.value, false);
        ctx.notify(notification);
        true
    }

    fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: Vec2u, ctx: &ButtonWidgetContext, held_mouse_keys: &FxHashSet<MouseButton>) {
        let aabb = Self::bounds(window_dims);
        let widget_uv = self.get_widget_uv(mouse, window_dims, held_mouse_keys);
        let uv = if ctx.shift { SEARCH_APPEND_BOOKMARKS_UV } else { SEARCH_BOOKMARKS_UV };
        
        if widget_uv == HOVERED_WIDGET_UV {
            builder.color = TextColor::White.to_raw();
            builder.draw_tooltip(&[if ctx.shift { "Append to search (Shift + Enter)" } else { "Search (Enter)" }], mouse, false);
        }

        builder.draw_texture_z(aabb.low(), BASE_Z, widget_uv, (16, 16));
        builder.draw_texture_z(aabb.low(), BASE_Z, uv, (16, 16));
    }
}
