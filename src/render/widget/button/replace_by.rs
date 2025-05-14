use fxhash::FxHashSet;
use winit::event::MouseButton;
use crate::assets::{HOVERED_WIDGET_UV, REPLACE_BOX_SELECTION_Z};
use crate::config;
use crate::render::{TextColor, VertexBufferBuilder};
use crate::util::{AxisAlignedBoundingBox, Vec2u};
use crate::widget::{ButtonWidget, ButtonWidgetContext, ButtonWidgetContextMut, SEARCH_BOX_END_X};

pub struct ReplaceByButton;

impl ButtonWidget for ReplaceByButton {
    fn new() -> Self
    where
        Self: Sized
    {
        Self
    }

    fn bounds(&self, window_dims: Vec2u) -> AxisAlignedBoundingBox {
        AxisAlignedBoundingBox::new(window_dims.x - SEARCH_BOX_END_X - 17, window_dims.x - SEARCH_BOX_END_X - 1, 50, 66)
    }

    fn is_valid_mouse_button(button: MouseButton) -> bool {
        matches!(button, MouseButton::Left | MouseButton::Right)
    }

    fn on_mouse_up(&mut self, _button: MouseButton, _ctx: &mut ButtonWidgetContextMut) -> bool { false }

    fn on_mouse_down(&mut self, button: MouseButton, ctx: &mut ButtonWidgetContextMut) -> bool {
        let reverse = matches!(button, MouseButton::Right) ^ ctx.shift;
        config::set_replace_by(if reverse { config::get_replace_by().rev_cycle() } else { config::get_replace_by().cycle() });
        true
    }

    fn is_clickable(&self, ctx: &ButtonWidgetContext) -> bool {
        ctx.search_box.is_selected() || ctx.replace_box.is_selected()
    }
    
    fn is_visible(&self, ctx: &ButtonWidgetContext) -> bool {
        ctx.search_box.is_selected() || ctx.replace_box.is_selected()
    }

    fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: Vec2u, _ctx: &ButtonWidgetContext, held_mouse_keys: &FxHashSet<MouseButton>) {
        let aabb = self.bounds(window_dims);
        let widget_uv = self.get_widget_uv(mouse, window_dims, held_mouse_keys);
        let replace_by = config::get_replace_by();
        let uv = replace_by.uv();

        if widget_uv == HOVERED_WIDGET_UV {
            builder.color = TextColor::White.to_raw();
            builder.draw_tooltip(&[&format!("Replace by {replace_by}")], mouse, false);
        }

        builder.draw_texture_z(aabb.low(), REPLACE_BOX_SELECTION_Z, widget_uv, (16, 16));
        builder.draw_texture_z(aabb.low(), REPLACE_BOX_SELECTION_Z, uv, (16, 16));
    }
}