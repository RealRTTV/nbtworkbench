use fxhash::FxHashSet;
use winit::event::MouseButton;
use crate::assets::{BASE_Z, HOVERED_WIDGET_UV};
use crate::config;
use crate::render::VertexBufferBuilder;
use crate::util::{AxisAlignedBoundingBox, Vec2u};
use crate::widget::{ButtonWidget, ButtonWidgetContext, ButtonWidgetContextMut, SEARCH_BOX_END_X};

pub struct SearchModeButton;

impl ButtonWidget for SearchModeButton {
    fn new() -> Self
    where
        Self: Sized
    {
        Self
    }

    fn bounds(window_dims: Vec2u) -> AxisAlignedBoundingBox {
        AxisAlignedBoundingBox::new(window_dims.x - SEARCH_BOX_END_X - 17 - 16, window_dims.x - SEARCH_BOX_END_X - 1 - 16, 26, 42)
    }

    fn is_valid_mouse_button(button: MouseButton) -> bool {
        matches!(button, MouseButton::Left | MouseButton::Right)
    }

    fn on_mouse_up(&mut self, _button: MouseButton, _ctx: &mut ButtonWidgetContextMut) -> bool { false }

    fn on_mouse_down(&mut self, button: MouseButton, ctx: &mut ButtonWidgetContextMut) -> bool {
        let reverse = ctx.shift ^ matches!(button, MouseButton::Right);
        config::set_search_mode(if reverse { config::get_search_mode().rev_cycle() } else { config::get_search_mode().cycle() });
        true
    }

    fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: Vec2u, _ctx: &ButtonWidgetContext, held_mouse_keys: &FxHashSet<MouseButton>) {
        let aabb = Self::bounds(window_dims);
        let widget_uv = self.get_widget_uv(mouse, window_dims, held_mouse_keys);
        let search_mode = config::get_search_mode();
        let uv = search_mode.uv();

        if widget_uv == HOVERED_WIDGET_UV {
            builder.draw_tooltip(&[&format!("{search_mode} Mode")], mouse, false);
        }

        builder.draw_texture_z(aabb.low(), BASE_Z, widget_uv, (16, 16));
        builder.draw_texture_z(aabb.low(), BASE_Z, uv, (16, 16));
    }
}
