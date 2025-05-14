use fxhash::FxHashSet;
use winit::event::MouseButton;
use crate::assets::{OPEN_FOLDER_UV, SELECTION_UV};
use crate::render::VertexBufferBuilder;
use crate::util::{AxisAlignedBoundingBox, Vec2u};
use crate::widget::{ButtonWidget, ButtonWidgetContext, ButtonWidgetContextMut};

pub struct OpenFileButton;

impl ButtonWidget for OpenFileButton {
    fn new() -> Self
    where
        Self: Sized
    {
        Self
    }

    fn bounds(&self, _window_dims: Vec2u) -> AxisAlignedBoundingBox {
        AxisAlignedBoundingBox::new(
            0,
            16,
            26,
            46,
        )
    }

    fn is_valid_mouse_button(button: MouseButton) -> bool {
        matches!(button, MouseButton::Left)
    }

    fn on_mouse_up(&mut self, _button: MouseButton, _ctx: &mut ButtonWidgetContextMut) -> bool {
        false
    }

    fn on_mouse_down(&mut self, _button: MouseButton, ctx: &mut ButtonWidgetContextMut) -> bool {
        ctx.open_file_request();
        true
    }

    fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: Vec2u, _ctx: &ButtonWidgetContext, _held_mouse_keys: &FxHashSet<MouseButton>) {
        let aabb = self.bounds(window_dims);
        let is_within_bounds = aabb.contains(mouse);
        
        builder.draw_texture(aabb.low(), OPEN_FOLDER_UV, (16, 16));
        if is_within_bounds {
            builder.draw_texture(aabb.low(), SELECTION_UV, (16, 16));
            builder.draw_tooltip(&["Open File (Ctrl + O)"], mouse, false);
        }
    }
}
