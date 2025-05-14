use fxhash::FxHashSet;
use winit::event::MouseButton;
use crate::assets::{NEW_FILE_UV, SELECTION_UV};
use crate::render::VertexBufferBuilder;
use crate::util::{AxisAlignedBoundingBox, Vec2u};
use crate::widget::{ButtonWidget, ButtonWidgetContext, ButtonWidgetContextMut};
use crate::workbench::Tab;

pub struct NewTabButton;

impl ButtonWidget for NewTabButton {
    fn new() -> Self
    where
        Self: Sized
    {
        Self
    }

    fn bounds(&self, _window_dims: Vec2u) -> AxisAlignedBoundingBox {
        AxisAlignedBoundingBox::new(
            16,
            32,
            26,
            46,
        )
    }

    fn is_valid_mouse_button(button: MouseButton) -> bool {
        matches!(button, MouseButton::Left)
    }

    fn on_mouse_up(&mut self, _button: MouseButton, _ctx: &mut ButtonWidgetContextMut) -> bool { false }

    fn on_mouse_down(&mut self, _button: MouseButton, ctx: &mut ButtonWidgetContextMut) -> bool {
        let shift = ctx.shift;
        let window_dims = ctx.tab.window_dims();
        ctx.tab(Tab::new_empty_tab(shift, window_dims));
        true
    }

    fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: Vec2u, ctx: &ButtonWidgetContext, _held_mouse_keys: &FxHashSet<MouseButton>) {
        let bounds = self.bounds(window_dims);
        let is_within_bounds = bounds.contains(mouse);

        builder.draw_texture(bounds.low(), NEW_FILE_UV, (16, 16));
        if is_within_bounds {
            builder.draw_texture(bounds.low(), SELECTION_UV, (16, 16));
            if ctx.shift {
                builder.draw_tooltip(&["Create New Region File (Ctrl + Shift + N)"], mouse, false);
            } else {
                builder.draw_tooltip(&["Create New NBT File (Ctrl + N)"], mouse, false);
            }
        }
    }
}
