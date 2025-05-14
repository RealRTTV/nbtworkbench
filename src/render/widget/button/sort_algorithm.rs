use fxhash::FxHashSet;
use winit::event::MouseButton;
use crate::assets::{BASE_Z, HOVERED_WIDGET_UV, SORT_COMPOUND_BY_NAME_UV, SORT_COMPOUND_BY_NOTHING_UV, SORT_COMPOUND_BY_TYPE_UV};
use crate::config;
use crate::render::{TextColor, VertexBufferBuilder};
use crate::util::{AxisAlignedBoundingBox, Vec2u};
use crate::widget::{ButtonWidget, ButtonWidgetContext, ButtonWidgetContextMut};
use crate::workbench::SortAlgorithm;

pub struct SortAlgorithmButton;

impl ButtonWidget for SortAlgorithmButton {
    fn new() -> Self
    where
        Self: Sized
    {
        Self
    }

    fn bounds(&self, _window_dims: Vec2u) -> AxisAlignedBoundingBox {
        AxisAlignedBoundingBox::new(
            280,
            296,
            26,
            42
        )
    }

    fn is_valid_mouse_button(button: MouseButton) -> bool {
        matches!(button, MouseButton::Left | MouseButton::Right)
    }

    fn on_mouse_up(&mut self, _button: MouseButton, _ctx: &mut ButtonWidgetContextMut) -> bool { false }

    fn on_mouse_down(&mut self, button: MouseButton, ctx: &mut ButtonWidgetContextMut) -> bool {
        let sort_algorithm = config::get_sort_algorithm();
        let reverse = matches!(button, MouseButton::Right) ^ ctx.shift;
        config::set_sort_algorithm(if reverse { sort_algorithm.rev_cycle() } else { sort_algorithm.cycle() });
        true
    }

    fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: Vec2u, _ctx: &ButtonWidgetContext, held_mouse_keys: &FxHashSet<MouseButton>) {
        let aabb = self.bounds(window_dims);
        let widget_uv = self.get_widget_uv(mouse, window_dims, held_mouse_keys);
        let sort_algorithm = config::get_sort_algorithm();
        let uv = match sort_algorithm {
            SortAlgorithm::None => SORT_COMPOUND_BY_NOTHING_UV,
            SortAlgorithm::Name => SORT_COMPOUND_BY_NAME_UV,
            SortAlgorithm::Type => SORT_COMPOUND_BY_TYPE_UV,
        };

        if widget_uv == HOVERED_WIDGET_UV {
            builder.color = TextColor::White.to_raw();
            builder.draw_tooltip(&[&format!("Compound Sorting Algorithm ({sort_algorithm})")], mouse, false);
        }

        builder.draw_texture_z(aabb.low(), BASE_Z, widget_uv, (16, 16));
        builder.draw_texture_z(aabb.low() + (3, 3), BASE_Z, uv, (10, 10));
    }
}