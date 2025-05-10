use fxhash::FxHashSet;
use winit::event::MouseButton;
use winit::window::Theme;
use crate::assets::{DIM_LIGHTBULB_UV, LIGHTBULB_UV};
use crate::config;
use crate::render::{TextColor, VertexBufferBuilder};
use crate::util::{AxisAlignedBoundingBox, Vec2u};
use crate::widget::{ButtonWidget, ButtonWidgetContext, ButtonWidgetContextMut};

pub struct ThemeButton;

impl ButtonWidget for ThemeButton {
    fn new() -> Self
    where
        Self: Sized
    {
        Self
    }

    fn bounds(_window_dims: Vec2u) -> AxisAlignedBoundingBox {
        AxisAlignedBoundingBox::new(
            312,
            328,
            26,
            42
        )
    }

    fn is_valid_mouse_button(button: MouseButton) -> bool {
        matches!(button, MouseButton::Left | MouseButton::Right)
    }

    fn on_mouse_up(&mut self, _button: MouseButton, _ctx: &mut ButtonWidgetContextMut) -> bool {
        config::set_theme(match config::get_theme() { Theme::Light => Theme::Dark, Theme::Dark => Theme::Light });
        true
    }

    fn on_mouse_down(&mut self, _button: MouseButton, _ctx: &mut ButtonWidgetContextMut) -> bool { false }

    fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: Vec2u, _ctx: &ButtonWidgetContext, _held_mouse_keys: &FxHashSet<MouseButton>) {
        let aabb = Self::bounds(window_dims);
        let is_within_bounds = aabb.contains(mouse);
        if is_within_bounds {
            builder.color = TextColor::White.to_raw();
            builder.draw_tooltip(&["Change Theme (Ctrl + Alt + T)"], mouse, false);
        }
        builder.draw_texture(aabb.low(), if is_within_bounds { DIM_LIGHTBULB_UV } else { LIGHTBULB_UV }, (16, 16));
    }
}