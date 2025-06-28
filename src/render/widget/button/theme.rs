use fxhash::FxHashSet;
use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::{
	action_result::ActionResult,
	config,
	render::{
		assets::{DIM_LIGHTBULB_UV, LIGHTBULB_UV},
		color::TextColor,
		vertex_buffer_builder::VertexBufferBuilder,
		widget::{Widget, WidgetContext, WidgetContextMut},
		window::Theme,
	},
	util::{AxisAlignedBoundingBox, Vec2u},
};

pub struct ThemeButton;

impl Widget for ThemeButton {
	fn new() -> Self
	where Self: Sized {
		Self
	}

	fn bounds(&self, _window_dims: PhysicalSize<u32>) -> AxisAlignedBoundingBox { AxisAlignedBoundingBox::new(312, 328, 26, 42) }

	fn is_valid_mouse_button(button: MouseButton) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }

	fn on_mouse_down(&mut self, _button: MouseButton, _ctx: &mut WidgetContextMut) -> ActionResult {
		config::set_theme(match config::get_theme() {
			Theme::Light => Theme::Dark,
			Theme::Dark => Theme::Light,
		});
		ActionResult::Success(())
	}

	fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: PhysicalSize<u32>, _ctx: &WidgetContext, _held_mouse_keys: &FxHashSet<MouseButton>) {
		let aabb = self.bounds(window_dims);
		let is_within_bounds = aabb.contains(mouse);
		if is_within_bounds {
			builder.color = TextColor::White.to_raw();
			builder.draw_tooltip(&["Change Theme (Ctrl + Alt + T)"], mouse, false);
		}
		builder.draw_texture(aabb.low(), if is_within_bounds { DIM_LIGHTBULB_UV } else { LIGHTBULB_UV }, (16, 16));
	}
}
