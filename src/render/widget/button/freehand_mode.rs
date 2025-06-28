use fxhash::FxHashSet;
use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::{
	action_result::ActionResult,
	render::{
		assets::{ENABLED_FREEHAND_MODE_UV, FREEHAND_MODE_UV},
		color::TextColor,
		vertex_buffer_builder::VertexBufferBuilder,
		widget::{Widget, WidgetContext, WidgetContextMut},
	},
	util::{AxisAlignedBoundingBox, Vec2u},
};

pub struct FreehandModeButton;

impl Widget for FreehandModeButton {
	fn new() -> Self
	where Self: Sized {
		Self
	}

	fn bounds(&self, _window_dims: PhysicalSize<u32>) -> AxisAlignedBoundingBox { AxisAlignedBoundingBox::new(264, 280, 26, 42) }

	fn is_valid_mouse_button(button: MouseButton) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }

	fn on_mouse_down(&mut self, _button: MouseButton, ctx: &mut WidgetContextMut) -> ActionResult {
		let tab = ctx.tabs.active_tab_mut();
		tab.freehand_mode = !tab.freehand_mode;
		ActionResult::Success(())
	}

	fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: PhysicalSize<u32>, ctx: &WidgetContext, _held_mouse_keys: &FxHashSet<MouseButton>) {
		let aabb = self.bounds(window_dims);
		let is_within_bounds = aabb.contains(mouse);
		let freehand_mode = ctx.tabs.active_tab().freehand_mode;
		let uv = if freehand_mode || is_within_bounds { ENABLED_FREEHAND_MODE_UV } else { FREEHAND_MODE_UV };
		if is_within_bounds {
			builder.color = TextColor::White.to_raw();
			builder.draw_tooltip(&["Freehand Mode (Ctrl + Shift + F)"], mouse, false);
		}
		builder.draw_texture(aabb.low(), uv, (16, 16));
	}
}
