use fxhash::FxHashSet;
use winit::event::MouseButton;

use crate::assets::{ENABLED_FREEHAND_MODE_UV, FREEHAND_MODE_UV};
use crate::render::{TextColor, VertexBufferBuilder};
use crate::util::{AxisAlignedBoundingBox, Vec2u};
use crate::widget::{ButtonWidget, ButtonWidgetContext, ButtonWidgetContextMut};

pub struct FreehandModeButton;

impl ButtonWidget for FreehandModeButton {
	fn new() -> Self
	where Self: Sized {
		Self
	}

	fn bounds(&self, _window_dims: Vec2u) -> AxisAlignedBoundingBox { AxisAlignedBoundingBox::new(264, 280, 26, 42) }

	fn is_valid_mouse_button(button: MouseButton) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }

	fn on_mouse_up(&mut self, _button: MouseButton, _ctx: &mut ButtonWidgetContextMut) -> bool { false }

	fn on_mouse_down(&mut self, _button: MouseButton, ctx: &mut ButtonWidgetContextMut) -> bool {
		ctx.tab.freehand_mode = !ctx.tab.freehand_mode;
		true
	}

	fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: Vec2u, ctx: &ButtonWidgetContext, _held_mouse_keys: &FxHashSet<MouseButton>) {
		let aabb = self.bounds(window_dims);
		let is_within_bounds = aabb.contains(mouse);
		let freehand_mode = ctx.tab.freehand_mode;
		let uv = if freehand_mode || is_within_bounds { ENABLED_FREEHAND_MODE_UV } else { FREEHAND_MODE_UV };
		if is_within_bounds {
			builder.color = TextColor::White.to_raw();
			builder.draw_tooltip(&["Freehand Mode (Ctrl + Shift + F)"], mouse, false);
		}
		builder.draw_texture(aabb.low(), uv, (16, 16));
	}
}
