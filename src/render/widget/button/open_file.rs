use fxhash::FxHashSet;
use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::{
	action_result::ActionResult,
	render::{
		assets::{OPEN_FOLDER_UV, SELECTION_UV},
		vertex_buffer_builder::VertexBufferBuilder,
		widget::{Widget, WidgetContext, WidgetContextMut},
	},
	util::{AxisAlignedBoundingBox, Vec2u},
};

pub struct OpenFileButton;

impl Widget for OpenFileButton {
	fn new() -> Self
	where Self: Sized {
		Self
	}

	fn bounds(&self, _window_dims: PhysicalSize<u32>) -> AxisAlignedBoundingBox { AxisAlignedBoundingBox::new(0, 16, 26, 46) }

	fn is_valid_mouse_button(button: MouseButton) -> bool { matches!(button, MouseButton::Left) }

	fn on_mouse_down(&mut self, _button: MouseButton, ctx: &mut WidgetContextMut) -> ActionResult {
		ctx.open_file_request();
		ActionResult::Success(())
	}

	fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: PhysicalSize<u32>, _ctx: &WidgetContext, _held_mouse_keys: &FxHashSet<MouseButton>) {
		let aabb = self.bounds(window_dims);
		let is_within_bounds = aabb.contains(mouse);

		builder.draw_texture(aabb.low(), OPEN_FOLDER_UV, (16, 16));
		if is_within_bounds {
			builder.draw_texture(aabb.low(), SELECTION_UV, (16, 16));
			builder.draw_tooltip(&["Open File (Ctrl + O)"], mouse, false);
		}
	}
}
