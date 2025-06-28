use fxhash::FxHashSet;
use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::{
	action_result::ActionResult,
	render::{
		assets::{NEW_FILE_UV, SELECTION_UV},
		vertex_buffer_builder::VertexBufferBuilder,
		widget::{Widget, WidgetContext, WidgetContextMut},
	},
	util::{AxisAlignedBoundingBox, Vec2u},
	workbench::tab::Tab,
};

pub struct NewTabButton;

impl Widget for NewTabButton {
	fn new() -> Self
	where Self: Sized {
		Self
	}

	fn bounds(&self, _window_dims: PhysicalSize<u32>) -> AxisAlignedBoundingBox { AxisAlignedBoundingBox::new(16, 32, 26, 46) }

	fn is_valid_mouse_button(button: MouseButton) -> bool { matches!(button, MouseButton::Left) }

	fn on_mouse_down(&mut self, _button: MouseButton, ctx: &mut WidgetContextMut) -> ActionResult {
		let shift = ctx.shift;
		let window_dims = ctx.tabs.active_tab().window_dims;
		ctx.tabs.add(Tab::new_empty_tab(shift, window_dims));
		ActionResult::Success(())
	}

	fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: PhysicalSize<u32>, ctx: &WidgetContext, _held_mouse_keys: &FxHashSet<MouseButton>) {
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
