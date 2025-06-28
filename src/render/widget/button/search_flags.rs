use fxhash::FxHashSet;
use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::{
	action_result::ActionResult,
	config,
	render::{
		assets::{BASE_Z, HOVERED_WIDGET_UV},
		color::TextColor,
		vertex_buffer_builder::VertexBufferBuilder,
		widget::{
			Widget, WidgetContext, WidgetContextMut,
			search_box::SEARCH_BOX_END_X,
		},
	},
	util::{AxisAlignedBoundingBox, Vec2u},
};

pub struct SearchFlagsButton;

impl Widget for SearchFlagsButton {
	fn new() -> Self
	where Self: Sized {
		Self
	}

	fn bounds(&self, window_dims: PhysicalSize<u32>) -> AxisAlignedBoundingBox { AxisAlignedBoundingBox::new(window_dims.width as usize - SEARCH_BOX_END_X - 17 - 16 - 16, window_dims.width as usize - SEARCH_BOX_END_X - 1 - 16 - 16, 26, 42) }

	fn is_valid_mouse_button(button: MouseButton) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }

	fn on_mouse_down(&mut self, button: MouseButton, ctx: &mut WidgetContextMut) -> ActionResult {
		let reverse = ctx.shift ^ matches!(button, MouseButton::Right);
		config::set_search_flags(if reverse { config::get_search_flags().rev_cycle() } else { config::get_search_flags().cycle() });
		ActionResult::Success(())
	}

	fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: PhysicalSize<u32>, _ctx: &WidgetContext, held_mouse_keys: &FxHashSet<MouseButton>) {
		let aabb = self.bounds(window_dims);
		let widget_uv = self.get_widget_uv(mouse, window_dims, held_mouse_keys);
		let search_flags = config::get_search_flags();
		let uv = search_flags.uv();

		if widget_uv == HOVERED_WIDGET_UV {
			builder.color = TextColor::White.to_raw();
			builder.draw_tooltip(&[&search_flags.to_string()], mouse, false);
		}

		builder.draw_texture_z(aabb.low(), BASE_Z, widget_uv, (16, 16));
		builder.draw_texture_z(aabb.low(), BASE_Z, uv, (16, 16));
	}
}
