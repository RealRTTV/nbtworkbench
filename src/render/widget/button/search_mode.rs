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
			{Widget, WidgetContext, WidgetContextMut},
			search_box::{SEARCH_BOX_END_X, SearchMode},
		},
	},
	util::{AxisAlignedBoundingBox, Vec2u},
};

pub struct SearchModeButton;

impl Widget for SearchModeButton {
	fn new() -> Self
	where Self: Sized {
		Self
	}

	fn bounds(&self, window_dims: PhysicalSize<u32>) -> AxisAlignedBoundingBox { AxisAlignedBoundingBox::new(window_dims.width as usize - SEARCH_BOX_END_X - 17 - 16, window_dims.width as usize - SEARCH_BOX_END_X - 1 - 16, 26, 42) }

	fn is_valid_mouse_button(button: MouseButton) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }

	fn on_mouse_down(&mut self, button: MouseButton, ctx: &mut WidgetContextMut) -> ActionResult {
		let reverse = ctx.shift ^ matches!(button, MouseButton::Right);
		config::set_search_mode(if reverse { config::get_search_mode().rev_cycle() } else { config::get_search_mode().cycle() });
		ActionResult::Success(())
	}

	fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: PhysicalSize<u32>, _ctx: &WidgetContext, held_mouse_keys: &FxHashSet<MouseButton>) {
		let aabb = self.bounds(window_dims);
		let widget_uv = self.get_widget_uv(mouse, window_dims, held_mouse_keys);
		let search_mode = config::get_search_mode();
		let replace_by = config::get_replace_by();
		let uv = search_mode.uv();

		if widget_uv == HOVERED_WIDGET_UV {
			builder.color = TextColor::White.to_raw();
			let binding = format!("{search_mode} Mode");
			let non_regex_warning_line = [&binding, "Regex $ replacement syntax is disabled on Bookmarked Lines replacement"];
			let regular_line = [binding.as_str()];
			builder.draw_tooltip(
				if let SearchMode::Regex = search_mode
					&& !replace_by.can_use_regex()
				{
					&non_regex_warning_line
				} else {
					&regular_line
				},
				mouse,
				false,
			);
		}

		builder.draw_texture_z(aabb.low(), BASE_Z, widget_uv, (16, 16));
		builder.draw_texture_z(aabb.low(), BASE_Z, uv, (16, 16));
	}
}
