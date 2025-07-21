use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::action_result::ActionResult;
use crate::config;
use crate::render::assets::{BASE_Z, HOVERED_WIDGET_UV};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::search_box::{SEARCH_BOX_END_X, SearchMode};
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::util::{AABB, Vec2u};
use crate::workbench::mouse::MouseManager;

#[derive(Default, Copy, Clone)]
pub struct SearchModeButton;

impl Widget for SearchModeButton {
	fn alignment(&self) -> WidgetAlignment { WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Static(-(SEARCH_BOX_END_X as i32 + 16)), VerticalWidgetAlignmentPreference::Static(26)) }

	fn dimensions(&self, _containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> { PhysicalSize::new(16, 16) }
	fn is_valid_mouse_button(&self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }
	fn on_mouse_down(&mut self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		let reverse = ctx.shift ^ matches!(button, MouseButton::Right);
		config::set_search_mode(if reverse { config::get_search_mode().rev_cycle() } else { config::get_search_mode().cycle() });
		ActionResult::Success(())
	}

	fn render_at(&self, pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, ctx: &WidgetContext) {
		let widget_uv = super::get_button_widget_uv(self, AABB::from_pos_and_dims(pos, dims), dims, mouse);
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
				mouse.coords,
				false,
			);
		}

		builder.draw_texture_z(pos, BASE_Z, widget_uv, dims);
		builder.draw_texture_z(pos, BASE_Z, uv, dims);
	}
}
