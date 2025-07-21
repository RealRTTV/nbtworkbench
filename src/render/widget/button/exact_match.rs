use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::action_result::ActionResult;
use crate::config;
use crate::render::assets::{BASE_Z, EXACT_MATCH_OFF_UV, EXACT_MATCH_ON_UV, HOVERED_WIDGET_UV};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::search_box::SEARCH_BOX_END_X;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::util::{AABB, Vec2u};
use crate::workbench::mouse::MouseManager;

#[derive(Default, Copy, Clone)]
pub struct ExactMatchButton;

impl Widget for ExactMatchButton {
	fn alignment(&self) -> WidgetAlignment { WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Static(-(SEARCH_BOX_END_X as i32)), VerticalWidgetAlignmentPreference::Static(26)) }

	fn dimensions(&self, _containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> { PhysicalSize::new(16, 16) }

	fn is_valid_mouse_button(&self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }
	
	fn on_mouse_down(&mut self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		if !config::get_search_mode().has_exact_match_mode() {
			return ActionResult::Pass
		}

		config::set_search_exact_match(!config::get_search_exact_match());
		ActionResult::Success(())
	}

	fn render_at(&self, pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, ctx: &WidgetContext) {
		let widget_uv = super::get_button_widget_uv(self, AABB::from_pos_and_dims(pos, dims), dims, mouse);
		let exact_match = config::get_search_exact_match();
		let search_mode = config::get_search_mode();
		let has_exact_match = search_mode.has_exact_match_mode();
		let uv = if exact_match || !has_exact_match { EXACT_MATCH_ON_UV } else { EXACT_MATCH_OFF_UV };

		if widget_uv == HOVERED_WIDGET_UV {
			builder.color = TextColor::White.to_raw();
			builder.draw_tooltip(
				&[if exact_match || !has_exact_match {
					search_mode.get_exact_search_on_name()
				} else {
					search_mode.get_exact_search_off_name()
				}],
				mouse.coords,
				false,
			);
		}

		builder.draw_texture_z(pos, BASE_Z, widget_uv, dims);
		builder.draw_texture_z(pos, BASE_Z, uv, dims);
	}
}
