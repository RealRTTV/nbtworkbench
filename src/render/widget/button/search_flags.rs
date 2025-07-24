use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::action_result::ActionResult;
use crate::config;
use crate::render::assets::{BASE_Z, HOVERED_WIDGET_UV};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::search_box::SEARCH_BOX_END_X;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::util::{AABB, Vec2u};
use crate::workbench::mouse::MouseManager;

#[derive(Default, Copy, Clone)]
pub struct SearchFlagsButton;

impl Widget for SearchFlagsButton {
	fn alignment(&self) -> WidgetAlignment { WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Static(-(SEARCH_BOX_END_X as i32 + 16 + 16)), VerticalWidgetAlignmentPreference::Static(26)) }

	fn dimensions(&self, _containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> { PhysicalSize::new(16, 16) }
	fn is_valid_mouse_button(&self, button: MouseButton, _pos: Vec2u, _dims: PhysicalSize<u32>) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }
	fn on_mouse_down(&mut self, button: MouseButton, _pos: Vec2u, _dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		let reverse = ctx.shift ^ matches!(button, MouseButton::Right);
		config::set_search_flags(if reverse { config::get_search_flags().rev_cycle() } else { config::get_search_flags().cycle() });
		ActionResult::Success(())
	}

	fn render_at(&self, pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, _ctx: &WidgetContext) {
		let widget_uv = super::get_button_widget_uv(self, AABB::from_pos_and_dims(pos, dims), dims, mouse);
		let search_flags = config::get_search_flags();
		let uv = search_flags.uv();

		if widget_uv == HOVERED_WIDGET_UV {
			builder.color = TextColor::White.to_raw();
			builder.draw_tooltip(&[&search_flags.to_string()], mouse.coords, false);
		}

		builder.draw_texture_z(pos, BASE_Z, widget_uv, dims);
		builder.draw_texture_z(pos, BASE_Z, uv, dims);
	}
}
