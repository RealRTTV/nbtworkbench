use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::action_result::ActionResult;
use crate::config;
use crate::render::assets::{HOVERED_WIDGET_UV, REPLACE_BOX_SELECTION_Z};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::search_box::SEARCH_BOX_END_X;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::util::{AABB, Vec2u};
use crate::workbench::mouse::MouseManager;

#[derive(Default, Copy, Clone)]
pub struct ReplaceByButton;

impl Widget for ReplaceByButton {
	fn alignment(&self) -> WidgetAlignment { WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Static(-(SEARCH_BOX_END_X as i32)), VerticalWidgetAlignmentPreference::Static(50)) }

	fn dimensions(&self, _containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> { PhysicalSize::new(16, 16) }
	fn is_valid_mouse_button(&self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }
	fn on_mouse_down(&mut self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		if !(ctx.search_box.is_selected() || ctx.replace_box.is_selected()) {
			return ActionResult::Pass
		}

		let reverse = matches!(button, MouseButton::Right) ^ ctx.shift;
		config::set_replace_by(if reverse { config::get_replace_by().rev_cycle() } else { config::get_replace_by().cycle() });
		ActionResult::Success(())
	}
	fn is_visible(&self, ctx: &WidgetContext) -> bool { ctx.search_box.is_selected() || ctx.replace_box.is_selected() }

	fn render_at(&self, pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, ctx: &WidgetContext) {
		let widget_uv = super::get_button_widget_uv(self, AABB::from_pos_and_dims(pos, dims), dims, mouse);
		let replace_by = config::get_replace_by();
		let uv = replace_by.uv();

		if widget_uv == HOVERED_WIDGET_UV {
			builder.color = TextColor::White.to_raw();
			builder.draw_tooltip(&[&format!("Replace by {replace_by}")], mouse.coords, false);
		}

		builder.draw_texture_z(pos, REPLACE_BOX_SELECTION_Z, widget_uv, dims);
		builder.draw_texture_z(pos, REPLACE_BOX_SELECTION_Z, uv, dims);
	}
}
