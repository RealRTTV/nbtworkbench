use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::action_result::ActionResult;
use crate::render::assets::{ENABLED_FREEHAND_MODE_UV, FREEHAND_MODE_UV};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::util::{AABB, Vec2u};
use crate::workbench::mouse::MouseManager;

#[derive(Default, Copy, Clone)]
pub struct FreehandModeButton;

impl Widget for FreehandModeButton {
	fn alignment(&self) -> WidgetAlignment { WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Static(264), VerticalWidgetAlignmentPreference::Static(26)) }

	fn dimensions(&self, _containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> { PhysicalSize::new(16, 16) }
	fn is_valid_mouse_button(&self, button: MouseButton, _pos: Vec2u, _dims: PhysicalSize<u32>) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }
	fn on_mouse_down(&mut self, _button: MouseButton, _pos: Vec2u, _dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		let tab = ctx.tabs.active_tab_mut();
		tab.freehand_mode = !tab.freehand_mode;
		ActionResult::Success(())
	}

	fn render_at(&self, pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, ctx: &WidgetContext) {
		let aabb = AABB::from_pos_and_dims(pos, dims);
		let is_within_bounds = aabb.contains(mouse.coords);
		let freehand_mode = ctx.tabs.active_tab().freehand_mode;
		let uv = if freehand_mode || is_within_bounds { ENABLED_FREEHAND_MODE_UV } else { FREEHAND_MODE_UV };
		if is_within_bounds {
			builder.color = TextColor::White.to_raw();
			builder.draw_tooltip(&["Freehand Mode (Ctrl + Shift + F)"], mouse.coords, false);
		}
		builder.draw_texture(pos, uv, dims);
	}
}
