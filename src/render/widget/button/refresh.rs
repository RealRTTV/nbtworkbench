use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::action_result::ActionResult;
use crate::render::assets::{DISABLED_REFRESH_UV, REFRESH_UV, UNSELECTED_WIDGET_UV};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::util::{AABB, Vec2u};
use crate::workbench::mouse::MouseManager;

#[derive(Default, Copy, Clone)]
pub struct RefreshButton;

impl Widget for RefreshButton {
	fn alignment(&self) -> WidgetAlignment { WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Static(296), VerticalWidgetAlignmentPreference::Static(26)) }

	fn dimensions(&self, _containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> { PhysicalSize::new(16, 16) }
	fn is_valid_mouse_button(&self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>) -> bool { matches!(button, MouseButton::Left) }
	#[cfg(not(target_arch = "wasm32"))]
	fn on_mouse_down(&mut self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		if let Err(e) = ctx.tabs.active_tab_mut().refresh() {
			ctx.alerts.alert(e);
		}
		ActionResult::Success(())
	}
	fn render_at(&self, pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, ctx: &WidgetContext) {
		let has_path = ctx.tabs.active_tab().path.path().exists();
		let aabb = AABB::from_pos_and_dims(pos, dims);
		let widget_uv = if !has_path || cfg!(target_arch = "wasm32") { UNSELECTED_WIDGET_UV } else { super::get_button_widget_uv(self, aabb, dims, mouse) };
		let is_within_bounds = aabb.contains(mouse.coords);

		let uv = if has_path && !cfg!(target_arch = "wasm32") { REFRESH_UV } else { DISABLED_REFRESH_UV };

		if is_within_bounds {
			builder.color = TextColor::White.to_raw();
			#[cfg(target_arch = "wasm32")]
			builder.draw_tooltip(&["Refresh Tab (Disabled on WebAssembly version)"], mouse, false);
			#[cfg(not(target_arch = "wasm32"))]
			builder.draw_tooltip(&["Refresh Tab (Ctrl + R)"], mouse.coords, false);
		}

		builder.draw_texture(pos, widget_uv, dims);
		builder.draw_texture(pos, uv, dims);
	}
}
