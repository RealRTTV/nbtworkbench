use fxhash::FxHashSet;
use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::{
	action_result::ActionResult,
	render::{
		assets::{DISABLED_REFRESH_UV, REFRESH_UV, UNSELECTED_WIDGET_UV},
		color::TextColor,
		vertex_buffer_builder::VertexBufferBuilder,
		widget::{Widget, WidgetContext, WidgetContextMut},
	},
	util::{AxisAlignedBoundingBox, Vec2u},
};

pub struct RefreshButton;

impl Widget for RefreshButton {
	fn new() -> Self
	where Self: Sized {
		Self
	}

	fn bounds(&self, _window_dims: PhysicalSize<u32>) -> AxisAlignedBoundingBox { AxisAlignedBoundingBox::new(296, 312, 26, 42) }

	fn is_valid_mouse_button(button: MouseButton) -> bool { matches!(button, MouseButton::Left) }

	fn on_mouse_down(&mut self, _button: MouseButton, ctx: &mut WidgetContextMut) -> ActionResult {
		if let Err(e) = ctx.tabs.active_tab_mut().refresh() {
			ctx.alerts.alert(e);
		}
		ActionResult::Success(())
	}

	fn is_clickable(&self, _ctx: &WidgetContext) -> bool { !cfg!(target_arch = "wasm32") }

	fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: PhysicalSize<u32>, ctx: &WidgetContext, held_mouse_keys: &FxHashSet<MouseButton>) {
		let has_path = ctx.tabs.active_tab().path.path().exists();
		let aabb = self.bounds(window_dims);
		let widget_uv = if !has_path || !self.is_clickable(ctx) {
			UNSELECTED_WIDGET_UV
		} else {
			self.get_widget_uv(mouse, window_dims, held_mouse_keys)
		};
		let is_within_bounds = aabb.contains(mouse);

		let uv = if has_path && self.is_clickable(ctx) { REFRESH_UV } else { DISABLED_REFRESH_UV };

		if is_within_bounds {
			builder.color = TextColor::White.to_raw();
			#[cfg(target_arch = "wasm32")]
			builder.draw_tooltip(&["Refresh Tab (Disabled on WebAssembly version)"], mouse, false);
			#[cfg(not(target_arch = "wasm32"))]
			builder.draw_tooltip(&["Refresh Tab (Ctrl + R)"], mouse, false);
		}

		builder.draw_texture(aabb.low(), widget_uv, (16, 16));
		builder.draw_texture(aabb.low(), uv, (16, 16));
	}
}
