use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::action_result::ActionResult;
use crate::render::assets::{NEW_FILE_UV, SELECTION_UV};
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::util::{AABB, Vec2u};
use crate::workbench::mouse::MouseManager;
use crate::workbench::tab::Tab;

#[derive(Default, Copy, Clone)]
pub struct NewTabButton;

impl Widget for NewTabButton {
	fn alignment(&self) -> WidgetAlignment { WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Static(0), VerticalWidgetAlignmentPreference::Static(26)) }

	fn dimensions(&self, _containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> { PhysicalSize::new(16, 16) }
	fn is_valid_mouse_button(&self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>) -> bool { matches!(button, MouseButton::Left) }
	fn on_mouse_down(&mut self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		let shift = ctx.shift;
		let window_dims = ctx.tabs.active_tab().window_dims;
		ctx.tabs.add(Tab::new_empty_tab(shift, window_dims));
		ActionResult::Success(())
	}

	fn render_at(&self, pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, ctx: &WidgetContext) {
		let aabb = AABB::from_pos_and_dims(pos, dims);
		let is_within_bounds = aabb.contains(mouse.coords);

		builder.draw_texture(pos, NEW_FILE_UV, dims);
		if is_within_bounds {
			builder.draw_texture(pos, SELECTION_UV, dims);
			if ctx.shift {
				builder.draw_tooltip(&["Create New Region File (Ctrl + Shift + N)"], mouse.coords, false);
			} else {
				builder.draw_tooltip(&["Create New NBT File (Ctrl + N)"], mouse.coords, false);
			}
		}
	}
}
