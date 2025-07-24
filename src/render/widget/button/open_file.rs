use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::action_result::{ActionResult, IntoFailingActionResult};
use crate::render::assets::{OPEN_FOLDER_UV, SELECTION_UV};
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::render::widget::alert::manager::Alertable;
use crate::render::window::{WINDOW_HEIGHT, WINDOW_WIDTH};
use crate::util::{AABB, Vec2u};
use crate::workbench::mouse::MouseManager;
use crate::workbench::tab::Tab;

#[derive(Default, Copy, Clone)]
pub struct OpenFileButton;

impl Widget for OpenFileButton {
	fn alignment(&self) -> WidgetAlignment { WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Static(16), VerticalWidgetAlignmentPreference::Static(26)) }

	fn dimensions(&self, _containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> { PhysicalSize::new(16, 16) }
	fn is_valid_mouse_button(&self, button: MouseButton, _pos: Vec2u, _dims: PhysicalSize<u32>) -> bool { matches!(button, MouseButton::Left) }
	fn on_mouse_down(&mut self, _button: MouseButton, _pos: Vec2u, _dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		let tab = Tab::from_file_dialog(ctx.tabs.iter().map(|tab| tab.window_dims).next().unwrap_or(PhysicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT))).alert_err(ctx.alerts).failure_on_err()?;
		ctx.tabs.add(tab);
		ActionResult::Success(())
	}

	fn render_at(&self, pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, _ctx: &WidgetContext) {
		let aabb = AABB::from_pos_and_dims(pos, dims);
		let is_within_bounds = aabb.contains(mouse.coords);

		builder.draw_texture(pos, OPEN_FOLDER_UV, dims);
		if is_within_bounds {
			builder.draw_texture(pos, SELECTION_UV, dims);
			builder.draw_tooltip(&["Open File (Ctrl + O)"], mouse.coords, false);
		}
	}
}
