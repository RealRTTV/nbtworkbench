use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::action_result::ActionResult;
use crate::config;
use crate::render::assets::{DIM_LIGHTBULB_UV, LIGHTBULB_UV};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::render::window::Theme;
use crate::util::{AABB, Vec2u};
use crate::workbench::mouse::MouseManager;

#[derive(Default, Copy, Clone)]
pub struct ThemeButton;

impl Widget for ThemeButton {
	fn alignment(&self) -> WidgetAlignment { WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Static(312), VerticalWidgetAlignmentPreference::Static(26)) }

	fn dimensions(&self, _containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> { PhysicalSize::new(16, 16) }
	
	fn is_valid_mouse_button(&self, button: MouseButton, _pos: Vec2u, _dims: PhysicalSize<u32>) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }
	
	fn on_mouse_down(&mut self, _button: MouseButton, _pos: Vec2u, _dims: PhysicalSize<u32>, _ctx: &mut WidgetContextMut) -> ActionResult {
		config::set_theme(match config::get_theme() {
			Theme::Light => Theme::Dark,
			Theme::Dark => Theme::Light,
		});
		ActionResult::Success(())
	}
	
	fn render_at(&self, pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, _ctx: &WidgetContext) {
		let is_within_bounds = AABB::from_pos_and_dims(pos, dims).contains(mouse.coords);
		if is_within_bounds {
			builder.color = TextColor::White.to_raw();
			builder.draw_tooltip(&["Change Theme (Ctrl + Alt + T)"], mouse.coords, false);
		}
		builder.draw_texture(pos, if is_within_bounds { DIM_LIGHTBULB_UV } else { LIGHTBULB_UV }, dims);
	}
}
