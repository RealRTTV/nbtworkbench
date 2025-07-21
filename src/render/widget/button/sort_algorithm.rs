use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::action_result::ActionResult;
use crate::config;
use crate::render::assets::{BASE_Z, HOVERED_WIDGET_UV, SORT_COMPOUND_BY_NAME_UV, SORT_COMPOUND_BY_NOTHING_UV, SORT_COMPOUND_BY_TYPE_UV};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::util::{AABB, Vec2u};
use crate::workbench::SortAlgorithm;
use crate::workbench::mouse::MouseManager;

#[derive(Default, Copy, Clone)]
pub struct SortAlgorithmButton;

impl Widget for SortAlgorithmButton {
	fn alignment(&self) -> WidgetAlignment { WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Static(280), VerticalWidgetAlignmentPreference::Static(26)) }

	fn dimensions(&self, _containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> { PhysicalSize::new(16, 16) }
	fn is_valid_mouse_button(&self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }
	fn on_mouse_down(&mut self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		let sort_algorithm = config::get_sort_algorithm();
		let reverse = matches!(button, MouseButton::Right) ^ ctx.shift;
		config::set_sort_algorithm(if reverse { sort_algorithm.rev_cycle() } else { sort_algorithm.cycle() });
		ActionResult::Success(())
	}

	fn render_at(&self, pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, ctx: &WidgetContext) {
		let widget_uv = super::get_button_widget_uv(self, AABB::from_pos_and_dims(pos, dims), dims, mouse);
		let sort_algorithm = config::get_sort_algorithm();
		let uv = match sort_algorithm {
			SortAlgorithm::None => SORT_COMPOUND_BY_NOTHING_UV,
			SortAlgorithm::Name => SORT_COMPOUND_BY_NAME_UV,
			SortAlgorithm::Type => SORT_COMPOUND_BY_TYPE_UV,
		};

		if widget_uv == HOVERED_WIDGET_UV {
			builder.color = TextColor::White.to_raw();
			builder.draw_tooltip(&[&format!("Compound Sorting Algorithm ({sort_algorithm})")], mouse.coords, false);
		}

		builder.draw_texture_z(pos, BASE_Z, widget_uv, dims);
		builder.draw_texture_z(pos + (3, 3), BASE_Z, uv, Vec2u::from(dims) - Vec2u::new(3, 3) * 2);
	}
}
