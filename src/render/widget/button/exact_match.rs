use fxhash::FxHashSet;
use winit::event::MouseButton;

use crate::assets::{BASE_Z, EXACT_MATCH_OFF_UV, EXACT_MATCH_ON_UV, HOVERED_WIDGET_UV};
use crate::config;
use crate::render::{TextColor, VertexBufferBuilder};
use crate::util::{AxisAlignedBoundingBox, Vec2u};
use crate::widget::{ButtonWidget, ButtonWidgetContext, ButtonWidgetContextMut, SEARCH_BOX_END_X};

pub struct ExactMatchButton;

impl ButtonWidget for ExactMatchButton {
	fn new() -> Self
	where Self: Sized {
		Self
	}

	fn bounds(&self, window_dims: Vec2u) -> AxisAlignedBoundingBox { AxisAlignedBoundingBox::new(window_dims.x - SEARCH_BOX_END_X - 17, window_dims.x - SEARCH_BOX_END_X - 1, 26, 42) }

	fn is_valid_mouse_button(button: MouseButton) -> bool { matches!(button, MouseButton::Left | MouseButton::Right) }

	fn on_mouse_up(&mut self, _button: MouseButton, _ctx: &mut ButtonWidgetContextMut) -> bool { false }

	fn on_mouse_down(&mut self, _button: MouseButton, _ctx: &mut ButtonWidgetContextMut) -> bool {
		config::set_search_exact_match(!config::get_search_exact_match());
		true
	}

	fn is_clickable(&self, _ctx: &ButtonWidgetContext) -> bool { config::get_search_mode().has_exact_match_mode() }

	fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: Vec2u, _ctx: &ButtonWidgetContext, held_mouse_keys: &FxHashSet<MouseButton>) {
		let aabb = self.bounds(window_dims);
		let widget_uv = self.get_widget_uv(mouse, window_dims, held_mouse_keys);
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
				mouse,
				false,
			);
		}

		builder.draw_texture_z(aabb.low(), BASE_Z, widget_uv, (16, 16));
		builder.draw_texture_z(aabb.low(), BASE_Z, uv, (16, 16));
	}
}
