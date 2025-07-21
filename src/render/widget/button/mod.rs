use winit::dpi::PhysicalSize;
use crate::render::assets::{HOVERED_WIDGET_UV, SELECTED_WIDGET_UV, UNSELECTED_WIDGET_UV};
use crate::render::widget::Widget;
use crate::util::{AABB, Vec2u};
use crate::workbench::mouse::MouseManager;

pub mod exact_match;
pub mod freehand_mode;
pub mod new_tab;
pub mod open_file;
pub mod refresh;
pub mod replace_by;
pub mod search_flags;
pub mod search_mode;
pub mod search_operation;
pub mod sort_algorithm;
pub mod theme;

#[must_use]
fn get_button_widget_uv(widget: &impl Widget, aabb: AABB, dims: PhysicalSize<u32>, mouse: &MouseManager) -> Vec2u {
	let relative = mouse.coords.relative_to(aabb);
	
	if let Some(pos) = relative {
		if mouse.held_keys().any(|button| widget.is_valid_mouse_button(button, pos, dims)) {
			SELECTED_WIDGET_UV
		} else {
			HOVERED_WIDGET_UV
		}
	} else {
		UNSELECTED_WIDGET_UV
	}
}
