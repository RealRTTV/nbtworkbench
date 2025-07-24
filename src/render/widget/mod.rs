pub mod alert;
pub mod button;
pub mod horizontal_list;
pub mod notification;
pub mod replace_box;
pub mod search_box;
pub mod selected_line;
pub mod selected_text;
pub mod text;
pub mod vertical_list;

use winit::dpi::PhysicalSize;
use winit::event::{ElementState, MouseButton};

use crate::action_result::ActionResult;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::alert::manager::AlertManager;
use crate::render::widget::notification::manager::NotificationManager;
use crate::render::widget::replace_box::ReplaceBox;
use crate::render::widget::search_box::SearchBox;
use crate::util::Vec2u;
use crate::workbench::mouse::MouseManager;
use crate::workbench::tab::manager::TabManager;

#[allow(dead_code)]
pub struct WidgetContext<'a> {
	tabs: &'a TabManager,
	search_box: &'a SearchBox,
	replace_box: &'a ReplaceBox,
	shift: bool,
}

impl<'a> WidgetContext<'a> {
	#[must_use]
	pub fn new(tabs: &'a TabManager, search_box: &'a SearchBox, replace_box: &'a ReplaceBox, shift: bool) -> Self { Self { tabs, search_box, replace_box, shift } }
}

#[allow(dead_code)]
pub struct WidgetContextMut<'w> {
	tabs: &'w mut TabManager,
	search_box: &'w mut SearchBox,
	replace_box: &'w mut ReplaceBox,
	alerts: &'w mut AlertManager,
	notifications: &'w mut NotificationManager,
	shift: bool,
}

impl<'w> WidgetContextMut<'w> {
	#[must_use]
	pub fn new(tabs: &'w mut TabManager, search_box: &'w mut SearchBox, replace_box: &'w mut ReplaceBox, alerts: &'w mut AlertManager, notifications: &'w mut NotificationManager, shift: bool) -> Self {
		Self {
			tabs,
			search_box,
			replace_box,
			alerts,
			notifications,
			shift,
		}
	}

	#[must_use]
	pub fn as_ref(&'w self) -> WidgetContext<'w> {
		WidgetContext {
			tabs: &self.tabs,
			search_box: &self.search_box,
			replace_box: &self.replace_box,
			shift: self.shift,
		}
	}
}

pub trait Widget {
	#[must_use]
	fn alignment(&self) -> WidgetAlignment { WidgetAlignment::default() }

	#[must_use]
	fn dimensions(&self, containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32>;

	#[allow(unused_variables)]
	fn is_valid_mouse_button(&self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>) -> bool { false }

	fn on_mouse_input(&mut self, state: ElementState, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		if !self.is_valid_mouse_button(button, pos, dims) {
			return ActionResult::Pass
		}

		match state {
			ElementState::Pressed => self.on_mouse_down(button, pos, dims, ctx),
			ElementState::Released => self.on_mouse_up(button, pos, dims, ctx),
		}
	}

	#[allow(unused_variables)]
	fn on_mouse_up(&mut self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult { ActionResult::Pass }

	#[allow(unused_variables)]
	fn on_mouse_down(&mut self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult { ActionResult::Pass }

	#[must_use]
	fn is_currently_hovering(&self) -> bool { false }
	
	#[allow(unused_variables)]
	fn on_hovering(&mut self, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) {}
	
	#[allow(unused_variables)]
	fn on_stop_hovering(&mut self, ctx: &mut WidgetContextMut) {}

	#[must_use]
	#[allow(unused_variables)]
	fn is_visible(&self, ctx: &WidgetContext) -> bool { true }

	fn render_at(&self, pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, ctx: &WidgetContext);
}

#[derive(Copy, Clone, Default)]
pub struct WidgetAlignment {
	pub horizontal: HorizontalWidgetAlignmentPreference,
	pub vertical: VerticalWidgetAlignmentPreference,
}

impl WidgetAlignment {
	#[must_use]
	pub const fn new(horizontal: HorizontalWidgetAlignmentPreference, vertical: VerticalWidgetAlignmentPreference) -> Self { Self { horizontal, vertical } }

	#[must_use]
	pub fn coordinates(self, dims: PhysicalSize<u32>, window_dims: PhysicalSize<u32>) -> Vec2u { (self.horizontal.coordinate(dims, window_dims) as usize, self.vertical.coordinate(dims, window_dims) as usize).into() }
}

#[derive(Copy, Clone, Default)]
pub enum HorizontalWidgetAlignmentPreference {
	#[default]
	Left,
	Right,
	Static(i32),
}

impl HorizontalWidgetAlignmentPreference {
	#[must_use]
	pub const fn coordinate(self, dims: PhysicalSize<u32>, containment_dims: PhysicalSize<u32>) -> u32 {
		match self {
			Self::Left => 0,
			Self::Right => containment_dims.width.saturating_sub(dims.width),
			Self::Static(shift) => (shift.rem_euclid(containment_dims.width as i32) as u32).saturating_sub(if shift < 0 { dims.width } else { 0 }),
		}
	}
}

#[derive(Copy, Clone, Default)]
pub enum VerticalWidgetAlignmentPreference {
	#[default]
	Top,
	Bottom,
	Static(i32),
}

impl VerticalWidgetAlignmentPreference {
	#[must_use]
	pub const fn coordinate(self, dims: PhysicalSize<u32>, containment_dims: PhysicalSize<u32>) -> u32 {
		match self {
			VerticalWidgetAlignmentPreference::Top => 0,
			VerticalWidgetAlignmentPreference::Bottom => containment_dims.height.saturating_sub(dims.height),
			VerticalWidgetAlignmentPreference::Static(shift) => (shift.rem_euclid(containment_dims.height as i32) as u32).saturating_sub(if shift < 0 { dims.height } else { 0 }),
		}
	}
}
