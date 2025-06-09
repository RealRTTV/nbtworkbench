mod exact_match;
mod freehand_mode;
mod new_tab;
mod open_file;
mod refresh;
mod replace_by;
mod search_flags;
mod search_mode;
mod search_operation;
mod sort_algorithm;
mod theme;

pub use exact_match::*;
pub use freehand_mode::*;
use fxhash::FxHashSet;
pub use new_tab::*;
pub use open_file::*;
pub use refresh::*;
pub use replace_by::*;
pub use search_flags::*;
pub use search_mode::*;
pub use search_operation::*;
pub use sort_algorithm::*;
pub use theme::*;
use winit::event::{ElementState, MouseButton};

use crate::assets::{HOVERED_WIDGET_UV, SELECTED_WIDGET_UV, UNSELECTED_WIDGET_UV};
use crate::render::VertexBufferBuilder;
use crate::util::{AxisAlignedBoundingBox, Vec2u};
use crate::widget::{Alert, Notification, ReplaceBox, SearchBox};
use crate::workbench::Tab;

pub trait ButtonWidget {
	#[must_use]
	fn new() -> Self
	where Self: Sized;

	#[must_use]
	fn bounds(&self, window_dims: Vec2u) -> AxisAlignedBoundingBox;

	#[must_use]
	fn on_mouse_input(&mut self, state: ElementState, button: MouseButton, ctx: &mut ButtonWidgetContextMut) -> bool {
		if !Self::is_valid_mouse_button(button) {
			return false
		}

		match state {
			ElementState::Pressed => self.on_mouse_down(button, ctx),
			ElementState::Released => self.on_mouse_up(button, ctx),
		}
	}

	#[must_use]
	fn is_valid_mouse_button(button: MouseButton) -> bool;

	#[must_use]
	fn on_mouse_up(&mut self, button: MouseButton, ctx: &mut ButtonWidgetContextMut) -> bool;

	#[must_use]
	fn on_mouse_down(&mut self, button: MouseButton, ctx: &mut ButtonWidgetContextMut) -> bool;

	#[allow(unused_variables)]
	#[must_use]
	fn is_clickable(&self, ctx: &ButtonWidgetContext) -> bool { true }

	#[allow(unused_variables)]
	#[must_use]
	fn is_visible(&self, ctx: &ButtonWidgetContext) -> bool { true }

	#[must_use]
	fn is_within_bounds(&self, mouse: Vec2u, window_dims: Vec2u) -> bool {
		let aabb = self.bounds(window_dims);
		aabb.contains(mouse)
	}

	#[must_use]
	fn get_widget_uv(&self, mouse: Vec2u, window_dims: Vec2u, held_mouse_keys: &FxHashSet<MouseButton>) -> Vec2u {
		let is_within_bounds = self.is_within_bounds(mouse, window_dims);
		let is_mouse_down = held_mouse_keys
			.iter()
			.copied()
			.any(|button| Self::is_valid_mouse_button(button));
		match (is_within_bounds, is_mouse_down) {
			(false, false) => UNSELECTED_WIDGET_UV,
			(false, true) => UNSELECTED_WIDGET_UV,
			(true, false) => HOVERED_WIDGET_UV,
			(true, true) => SELECTED_WIDGET_UV,
		}
	}

	fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: Vec2u, ctx: &ButtonWidgetContext, held_mouse_keys: &FxHashSet<MouseButton>);
}

#[allow(dead_code)]
pub struct ButtonWidgetContext<'a> {
	tab: &'a Tab,
	search_box: &'a SearchBox,
	replace_box: &'a ReplaceBox,
	shift: bool,
}

impl<'a> ButtonWidgetContext<'a> {
	#[must_use]
	pub fn new(tab: &'a Tab, search_box: &'a SearchBox, replace_box: &'a ReplaceBox, shift: bool) -> Self { Self { tab, search_box, replace_box, shift } }
}

#[must_use]
#[derive(Default)]
pub struct ButtonWidgetAccumulatedResult {
	pub open_file_requests: usize,
	pub notifications: Vec<Notification>,
	pub alerts: Vec<Alert>,
	pub tabs: Vec<Tab>,
}

#[allow(dead_code)]
pub struct ButtonWidgetContextMut<'a> {
	tab: &'a mut Tab,
	search_box: &'a mut SearchBox,
	replace_box: &'a mut ReplaceBox,
	shift: bool,
	accumulated: ButtonWidgetAccumulatedResult,
}

impl<'a> ButtonWidgetContextMut<'a> {
	#[must_use]
	pub fn new(tab: &'a mut Tab, search_box: &'a mut SearchBox, replace_box: &'a mut ReplaceBox, shift: bool) -> Self {
		Self {
			tab,
			search_box,
			replace_box,
			shift,
			accumulated: ButtonWidgetAccumulatedResult::default(),
		}
	}

	pub fn notify(&mut self, notification: Notification) { self.accumulated.notifications.push(notification); }

	pub fn alert(&mut self, alert: impl Into<Alert>) { self.accumulated.alerts.push(alert.into()); }

	pub fn tab(&mut self, tab: Tab) { self.accumulated.tabs.push(tab); }

	pub fn open_file_request(&mut self) { self.accumulated.open_file_requests += 1; }

	#[must_use]
	pub fn take_accumulated(&mut self) -> ButtonWidgetAccumulatedResult { core::mem::take(&mut self.accumulated) }

	#[must_use]
	pub fn as_ref(&'a self) -> ButtonWidgetContext<'a> {
		ButtonWidgetContext {
			tab: &self.tab,
			search_box: &self.search_box,
			replace_box: &self.replace_box,
			shift: self.shift,
		}
	}
}
