mod bookmark_lines;
mod search_flags;
mod search_mode;
mod exact_match;
mod sort_algorithm;
mod theme;
mod freehand_mode;
mod refresh;

pub use bookmark_lines::*;
pub use search_flags::*;
pub use search_mode::*;
pub use exact_match::*;
pub use sort_algorithm::*;
pub use theme::*;
pub use freehand_mode::*;
pub use refresh::*;

use crate::assets::{HOVERED_WIDGET_UV, SELECTED_WIDGET_UV, UNSELECTED_WIDGET_UV};
use crate::render::VertexBufferBuilder;
use crate::util::{AxisAlignedBoundingBox, Vec2u};
use crate::widget::{Alert, Notification, ReplaceBox, SearchBox};
use crate::workbench::Tab;

use fxhash::FxHashSet;
use winit::event::{ElementState, MouseButton};

pub trait ButtonWidget {
    #[must_use]
    fn new() -> Self where Self: Sized;

    #[must_use]
    fn bounds(window_dims: Vec2u) -> AxisAlignedBoundingBox;

    #[must_use]
    fn on_mouse_input(&mut self, state: ElementState, button: MouseButton, ctx: &mut ButtonWidgetContextMut) -> bool {
        if !Self::is_valid_mouse_button(button) { return false }

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

    #[must_use]
    fn is_clickable(&self) -> bool {
        true
    }

    #[must_use]
    fn is_within_bounds(mouse: Vec2u, window_dims: Vec2u) -> bool {
        let aabb = Self::bounds(window_dims);
        aabb.contains(mouse)
    }

    #[must_use]
    fn get_widget_uv(&self, mouse: Vec2u, window_dims: Vec2u, held_mouse_keys: &FxHashSet<MouseButton>) -> Vec2u {
        let is_within_bounds = Self::is_within_bounds(mouse, window_dims);
        let is_mouse_down = held_mouse_keys.iter().copied().any(|button| Self::is_valid_mouse_button(button));
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
    pub fn new(tab: &'a Tab, search_box: &'a SearchBox, replace_box: &'a ReplaceBox, shift: bool) -> Self {
        Self {
            tab,
            search_box,
            replace_box,
            shift,
        }
    }
}

#[allow(dead_code)]
pub struct ButtonWidgetContextMut<'a> {
    tab: &'a mut Tab,
    search_box: &'a mut SearchBox,
    replace_box: &'a mut ReplaceBox,
    shift: bool,
    notifications: Vec<Notification>,
    alerts: Vec<Alert>,
}

impl<'a> ButtonWidgetContextMut<'a> {
    #[must_use]
    pub fn new(tab: &'a mut Tab, search_box: &'a mut SearchBox, replace_box: &'a mut ReplaceBox, shift: bool) -> Self {
        Self {
            tab,
            search_box,
            replace_box,
            shift,
            notifications: vec![],
            alerts: vec![],
        }
    }

    pub fn notify(&mut self, notification: Notification) {
        self.notifications.push(notification);
    }

    pub fn alert(&mut self, alert: Alert) {
        self.alerts.push(alert);
    }

    #[must_use]
    pub fn into_notifications_and_alerts(self) -> (Vec<Notification>, Vec<Alert>) {
        (self.notifications, self.alerts)
    }
}
