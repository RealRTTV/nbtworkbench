pub mod alert;
pub mod button;
pub mod notification;
pub mod replace_box;
pub mod search_box;
pub mod selected_line;
pub mod selected_text;
pub mod text;

use fxhash::FxHashSet;
use winit::dpi::PhysicalSize;
use winit::event::{ElementState, MouseButton};

use crate::{
    action_result::ActionResult,
    render::{
        assets::{HOVERED_WIDGET_UV, SELECTED_WIDGET_UV, UNSELECTED_WIDGET_UV},
        vertex_buffer_builder::VertexBufferBuilder,
        widget::{alert::manager::AlertManager, notification::manager::NotificationManager, replace_box::ReplaceBox, search_box::SearchBox},
    },
    util::{AxisAlignedBoundingBox, Vec2u},
    workbench::tab::manager::TabManager,
};

pub trait Widget {
    #[must_use]
    fn new() -> Self
    where Self: Sized;

    #[must_use]
    fn bounds(&self, window_dims: PhysicalSize<u32>) -> AxisAlignedBoundingBox;

    fn on_mouse_input(&mut self, state: ElementState, button: MouseButton, ctx: &mut WidgetContextMut) -> ActionResult {
        if !Self::is_valid_mouse_button(button) {
            return ActionResult::Pass
        }

        match state {
            ElementState::Pressed => self.on_mouse_down(button, ctx),
            ElementState::Released => self.on_mouse_up(button, ctx),
        }
    }

    #[must_use]
    fn is_valid_mouse_button(button: MouseButton) -> bool;

    #[allow(unused_variables)] // cannot _ prefix ebcause it's impl'd on stuff
    fn on_mouse_up(&mut self, button: MouseButton, ctx: &mut WidgetContextMut) -> ActionResult { ActionResult::Pass }

    #[allow(unused_variables)] // cannot _ prefix ebcause it's impl'd on stuff
    fn on_mouse_down(&mut self, button: MouseButton, ctx: &mut WidgetContextMut) -> ActionResult { ActionResult::Pass }

    #[allow(unused_variables)]
    #[must_use]
    fn is_clickable(&self, ctx: &WidgetContext) -> bool { true }

    #[allow(unused_variables)]
    #[must_use]
    fn is_visible(&self, ctx: &WidgetContext) -> bool { true }

    #[must_use]
    fn is_within_bounds(&self, mouse: Vec2u, window_dims: PhysicalSize<u32>) -> bool {
        let aabb = self.bounds(window_dims);
        aabb.contains(mouse)
    }

    #[must_use]
    fn get_widget_uv(&self, mouse: Vec2u, window_dims: PhysicalSize<u32>, held_mouse_keys: &FxHashSet<MouseButton>) -> Vec2u {
        let is_within_bounds = self.is_within_bounds(mouse, window_dims);
        let is_mouse_down = held_mouse_keys.iter().copied().any(|button| Self::is_valid_mouse_button(button));
        match (is_within_bounds, is_mouse_down) {
            (false, false) => UNSELECTED_WIDGET_UV,
            (false, true) => UNSELECTED_WIDGET_UV,
            (true, false) => HOVERED_WIDGET_UV,
            (true, true) => SELECTED_WIDGET_UV,
        }
    }

    fn render(&self, builder: &mut VertexBufferBuilder, mouse: Vec2u, window_dims: PhysicalSize<u32>, ctx: &WidgetContext, held_mouse_keys: &FxHashSet<MouseButton>);
}

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

#[must_use]
#[derive(Default)]
pub struct WidgetAccumulatedResult {
    pub open_file_requests: usize,
}

#[allow(dead_code)]
pub struct WidgetContextMut<'w> {
    tabs: &'w mut TabManager,
    search_box: &'w mut SearchBox,
    replace_box: &'w mut ReplaceBox,
    alerts: &'w mut AlertManager,
    notifications: &'w mut NotificationManager,
    shift: bool,
    accumulated: WidgetAccumulatedResult,
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
            accumulated: WidgetAccumulatedResult::default(),
        }
    }

    pub fn open_file_request(&mut self) { self.accumulated.open_file_requests += 1; }

    pub fn take_accumulated(&mut self) -> WidgetAccumulatedResult { core::mem::take(&mut self.accumulated) }

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
