use winit::dpi::PhysicalSize;
use winit::event::MouseButton;

use crate::action_result::ActionResult;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::{Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::util::{AABB, Vec2u};
use crate::workbench::mouse::MouseManager;

pub struct VerticalList<'a> {
	widgets: Vec<&'a mut dyn Widget>,
	alignment: WidgetAlignment,
}

impl<'a> VerticalList<'a> {
	#[must_use]
	pub fn new(widgets: impl IntoIterator<Item = &'a mut dyn Widget>, alignment: WidgetAlignment) -> Self {
		Self {
			widgets: widgets.into_iter().collect(),
			alignment,
		}
	}
}

impl<'a> Widget for VerticalList<'a> {
	fn alignment(&self) -> WidgetAlignment { self.alignment }

	fn dimensions(&self, containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> {
		let (mut min_x, mut max_x) = (containment_dims.width, 0);
		let mut height = 0;
		for widget in &self.widgets {
			let alignment = widget.alignment();
			let dims = widget.dimensions(containment_dims);
			let widget_min_x = alignment.horizontal.coordinate(dims, containment_dims);
			let widget_max_x = widget_min_x + dims.width;
			min_x = min_x.min(widget_min_x);
			max_x = max_x.max(widget_max_x);

			height += dims.height;
		}
		let width = max_x.saturating_sub(min_x);
		PhysicalSize::new(width, height)
	}

	fn is_valid_mouse_button(&self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>) -> bool {
		let mut y = 0;
		for (aabb, widget) in self.widgets.iter().map(|w| (aabb_of(&**w, &mut y, dims), w)) {
			if let Some(pos) = pos.relative_to(aabb)
				&& widget.is_valid_mouse_button(button, pos, aabb.dims())
			{
				return true;
			}
		}
		false
	}

	fn on_mouse_up(&mut self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		let mut y = 0;
		for (aabb, widget) in self.widgets.iter_mut().map(|w| (aabb_of(&**w, &mut y, dims), w)) {
			if let Some(pos) = pos.relative_to(aabb) {
				widget.on_mouse_up(button, pos, aabb.dims(), ctx)?;
			}
		}
		ActionResult::Pass
	}

	fn on_mouse_down(&mut self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		let mut y = 0;
		for (aabb, widget) in self.widgets.iter_mut().map(|w| (aabb_of(&**w, &mut y, dims), w)) {
			if let Some(pos) = pos.relative_to(aabb) {
				widget.on_mouse_down(button, pos, aabb.dims(), ctx)?;
			}
		}
		ActionResult::Pass
	}

	fn is_currently_hovering(&self) -> bool { self.widgets.iter().any(|w| w.is_currently_hovering()) }

	fn on_hovering(&mut self, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) {
		let mut y = 0;
		for (aabb, widget) in self.widgets.iter_mut().map(|w| (aabb_of(&**w, &mut y, dims), w)) {
			if let Some(pos) = pos.relative_to(aabb) {
				widget.on_hovering(pos, aabb.dims(), ctx);
			}
		}
	}

	fn on_stop_hovering(&mut self, ctx: &mut WidgetContextMut) {
		for widget in &mut self.widgets {
			if widget.is_currently_hovering() {
				widget.on_stop_hovering(ctx);
			}
		}
	}

	fn is_visible(&self, ctx: &WidgetContext) -> bool { self.widgets.iter().any(|w| w.is_visible(ctx)) }

	fn render_at(&self, pos: Vec2u, list_dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, ctx: &WidgetContext) {
		let mut y = 0;
		for (aabb, widget) in self.widgets.iter().map(|w| (aabb_of(&**w, &mut y, list_dims), w)) {
			widget.render_at(pos + aabb.low(), aabb.dims(), builder, mouse, ctx);
		}
	}
}

fn aabb_of(widget: &dyn Widget, y: &mut u32, list_dims: PhysicalSize<u32>) -> AABB {
	let alignment = widget.alignment();
	let dims = widget.dimensions(list_dims);
	let x_offset = alignment.horizontal.coordinate(dims, list_dims);
	let pos = Vec2u::new(x_offset as _, *y as _);
	*y += dims.height;
	AABB::from_pos_and_dims(pos, dims)
}
