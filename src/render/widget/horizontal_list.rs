// todo, make updated
// use winit::dpi::PhysicalSize;
//
// use crate::render::vertex_buffer_builder::VertexBufferBuilder;
// use crate::render::widget::{Widget, WidgetAlignment, WidgetContext};
// use crate::util::Vec2u;
// use crate::workbench::mouse::MouseManager;
//
// pub struct HorizontalList<'a> {
// widgets: Vec<&'a dyn Widget>,
// alignment: WidgetAlignment,
// }
//
// impl<'a> HorizontalList<'a> {
// #[must_use]
// pub fn new(widgets: impl IntoIterator<Item = &'a dyn Widget>, alignment: WidgetAlignment) -> Self {
// Self {
// widgets: widgets.into_iter().collect(),
// alignment,
// }
// }
// }
//
// impl<'a> Widget for HorizontalList<'a> {
// fn alignment(&self) -> WidgetAlignment { self.alignment }
//
// fn dimensions(&self, containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> {
// let (mut min_y, mut max_y) = (0, 0);
// let mut width = 0;
// for widget in &self.widgets {
// let alignment = widget.alignment();
// let dims = widget.dimensions(containment_dims);
// let widget_min_y = alignment.vertical.coordinate(dims, containment_dims);
// let widget_max_y = widget_min_y + dims.height;
// min_y = min_y.min(widget_min_y);
// max_y = max_y.max(widget_max_y);
//
// width += dims.width;
// }
// let height = max_y - min_y;
// let dims = PhysicalSize::new(width, height);
// dims
// }
// fn render_at(&self, pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, ctx: &WidgetContext) {
// for widget in &self.widgets {
// let alignment = widget.alignment();
// let dims = widget.dimensions(dims);
// let y_offset = alignment.vertical.coordinate(dims, dims);
// widget.render_at(pos + (0, y_offset as usize), dims, builder, mouse, ctx);
// pos.x += dims.width as usize;
// }
// }
// }
