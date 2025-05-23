use std::fmt::{Display, Formatter, Write};

use crate::assets::{BASE_Z, JUST_OVERLAPPING_BASE_TEXT_Z, ZOffset};
use crate::render::{RenderContext, TextColor, VertexBufferBuilder};
use crate::serialization::{PrettyFormatter, UncheckedBufWriter};

#[derive(Clone, PartialEq)]
pub struct NbtNull;

impl NbtNull {
	pub const ID: u8 = 0;

	pub fn to_be_bytes(&self, _: &mut UncheckedBufWriter) {}

	pub fn to_le_bytes(&self, _: &mut UncheckedBufWriter) {}

	pub fn render(&self, builder: &mut VertexBufferBuilder, name: Option<&str>, ctx: &mut RenderContext) {
		ctx.line_number();
		self.render_icon(ctx.pos(), BASE_Z, builder);
		ctx.render_errors(ctx.pos(), builder);
		if ctx.forbid(ctx.pos()) {
			builder.settings(ctx.pos() + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
			if let Some(key) = name {
				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "{key}: null");
			};

			builder.color = TextColor::TreeKey.to_raw();
			let _ = write!(builder, "null");
		}

		ctx.offset_pos(0, 16);
	}

	pub fn render_icon(&self, pos: impl Into<(usize, usize)>, z: ZOffset, builder: &mut VertexBufferBuilder) { builder.draw_texture_z(pos, z, (240, 240), (16, 16)); }
}

impl Display for NbtNull {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "null") }
}

impl NbtNull {
	pub fn pretty_fmt(&self, f: &mut PrettyFormatter) { f.write_str("null") }
}

impl NbtNull {
	pub fn matches(&self, _: &Self) -> bool { true }
}
