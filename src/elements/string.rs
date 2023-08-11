use std::fmt::{Debug, Display, Formatter};
use std::io::Write;

use crate::assets::STRING_UV;
use crate::decoder::Decoder;
use crate::encoder::write_string;
use crate::{RenderContext, StrExt, VertexBufferBuilder};

#[derive(Clone)]
#[repr(transparent)]
#[allow(clippy::module_name_repetitions)]
pub struct NbtString {
	str: Box<str>,
}

impl NbtString {
	pub const ID: u8 = 8;

	pub(in crate::elements) fn from_str0(s: &str) -> Option<(&str, Self)> {
		let (str, s) = s.snbt_string_read()?;
		Some((s, Self { str: str.into_boxed_str() }))
	}

	pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
		unsafe {
			decoder.assert_len(2);
			Some(Self::new(decoder.string()?))
		}
	}

	pub fn to_bytes<W: Write>(&self, writer: &mut W) {
		write_string(writer, &self.str);
	}
}

impl NbtString {
	#[inline]
	#[must_use]
	pub fn new(str: Box<str>) -> Self {
		Self { str }
	}

	#[inline]
	#[must_use]
	pub const fn unwrap(&self) -> &str {
		&self.str
	}
}

impl Display for NbtString {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		if self.str.needs_escape() {
			write!(f, "{:?}", self.str)
		} else {
			write!(f, "{}", self.str)
		}
	}
}

impl Debug for NbtString {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		Display::fmt(self, f)
	}
}

impl NbtString {
	#[inline]
	pub fn render(&self, builder: &mut VertexBufferBuilder, x_offset: &mut usize, y_offset: &mut usize, name: Option<&str>, ctx: &mut RenderContext) {
		use std::fmt::Write;

		ctx.line_number(*y_offset, builder);
		Self::render_icon(*x_offset, *y_offset, builder);
		ctx.highlight(
			(*x_offset, *y_offset),
			name.map_or(0, StrExt::width) + name.is_some() as usize * ": ".width() + self.str.width(),
			builder,
		);

		if ctx.forbid(*x_offset, *y_offset, builder) {
			builder.settings(*x_offset + 20, *y_offset, false);
			let _ = match name {
				Some(x) => write!(builder, "{x}: {}", self.str),
				None => write!(builder, "{}", self.str),
			};
		}

		*y_offset += 16;
	}

	#[inline]
	pub fn render_icon(x: usize, y: usize, builder: &mut VertexBufferBuilder) {
		builder.draw_texture((x, y), STRING_UV, (16, 16));
	}
}
