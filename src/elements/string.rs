use std::alloc::{alloc, Layout};
use std::fmt::{Debug, Display, Formatter};

use crate::assets::STRING_UV;
use crate::decoder::Decoder;
use crate::encoder::UncheckedBufWriter;
use crate::{RenderContext, StrExt, VertexBufferBuilder};

#[repr(transparent)]
#[allow(clippy::module_name_repetitions)]
pub struct NbtString {
	pub str: Box<str>,
}

impl Clone for NbtString {
	#[inline]
	fn clone(&self) -> Self {
		unsafe {
			let len = self.str.len();
			let ptr = alloc(Layout::array::<u8>(len).unwrap_unchecked());
			ptr.copy_from_nonoverlapping(self.str.as_ptr(), len);
			Self { str: Box::from_raw(core::str::from_utf8_unchecked_mut(core::slice::from_raw_parts_mut(ptr, len))) }
		}
	}
}

impl NbtString {
	pub const ID: u8 = 8;
	pub(in crate::elements) fn from_str0(s: &str) -> Option<(&str, Self)> {
		let (str, s) = s.snbt_string_read()?;
		Some((s, Self { str }))
	}

	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
		unsafe {
			decoder.assert_len(2)?;
			Some(Self { str: decoder.string()? })
		}
	}
	pub fn to_bytes(&self, writer: &mut UncheckedBufWriter) {
		writer.write_str(&self.str);
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
	pub fn render(&self, builder: &mut VertexBufferBuilder, name: Option<&str>, ctx: &mut RenderContext) {
		use std::fmt::Write;

		ctx.line_number();
		Self::render_icon(ctx.pos(), 0.0, builder);
		ctx.highlight(
			ctx.pos(),
			name.map(StrExt::width).map_or(0, |x| x + ": ".width()) + self.str.width(),
			builder,
		);

		if ctx.forbid(ctx.pos(), builder) {
			builder.settings(ctx.pos() + (20, 0), false, 1);
			let _ = match name {
				Some(x) => write!(builder, "{x}: {}", self.str),
				None => write!(builder, "{}", self.str),
			};
		}

		ctx.y_offset += 16;
	}

	#[inline]
	pub fn render_icon(pos: impl Into<(usize, usize)>, z: f32, builder: &mut VertexBufferBuilder) {
		builder.draw_texture_z(pos, z, STRING_UV, (16, 16));
	}
}
