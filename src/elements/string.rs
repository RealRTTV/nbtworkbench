use std::alloc::{Layout, alloc, dealloc};
use std::array;
use std::fmt::{Display, Formatter};
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::Deref;
use std::ptr::NonNull;

use compact_str::CompactString;

use crate::assets::{BASE_Z, JUST_OVERLAPPING_BASE_TEXT_Z, STRING_UV, ZOffset};
use crate::elements::result::NbtParseResult;
use crate::render::{RenderContext, TextColor, VertexBufferBuilder};
use crate::serialization::{Decoder, PrettyFormatter, UncheckedBufWriter};
use crate::util::StrExt;

#[repr(transparent)]
#[derive(Clone, PartialEq)]
pub struct NbtString {
	pub str: TwentyThree,
}

impl NbtString {
	pub fn matches(&self, other: &Self) -> bool { self.str.as_str() == other.str.as_str() }
}

impl NbtString {
	pub const ID: u8 = 8;
	pub(super) fn from_str0(s: &str) -> Result<(&str, Self), usize> {
		let (str, s) = s.snbt_string_read()?;
		Ok((s, Self { str: TwentyThree::new(str) }))
	}

	pub fn from_bytes<'a, D: Decoder<'a>>(decoder: &mut D) -> NbtParseResult<Self> {
		use super::result::*;

		unsafe {
			decoder.assert_len(2)?;
			ok(Self { str: TwentyThree::new(decoder.string()?) })
		}
	}

	pub fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) { writer.write_be_str(self.str.as_str()); }

	pub fn to_le_bytes(&self, writer: &mut UncheckedBufWriter) { writer.write_le_str(self.str.as_str()); }
}

impl NbtString {
	#[must_use]
	pub fn new(str: CompactString) -> Self { Self { str: TwentyThree::new(str) } }
}

impl Display for NbtString {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { if self.str.as_str().needs_escape() { write!(f, "{:?}", self.str.as_str()) } else { write!(f, "{}", self.str.as_str()) } }
}

impl NbtString {
	pub fn pretty_fmt(&self, f: &mut PrettyFormatter) { f.write_str(&self.to_string()) }
}

impl NbtString {
	pub fn render(&self, builder: &mut VertexBufferBuilder, name: Option<&str>, ctx: &mut RenderContext) {
		use std::fmt::Write as _;

		ctx.line_number();
		self.render_icon(ctx.pos(), BASE_Z, builder);

		ctx.render_errors(ctx.pos(), builder);
		if ctx.forbid(ctx.pos()) {
			builder.settings(ctx.pos() + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
			if let Some(name) = name {
				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "{name}: ");
			}
			builder.color = TextColor::TreeString.to_raw();
			let _ = write!(builder, "{}", self.str.as_str());
		}

		ctx.offset_pos(0, 16);
	}

	pub fn render_icon(&self, pos: impl Into<(usize, usize)>, z: ZOffset, builder: &mut VertexBufferBuilder) { builder.draw_texture_z(pos, z, STRING_UV, (16, 16)); }
}

#[repr(C)]
pub union TwentyThree {
	heap: ManuallyDrop<HeapTwentyThree>,
	stack: ManuallyDrop<StackTwentyThree>,
}

impl Clone for TwentyThree {
	fn clone(&self) -> Self {
		unsafe {
			if self.heap.variant == 254 {
				let ptr = alloc(Layout::array::<u8>(self.heap.len).unwrap_unchecked());
				ptr.copy_from_nonoverlapping(self.heap.ptr.as_ptr(), self.heap.len);
				Self {
					heap: ManuallyDrop::new(HeapTwentyThree {
						ptr: NonNull::new_unchecked(ptr),
						len: self.heap.len,
						_pad: [const { MaybeUninit::<u8>::uninit() }; 22 - core::mem::size_of::<usize>() - core::mem::size_of::<NonNull<u8>>()],
						variant: 254,
					}),
				}
			} else {
				Self { stack: self.stack }
			}
		}
	}
}

impl PartialEq for TwentyThree {
	fn eq(&self, other: &Self) -> bool { self.as_str().eq(other.as_str()) }
}

impl TwentyThree {
	#[must_use]
	pub fn new(s: CompactString) -> Self {
		unsafe {
			let len = s.len();
			let res = if len > 23 {
				let mut owned = s.into_string();
				let res = Self {
					heap: ManuallyDrop::new(HeapTwentyThree {
						ptr: NonNull::new_unchecked(owned.as_mut_ptr()),
						len,
						_pad: [const { MaybeUninit::<u8>::uninit() }; 22 - core::mem::size_of::<usize>() - core::mem::size_of::<NonNull<u8>>()],
						variant: 254,
					}),
				};
				core::mem::forget(owned);
				res
			} else {
				let ptr = s.as_ptr();
				let res = Self {
					stack: ManuallyDrop::new(if len == 23 {
						StackTwentyThree { data: ptr.cast::<[u8; 23]>().read() }
					} else {
						let mut data = array::from_fn::<u8, 23, _>(|idx| s.as_bytes().get(idx).copied().unwrap_or(0));
						data[22] = 192 + len as u8;
						StackTwentyThree { data }
					}),
				};
				core::mem::forget(s);
				res
			};
			res
		}
	}

	#[must_use]
	pub fn as_str(&self) -> &str {
		unsafe {
			let last = self.heap.variant;
			let (ptr, len) = if last == 254 {
				(self.heap.ptr.as_ptr().cast_const(), self.heap.len)
			} else if last < 192 {
				(self.stack.data.as_ptr(), 23)
			} else {
				(self.stack.data.as_ptr(), (last - 192) as usize)
			};
			core::str::from_utf8_unchecked(core::slice::from_raw_parts(ptr, len))
		}
	}
}

impl Deref for TwentyThree {
	type Target = str;

	fn deref(&self) -> &Self::Target { self.as_str() }
}

impl Drop for TwentyThree {
	fn drop(&mut self) {
		unsafe {
			if self.heap.variant == 254 {
				dealloc(self.heap.ptr.as_ptr(), Layout::array::<u8>(self.heap.len).unwrap_unchecked());
			}
		}
	}
}

#[repr(C)]
struct HeapTwentyThree {
	ptr: NonNull<u8>,
	len: usize,
	_pad: [MaybeUninit<u8>; 22 - core::mem::size_of::<usize>() - core::mem::size_of::<NonNull<u8>>()],
	variant: u8,
}

unsafe impl Send for HeapTwentyThree {}
unsafe impl Sync for HeapTwentyThree {}

#[repr(C)]
#[derive(Copy, Clone)]
struct StackTwentyThree {
	data: [u8; 23],
}
