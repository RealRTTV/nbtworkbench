use std::alloc::{alloc, dealloc, Layout};
use std::fmt::{Debug, Display, Formatter};
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::Deref;
use std::ptr::NonNull;

use compact_str::CompactString;

use crate::assets::*;
use crate::decoder::Decoder;
use crate::encoder::UncheckedBufWriter;
use crate::{RenderContext, StrExt, VertexBufferBuilder};

#[repr(transparent)]
#[allow(clippy::module_name_repetitions)]
#[derive(Clone)]
pub struct NbtString {
	pub str: TwentyThree,
}

impl NbtString {
	pub const ID: u8 = 8;
	pub(in crate::elements) fn from_str0(s: &str) -> Option<(&str, Self)> {
		let (str, s) = s.snbt_string_read()?;
		Some((s, Self { str: TwentyThree::new(str) }))
	}

	pub fn from_bytes(decoder: &mut Decoder) -> Option<Self> {
		unsafe {
			decoder.assert_len(2)?;
			Some(Self {
				str: TwentyThree::new(decoder.string()?),
			})
		}
	}
	pub fn to_bytes(&self, writer: &mut UncheckedBufWriter) {
		writer.write_str(self.str.as_str());
	}
}

impl NbtString {
	#[inline]
	#[must_use]
	pub fn new(str: CompactString) -> Self {
		Self { str: TwentyThree::new(str) }
	}
}

impl Display for NbtString {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		if self.str.as_str().needs_escape() {
			write!(f, "{:?}", self.str.as_str())
		} else {
			write!(f, "{}", self.str.as_str())
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
		Self::render_icon(ctx.pos(), BASE_Z, builder);

		if ctx.forbid(ctx.pos(), builder) {
			builder.settings(ctx.pos() + (20, 0), false, BASE_TEXT_Z);
			let _ = match name {
				Some(x) => write!(builder, "{x}: {}", self.str.as_str()),
				None => write!(builder, "{}", self.str.as_str()),
			};
		}

		ctx.y_offset += 16;
	}

	#[inline]
	pub fn render_icon(pos: impl Into<(usize, usize)>, z: u8, builder: &mut VertexBufferBuilder) {
		builder.draw_texture_z(pos, z, STRING_UV, (16, 16));
	}
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
						_pad: MaybeUninit::uninit_array(),
						variant: 254,
					}),
				}
			} else {
				Self { stack: self.stack }
			}
		}
	}
}

impl TwentyThree {
	pub fn new(mut s: CompactString) -> Self {
		unsafe {
			let len = s.len();
			if len > 23 {
				let res = Self {
					heap: ManuallyDrop::new(HeapTwentyThree {
						ptr: NonNull::new_unchecked(s.as_mut_ptr()),
						len,
						_pad: MaybeUninit::uninit_array(),
						variant: 254,
					}),
				};
				core::mem::forget(s);
				res
			} else {
				let ptr = s.as_ptr();
				Self {
					stack: ManuallyDrop::new(if len == 23 {
						StackTwentyThree { data: ptr.cast::<[u8; 23]>().read() }
					} else {
						let mut data = MaybeUninit::<u8>::uninit_array();
						data.as_mut_ptr().cast::<u8>().copy_from_nonoverlapping(ptr, len);
						// data.as_mut_ptr().cast::<u8>().add(len).write_bytes(0, 22 - len);
						data[22].write(192 + len as u8);
						StackTwentyThree {
							data: MaybeUninit::array_assume_init(data),
						}
					}),
				}
			}
		}
	}

	pub fn as_str(&self) -> &str {
		unsafe {
			let last = self.heap.variant;
			if last == 254 {
				core::str::from_utf8_unchecked(core::slice::from_raw_parts(self.heap.ptr.as_ptr(), self.heap.len))
			} else {
				if last < 192 {
					core::str::from_utf8_unchecked(core::slice::from_raw_parts(self.stack.data.as_ptr(), 23))
				} else {
					core::str::from_utf8_unchecked(core::slice::from_raw_parts(self.stack.data.as_ptr(), (last - 192) as usize))
				}
			}
		}
	}
}

impl Deref for TwentyThree {
	type Target = str;

	fn deref(&self) -> &Self::Target {
		self.as_str()
	}
}

impl Drop for TwentyThree {
	fn drop(&mut self) {
		unsafe {
			let last = self.heap.variant;
			if last == 254 {
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
