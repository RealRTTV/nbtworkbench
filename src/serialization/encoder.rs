use std::alloc::{alloc, dealloc, realloc, Layout};
use std::intrinsics::likely;
use std::mem::MaybeUninit;

const WIDTH: usize = 1 << 24;

pub struct UncheckedBufWriter {
	buf: *mut MaybeUninit<u8>,
	buf_len: usize,
	inner: *mut u8,
	inner_len: usize,
}

impl Default for UncheckedBufWriter {
	fn default() -> Self {
		Self {
			buf: unsafe { alloc(Layout::array::<u8>(WIDTH).unwrap_unchecked()).cast::<MaybeUninit<u8>>() },
			buf_len: 0,
			inner: core::ptr::null_mut(),
			inner_len: 0,
		}
	}
}

impl Drop for UncheckedBufWriter {
	fn drop(&mut self) {
		unsafe {
			dealloc(
				self.buf.cast::<u8>(),
				Layout::array::<u8>(WIDTH).unwrap_unchecked(),
			);
		}
	}
}

impl UncheckedBufWriter {
	pub fn new() -> Self { Self::default() }

	pub const fn remaining(&self) -> usize { WIDTH - 1 - self.buf_len }

	pub fn write(&mut self, bytes: &[u8]) {
		unsafe {
			if likely(bytes.len() < self.remaining()) {
				self.buf
					.add(self.buf_len)
					.cast::<u8>()
					.copy_from_nonoverlapping(bytes.as_ptr(), bytes.len());
				self.buf_len += bytes.len();
			} else {
				self.write_pushing_cold(bytes);
			}
		}
	}

	pub fn write_be_str(&mut self, str: &str) {
		self.write(&(str.len() as u16).to_be_bytes());
		self.write(str.as_bytes());
	}

	pub fn write_le_str(&mut self, str: &str) {
		self.write(&(str.len() as u16).to_le_bytes());
		self.write(str.as_bytes());
	}

	#[cold]
	#[inline(never)]
	unsafe fn write_pushing_cold(&mut self, bytes: &[u8]) {
		let malloc_size = (self.inner_len + WIDTH - 1) & !(WIDTH - 1);
		let new_size = (self.inner_len + bytes.len() + self.buf_len + WIDTH - 1) & !(WIDTH - 1);
		self.inner = if self.inner.is_null() {
			alloc(Layout::array::<u8>(new_size).unwrap_unchecked())
		} else {
			realloc(
				self.inner,
				Layout::array::<u8>(malloc_size).unwrap_unchecked(),
				new_size,
			)
		};
		self.inner
			.add(self.inner_len)
			.copy_from_nonoverlapping(self.buf.cast::<u8>(), self.buf_len);
		self.inner_len += self.buf_len;
		self.inner
			.add(self.inner_len)
			.copy_from_nonoverlapping(bytes.as_ptr(), bytes.len());
		self.inner_len += bytes.len();
		self.buf_len = 0;
	}

	pub fn finish(mut self) -> Vec<u8> {
		unsafe {
			let malloc_size = (self.inner_len + WIDTH - 1) & !(WIDTH - 1);
			self.inner = if self.inner.is_null() {
				alloc(Layout::array::<u8>(self.inner_len + self.buf_len).unwrap_unchecked())
			} else {
				realloc(
					self.inner,
					Layout::array::<u8>(malloc_size).unwrap_unchecked(),
					self.inner_len + self.buf_len,
				)
			};
			self.inner
				.add(self.inner_len)
				.copy_from_nonoverlapping(self.buf.cast::<u8>(), self.buf_len);
			self.inner_len += self.buf_len;
			Vec::from_raw_parts(self.inner, self.inner_len, self.inner_len)
		}
	}
}
