use std::alloc::{alloc, Layout};
use std::intrinsics::likely;

pub struct Decoder {
	pub data: *const u8,
	end: *const u8,
}

#[allow(improper_ctypes_definitions)]
impl Decoder {
	#[inline]
	#[optimize(speed)]
	pub const fn new(data: &[u8]) -> Self {
		Self {
			end: unsafe { data.as_ptr().add(data.len()) },
			data: data.as_ptr(),
		}
	}

	#[inline]
	#[optimize(speed)]
	#[must_use]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub fn assert_len(&mut self, remaining_len: usize) -> Option<()> {
		if unsafe { likely((self.data.add(remaining_len) as usize) < self.end as usize) } {
			Some(())
		} else {
			None
		}
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub unsafe fn read_bytes<const N: usize>(&mut self) -> [u8; N] {
		let array = self.data.cast::<[u8; N]>().read();
		self.data = self.data.add(N);
		array
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub unsafe fn u8(&mut self) -> u8 {
		u8::from_be_bytes(self.read_bytes())
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub unsafe fn u16(&mut self) -> u16 {
		u16::from_be_bytes(self.read_bytes())
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub unsafe fn u32(&mut self) -> u32 {
		u32::from_be_bytes(self.read_bytes())
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub unsafe fn u64(&mut self) -> u64 {
		u64::from_be_bytes(self.read_bytes())
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub unsafe fn i8(&mut self) -> i8 {
		i8::from_be_bytes(self.read_bytes())
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub unsafe fn i16(&mut self) -> i16 {
		i16::from_be_bytes(self.read_bytes())
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]

	pub unsafe fn i32(&mut self) -> i32 {
		i32::from_be_bytes(self.read_bytes())
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]

	pub unsafe fn i64(&mut self) -> i64 {
		i64::from_be_bytes(self.read_bytes())
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]

	pub unsafe fn f32(&mut self) -> f32 {
		f32::from_be_bytes(self.read_bytes())
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub unsafe fn f64(&mut self) -> f64 {
		f64::from_be_bytes(self.read_bytes())
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub unsafe fn skip(&mut self, amount: usize) {
		self.data = self.data.add(amount);
	}

	#[inline]
	#[optimize(speed)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub unsafe fn string(&mut self) -> Option<Box<str>> {
		let len = self.u16() as usize;
		self.assert_len(len)?;

		let ptr = alloc(Layout::array::<u8>(len).unwrap_unchecked());
		ptr.copy_from_nonoverlapping(self.data, len);
		self.data = self.data.add(len);
		Some(Box::from_raw(core::str::from_utf8_unchecked_mut(core::slice::from_raw_parts_mut(ptr, len))))
	}
}
