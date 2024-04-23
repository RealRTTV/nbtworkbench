use std::intrinsics::likely;
use std::marker::PhantomData;

use compact_str::CompactString;
use crate::elements::compound::CompoundMap;
use crate::SortAlgorithm;

pub struct LittleEndianDecoder<'a> {
	pub data: *const u8,
	end: *const u8,
	sort: SortAlgorithm,
	_marker: PhantomData<&'a ()>,
	header: bool,
}

#[allow(improper_ctypes_definitions)]
impl<'a> LittleEndianDecoder<'a> {
	#[inline]
	#[optimize(speed)]
	pub fn new(data: &'a [u8], sort: SortAlgorithm) -> Self {
		let mut this = Self {
			end: unsafe { data.as_ptr().add(data.len()) },
			data: data.as_ptr(),
			sort,
			_marker: PhantomData,
			header: false,
		};
		unsafe {
			if this.assert_len(8).is_some() && this.data.add(4).cast::<u32>().read_unaligned() as usize == this.remaining_len() - 8 {
				// todo, what the hell is this version for
				let _version = this.u32();
				let _remaining_length = this.u32() as usize;
				this.header = true;
			}
		}
		this
	}

	#[inline]
	pub fn sort(&self, map: &mut CompoundMap) {
		self.sort.sort(map)
	}

	#[inline]
	#[must_use]
	pub fn header(&self) -> bool {
		self.header
	}

	#[optimize(speed)]
	#[must_use]
	pub fn assert_len(&self, remaining_len: usize) -> Option<()> {
		// <= end because it will read *until* that byte
		if unsafe { likely((self.data.add(remaining_len) as usize) <= self.end as usize) } {
			Some(())
		} else {
			None
		}
	}

	#[optimize(speed)]
	#[must_use]
	pub fn assert_exact_len(&self, remaining_len: usize) -> Option<()> {
		// <= end because it will read *until* that byte
		if unsafe { likely((self.data.add(remaining_len) as usize) == self.end as usize) } {
			Some(())
		} else {
			None
		}
	}

	#[inline]
	#[must_use]
	pub fn remaining_len(&self) -> usize {
		self.end as usize - self.data as usize
	}

	#[optimize(speed)]
	pub unsafe fn read_bytes<const N: usize>(&mut self) -> [u8; N] {
		let array = self.data.cast::<[u8; N]>().read();
		self.data = self.data.add(N);
		array
	}

	#[optimize(speed)]
	pub unsafe fn u8(&mut self) -> u8 { u8::from_le_bytes(self.read_bytes()) }

	#[optimize(speed)]
	pub unsafe fn u16(&mut self) -> u16 { u16::from_le_bytes(self.read_bytes()) }

	#[optimize(speed)]
	pub unsafe fn u32(&mut self) -> u32 { u32::from_le_bytes(self.read_bytes()) }

	#[optimize(speed)]
	pub unsafe fn u64(&mut self) -> u64 { u64::from_le_bytes(self.read_bytes()) }

	#[optimize(speed)]
	pub unsafe fn i8(&mut self) -> i8 { i8::from_le_bytes(self.read_bytes()) }

	#[optimize(speed)]
	pub unsafe fn i16(&mut self) -> i16 { i16::from_le_bytes(self.read_bytes()) }

	#[optimize(speed)]
	pub unsafe fn i32(&mut self) -> i32 { i32::from_le_bytes(self.read_bytes()) }

	#[optimize(speed)]
	pub unsafe fn i64(&mut self) -> i64 { i64::from_le_bytes(self.read_bytes()) }

	#[optimize(speed)]
	pub unsafe fn f32(&mut self) -> f32 { f32::from_le_bytes(self.read_bytes()) }

	#[optimize(speed)]
	pub unsafe fn f64(&mut self) -> f64 { f64::from_le_bytes(self.read_bytes()) }

	#[optimize(speed)]
	pub unsafe fn skip(&mut self, amount: usize) { self.data = self.data.add(amount); }

	#[optimize(speed)]
	pub unsafe fn string(&mut self) -> Option<CompactString> {
		let len = self.u16() as usize;
		self.assert_len(len)?;

		let out = CompactString::from_utf8_lossy(core::slice::from_raw_parts(self.data, len));
		self.data = self.data.add(len);
		Some(out)
	}
}
