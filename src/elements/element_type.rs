use std::fmt::{Debug, Display, Error, Formatter};
use std::intrinsics::likely;

use std::string::String;
use std::{fmt, fmt::Write};

use crate::assets::{BYTE_ARRAY_UV, BYTE_UV, CONNECTION_UV, DOUBLE_UV, FLOAT_UV, INT_ARRAY_UV, INT_UV, LONG_ARRAY_UV, LONG_UV, SHORT_UV};
use crate::decoder::Decoder;
use crate::elements::chunk::{NbtChunk, NbtRegion};
use crate::elements::compound::{CompoundMapIter, NbtCompound};
use crate::elements::list::{NbtList, ValueIterator, ValueMutIterator};
use crate::elements::string::NbtString;
use crate::tab::FileFormat;
use crate::{array, primitive, DropFn, RenderContext, StrExt, VertexBufferBuilder};
use crate::{panic_unchecked, OptionExt};
use core::fmt::DebugList;
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::Deref;
use std::thread::Scope;
use std::time::{Instant, SystemTime};
use std::alloc::{alloc, Layout};
use crate::encoder::UncheckedBufWriter;

primitive!(BYTE_UV, { Some('b') }, NbtByte, i8, 1);
primitive!(SHORT_UV, { Some('s') }, NbtShort, i16, 2);
primitive!(INT_UV, { None::<char> }, NbtInt, i32, 3);
primitive!(LONG_UV, { Some('L') }, NbtLong, i64, 4);
primitive!(FLOAT_UV, { Some('f') }, NbtFloat, f32, 5);
primitive!(DOUBLE_UV, { Some('d') }, NbtDouble, f64, 6);
array!(byte, NbtByteArray, i8, 7, 1, 'B', BYTE_ARRAY_UV, BYTE_UV);
array!(int, NbtIntArray, i32, 11, 3, 'I', INT_ARRAY_UV, INT_UV);
array!(long, NbtLongArray, i64, 12, 4, 'L', LONG_ARRAY_UV, LONG_UV);

#[repr(C)]
pub union NbtElementData {
	pub chunk: ManuallyDrop<NbtChunk>,
	pub region: ManuallyDrop<NbtRegion>,
	pub byte: ManuallyDrop<NbtByte>,
	pub short: ManuallyDrop<NbtShort>,
	pub int: ManuallyDrop<NbtInt>,
	pub long: ManuallyDrop<NbtLong>,
	pub float: ManuallyDrop<NbtFloat>,
	pub double: ManuallyDrop<NbtDouble>,
	pub byte_array: ManuallyDrop<NbtByteArray>,
	pub string: ManuallyDrop<NbtString>,
	pub list: ManuallyDrop<NbtList>,
	pub compound: ManuallyDrop<NbtCompound>,
	pub int_array: ManuallyDrop<NbtIntArray>,
	pub long_array: ManuallyDrop<NbtLongArray>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct NbtElementDiscriminant {
	_pad: [MaybeUninit<u8>; 23],
	pub id: u8,
}

#[repr(C)]
pub union NbtElement {
	pub data: ManuallyDrop<NbtElementData>,
	pub id: NbtElementDiscriminant,
}

impl Clone for NbtElement {
	#[inline(never)]
	fn clone(&self) -> Self {
		unsafe {
			let mut data = match self.id() {
				NbtChunk::ID => Self {
					data: ManuallyDrop::new(NbtElementData { chunk: self.data.chunk.clone() }),
				},
				NbtRegion::ID => Self {
					data: ManuallyDrop::new(NbtElementData { region: self.data.region.clone() }),
				},
				NbtByte::ID => Self {
					data: ManuallyDrop::new(NbtElementData { byte: self.data.byte.clone() }),
				},
				NbtShort::ID => Self {
					data: ManuallyDrop::new(NbtElementData { short: self.data.short.clone() }),
				},
				NbtInt::ID => Self {
					data: ManuallyDrop::new(NbtElementData { int: self.data.int.clone() }),
				},
				NbtLong::ID => Self {
					data: ManuallyDrop::new(NbtElementData { long: self.data.long.clone() }),
				},
				NbtFloat::ID => Self {
					data: ManuallyDrop::new(NbtElementData { float: self.data.float.clone() }),
				},
				NbtDouble::ID => Self {
					data: ManuallyDrop::new(NbtElementData { double: self.data.double.clone() }),
				},
				NbtByteArray::ID => Self {
					data: ManuallyDrop::new(NbtElementData {
						byte_array: self.data.byte_array.clone(),
					}),
				},
				NbtString::ID => Self {
					data: ManuallyDrop::new(NbtElementData { string: self.data.string.clone() }),
				},
				NbtList::ID => Self {
					data: ManuallyDrop::new(NbtElementData { list: self.data.list.clone() }),
				},
				NbtCompound::ID => Self {
					data: ManuallyDrop::new(NbtElementData { compound: self.data.compound.clone() }),
				},
				NbtIntArray::ID => Self {
					data: ManuallyDrop::new(NbtElementData {
						int_array: self.data.int_array.clone(),
					}),
				},
				NbtLongArray::ID => Self {
					data: ManuallyDrop::new(NbtElementData {
						long_array: self.data.long_array.clone(),
					}),
				},
				_ => core::hint::unreachable_unchecked(),
			};
			data.id.id = self.id.id;
			data
		}
	}
}

#[allow(non_snake_case)]
impl NbtElement {
	#[must_use]
	#[inline]
	pub const fn Byte(x: NbtByte) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { byte: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtByte::ID;
		this
	}

	#[must_use]
	#[inline]
	pub const fn Short(x: NbtShort) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { short: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtShort::ID;
		this
	}

	#[must_use]
	#[inline]
	pub const fn Int(x: NbtInt) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { int: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtInt::ID;
		this
	}

	#[must_use]
	#[inline]
	pub const fn Long(x: NbtLong) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { long: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtLong::ID;
		this
	}

	#[must_use]
	#[inline]
	pub const fn Float(x: NbtFloat) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { float: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtFloat::ID;
		this
	}

	#[must_use]
	#[inline]
	pub const fn Double(x: NbtDouble) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { double: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtDouble::ID;
		this
	}

	#[must_use]
	#[inline]
	pub fn ByteArray(x: NbtByteArray) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { byte_array: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtByteArray::ID;
		this
	}

	#[must_use]
	#[inline]
	pub fn String(x: NbtString) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { string: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtString::ID;
		this
	}

	#[must_use]
	#[inline]
	pub fn List(x: NbtList) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { list: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtList::ID;
		this
	}

	#[must_use]
	#[inline]
	pub fn Compound(x: NbtCompound) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { compound: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtCompound::ID;
		this
	}

	#[must_use]
	pub fn IntArray(x: NbtIntArray) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { int_array: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtIntArray::ID;
		this
	}

	#[must_use]
	#[inline]
	pub fn LongArray(x: NbtLongArray) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { long_array: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtLongArray::ID;
		this
	}

	#[must_use]
	#[inline]
	pub fn Chunk(x: NbtChunk) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { chunk: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtChunk::ID;
		this
	}

	#[must_use]
	#[inline]
	pub fn Region(x: NbtRegion) -> Self {
		let mut this = Self {
			data: ManuallyDrop::new(NbtElementData { region: ManuallyDrop::new(x) }),
		};
		this.id.id = NbtRegion::ID;
		this
	}
}

impl NbtElement {
	#[must_use]
	#[allow(clippy::should_implement_trait)] // i can't, sorry :(
	pub fn from_str(mut s: &str) -> Option<(Option<Box<str>>, Self)> {
		s = s.trim_start();

		if s.is_empty() {
			return None;
		}

		let prefix = s.snbt_string_read().and_then(|(prefix, s2)| {
			s2.strip_prefix(':').map(|s2| {
				s = s2;
				prefix
			})
		});
		let element = Self::from_str0(s).map(|(_, x)| x)?;
		Some((prefix, element))
	}
	pub(in crate::elements) fn from_str0(s: &str) -> Option<(&str, Self)> {
		if let Some(s2) = s.strip_prefix("false") {
			return Some((s2, Self::Byte(NbtByte { value: 0 })));
		}
		if let Some(s2) = s.strip_prefix("true") {
			return Some((s2, Self::Byte(NbtByte { value: 1 })));
		}
		if s.starts_with("[B;") {
			return NbtByteArray::from_str0(s).map(|(s, x)| (s, Self::ByteArray(x)));
		}
		if s.starts_with("[I;") {
			return NbtIntArray::from_str0(s).map(|(s, x)| (s, Self::IntArray(x)));
		}
		if s.starts_with("[L;") {
			return NbtLongArray::from_str0(s).map(|(s, x)| (s, Self::LongArray(x)));
		}
		if s.starts_with('[') {
			return NbtList::from_str0(s).map(|(s, x)| (s, Self::List(x)));
		}
		if s.starts_with('{') {
			return NbtCompound::from_str0(s).map(|(s, x)| (s, Self::Compound(x)));
		}
		if s.starts_with('"') {
			return NbtString::from_str0(s).map(|(s, x)| (s, Self::String(x)));
		}

		let digit_end_idx = {
			let mut s = s;
			let mut digit_end_idx = 0;

			if s.starts_with('+') || s.starts_with('-') {
				s = &s[1..];
				digit_end_idx += 1;
			}

			let int_part = s.bytes().take_while(u8::is_ascii_digit).count();
			s = &s[int_part..];
			digit_end_idx += int_part;
			if int_part == 0 && !s.starts_with('.') {
				return None;
			}
			if let Some(s2) = s.strip_prefix('.') {
				digit_end_idx += 1;
				s = s2;
				let frac_part = s.bytes().take_while(u8::is_ascii_digit).count();
				digit_end_idx += frac_part;
			}

			digit_end_idx
		};
		if digit_end_idx > 0 {
			let suffix = (s[digit_end_idx..]).as_bytes().first().map(u8::to_ascii_lowercase);
			return match suffix {
				Some(b'b') => Some((
					&s[(digit_end_idx + 1)..],
					Self::Byte(NbtByte {
						value: (s[..digit_end_idx]).parse().ok()?,
					}),
				)),
				Some(b's') => Some((
					&s[(digit_end_idx + 1)..],
					Self::Short(NbtShort {
						value: (s[..digit_end_idx]).parse().ok()?,
					}),
				)),
				Some(b'l') => Some((
					&s[(digit_end_idx + 1)..],
					Self::Long(NbtLong {
						value: (s[..digit_end_idx]).parse().ok()?,
					}),
				)),
				Some(b'f') => Some((
					&s[(digit_end_idx + 1)..],
					Self::Float(NbtFloat {
						value: (s[..digit_end_idx]).parse().ok()?,
					}),
				)),
				Some(b'd') => Some((
					&s[(digit_end_idx + 1)..],
					Self::Double(NbtDouble {
						value: (s[..digit_end_idx]).parse().ok()?,
					}),
				)),
				_ => Some((
					&s[digit_end_idx..],
					Self::Int(NbtInt {
						value: (s[..digit_end_idx]).parse().ok()?,
					}),
				)),
			};
		}

		None
	}
	#[inline(never)]
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	pub fn from_bytes(element: u8, decoder: &mut Decoder) -> Option<Self> {
		Some(match element {
			NbtByte::ID => Self::Byte(NbtByte::from_bytes(decoder)?),
			NbtShort::ID => Self::Short(NbtShort::from_bytes(decoder)?),
			NbtInt::ID => Self::Int(NbtInt::from_bytes(decoder)?),
			NbtLong::ID => Self::Long(NbtLong::from_bytes(decoder)?),
			NbtFloat::ID => Self::Float(NbtFloat::from_bytes(decoder)?),
			NbtDouble::ID => Self::Double(NbtDouble::from_bytes(decoder)?),
			NbtByteArray::ID => Self::ByteArray(NbtByteArray::from_bytes(decoder)?),
			NbtString::ID => Self::String(NbtString::from_bytes(decoder)?),
			NbtList::ID => Self::List(NbtList::from_bytes(decoder)?),
			NbtCompound::ID => Self::Compound(NbtCompound::from_bytes(decoder)?),
			NbtIntArray::ID => Self::IntArray(NbtIntArray::from_bytes(decoder)?),
			NbtLongArray::ID => Self::LongArray(NbtLongArray::from_bytes(decoder)?),
			_ => return None,
		})
	}

	#[inline(never)]
	pub fn to_bytes(&self, writer: &mut UncheckedBufWriter) {
		unsafe {
			match self.id() {
				NbtByte::ID => self.data.byte.to_bytes(writer),
				NbtShort::ID => self.data.short.to_bytes(writer),
				NbtInt::ID => self.data.int.to_bytes(writer),
				NbtLong::ID => self.data.long.to_bytes(writer),
				NbtFloat::ID => self.data.float.to_bytes(writer),
				NbtDouble::ID => self.data.double.to_bytes(writer),
				NbtByteArray::ID => self.data.byte_array.to_bytes(writer),
				NbtString::ID => self.data.string.to_bytes(writer),
				NbtList::ID => self.data.list.to_bytes(writer),
				NbtCompound::ID => self.data.compound.to_bytes(writer),
				NbtIntArray::ID => self.data.int_array.to_bytes(writer),
				NbtLongArray::ID => self.data.long_array.to_bytes(writer),
				NbtChunk::ID => self.data.chunk.to_bytes(writer),
				NbtRegion::ID => self.data.region.to_bytes(writer),
				_ => core::hint::unreachable_unchecked(),
			};
		}
	}

	#[inline]
	#[must_use]
	pub const fn id(&self) -> u8 {
		unsafe { self.id.id }
	}

	#[inline]
	#[must_use]
	pub const fn is_null(&self) -> bool {
		self.id() == 0
	}

	#[inline]
	#[must_use]
	pub fn from_id(id: u8) -> Option<Self> {
		Some(match id {
			NbtByte::ID => Self::Byte(NbtByte::default()),
			NbtShort::ID => Self::Short(NbtShort::default()),
			NbtInt::ID => Self::Int(NbtInt::default()),
			NbtLong::ID => Self::Long(NbtLong::default()),
			NbtFloat::ID => Self::Float(NbtFloat::default()),
			NbtDouble::ID => Self::Double(NbtDouble::default()),
			NbtByteArray::ID => Self::ByteArray(NbtByteArray::new()),
			NbtString::ID => Self::String(NbtString::new(String::new().into_boxed_str())),
			NbtList::ID => Self::List(NbtList::new(vec![], 0x00)),
			NbtCompound::ID => Self::Compound(NbtCompound::new()),
			NbtIntArray::ID => Self::IntArray(NbtIntArray::new()),
			NbtLongArray::ID => Self::LongArray(NbtLongArray::new()),
			NbtChunk::ID => Self::Chunk(NbtChunk::from_compound(
				NbtCompound::new(),
				(0, 0),
				FileFormat::Zlib,
				unsafe { SystemTime::UNIX_EPOCH.elapsed().ok().panic_unchecked("Time can't go before the epoch") }.as_secs() as u32,
			)),
			_ => return None,
		})
	}

	#[inline]
	#[must_use]
	pub fn from_file(bytes: &[u8]) -> Option<Self> {
		// #[cfg(debug_assertions)]
		// let start = std::time::Instant::now();
		let mut decoder = Decoder::new(bytes);
		decoder.assert_len(3)?;
		unsafe {
			if decoder.u8() != 0x0A {
				return None;
			}
			let skip = decoder.u16() as usize;
			decoder.skip(skip);
		}
		let nbt = Self::Compound(NbtCompound::from_bytes(&mut decoder)?);
		// #[cfg(debug_assertions)]
		// println!("{}ms for file read", start.elapsed().as_nanos() as f64 / 1_000_000.0);
		Some(nbt)
	}

	#[inline]
	#[must_use]
	pub fn to_file(&self) -> Vec<u8> {
		// #[cfg(debug_assertions)]
		// let start = std::time::Instant::now();
		let mut writer = UncheckedBufWriter::new();
		if self.id() == NbtCompound::ID {
			writer.write(&[0x0A, 0x00, 0x00]);
		}
		self.to_bytes(&mut writer);
		let res = writer.finish();
		// #[cfg(debug_assertions)]
		// println!("{}ms for file write", start.elapsed().as_nanos() as f64 / 1_000_000.0);
		res
	}

	#[inline]
	#[must_use]
	pub fn from_mca(bytes: &[u8]) -> Option<Self> {
		// #[cfg(debug_assertions)]
		// let start = std::time::Instant::now();
		let res = NbtRegion::from_bytes(bytes).map(Self::Region);
		// #[cfg(debug_assertions)]
		// println!("{}ms for file read", start.elapsed().as_nanos() as f64 / 1_000_000.0);
		res
	}

	#[inline]
	pub fn render(&self, remaining_scroll: &mut usize, builder: &mut VertexBufferBuilder, str: Option<&str>, tail: bool, ctx: &mut RenderContext) {
		unsafe {
			match self.id() {
				NbtByte::ID => self.data.byte.render(builder, str, ctx),
				NbtShort::ID => self.data.short.render(builder, str, ctx),
				NbtInt::ID => self.data.int.render(builder, str, ctx),
				NbtLong::ID => self.data.long.render(builder, str, ctx),
				NbtFloat::ID => self.data.float.render(builder, str, ctx),
				NbtDouble::ID => self.data.double.render(builder, str, ctx),
				NbtByteArray::ID => self.data.byte_array.render(builder, str, remaining_scroll, tail, ctx),
				NbtString::ID => self.data.string.render(builder, str, ctx),
				NbtList::ID => self.data.list.render(builder, str, remaining_scroll, tail, ctx),
				NbtCompound::ID => self.data.compound.render(builder, str, remaining_scroll, tail, ctx),
				NbtIntArray::ID => self.data.int_array.render(builder, str, remaining_scroll, tail, ctx),
				NbtLongArray::ID => self.data.long_array.render(builder, str, remaining_scroll, tail, ctx),
				NbtChunk::ID => self.data.chunk.render(builder, remaining_scroll, tail, ctx),
				NbtRegion::ID => {
					// can't be done at all
				}
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn len(&self) -> Option<usize> {
		unsafe {
			Some(match self.id() {
				NbtCompound::ID => self.data.compound.len(),
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID | NbtList::ID => self.data.list.len(),
				NbtRegion::ID => self.data.region.len(),
				NbtChunk::ID => self.data.chunk.inner.len(),
				_ => return None,
			})
		}
	}

	#[inline]
	#[must_use]
	pub fn is_empty(&self) -> bool {
		self.len().is_some_and(|x| x > 0)
	}

	#[inline]
	pub fn render_icon(id: u8, pos: impl Into<(usize, usize)>, z: f32, builder: &mut VertexBufferBuilder) {
		match id {
			0 => {}
			NbtByte::ID => NbtByte::render_icon(pos, z, builder),
			NbtShort::ID => NbtShort::render_icon(pos, z, builder),
			NbtInt::ID => NbtInt::render_icon(pos, z, builder),
			NbtLong::ID => NbtLong::render_icon(pos, z, builder),
			NbtFloat::ID => NbtFloat::render_icon(pos, z, builder),
			NbtDouble::ID => NbtDouble::render_icon(pos, z, builder),
			NbtByteArray::ID => NbtByteArray::render_icon(pos, z, builder),
			NbtString::ID => NbtString::render_icon(pos, z, builder),
			NbtList::ID => NbtList::render_icon(pos, z, builder),
			NbtCompound::ID => NbtCompound::render_icon(pos, z, builder),
			NbtIntArray::ID => NbtIntArray::render_icon(pos, z, builder),
			NbtLongArray::ID => NbtLongArray::render_icon(pos, z, builder),
			NbtChunk::ID => NbtChunk::render_icon(pos, z, builder),
			NbtRegion::ID => NbtRegion::render_icon(pos, z, builder),
			_ => unsafe { panic_unchecked("Invalid element id") },
		}
	}

	#[inline]
	#[must_use]
	pub fn height(&self) -> usize {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.data.byte_array.height(),
				NbtList::ID => self.data.list.height(),
				NbtCompound::ID => self.data.compound.height(),
				NbtChunk::ID => self.data.chunk.height(),
				NbtRegion::ID => self.data.region.height(),
				_ => 1,
			}
		}
	}

	#[inline]
	pub fn swap(&mut self, a: usize, b: usize) {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID | NbtList::ID => self.data.list.elements.swap(a, b),
				NbtCompound::ID => self.data.compound.entries.swap(a, b),
				NbtChunk::ID => self.data.chunk.inner.entries.swap(a, b),
				NbtRegion::ID => self.data.region.chunks.as_mut().0.swap(a, b),
				_ => {}
			}
		}
	}

	#[inline]
	#[must_use]
	#[allow(clippy::type_complexity)] // a type probably shouldn't abstract what this is, like... yeah
	pub fn children(&self) -> Option<Result<ValueIterator, CompoundMapIter<'_>>> {
		unsafe {
			Some(match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID | NbtList::ID => Ok(self.data.list.children()),
				NbtCompound::ID => Err(self.data.compound.children()),
				NbtChunk::ID => Err(self.data.chunk.children()),
				NbtRegion::ID => Ok(self.data.region.children()),
				_ => return None,
			})
		}
	}

	#[inline]
	pub fn set_value(&mut self, value: Box<str>) -> Option<Box<str>> {
		unsafe {
			Some(match self.id() {
				NbtByte::ID => {
					let before = self.data.byte.value.to_string().into();
					self.data.byte.set(value.parse().ok());
					before
				}
				NbtShort::ID => {
					let before = self.data.short.value.to_string().into();
					self.data.short.set(value.parse().ok());
					before
				}
				NbtInt::ID => {
					let before = self.data.int.value.to_string().into();
					self.data.int.set(value.parse().ok());
					before
				}
				NbtLong::ID => {
					let before = self.data.long.value.to_string().into();
					self.data.long.set(value.parse().ok());
					before
				}
				NbtFloat::ID => {
					let before = self.data.float.value.to_string().into();
					self.data.float.set(value.parse().ok());
					before
				}
				NbtDouble::ID => {
					let before = self.data.double.value.to_string().into();
					self.data.double.set(value.parse().ok());
					before
				}
				NbtString::ID => core::mem::replace(&mut self.data.string.str, value),
				_ => return None,
			})
		}
	}

	#[must_use]
	pub fn true_height(&self) -> usize {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.data.byte_array.true_height(),
				NbtList::ID => self.data.list.true_height(),
				NbtCompound::ID => self.data.compound.true_height(),
				NbtRegion::ID => self.data.region.true_height(),
				NbtChunk::ID => self.data.chunk.true_height(),
				_ => 1,
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn toggle(&mut self) -> Option<()> {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.data.byte_array.toggle(),
				NbtList::ID => self.data.list.toggle(),
				NbtCompound::ID => self.data.compound.toggle(),
				NbtRegion::ID => self.data.region.toggle(),
				NbtChunk::ID => self.data.chunk.toggle(),
				_ => None,
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn open(&self) -> bool {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.data.byte_array.open(),
				NbtList::ID => self.data.list.open(),
				NbtCompound::ID => self.data.compound.open(),
				NbtRegion::ID => self.data.region.open(),
				NbtChunk::ID => self.data.chunk.open(),
				_ => false,
			}
		}
	}

	#[inline]
	pub fn increment(&mut self, amount: usize, true_amount: usize) {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.data.byte_array.increment(amount, true_amount),
				NbtList::ID => self.data.list.increment(amount, true_amount),
				NbtCompound::ID => self.data.compound.increment(amount, true_amount),
				NbtRegion::ID => self.data.region.increment(amount, true_amount),
				NbtChunk::ID => self.data.chunk.increment(amount, true_amount),
				_ => {}
			}
		}
	}

	#[inline]
	pub fn decrement(&mut self, amount: usize, true_amount: usize) {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.data.byte_array.decrement(amount, true_amount),
				NbtList::ID => self.data.list.decrement(amount, true_amount),
				NbtCompound::ID => self.data.compound.decrement(amount, true_amount),
				NbtRegion::ID => self.data.region.decrement(amount, true_amount),
				NbtChunk::ID => self.data.chunk.decrement(amount, true_amount),
				_ => {}
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn value(&self) -> (Box<str>, bool) {
		unsafe {
			match self.id() {
				NbtByte::ID => (self.data.byte.value.to_string().into_boxed_str(), true),
				NbtShort::ID => (self.data.short.value.to_string().into_boxed_str(), true),
				NbtInt::ID => (self.data.int.value.to_string().into_boxed_str(), true),
				NbtLong::ID => (self.data.long.value.to_string().into_boxed_str(), true),
				NbtFloat::ID => (self.data.float.value.to_string().into_boxed_str(), true),
				NbtDouble::ID => (self.data.double.value.to_string().into_boxed_str(), true),
				NbtByteArray::ID => (self.data.byte_array.value().into_boxed_str(), false),
				NbtString::ID => (self.data.string.unwrap().to_owned().into_boxed_str(), true),
				NbtList::ID => (self.data.list.value().into_boxed_str(), false),
				NbtCompound::ID => (self.data.compound.value().into_boxed_str(), false),
				NbtIntArray::ID => (self.data.int_array.value().into_boxed_str(), false),
				NbtLongArray::ID => (self.data.long_array.value().into_boxed_str(), false),
				NbtChunk::ID => (self.data.chunk.z.to_string().into_boxed_str(), true),
				NbtRegion::ID => (self.data.region.value().into_boxed_str(), false),
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}

	#[inline]
	pub fn drop(&mut self, key: Option<Box<str>>, element: Self, y: &mut usize, depth: usize, target_depth: usize, line_number: usize, indices: &mut Vec<usize>) -> DropFn {
		unsafe {
			let height = self.height() * 16;
			match self.id() {
				_ if *y >= height + 8 => {
					*y -= height;
					DropFn::Missed(key, element)
				}
				NbtByteArray::ID => self.data.byte_array.drop(key, element, y, depth, target_depth, line_number, indices),
				NbtList::ID => self.data.list.drop(key, element, y, depth, target_depth, line_number, indices),
				NbtCompound::ID => self.data.compound.drop(key, element, y, depth, target_depth, line_number, indices),
				NbtIntArray::ID => self.data.int_array.drop(key, element, y, depth, target_depth, line_number, indices),
				NbtLongArray::ID => self.data.long_array.drop(key, element, y, depth, target_depth, line_number, indices),
				NbtChunk::ID => NbtCompound::drop(&mut self.data.chunk.inner, key, element, y, depth, target_depth, line_number, indices),
				NbtRegion::ID => self.data.region.drop(key, element, y, depth, target_depth, line_number, indices),
				_ => {
					*y = y.saturating_sub(16);
					DropFn::Missed(key, element)
				}
			}
		}
	}

	#[inline]
	pub fn shut(&mut self) {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.data.byte_array.shut(),
				NbtList::ID => self.data.list.shut(),
				NbtCompound::ID => self.data.compound.shut(),
				NbtChunk::ID => self.data.chunk.shut(),
				NbtRegion::ID => self.data.region.shut(),
				_ => {}
			}
		}
	}

	#[inline]
	pub fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.data.byte_array.expand(),
				NbtList::ID => self.data.list.expand(scope),
				NbtCompound::ID => self.data.compound.expand(scope),
				NbtChunk::ID => self.data.chunk.inner.expand(scope),
				NbtRegion::ID => self.data.region.expand(scope),
				_ => {}
			}
		}
	}

	/// # Errors
	///
	/// * `self` cannot contain that specific variant of `Self`, i.e. `Self::NbtByte` in an `Self::NbtIntArray`
	///
	/// If any changes are made to this error list then duplicate may have to be updated as it relies on this never occurring
	#[inline]
	pub fn insert(&mut self, idx: usize, value: Self) -> Result<(), Self> {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.data.byte_array.insert(idx, value),
				NbtList::ID => self.data.list.insert(idx, value),
				NbtCompound::ID => {
					self.data.compound.insert(idx, "_".to_owned(), value);
					Ok(())
				}
				NbtIntArray::ID => self.data.int_array.insert(idx, value),
				NbtLongArray::ID => self.data.long_array.insert(idx, value),
				NbtRegion::ID => self.data.region.insert(idx, value),
				NbtChunk::ID => {
					self.data.chunk.insert_full(idx, "_".to_owned(), value);
					Ok(())
				}
				_ => Err(value),
			}
		}
	}

	#[inline]
	pub fn remove(&mut self, idx: usize) -> Option<(Option<Box<str>>, Self)> {
		unsafe {
			Some(match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => (None, self.data.byte_array.remove(idx)),
				NbtList::ID => (None, self.data.list.remove(idx)),
				NbtCompound::ID => return self.data.compound.remove_idx(idx).map(|(a, b)| (Some(a), b)),
				NbtRegion::ID => (None, self.data.region.remove(idx)),
				NbtChunk::ID => return self.data.chunk.remove_idx(idx).map(|(a, b)| (Some(a), b)),
				_ => return None,
			})
		}
	}

	#[inline]
	#[must_use]
	pub fn get(&self, idx: usize) -> Option<&Self> {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.data.byte_array.get(idx),
				NbtList::ID => self.data.list.get(idx),
				NbtCompound::ID => self.data.compound.get(idx).map(|(_, x)| x),
				NbtRegion::ID => self.data.region.get(idx),
				NbtChunk::ID => self.data.chunk.get(idx).map(|(_, x)| x),
				_ => None,
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<&mut Self> {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.data.byte_array.get_mut(idx),
				NbtList::ID => self.data.list.get_mut(idx),
				NbtCompound::ID => self.data.compound.get_mut(idx).map(|(_, x)| x),
				NbtRegion::ID => self.data.region.get_mut(idx),
				NbtChunk::ID => self.data.chunk.get_mut(idx).map(|(_, x)| x),
				_ => None,
			}
		}
	}

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Chunk`
	#[inline]
	#[must_use]
	pub unsafe fn into_chunk_unchecked(self) -> NbtChunk {
		core::mem::transmute(core::mem::transmute::<_, NbtElementData>(self).chunk)
	}

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Chunk`
	#[inline]
	#[must_use]
	pub unsafe fn as_chunk_unchecked(&self) -> &NbtChunk {
		&self.data.chunk
	}

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Chunk`
	#[inline]
	#[must_use]
	pub unsafe fn as_chunk_unchecked_mut(&mut self) -> &mut NbtChunk {
		&mut self.data.chunk
	}

	#[inline]
	#[must_use]
	pub fn max_depth(&self) -> usize {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.data.deref().byte_array.max_depth(),
				NbtList::ID => self.data.deref().list.max_depth(),
				NbtCompound::ID => self.data.deref().compound.max_depth(),
				NbtRegion::ID => self.data.deref().region.max_depth(),
				NbtChunk::ID => self.data.deref().chunk.deref().inner.max_depth(),
				_ => 0,
			}
		}
	}
}

impl Display for NbtElement {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		unsafe {
			match self.id() {
				NbtByte::ID => write!(f, "{}", &*self.data.byte),
				NbtShort::ID => write!(f, "{}", &*self.data.short),
				NbtInt::ID => write!(f, "{}", &*self.data.int),
				NbtLong::ID => write!(f, "{}", &*self.data.long),
				NbtFloat::ID => write!(f, "{}", &*self.data.float),
				NbtDouble::ID => write!(f, "{}", &*self.data.double),
				NbtByteArray::ID => write!(f, "{}", &*self.data.byte_array),
				NbtString::ID => write!(f, "{}", &*self.data.string),
				NbtList::ID => write!(f, "{}", &*self.data.list),
				NbtCompound::ID => write!(f, "{}", &*self.data.compound),
				NbtIntArray::ID => write!(f, "{}", &*self.data.int_array),
				NbtLongArray::ID => write!(f, "{}", &*self.data.long_array),
				NbtChunk::ID => write!(f, "{}", self.data.chunk.inner.as_ref()),
				NbtRegion::ID => Err(Error),
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}
}

impl Debug for NbtElement {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		unsafe {
			match self.id() {
				NbtByte::ID => Debug::fmt(&*self.data.byte, f),
				NbtShort::ID => Debug::fmt(&*self.data.short, f),
				NbtInt::ID => Debug::fmt(&*self.data.int, f),
				NbtLong::ID => Debug::fmt(&*self.data.long, f),
				NbtFloat::ID => Debug::fmt(&*self.data.float, f),
				NbtDouble::ID => Debug::fmt(&*self.data.double, f),
				NbtByteArray::ID => Debug::fmt(&*self.data.byte_array, f),
				NbtString::ID => Debug::fmt(&*self.data.string, f),
				NbtList::ID => Debug::fmt(&*self.data.list, f),
				NbtCompound::ID => Debug::fmt(&*self.data.compound, f),
				NbtIntArray::ID => Debug::fmt(&*self.data.int_array, f),
				NbtLongArray::ID => Debug::fmt(&*self.data.long_array, f),
				NbtChunk::ID => Debug::fmt(&*self.data.chunk, f),
				NbtRegion::ID => Err(Error),
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}
}

impl Drop for NbtElement {
	#[cfg_attr(not(debug_assertions), no_panic::no_panic)]
	// todo, there shouldn't be a panicking branch
	fn drop(&mut self) {
		unsafe {
			let id = self.id();
			let mut data = core::ptr::addr_of_mut!(self.data).read();
			match id {
				NbtByteArray::ID => { core::ptr::addr_of_mut!(data.byte_array.values).drop_in_place(); },
				NbtString::ID => { core::ptr::addr_of_mut!(data.string.str).drop_in_place(); },
				NbtList::ID => { core::ptr::addr_of_mut!(data.list.elements).drop_in_place(); },
				NbtCompound::ID => { core::ptr::addr_of_mut!(data.compound.entries).drop_in_place(); },
				NbtIntArray::ID => { core::ptr::addr_of_mut!(data.int_array.values).drop_in_place(); },
				NbtLongArray::ID => { core::ptr::addr_of_mut!(data.long_array.values).drop_in_place(); },
				NbtChunk::ID => { core::ptr::addr_of_mut!(data.chunk.inner).drop_in_place(); },
				NbtRegion::ID => std::thread::scope(move |s| {
					let (map, chunks) = *core::ptr::addr_of_mut!(data.region.chunks).read();
					drop(map);
					for chunk in core::mem::transmute::<_, [ManuallyDrop<Self>; 1024]>(chunks) {
						s.spawn(move || {
							if !chunk.is_null() {
								let start = Instant::now();
								drop(ManuallyDrop::into_inner(core::mem::transmute::<_, NbtElementData>(chunk).chunk).inner);
								println!("took {}ms", start.elapsed().as_nanos() as f64 / 1_000_000.0);
							}
						});
					}
				}),
				_ => {},
			}
		}
	}
}

#[inline]
#[must_use]
pub fn id_to_string_name(id: u8) -> (&'static str, &'static str) {
	match id {
		0 => ("entry", "entries"),
		NbtByte::ID => ("byte", "bytes"),
		NbtShort::ID => ("short", "shorts"),
		NbtInt::ID => ("int", "ints"),
		NbtLong::ID => ("long", "longs"),
		NbtFloat::ID => ("float", "floats"),
		NbtDouble::ID => ("double", "doubles"),
		NbtByteArray::ID => ("byte array", "byte arrays"),
		NbtString::ID => ("string", "strings"),
		NbtList::ID => ("list", "lists"),
		NbtCompound::ID => ("compound", "compounds"),
		NbtIntArray::ID => ("int array", "int arrays"),
		NbtLongArray::ID => ("long array", "long arrays"),
		NbtChunk::ID => ("chunk", "chunks"),
		NbtRegion::ID => ("region", "regions"),
		_ => unsafe { panic_unchecked("Invalid id") },
	}
}
