use core::fmt::DebugList;
use std::alloc::{alloc, dealloc, Layout};
use std::fmt::{Debug, Display, Error, Formatter};
use std::intrinsics::likely;
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::Deref;
use std::thread::Scope;
use std::time::SystemTime;
use std::{fmt, fmt::Write};

use compact_str::{format_compact, CompactString, ToCompactString};
use hashbrown::raw::RawTable;

use crate::assets::*;
use crate::decoder::Decoder;
use crate::elements::chunk::{NbtChunk, NbtRegion};
use crate::elements::compound::{CompoundMap, CompoundMapIter, Entry, NbtCompound};
use crate::elements::element_action::ElementAction;
use crate::elements::list::{NbtList, ValueIterator, ValueMutIterator};
use crate::elements::string::NbtString;
use crate::encoder::UncheckedBufWriter;
use crate::panic_unchecked;
use crate::tab::FileFormat;
use crate::{array, primitive, DropFn, RenderContext, StrExt, VertexBufferBuilder};

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
					data: ManuallyDrop::new(NbtElementData { byte: self.data.byte }),
				},
				NbtShort::ID => Self {
					data: ManuallyDrop::new(NbtElementData { short: self.data.short }),
				},
				NbtInt::ID => Self {
					data: ManuallyDrop::new(NbtElementData { int: self.data.int }),
				},
				NbtLong::ID => Self {
					data: ManuallyDrop::new(NbtElementData { long: self.data.long }),
				},
				NbtFloat::ID => Self {
					data: ManuallyDrop::new(NbtElementData { float: self.data.float }),
				},
				NbtDouble::ID => Self {
					data: ManuallyDrop::new(NbtElementData { double: self.data.double }),
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
	pub fn from_str(mut s: &str) -> Option<(Option<CompactString>, Self)> {
		s = s.trim_start();

		if s.is_empty() {
			return None;
		}

		let prefix = s.snbt_string_read().and_then(|(prefix, s2)| s2.trim_start().strip_prefix(':').map(|s2| { s = s2.trim_start(); prefix }));
		let (s, element) = Self::from_str0(s).map(|(s, x)| (s.trim_start(), x))?;
		if !s.is_empty() {
			return None;
		}
		Some((prefix, element))
	}

	#[allow(clippy::too_many_lines)]
	pub(in crate::elements) fn from_str0(mut s: &str) -> Option<(&str, Self)> {
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

		if let Some(s2) = s.strip_prefix("NaN") {
			s = s2.trim_start();
			if let Some(s2) = s.strip_prefix('f') {
				return Some((s2.trim_start(), Self::Float(NbtFloat { value: f32::NAN })));
			} else if let Some(s2) = s.strip_prefix('d') {
				return Some((s2.trim_start(), Self::Double(NbtDouble { value: f64::NAN })));
			} else {
				return Some((s2.trim_start(), Self::Double(NbtDouble { value: f64::NAN })));
			}
		}

		if let Some(s2) = s.strip_prefix("Infinity").or_else(|| s.strip_prefix("inf")) {
			s = s2.trim_start();
			if let Some(s2) = s.strip_prefix('f') {
				return Some((s2.trim_start(), Self::Float(NbtFloat { value: f32::INFINITY })));
			} else if let Some(s2) = s.strip_prefix('d') {
				return Some((s2.trim_start(), Self::Double(NbtDouble { value: f64::INFINITY })));
			} else {
				return Some((s2.trim_start(), Self::Double(NbtDouble { value: f64::INFINITY })));
			}
		}

		if let Some(s2) = s.strip_prefix("-Infinity").or_else(|| s.strip_prefix("-inf")) {
			s = s2.trim_start();
			if let Some(s2) = s.strip_prefix('f') {
				return Some((s2.trim_start(), Self::Float(NbtFloat { value: f32::NEG_INFINITY })));
			} else if let Some(s2) = s.strip_prefix('d') {
				return Some((s2.trim_start(), Self::Double(NbtDouble { value: f64::NEG_INFINITY })));
			} else {
				return Some((s2.trim_start(), Self::Double(NbtDouble { value: f64::NEG_INFINITY })));
			}
		}

		let digit_end_idx = 'a: {
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
				break 'a 0;
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
			let suffix = (s[digit_end_idx..]).trim_start().as_bytes().first().map(u8::to_ascii_lowercase);
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
				Some(b'|') => Some({
					let mut s = s;
					let Ok(x @ 0..=31) = (s[..digit_end_idx]).parse::<u8>() else { return None };
					s = s[digit_end_idx..].trim_start().split_at(1).1.trim_start();
					let digit_end_idx = s.bytes().position(|x| !x.is_ascii_digit())?;
					let Ok(z @ 0..=31) = s[..digit_end_idx].parse::<u8>() else { return None };
					s = s[digit_end_idx..].trim_start();
					let (s, inner) = NbtCompound::from_str0(s)?;
					(
						s,
						Self::Chunk(NbtChunk::from_compound(
							inner,
							(x, z),
							FileFormat::Zlib,
							SystemTime::UNIX_EPOCH.elapsed().unwrap_or_else(|e| e.duration()).as_secs() as u32,
						)),
					)
				}),
				_ => Some((
					&s[digit_end_idx..],
					Self::Int(NbtInt {
						value: (s[..digit_end_idx]).parse().ok()?,
					}),
				)),
			};
		}

		NbtString::from_str0(s).map(|(s, x)| (s, Self::String(x)))
	}
	#[inline(never)]
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
			NbtString::ID => Self::String(NbtString::new(CompactString::new_inline(""))),
			NbtList::ID => Self::List(NbtList::new(vec![], 0x00)),
			NbtCompound::ID => Self::Compound(NbtCompound::new()),
			NbtIntArray::ID => Self::IntArray(NbtIntArray::new()),
			NbtLongArray::ID => Self::LongArray(NbtLongArray::new()),
			NbtChunk::ID => Self::Chunk(NbtChunk::from_compound(
				NbtCompound::new(),
				(0, 0),
				FileFormat::Zlib,
				SystemTime::UNIX_EPOCH.elapsed().unwrap_or_else(|e| e.duration()).as_secs() as u32,
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
		let start = std::time::Instant::now();
		let mut writer = UncheckedBufWriter::new();
		if self.id() == NbtCompound::ID {
			writer.write(&[0x0A, 0x00, 0x00]);
		}
		self.to_bytes(&mut writer);

		// #[cfg(debug_assertions)]
		println!("{}ms for file write", start.elapsed().as_nanos() as f64 / 1_000_000.0);
		writer.finish()
	}

	#[inline]
	#[must_use]
	pub fn from_mca(bytes: &[u8]) -> Option<Self> {
		// #[cfg(debug_assertions)]
		// let start = std::time::Instant::now();

		// #[cfg(debug_assertions)]
		// println!("{}ms for file read", start.elapsed().as_nanos() as f64 / 1_000_000.0);
		NbtRegion::from_bytes(bytes).map(Self::Region)
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
	#[must_use]
	pub fn display_name(&self) -> &'static str {
		match self.id() {
			NbtByte::ID => "Byte",
			NbtShort::ID => "Short",
			NbtInt::ID => "Int",
			NbtLong::ID => "Long",
			NbtFloat::ID => "Float",
			NbtDouble::ID => "Double",
			NbtByteArray::ID => "Byte Array",
			NbtString::ID => "String",
			NbtList::ID => "List",
			NbtCompound::ID => "Compound",
			NbtIntArray::ID => "Int Array",
			NbtLongArray::ID => "Long Array",
			NbtChunk::ID => "Chunk",
			NbtRegion::ID => "Region",
			_ => unsafe { panic_unchecked("Invalid element id") },
		}
	}

	#[inline]
	pub fn render_icon(id: u8, pos: impl Into<(usize, usize)>, z: u8, builder: &mut VertexBufferBuilder) {
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
	pub fn set_value(&mut self, value: CompactString) -> Option<CompactString> {
		unsafe {
			Some(match self.id() {
				NbtByte::ID => {
					let before = self.data.byte.value.to_compact_string();
					self.data.byte.set(value.parse().ok());
					before
				}
				NbtShort::ID => {
					let before = self.data.short.value.to_compact_string();
					self.data.short.set(value.parse().ok());
					before
				}
				NbtInt::ID => {
					let before = self.data.int.value.to_compact_string();
					self.data.int.set(value.parse().ok());
					before
				}
				NbtLong::ID => {
					let before = self.data.long.value.to_compact_string();
					self.data.long.set(value.parse().ok());
					before
				}
				NbtFloat::ID => {
					let before = self.data.float.value.to_compact_string();
					self.data.float.set(value.parse().ok());
					before
				}
				NbtDouble::ID => {
					let before = self.data.double.value.to_compact_string();
					self.data.double.set(value.parse().ok());
					before
				}
				NbtString::ID => core::mem::replace(self, Self::String(NbtString::new(value))).data.string.str.as_str().to_compact_string(),
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
	pub fn value(&self) -> (CompactString, bool) {
		unsafe {
			match self.id() {
				NbtByte::ID => (self.data.byte.value.to_compact_string(), true),
				NbtShort::ID => (self.data.short.value.to_compact_string(), true),
				NbtInt::ID => (self.data.int.value.to_compact_string(), true),
				NbtLong::ID => (self.data.long.value.to_compact_string(), true),
				NbtFloat::ID => (self.data.float.value.to_compact_string(), true),
				NbtDouble::ID => (self.data.double.value.to_compact_string(), true),
				NbtByteArray::ID => (self.data.byte_array.value(), false),
				NbtString::ID => (self.data.string.str.as_str().to_compact_string(), true),
				NbtList::ID => (self.data.list.value(), false),
				NbtCompound::ID => (self.data.compound.value(), false),
				NbtIntArray::ID => (self.data.int_array.value(), false),
				NbtLongArray::ID => (self.data.long_array.value(), false),
				NbtChunk::ID => (self.data.chunk.z.to_compact_string(), true),
				NbtRegion::ID => (self.data.region.value(), false),
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}

	#[inline]
	pub fn drop(&mut self, key: Option<CompactString>, element: Self, y: &mut usize, depth: usize, target_depth: usize, line_number: usize, indices: &mut Vec<usize>) -> DropFn {
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
					self.data.compound.insert(idx, CompactString::new_inline("_"), value);
					Ok(())
				}
				NbtIntArray::ID => self.data.int_array.insert(idx, value),
				NbtLongArray::ID => self.data.long_array.insert(idx, value),
				NbtRegion::ID => self.data.region.insert(idx, value),
				NbtChunk::ID => {
					self.data.chunk.insert_full(idx, CompactString::new_inline("_"), value);
					Ok(())
				}
				_ => Err(value),
			}
		}
	}

	#[inline]
	pub fn remove(&mut self, idx: usize) -> Option<(Option<CompactString>, Self)> {
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

	#[inline]
	#[must_use]
	#[allow(clippy::match_same_arms)]
	pub const fn actions(&self) -> &[ElementAction] {
		unsafe {
			match self.id() {
				NbtByte::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt],
				NbtShort::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt],
				NbtInt::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt],
				NbtLong::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt],
				NbtFloat::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt],
				NbtDouble::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt],
				NbtByteArray::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt, ElementAction::OpenArrayInHex],
				NbtString::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt],
				NbtList::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt],
				NbtCompound::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt],
				NbtIntArray::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt, ElementAction::OpenArrayInHex],
				NbtLongArray::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt, ElementAction::OpenArrayInHex],
				NbtChunk::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt],
				NbtRegion::ID => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::OpenInTxt],
				_ => core::hint::unreachable_unchecked(),
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
				NbtChunk::ID => write!(f, "{}", &*self.data.chunk),
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
	fn drop(&mut self) {
		unsafe {
			let id = self.id();
			let mut data = core::ptr::addr_of_mut!(self.data).read();
			match id {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => {
					let vec = &mut *data.byte_array.values;
					if !vec.is_empty() {
						dealloc(vec.as_mut_ptr().cast(), Layout::array::<Self>(vec.capacity()).unwrap_unchecked());
					}
					dealloc((vec as *mut Vec<Self>).cast(), Layout::new::<Vec<Self>>());
				}
				NbtString::ID => {
					core::ptr::addr_of_mut!(data.string.str).drop_in_place();
				}
				NbtList::ID => {
					let element = data.list.element;
					let list = &mut *data.list.elements;
					if element > NbtDouble::ID {
						for entry in &mut *list {
							(entry as *mut Self).drop_in_place();
						}
					}
					if !list.is_empty() {
						dealloc(list.as_mut_ptr().cast(), Layout::array::<Self>(list.capacity()).unwrap_unchecked());
					}
					dealloc((list as *mut Vec<Self>).cast(), Layout::new::<Vec<Self>>());
				}
				NbtCompound::ID => {
					let map = &mut *data.compound.entries;
					let CompoundMap { indices, entries } = map;
					(indices as *mut RawTable<usize>).drop_in_place();
					for Entry { value, key, .. } in &mut *entries {
						(value as *mut Self).drop_in_place();
						if key.is_heap_allocated() {
							dealloc(key.as_mut_ptr(), Layout::array::<u8>(key.len()).unwrap_unchecked());
						}
					}
					if !entries.is_empty() {
						dealloc(entries.as_mut_ptr().cast(), Layout::array::<Entry>(entries.capacity()).unwrap_unchecked());
					}
					dealloc((map as *mut CompoundMap).cast(), Layout::new::<CompoundMap>());
				}
				NbtChunk::ID => {
					let map = &mut *data.chunk.inner.entries;
					let CompoundMap { indices, entries } = map;
					(indices as *mut RawTable<usize>).drop_in_place();
					for Entry { value, key, .. } in &mut *entries {
						(value as *mut Self).drop_in_place();
						if key.is_heap_allocated() {
							dealloc(key.as_mut_ptr(), Layout::array::<u8>(key.len()).unwrap_unchecked());
						}
					}
					if !entries.is_empty() {
						dealloc(entries.as_mut_ptr().cast(), Layout::array::<Entry>(entries.capacity()).unwrap_unchecked());
					}
					dealloc((map as *mut CompoundMap).cast(), Layout::new::<CompoundMap>());
				}
				// no real speedup from using threads, seems to be memory-bound, or dealloc-call-bound
				NbtRegion::ID => {
					let (map, chunks) = *core::ptr::addr_of_mut!(data.region.chunks).read();
					drop(map);
					for mut chunk in core::mem::transmute::<_, [ManuallyDrop<Self>; 1024]>(chunks) {
						if !chunk.is_null() {
							let ptr = chunk.as_chunk_unchecked_mut().inner.as_mut();
							let map = &mut *ptr.entries;
							let CompoundMap { indices, entries } = map;
							(indices as *mut RawTable<usize>).drop_in_place();
							for Entry { value, key, .. } in &mut *entries {
								(value as *mut Self).drop_in_place();
								if key.is_heap_allocated() {
									dealloc(key.as_mut_ptr(), Layout::array::<u8>(key.len()).unwrap_unchecked());
								}
							}
							if !entries.is_empty() {
								dealloc(entries.as_mut_ptr().cast(), Layout::array::<Entry>(entries.capacity()).unwrap_unchecked());
							}
							dealloc((map as *mut CompoundMap).cast(), Layout::new::<CompoundMap>());
							dealloc((ptr as *mut NbtCompound).cast(), Layout::new::<NbtCompound>());
						}
					}
				}
				_ => {}
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
