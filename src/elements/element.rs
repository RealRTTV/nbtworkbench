use std::{fmt, fmt::Write};
use std::alloc::{alloc, dealloc, Layout};
use std::fmt::{Debug, Display, Error, Formatter};
use std::intrinsics::likely;
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::{Deref, Index, IndexMut};
use std::slice::{Iter, IterMut};
#[cfg(not(target_arch = "wasm32"))]
use std::thread::Scope;

use compact_str::{CompactString, format_compact, ToCompactString};
use hashbrown::raw::RawTable;
use polonius_the_crab::{polonius, polonius_return};

use crate::{array, assets::JUST_OVERLAPPING_BASE_TEXT_Z, DropFn, primitive, RenderContext, since_epoch, StrExt, TextColor, VertexBufferBuilder, NbtElementAndKey};
use crate::assets::{BASE_Z, BYTE_ARRAY_UV, BYTE_UV, CONNECTION_UV, DOUBLE_UV, FLOAT_UV, INT_ARRAY_UV, INT_UV, LONG_ARRAY_UV, LONG_UV, SHORT_UV, ZOffset};
use crate::be_decoder::BigEndianDecoder;
use crate::element_action::ElementAction;
use crate::elements::chunk::{NbtChunk, NbtRegion};
use crate::elements::compound::{CompoundMap, CompoundMapIter, Entry, NbtCompound};
use crate::elements::list::NbtList;
use crate::elements::null::NbtNull;
use crate::elements::string::NbtString;
use crate::encoder::UncheckedBufWriter;
use crate::formatter::PrettyFormatter;
use crate::le_decoder::LittleEndianDecoder;
use crate::marked_line::MarkedLines;
use crate::tab::FileFormat;

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
#[derive(Copy, Clone)]
pub struct NbtElementId {
	_pad: [MaybeUninit<u8>; 23],
	pub id: u8,
}

#[repr(C)]
pub union NbtElement {
	chunk: ManuallyDrop<NbtChunk>,
	region: ManuallyDrop<NbtRegion>,
	byte: ManuallyDrop<NbtByte>,
	short: ManuallyDrop<NbtShort>,
	int: ManuallyDrop<NbtInt>,
	long: ManuallyDrop<NbtLong>,
	float: ManuallyDrop<NbtFloat>,
	double: ManuallyDrop<NbtDouble>,
	byte_array: ManuallyDrop<NbtByteArray>,
	string: ManuallyDrop<NbtString>,
	list: ManuallyDrop<NbtList>,
	compound: ManuallyDrop<NbtCompound>,
	int_array: ManuallyDrop<NbtIntArray>,
	long_array: ManuallyDrop<NbtLongArray>,
	null: ManuallyDrop<NbtNull>,
	id: NbtElementId,
}

impl NbtElement {
	pub fn matches(&self, other: &Self) -> bool {
		if self.id() != other.id() { return false }

		unsafe {
			match self.id() {
				NbtChunk::ID => self.chunk.matches(&other.chunk),
				NbtRegion::ID => self.region.matches(&other.region),
				NbtByte::ID => self.byte.matches(&other.byte),
				NbtShort::ID => self.short.matches(&other.short),
				NbtInt::ID => self.int.matches(&other.int),
				NbtLong::ID => self.long.matches(&other.long),
				NbtFloat::ID => self.float.matches(&other.float),
				NbtDouble::ID => self.double.matches(&other.double),
				NbtByteArray::ID => self.byte_array.matches(&other.byte_array),
				NbtString::ID => self.string.matches(&other.string),
				NbtList::ID => self.list.matches(&other.list),
				NbtCompound::ID => self.compound.matches(&other.compound),
				NbtIntArray::ID => self.int_array.matches(&other.int_array),
				NbtLongArray::ID => self.long_array.matches(&other.long_array),
				NbtNull::ID => self.null.matches(&other.null),
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}
}

impl Clone for NbtElement {
	#[inline(never)]
	fn clone(&self) -> Self {
		unsafe {
			let mut element = match self.id() {
				NbtChunk::ID => Self {
					chunk: self.chunk.clone(),
				},
				NbtRegion::ID => Self {
					region: self.region.clone(),
				},
				NbtByte::ID => Self { byte: self.byte },
				NbtShort::ID => Self { short: self.short },
				NbtInt::ID => Self { int: self.int },
				NbtLong::ID => Self { long: self.long },
				NbtFloat::ID => Self { float: self.float },
				NbtDouble::ID => Self {
					double: self.double,
				},
				NbtByteArray::ID => Self {
					byte_array: self.byte_array.clone(),
				},
				NbtString::ID => Self {
					string: self.string.clone(),
				},
				NbtList::ID => Self {
					list: self.list.clone(),
				},
				NbtCompound::ID => Self {
					compound: self.compound.clone(),
				},
				NbtIntArray::ID => Self {
					int_array: self.int_array.clone(),
				},
				NbtLongArray::ID => Self {
					long_array: self.long_array.clone(),
				},
				NbtNull::ID => Self {
					null: self.null.clone()
				},
				_ => core::hint::unreachable_unchecked(),
			};
			element.id.id = self.id.id;
			element
		}
	}
}

#[allow(non_snake_case)]
impl NbtElement {
	pub const NULL: NbtElement = unsafe { core::mem::zeroed() };
	pub const NULL_REF: &'static NbtElement = &Self::NULL;

	#[inline]
	pub fn set_id(&mut self, id: u8) {
		unsafe { core::ptr::write(core::ptr::addr_of_mut!(self.id.id), id); }
	}

	#[must_use]
	#[inline]
	pub fn Byte(this: NbtByte) -> Self {
		let mut this = Self {
			byte: ManuallyDrop::new(this),
		};
		this.set_id(NbtByte::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn Short(this: NbtShort) -> Self {
		let mut this = Self {
			short: ManuallyDrop::new(this),
		};
		this.set_id(NbtShort::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn Int(this: NbtInt) -> Self {
		let mut this = Self {
			int: ManuallyDrop::new(this),
		};
		this.set_id(NbtInt::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn Long(this: NbtLong) -> Self {
		let mut this = Self {
			long: ManuallyDrop::new(this),
		};
		this.set_id(NbtLong::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn Float(this: NbtFloat) -> Self {
		let mut this = Self {
			float: ManuallyDrop::new(this),
		};
		this.set_id(NbtFloat::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn Double(this: NbtDouble) -> Self {
		let mut this = Self {
			double: ManuallyDrop::new(this),
		};
		this.set_id(NbtDouble::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn ByteArray(this: NbtByteArray) -> Self {
		let mut this = Self {
			byte_array: ManuallyDrop::new(this),
		};
		this.set_id(NbtByteArray::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn String(this: NbtString) -> Self {
		let mut this = Self {
			string: ManuallyDrop::new(this),
		};
		this.set_id(NbtString::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn List(this: NbtList) -> Self {
		let mut this = Self {
			list: ManuallyDrop::new(this),
		};
		this.set_id(NbtList::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn Compound(this: NbtCompound) -> Self {
		let mut this = Self {
			compound: ManuallyDrop::new(this),
		};
		this.set_id(NbtCompound::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn IntArray(this: NbtIntArray) -> Self {
		let mut this = Self {
			int_array: ManuallyDrop::new(this),
		};
		this.set_id(NbtIntArray::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn LongArray(this: NbtLongArray) -> Self {
		let mut this = Self {
			long_array: ManuallyDrop::new(this),
		};
		this.set_id(NbtLongArray::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn Chunk(this: NbtChunk) -> Self {
		let mut this = Self {
			chunk: ManuallyDrop::new(this),
		};
		this.set_id(NbtChunk::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn Region(this: NbtRegion) -> Self {
		let mut this = Self {
			region: ManuallyDrop::new(this),
		};
		this.set_id(NbtRegion::ID);
		this
	}

	#[must_use]
	#[inline]
	pub fn Null(this: NbtNull) -> Self {
		let mut this = Self {
			null: ManuallyDrop::new(this),
		};
		this.set_id(NbtNull::ID);
		this
	}
}

impl NbtElement {
	#[must_use]
	#[allow(clippy::should_implement_trait)] // i can't, sorry :(
	pub fn from_str(mut s: &str) -> Result<(Option<CompactString>, Self), usize> {
		let total_len = s.len();
		s = s.trim();

		if s.is_empty() { return Err(total_len - s.len()) }

		let prefix = s.snbt_string_read().ok().and_then(|(prefix, s2)| {
			s2.trim_start().strip_prefix(':').filter(|s| !s.is_empty()).map(|s2| {
				s = s2.trim_start();
				prefix
			})
		});
		let (s, element) = Self::from_str0(s).map(|(s, x)| (s.trim_start(), x)).map_err(|x| total_len - x)?;
		if !s.is_empty() { return Err(total_len - s.len()) }
		Ok((prefix, element))
	}

	#[allow(clippy::too_many_lines)]
	pub(in crate::elements) fn from_str0(mut s: &str) -> Result<(&str, Self), usize> {
		if let Some(s2) = s.strip_prefix("false") { return Ok((s2, Self::Byte(NbtByte { value: 0 }))) }
		if let Some(s2) = s.strip_prefix("true") { return Ok((s2, Self::Byte(NbtByte { value: 1 }))) }
		if s.starts_with("[B;") { return NbtByteArray::from_str0(s).map(|(s, x)| (s, Self::ByteArray(x))) }
		if s.starts_with("[I;") { return NbtIntArray::from_str0(s).map(|(s, x)| (s, Self::IntArray(x))) }
		if s.starts_with("[L;") { return NbtLongArray::from_str0(s).map(|(s, x)| (s, Self::LongArray(x))) }
		if s.starts_with('[') { return NbtList::from_str0(s).map(|(s, x)| (s, Self::List(x))) }
		if s.starts_with('{') { return NbtCompound::from_str0(s).map(|(s, x)| (s, Self::Compound(x))) }
		if s.starts_with('"') { return NbtString::from_str0(s).map(|(s, x)| (s, Self::String(x))) }

		if let Some(s2) = s.strip_prefix("NaN") {
			s = s2.trim_start();
			return if let Some(s2) = s.strip_prefix('f') {
				Ok((s2.trim_start(), Self::Float(NbtFloat { value: f32::NAN })))
			} else {
				Ok((s2.strip_prefix('d').unwrap_or(s2).trim_start(), Self::Double(NbtDouble { value: f64::NAN })))
			};
		}

		if let Some(s2) = s.strip_prefix("Infinity").or_else(|| s.strip_prefix("inf")) {
			s = s2.trim_start();
			return if let Some(s2) = s.strip_prefix('f') {
				Ok((
					s2.trim_start(),
					Self::Float(NbtFloat {
						value: f32::INFINITY,
					}),
				))
			} else {
				Ok((
					s2.strip_prefix('d').unwrap_or(s2).trim_start(),
					Self::Double(NbtDouble {
						value: f64::INFINITY,
					}),
				))
			};
		}

		if let Some(s2) = s
			.strip_prefix("-Infinity")
			.or_else(|| s.strip_prefix("-inf"))
		{
			s = s2.trim_start();
			return if let Some(s2) = s.strip_prefix('f') {
				Ok((
					s2.trim_start(),
					Self::Float(NbtFloat {
						value: f32::NEG_INFINITY,
					}),
				))
			} else if let Some(s2) = s.strip_prefix('d') {
				Ok((
					s2.trim_start(),
					Self::Double(NbtDouble {
						value: f64::NEG_INFINITY,
					}),
				))
			} else {
				Ok((
					s2.trim_start(),
					Self::Double(NbtDouble {
						value: f64::NEG_INFINITY,
					}),
				))
			};
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
			let suffix = s[digit_end_idx..]
				.trim_start()
				.as_bytes()
				.first()
				.map(u8::to_ascii_lowercase);
			return match suffix {
				Some(b'b') => Ok((
					&s[(digit_end_idx + 1)..],
					Self::Byte(NbtByte {
						value: s[..digit_end_idx].parse().map_err(|_| s.len())?,
					}),
				)),
				Some(b's') => Ok((
					&s[(digit_end_idx + 1)..],
					Self::Short(NbtShort {
						value: s[..digit_end_idx].parse().map_err(|_| s.len())?,
					}),
				)),
				Some(b'l') => Ok((
					&s[(digit_end_idx + 1)..],
					Self::Long(NbtLong {
						value: s[..digit_end_idx].parse().map_err(|_| s.len())?,
					}),
				)),
				Some(b'f') => Ok((
					&s[(digit_end_idx + 1)..],
					Self::Float(NbtFloat {
						value: s[..digit_end_idx].parse().map_err(|_| s.len())?,
					}),
				)),
				Some(b'd') => Ok((
					&s[(digit_end_idx + 1)..],
					Self::Double(NbtDouble {
						value: s[..digit_end_idx].parse().map_err(|_| s.len())?,
					}),
				)),
				Some(b'|') => Ok({
					let mut s = s;
					let Ok(x @ 0..=31) = s[..digit_end_idx].parse::<u8>() else {
						return Err(s.len());
					};
					s = s[digit_end_idx..].trim_start().split_at(1).1.trim_start();
					let digit_end_idx = s.bytes().position(|x| !x.is_ascii_digit()).ok_or(s.len())?;
					let Ok(z @ 0..=31) = s[..digit_end_idx].parse::<u8>() else {
						return Err(s.len());
					};
					s = s[digit_end_idx..].trim_start();
					let (s, inner) = NbtCompound::from_str0(s)?;
					(
						s,
						Self::Chunk(NbtChunk::from_compound(
							inner,
							(x, z),
							FileFormat::Zlib,
							since_epoch().as_secs() as u32,
						)),
					)
				}),
				_ => Ok((
					&s[digit_end_idx..],
					Self::Int(NbtInt {
						value: s[..digit_end_idx].parse().map_err(|_| s.len())?,
					}),
				)),
			};
		}

		NbtString::from_str0(s).map(|(s, x)| (s, Self::String(x)))
	}

	#[inline(never)]
	pub fn from_be_bytes(element: u8, decoder: &mut BigEndianDecoder) -> Option<Self> {
		Some(match element {
			NbtByte::ID => Self::Byte(NbtByte::from_be_bytes(decoder)?),
			NbtShort::ID => Self::Short(NbtShort::from_be_bytes(decoder)?),
			NbtInt::ID => Self::Int(NbtInt::from_be_bytes(decoder)?),
			NbtLong::ID => Self::Long(NbtLong::from_be_bytes(decoder)?),
			NbtFloat::ID => Self::Float(NbtFloat::from_be_bytes(decoder)?),
			NbtDouble::ID => Self::Double(NbtDouble::from_be_bytes(decoder)?),
			NbtByteArray::ID => Self::ByteArray(NbtByteArray::from_be_bytes(decoder)?),
			NbtString::ID => Self::String(NbtString::from_be_bytes(decoder)?),
			NbtList::ID => Self::List(NbtList::from_be_bytes(decoder)?),
			NbtCompound::ID => Self::Compound(NbtCompound::from_be_bytes(decoder)?),
			NbtIntArray::ID => Self::IntArray(NbtIntArray::from_be_bytes(decoder)?),
			NbtLongArray::ID => Self::LongArray(NbtLongArray::from_be_bytes(decoder)?),
			_ => return None,
		})
	}

	#[inline(never)]
	pub fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) {
		unsafe {
			match self.id() {
				NbtByte::ID => self.byte.to_be_bytes(writer),
				NbtShort::ID => self.short.to_be_bytes(writer),
				NbtInt::ID => self.int.to_be_bytes(writer),
				NbtLong::ID => self.long.to_be_bytes(writer),
				NbtFloat::ID => self.float.to_be_bytes(writer),
				NbtDouble::ID => self.double.to_be_bytes(writer),
				NbtByteArray::ID => self.byte_array.to_be_bytes(writer),
				NbtString::ID => self.string.to_be_bytes(writer),
				NbtList::ID => self.list.to_be_bytes(writer),
				NbtCompound::ID => self.compound.to_be_bytes(writer),
				NbtIntArray::ID => self.int_array.to_be_bytes(writer),
				NbtLongArray::ID => self.long_array.to_be_bytes(writer),
				NbtChunk::ID => self.chunk.to_be_bytes(writer),
				NbtRegion::ID => self.region.to_be_bytes(writer),
				NbtNull::ID => self.null.to_be_bytes(writer),
				_ => core::hint::unreachable_unchecked(),
			};
		}
	}

	#[inline(never)]
	pub fn from_le_bytes(element: u8, decoder: &mut LittleEndianDecoder) -> Option<Self> {
		Some(match element {
			NbtByte::ID => Self::Byte(NbtByte::from_le_bytes(decoder)?),
			NbtShort::ID => Self::Short(NbtShort::from_le_bytes(decoder)?),
			NbtInt::ID => Self::Int(NbtInt::from_le_bytes(decoder)?),
			NbtLong::ID => Self::Long(NbtLong::from_le_bytes(decoder)?),
			NbtFloat::ID => Self::Float(NbtFloat::from_le_bytes(decoder)?),
			NbtDouble::ID => Self::Double(NbtDouble::from_le_bytes(decoder)?),
			NbtByteArray::ID => Self::ByteArray(NbtByteArray::from_le_bytes(decoder)?),
			NbtString::ID => Self::String(NbtString::from_le_bytes(decoder)?),
			NbtList::ID => Self::List(NbtList::from_le_bytes(decoder)?),
			NbtCompound::ID => Self::Compound(NbtCompound::from_le_bytes(decoder)?),
			NbtIntArray::ID => Self::IntArray(NbtIntArray::from_le_bytes(decoder)?),
			NbtLongArray::ID => Self::LongArray(NbtLongArray::from_le_bytes(decoder)?),
			_ => return None,
		})
	}

	#[inline(never)]
	pub fn to_le_bytes(&self, writer: &mut UncheckedBufWriter) {
		unsafe {
			match self.id() {
				NbtByte::ID => self.byte.to_le_bytes(writer),
				NbtShort::ID => self.short.to_le_bytes(writer),
				NbtInt::ID => self.int.to_le_bytes(writer),
				NbtLong::ID => self.long.to_le_bytes(writer),
				NbtFloat::ID => self.float.to_le_bytes(writer),
				NbtDouble::ID => self.double.to_le_bytes(writer),
				NbtByteArray::ID => self.byte_array.to_le_bytes(writer),
				NbtString::ID => self.string.to_le_bytes(writer),
				NbtList::ID => self.list.to_le_bytes(writer),
				NbtCompound::ID => self.compound.to_le_bytes(writer),
				NbtIntArray::ID => self.int_array.to_le_bytes(writer),
				NbtLongArray::ID => self.long_array.to_le_bytes(writer),
				NbtChunk::ID => { /* no */ },
				NbtRegion::ID => { /* no */ },
				NbtNull::ID => self.null.to_le_bytes(writer),
				_ => core::hint::unreachable_unchecked(),
			};
		}
	}

	#[inline]
	#[must_use]
	pub const fn id(&self) -> u8 { unsafe { self.id.id } }

	#[inline]
	#[must_use]
	pub fn from_id(id: u8) -> Self {
		match id {
			NbtByte::ID => Self::Byte(NbtByte::default()),
			NbtShort::ID => Self::Short(NbtShort::default()),
			NbtInt::ID => Self::Int(NbtInt::default()),
			NbtLong::ID => Self::Long(NbtLong::default()),
			NbtFloat::ID => Self::Float(NbtFloat::default()),
			NbtDouble::ID => Self::Double(NbtDouble::default()),
			NbtByteArray::ID => Self::ByteArray(NbtByteArray::new()),
			NbtString::ID => Self::String(NbtString::new(CompactString::const_new(""))),
			NbtList::ID => Self::List(NbtList::new(vec![], 0x00)),
			NbtCompound::ID => Self::Compound(NbtCompound::new()),
			NbtIntArray::ID => Self::IntArray(NbtIntArray::new()),
			NbtLongArray::ID => Self::LongArray(NbtLongArray::new()),
			NbtChunk::ID => Self::Chunk(NbtChunk::from_compound(
				NbtCompound::new(),
				(0, 0),
				FileFormat::Zlib,
				since_epoch().as_secs() as u32,
			)),
			_ => Self::Null(NbtNull),
		}
	}

	#[inline]
	#[must_use]
	pub fn from_be_file(bytes: &[u8]) -> Option<Self> {
		let mut decoder = BigEndianDecoder::new(bytes);
		decoder.assert_len(1)?;
		unsafe {
			if decoder.u8() != NbtCompound::ID { return None }
			// fix for >= 1.20.2 protocol since they removed the empty field
			if decoder.assert_len(2).is_none() || decoder.data.cast::<u16>().read_unaligned() == 0_u16.to_be() {
				let _ = decoder.u16();
			}
		}
		let nbt = Self::Compound(NbtCompound::from_be_bytes(&mut decoder)?);
		Some(nbt)
	}

	#[inline]
	#[must_use]
	pub fn to_be_file(&self) -> Vec<u8> {
		let mut writer = UncheckedBufWriter::new();
		if self.id() == NbtCompound::ID {
			writer.write(&[NbtCompound::ID, 0x00, 0x00]);
		}
		self.to_be_bytes(&mut writer);
		writer.finish()
	}

	#[inline]
	#[must_use]
	pub fn from_be_mca(bytes: &[u8]) -> Option<Self> {
		NbtRegion::from_be_bytes(bytes).map(Self::Region)
	}

	#[inline]
	#[must_use]
	pub fn from_le_file(bytes: &[u8]) -> Option<(Self, bool)> {
		let mut decoder = LittleEndianDecoder::new(bytes);
		unsafe {
			decoder.assert_len(1)?;
			let kind = decoder.u8();
			match kind {
				NbtCompound::ID => {
					decoder.assert_len(2)?;
					let skip = decoder.u16() as usize;
					decoder.skip(skip);
					Some((Self::Compound(NbtCompound::from_le_bytes(&mut decoder)?), decoder.header()))
				},
				NbtList::ID => {
					decoder.assert_len(2)?;
					let skip = decoder.u16() as usize;
					decoder.skip(skip);
					Some((Self::List(NbtList::from_le_bytes(&mut decoder)?), decoder.header()))
				},
				_ => return None,
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn to_le_file(&self, header: bool) -> Vec<u8> {
		let mut writer = UncheckedBufWriter::new();
		writer.write(&[self.id(), 0x00, 0x00]);
		self.to_le_bytes(&mut writer);
		let raw = writer.finish();
		if header {
			let mut header = UncheckedBufWriter::new();
			header.write(&[0x08, 0x00, 0x00, 0x00]);
			header.write(&(raw.len() as u32).to_le_bytes());
			header.write(&raw);
			header.finish()
		} else {
			raw
		}
	}

	#[inline]
	pub fn render(&self, remaining_scroll: &mut usize, builder: &mut VertexBufferBuilder, str: Option<&str>, tail: bool, ctx: &mut RenderContext) {
		unsafe {
			match self.id() {
				NbtByte::ID => self.byte.render(builder, str, ctx),
				NbtShort::ID => self.short.render(builder, str, ctx),
				NbtInt::ID => self.int.render(builder, str, ctx),
				NbtLong::ID => self.long.render(builder, str, ctx),
				NbtFloat::ID => self.float.render(builder, str, ctx),
				NbtDouble::ID => self.double.render(builder, str, ctx),
				NbtByteArray::ID => self.byte_array.render(builder, str, remaining_scroll, tail, ctx),
				NbtString::ID => self.string.render(builder, str, ctx),
				NbtList::ID => self.list.render(builder, str, remaining_scroll, tail, ctx),
				NbtCompound::ID => self.compound.render(builder, str, remaining_scroll, tail, ctx),
				NbtIntArray::ID => self.int_array.render(builder, str, remaining_scroll, tail, ctx),
				NbtLongArray::ID => self.long_array.render(builder, str, remaining_scroll, tail, ctx),
				NbtChunk::ID => self.chunk.render(builder, remaining_scroll, tail, ctx),
				NbtRegion::ID => {
					// can't be done at all
				},
				NbtNull::ID => self.null.render(builder, str, ctx),
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn len(&self) -> Option<usize> {
		unsafe {
			Some(match self.id() {
				NbtCompound::ID => self.compound.len(),
				NbtByteArray::ID => self.byte_array.len(),
				NbtIntArray::ID => self.int_array.len(),
				NbtLongArray::ID => self.long_array.len(),
				NbtList::ID => self.list.len(),
				NbtRegion::ID => self.region.len(),
				NbtChunk::ID => self.chunk.len(),
				_ => return None,
			})
		}
	}

	#[inline]
	#[must_use]
	pub fn is_empty(&self) -> bool { self.len().is_some_and(|x| x > 0) }

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
			NbtNull::ID => "null",
			_ => panic!("Invalid element id"),
		}
	}

	#[inline]
	pub fn render_icon(&self, pos: impl Into<(usize, usize)>, z: ZOffset, builder: &mut VertexBufferBuilder) {
		unsafe {
			match self.id() {
				NbtByte::ID => self.byte.render_icon(pos, z, builder),
				NbtShort::ID => self.short.render_icon(pos, z, builder),
				NbtInt::ID => self.int.render_icon(pos, z, builder),
				NbtLong::ID => self.long.render_icon(pos, z, builder),
				NbtFloat::ID => self.float.render_icon(pos, z, builder),
				NbtDouble::ID => self.double.render_icon(pos, z, builder),
				NbtByteArray::ID => self.byte_array.render_icon(pos, z, builder),
				NbtString::ID => self.string.render_icon(pos, z, builder),
				NbtList::ID => self.list.render_icon(pos, z, builder),
				NbtCompound::ID => self.compound.render_icon(pos, z, builder),
				NbtIntArray::ID => self.int_array.render_icon(pos, z, builder),
				NbtLongArray::ID => self.long_array.render_icon(pos, z, builder),
				NbtChunk::ID => self.chunk.render_icon(pos, z, builder),
				NbtRegion::ID => self.region.render_icon(pos, z, builder),
				NbtNull::ID => self.null.render_icon(pos, z, builder),
				_ => panic!("Invalid element id"),
			}
		}
	}

	#[inline]
	pub fn on_root_style_change(&mut self, bookmarks: &mut MarkedLines) {
		unsafe {
			match self.id() {
				NbtRegion::ID => self.region.on_root_style_change(bookmarks),
				_ => {}
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn height(&self) -> usize {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.height(),
				NbtIntArray::ID => self.int_array.height(),
				NbtLongArray::ID => self.long_array.height(),
				NbtList::ID => self.list.height(),
				NbtCompound::ID => self.compound.height(),
				NbtChunk::ID => self.chunk.height(),
				NbtRegion::ID => self.region.height(),
				_ => 1,
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn has_keys(&self) -> bool {
		match self.id() {
			NbtCompound::ID | NbtChunk::ID => true,
			_ => false,
		}
	}

	#[inline]
	#[must_use]
	#[allow(clippy::type_complexity)] // a type probably shouldn't abstract what this is, like... yeah
	pub fn children(&self) -> Option<Result<Iter<'_, NbtElement>, CompoundMapIter<'_>>> {
		unsafe {
			Some(match self.id() {
				NbtByteArray::ID => Ok(self.byte_array.children()),
				NbtIntArray::ID => Ok(self.int_array.children()),
				NbtLongArray::ID => Ok(self.long_array.children()),
				NbtList::ID => Ok(self.list.children()),
				NbtCompound::ID => Err(self.compound.children()),
				NbtChunk::ID => Err(self.chunk.children()),
				NbtRegion::ID => Ok(self.region.children()),
				_ => return None,
			})
		}
	}

	#[inline]
	pub fn set_value(&mut self, value: CompactString) -> Option<(CompactString, bool)> {
		unsafe {
			Some(match self.id() {
				NbtByte::ID => {
					let before = self.byte.value();
					let success = value.parse().map(|x| self.byte.value = x).is_ok();
					(before, success)
				}
				NbtShort::ID => {
					let before = self.short.value();
					let success = value.parse().map(|x| self.short.value = x).is_ok();
					(before, success)
				}
				NbtInt::ID => {
					let before = self.int.value();
					let success = value.parse().map(|x| self.int.value = x).is_ok();
					(before, success)
				}
				NbtLong::ID => {
					let before = self.long.value();
					let success = value.parse().map(|x| self.long.value = x).is_ok();
					(before, success)
				}
				NbtFloat::ID => {
					let before = self.float.value();
					let success = value.parse().map(|x| self.float.value = x).is_ok();
					(before, success)
				}
				NbtDouble::ID => {
					let before = self.double.value();
					let success = value.parse().map(|x| self.double.value = x).is_ok();
					(before, success)
				}
				NbtString::ID => (
					core::mem::replace(self, Self::String(NbtString::new(value)))
						.string
						.str
						.as_str()
						.to_compact_string(),
					true,
				),
				_ => return None,
			})
		}
	}

	#[must_use]
	pub fn true_height(&self) -> usize {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.true_height(),
				NbtIntArray::ID => self.int_array.true_height(),
				NbtLongArray::ID => self.long_array.true_height(),
				NbtList::ID => self.list.true_height(),
				NbtCompound::ID => self.compound.true_height(),
				NbtRegion::ID => self.region.true_height(),
				NbtChunk::ID => self.chunk.true_height(),
				_ => 1,
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn toggle(&mut self) -> Option<()> {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.toggle(),
				NbtIntArray::ID => self.int_array.toggle(),
				NbtLongArray::ID => self.long_array.toggle(),
				NbtList::ID => self.list.toggle(),
				NbtCompound::ID => self.compound.toggle(),
				NbtRegion::ID => self.region.toggle(),
				NbtChunk::ID => self.chunk.toggle(),
				_ => None,
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn open(&self) -> bool {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.open(),
				NbtIntArray::ID => self.int_array.open(),
				NbtLongArray::ID => self.long_array.open(),
				NbtList::ID => self.list.open(),
				NbtCompound::ID => self.compound.open(),
				NbtRegion::ID => self.region.is_open(),
				NbtChunk::ID => self.chunk.open(),
				_ => false,
			}
		}
	}

	#[inline]
	pub fn increment(&mut self, amount: usize, true_amount: usize) {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.increment(amount, true_amount),
				NbtIntArray::ID => self.int_array.increment(amount, true_amount),
				NbtLongArray::ID => self.long_array.increment(amount, true_amount),
				NbtList::ID => self.list.increment(amount, true_amount),
				NbtCompound::ID => self.compound.increment(amount, true_amount),
				NbtRegion::ID => self.region.increment(amount, true_amount),
				NbtChunk::ID => self.chunk.increment(amount, true_amount),
				_ => {}
			}
		}
	}

	#[inline]
	pub fn decrement(&mut self, amount: usize, true_amount: usize) {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.decrement(amount, true_amount),
				NbtIntArray::ID => self.int_array.decrement(amount, true_amount),
				NbtLongArray::ID => self.long_array.decrement(amount, true_amount),
				NbtList::ID => self.list.decrement(amount, true_amount),
				NbtCompound::ID => self.compound.decrement(amount, true_amount),
				NbtRegion::ID => self.region.decrement(amount, true_amount),
				NbtChunk::ID => self.chunk.decrement(amount, true_amount),
				_ => {}
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn value(&self) -> (CompactString, TextColor) {
		unsafe {
			match self.id() {
				NbtByte::ID => (self.byte.value(), TextColor::TreePrimitive),
				NbtShort::ID => (self.short.value(), TextColor::TreePrimitive),
				NbtInt::ID => (self.int.value(), TextColor::TreePrimitive),
				NbtLong::ID => (self.long.value(), TextColor::TreePrimitive),
				NbtFloat::ID => (self.float.value(), TextColor::TreePrimitive),
				NbtDouble::ID => (self.double.value(), TextColor::TreePrimitive),
				NbtByteArray::ID => (self.byte_array.value(), TextColor::TreeKey),
				NbtString::ID => (self.string.str.as_str().to_compact_string(), TextColor::TreeString),
				NbtList::ID => (self.list.value(), TextColor::TreeKey),
				NbtCompound::ID => (self.compound.value(), TextColor::TreeKey),
				NbtIntArray::ID => (self.int_array.value(), TextColor::TreeKey),
				NbtLongArray::ID => (self.long_array.value(), TextColor::TreeKey),
				NbtChunk::ID => (self.chunk.value(), TextColor::TreeKey),
				NbtRegion::ID => (self.region.value(), TextColor::TreeKey),
				NbtNull::ID => (CompactString::const_new("null"), TextColor::TreeKey),
				_ => panic!("Unknown element id"),
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn is_primitive(&self) -> bool {
		match self.id() {
			NbtByte::ID | NbtShort::ID | NbtInt::ID | NbtLong::ID | NbtFloat::ID | NbtDouble::ID | NbtString::ID => true,
			_ => false,
		}
	}

	#[inline]
	#[must_use]
	pub fn is_default_state(&self) -> bool {
		unsafe {
			match self.id() {
				NbtByte::ID => self.byte.value == 0,
				NbtShort::ID => self.short.value == 0,
				NbtInt::ID => self.int.value == 0,
				NbtLong::ID => self.long.value == 0,
				NbtFloat::ID => self.float.value == 0.0,
				NbtDouble::ID => self.double.value == 0.0,
				NbtByteArray::ID => self.byte_array.is_empty(),
				NbtString::ID => self.string.str.as_str().is_empty(),
				NbtList::ID => self.list.is_empty(),
				NbtCompound::ID => self.compound.is_empty(),
				NbtIntArray::ID => self.int_array.is_empty(),
				NbtLongArray::ID => self.long_array.is_empty(),
				NbtChunk::ID => self.chunk.is_unloaded(),
				NbtRegion::ID => self.region.loaded_chunks() == 0,
				NbtNull::ID => true,
				_ => panic!("Unknown element id"),
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn is_grid_layout(&self) -> bool {
		unsafe {
			match self.id() {
				NbtRegion::ID => self.region.is_grid_layout(),
				_ => false,
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn should_render_description(&self) -> bool {
		unsafe {
			match self.id() {
				NbtChunk::ID => self.chunk.is_loaded() && !(self.chunk.x == 0 && self.chunk.z == 0 && self.chunk.is_empty()),
				_ => true,
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
					DropFn::Missed((key, element))
				}
				NbtByteArray::ID => self
					.byte_array
					.drop(key, element, y, depth, target_depth, line_number, indices),
				NbtList::ID => self
					.list
					.drop(key, element, y, depth, target_depth, line_number, indices),
				NbtCompound::ID => self
					.compound
					.drop(key, element, y, depth, target_depth, line_number, indices),
				NbtIntArray::ID => self
					.int_array
					.drop(key, element, y, depth, target_depth, line_number, indices),
				NbtLongArray::ID => self
					.long_array
					.drop(key, element, y, depth, target_depth, line_number, indices),
				NbtChunk::ID => NbtCompound::drop(
					&mut self.chunk,
					key,
					element,
					y,
					depth,
					target_depth,
					line_number,
					indices,
				),
				NbtRegion::ID => self
					.region
					.drop(key, element, y, depth, target_depth, line_number, indices),
				_ => {
					*y = y.saturating_sub(16);
					DropFn::Missed((key, element))
				}
			}
		}
	}

	#[inline]
	pub fn shut(&mut self) {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.shut(),
				NbtIntArray::ID => self.int_array.shut(),
				NbtLongArray::ID => self.long_array.shut(),
				NbtList::ID => self.list.shut(),
				NbtCompound::ID => self.compound.shut(),
				NbtChunk::ID => self.chunk.shut(),
				NbtRegion::ID => self.region.shut(),
				_ => {}
			}
		}
	}

	#[cfg(not(target_arch = "wasm32"))]
	#[inline]
	pub fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.expand(),
				NbtIntArray::ID => self.int_array.expand(),
				NbtLongArray::ID => self.long_array.expand(),
				NbtList::ID => self.list.expand(scope),
				NbtCompound::ID => self.compound.expand(scope),
				NbtChunk::ID => self.chunk.expand(scope),
				NbtRegion::ID => self.region.expand(scope),
				_ => {}
			}
		}
	}

	#[cfg(target_arch = "wasm32")]
	#[inline]
	pub fn expand(&mut self) {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.expand(),
				NbtIntArray::ID => self.int_array.expand(),
				NbtLongArray::ID => self.long_array.expand(),
				NbtList::ID => self.list.expand(),
				NbtCompound::ID => self.compound.expand(),
				NbtChunk::ID => self.chunk.expand(),
				NbtRegion::ID => self.region.expand(),
				_ => {}
			}
		}
	}

	/// # Errors
	///
	/// * `self` cannot contain that specific variant of `Self`, i.e. `Self::NbtByte` in an `Self::NbtIntArray`
	///
	/// If any changes are made to this error list, then the duplicate may have to be updated as it relies on this never occurring
	#[inline]
	pub unsafe fn insert(&mut self, idx: usize, value: NbtElementAndKey) -> Result<Option<Self>, Self> {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.insert(idx, value.1),
				NbtList::ID => self.list.insert(idx, value.1),
				NbtCompound::ID => {
					self.compound.insert(idx, value.0.unwrap_or(CompactString::const_new("_")), value.1);
					Ok(None)
				}
				NbtIntArray::ID => self.int_array.insert(idx, value.1),
				NbtLongArray::ID => self.long_array.insert(idx, value.1),
				NbtRegion::ID => self.region.insert(idx, value.1),
				NbtChunk::ID => {
					self.chunk.insert(idx, value.0.unwrap_or(CompactString::const_new("_")), value.1);
					Ok(None)
				}
				_ => Err(value.1),
			}
		}
	}

	#[inline]
	pub unsafe fn replace_key_value(&mut self, idx: usize, kv: NbtElementAndKey) -> Option<NbtElementAndKey> {
		unsafe {
			match self.id() {
				NbtByteArray::ID => Some((None, self.byte_array.replace(idx, kv.1)?)),
				NbtIntArray::ID => Some((None, self.int_array.replace(idx, kv.1)?)),
				NbtLongArray::ID => Some((None, self.long_array.replace(idx, kv.1)?)),
				NbtList::ID => Some((None, self.list.replace(idx, kv.1)?)),
				NbtCompound::ID => self.compound.replace(idx, kv.0?, kv.1),
				NbtChunk::ID => self.chunk.replace(idx, kv.0?, kv.1),
				NbtRegion::ID => Some((None, self.region.replace(idx, kv.1).ok()??)),
				_ => None,
			}
		}
	}

	#[inline]
	pub unsafe fn remove(&mut self, idx: usize) -> Option<(Option<CompactString>, Self)> {
		unsafe {
			Some(match self.id() {
				NbtByteArray::ID => (None, self.byte_array.remove(idx)),
				NbtIntArray::ID => (None, self.int_array.remove(idx)),
				NbtLongArray::ID => (None, self.long_array.remove(idx)),
				NbtList::ID => (None, self.list.remove(idx)),
				NbtCompound::ID => return self.compound.remove(idx).map(|(a, b)| (Some(a), b)),
				NbtRegion::ID => (None, self.region.replace_with_empty(idx)),
				NbtChunk::ID => return self.chunk.remove(idx).map(|(a, b)| (Some(a), b)),
				_ => return None,
			})
		}
	}

	#[inline]
	pub fn swap(&mut self, a: usize, b: usize) {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.values.swap(a, b),
				NbtIntArray::ID => self.int_array.values.swap(a, b),
				NbtLongArray::ID  => self.long_array.values.swap(a, b),
				NbtList::ID => self.list.elements.swap(a, b),
				NbtCompound::ID => self.compound.entries.swap(a, b),
				NbtChunk::ID => self.chunk.entries.swap(a, b),
				NbtRegion::ID => self.region.swap(a, b),
				_ => {}
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn get(&self, idx: usize) -> Option<&Self> {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.get(idx),
				NbtIntArray::ID => self.int_array.get(idx),
				NbtLongArray::ID => self.long_array.get(idx),
				NbtList::ID => self.list.get(idx),
				NbtCompound::ID => self.compound.get(idx).map(|(_, x)| x),
				NbtRegion::ID => self.region.get(idx),
				NbtChunk::ID => self.chunk.get(idx).map(|(_, x)| x),
				_ => None,
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<&mut Self> {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.get_mut(idx),
				NbtIntArray::ID => self.int_array.get_mut(idx),
				NbtLongArray::ID => self.long_array.get_mut(idx),
				NbtList::ID => self.list.get_mut(idx),
				NbtCompound::ID => self.compound.get_mut(idx).map(|(_, x)| x),
				NbtRegion::ID => self.region.get_mut(idx),
				NbtChunk::ID => self.chunk.get_mut(idx).map(|(_, x)| x),
				_ => None,
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn max_depth(&self) -> usize {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.max_depth(),
				NbtIntArray::ID => self.int_array.max_depth(),
				NbtLongArray::ID => self.long_array.max_depth(),
				NbtList::ID => self.list.max_depth(),
				NbtCompound::ID => self.compound.max_depth(),
				NbtRegion::ID => self.region.max_depth(),
				NbtChunk::ID => self.chunk.max_depth(),
				_ => 0,
			}
		}
	}

	#[inline]
	#[must_use]
	#[allow(clippy::match_same_arms)]
	pub fn actions(&self) -> &[ElementAction] {
		unsafe {
			match self.id() {
				NbtByte::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
				],
				NbtShort::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
				],
				NbtInt::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
				],
				NbtLong::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
				],
				NbtFloat::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
				],
				NbtDouble::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
				],
				NbtByteArray::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenArrayInHex,
					ElementAction::InsertFromClipboard,
				],
				NbtString::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
				],
				#[cfg(not(target_arch = "wasm32"))]
				NbtList::ID => {
					const FULL: [ElementAction; 5] = [
						ElementAction::CopyRaw,
						ElementAction::CopyFormatted,
						ElementAction::OpenInTxt,
						ElementAction::InsertFromClipboard,
						ElementAction::OpenArrayInHex,
					];
					let id = self.as_list_unchecked().element;
					if matches!(id, NbtByte::ID | NbtShort::ID | NbtInt::ID | NbtLong::ID) {
						&FULL
					} else {
						&FULL[..FULL.len() - 1]
					}
				},
				#[cfg(target_arch = "wasm32")]
				NbtList::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					ElementAction::InsertFromClipboard,
				],
				NbtCompound::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
					ElementAction::SortCompoundByName,
					ElementAction::SortCompoundByType,
					ElementAction::InsertFromClipboard,
				],
				NbtIntArray::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenArrayInHex,
					ElementAction::InsertFromClipboard,
				],
				NbtLongArray::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenArrayInHex,
					ElementAction::InsertFromClipboard,
				],
				NbtChunk::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
					ElementAction::SortCompoundByName,
					ElementAction::SortCompoundByType,
					ElementAction::InsertFromClipboard,
				],
				NbtRegion::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
				],
				NbtNull::ID => &[],
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}

	#[must_use]
	pub fn can_insert(&self, value: &NbtElement) -> bool {
		unsafe {
			match self.id() {
				NbtByte::ID => false,
				NbtShort::ID => false,
				NbtInt::ID => false,
				NbtLong::ID => false,
				NbtFloat::ID => false,
				NbtDouble::ID => false,
				NbtByteArray::ID => self.byte_array.can_insert(value),
				NbtString::ID => false,
				NbtList::ID => self.list.can_insert(value),
				NbtCompound::ID => self.compound.can_insert(value),
				NbtIntArray::ID => self.int_array.can_insert(value),
				NbtLongArray::ID => self.long_array.can_insert(value),
				NbtChunk::ID => self.chunk.can_insert(value),
				NbtRegion::ID => self.region.can_insert(value),
				NbtNull::ID => false,
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}
}

impl Display for NbtElement {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		unsafe {
			match self.id() {
				NbtByte::ID => write!(f, "{}", &*self.byte),
				NbtShort::ID => write!(f, "{}", &*self.short),
				NbtInt::ID => write!(f, "{}", &*self.int),
				NbtLong::ID => write!(f, "{}", &*self.long),
				NbtFloat::ID => write!(f, "{}", &*self.float),
				NbtDouble::ID => write!(f, "{}", &*self.double),
				NbtByteArray::ID => write!(f, "{}", &*self.byte_array),
				NbtString::ID => write!(f, "{}", &*self.string),
				NbtList::ID => write!(f, "{}", &*self.list),
				NbtCompound::ID => write!(f, "{}", &*self.compound),
				NbtIntArray::ID => write!(f, "{}", &*self.int_array),
				NbtLongArray::ID => write!(f, "{}", &*self.long_array),
				NbtChunk::ID => write!(f, "{}", &*self.chunk),
				NbtRegion::ID => Err(Error),
				NbtNull::ID => write!(f, "{}", &*self.null),
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}
}

impl NbtElement {
	pub fn pretty_fmt(&self, f: &mut PrettyFormatter) {
		unsafe {
			match self.id() {
				NbtByte::ID => self.byte.pretty_fmt(f),
				NbtShort::ID => self.short.pretty_fmt(f),
				NbtInt::ID => self.int.pretty_fmt(f),
				NbtLong::ID => self.long.pretty_fmt(f),
				NbtFloat::ID => self.float.pretty_fmt(f),
				NbtDouble::ID => self.double.pretty_fmt(f),
				NbtByteArray::ID => self.byte_array.pretty_fmt(f),
				NbtString::ID => self.string.pretty_fmt(f),
				NbtList::ID => self.list.pretty_fmt(f),
				NbtCompound::ID => self.compound.pretty_fmt(f),
				NbtIntArray::ID => self.int_array.pretty_fmt(f),
				NbtLongArray::ID => self.long_array.pretty_fmt(f),
				NbtChunk::ID => self.chunk.pretty_fmt(f),
				NbtRegion::ID => self.region.pretty_fmt(f),
				NbtNull::ID => self.null.pretty_fmt(f),
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}
}

impl Debug for NbtElement {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let mut formatter = PrettyFormatter::new();
		self.pretty_fmt(&mut formatter);
		write!(f, "{}", formatter.finish())
	}
}

impl Drop for NbtElement {
	fn drop(&mut self) {
		unsafe {
			let id = self.id();
			match id {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => {
					let vec = &mut *self.byte_array.values;
					if !vec.is_empty() {
						dealloc(
							vec.as_mut_ptr().cast(),
							Layout::array::<Self>(vec.capacity()).unwrap_unchecked(),
						);
					}
					dealloc((vec as *mut Vec<Self>).cast(), Layout::new::<Vec<Self>>());
				}
				NbtString::ID => {
					core::ptr::addr_of_mut!(self.string.str).drop_in_place();
				}
				NbtList::ID => {
					let element = self.list.element;
					let list = &mut *self.list.elements;
					if element > NbtDouble::ID {
						for entry in &mut *list {
							(entry as *mut Self).drop_in_place();
						}
					}
					if !list.is_empty() {
						dealloc(
							list.as_mut_ptr().cast(),
							Layout::array::<Self>(list.capacity()).unwrap_unchecked(),
						);
					}
					dealloc((list as *mut Vec<Self>).cast(), Layout::new::<Vec<Self>>());
				}
				NbtCompound::ID => {
					let map = &mut *self.compound.entries;
					let CompoundMap { indices, entries } = map;
					(indices as *mut RawTable<usize>).drop_in_place();
					for Entry { value, key, .. } in &mut *entries {
						(value as *mut Self).drop_in_place();
						if key.is_heap_allocated() {
							dealloc(
								key.as_mut_ptr(),
								Layout::array::<u8>(key.len()).unwrap_unchecked(),
							);
						}
					}
					if !entries.is_empty() {
						dealloc(
							entries.as_mut_ptr().cast(),
							Layout::array::<Entry>(entries.capacity()).unwrap_unchecked(),
						);
					}
					dealloc(
						(map as *mut CompoundMap).cast(),
						Layout::new::<CompoundMap>(),
					);
				}
				NbtChunk::ID => {
					let map = &mut *self.chunk.entries;
					let CompoundMap { indices, entries } = map;
					(indices as *mut RawTable<usize>).drop_in_place();
					for Entry { value, key, .. } in &mut *entries {
						(value as *mut Self).drop_in_place();
						if key.is_heap_allocated() {
							dealloc(
								key.as_mut_ptr(),
								Layout::array::<u8>(key.len()).unwrap_unchecked(),
							);
						}
					}
					if !entries.is_empty() {
						dealloc(
							entries.as_mut_ptr().cast(),
							Layout::array::<Entry>(entries.capacity()).unwrap_unchecked(),
						);
					}
					dealloc(
						(map as *mut CompoundMap).cast(),
						Layout::new::<CompoundMap>(),
					);
				}
				// no real speedup from using threads, seems to be memory-bound, or dealloc-call-bound
				NbtRegion::ID => {
					let chunks = *core::ptr::addr_of_mut!(self.region.chunks).read();
					for mut chunk in core::mem::transmute::<_, [ManuallyDrop<Self>; 1024]>(chunks) {
						let ptr = &mut **chunk.as_chunk_unchecked_mut();
						let map = &mut *ptr.entries;
						let CompoundMap { indices, entries } = map;
						(indices as *mut RawTable<usize>).drop_in_place();
						for Entry { value, key, .. } in &mut *entries {
							(value as *mut Self).drop_in_place();
							if key.is_heap_allocated() {
								dealloc(
									key.as_mut_ptr(),
									Layout::array::<u8>(key.len()).unwrap_unchecked(),
								);
							}
						}
						if !entries.is_empty() {
							dealloc(
								entries.as_mut_ptr().cast(),
								Layout::array::<Entry>(entries.capacity()).unwrap_unchecked(),
							);
						}
						dealloc(
							(map as *mut CompoundMap).cast(),
							Layout::new::<CompoundMap>(),
						);
						dealloc(
							(ptr as *mut NbtCompound).cast(),
							Layout::new::<NbtCompound>(),
						);
					}
				}
				NbtByte::ID | NbtShort::ID | NbtInt::ID | NbtLong::ID | NbtFloat::ID | NbtDouble::ID | NbtNull::ID => {}
				_ => core::hint::unreachable_unchecked()
			}
		}
	}
}

impl<'a> Index<&'a str> for NbtElement {
	type Output = NbtElement;

	fn index(&self, index: &'a str) -> &Self::Output {
		let map = match self.as_pattern() {
			NbtPattern::Compound(compound) => &*compound.entries,
			NbtPattern::Chunk(chunk) => &*chunk.entries,
			_ => return Self::NULL_REF,
		};

		if let Some(idx) = map.idx_of(index) && let Some((_, value)) = map.get_idx(idx) {
			value
		} else {
			Self::NULL_REF
		}
	}
}

impl<'a> IndexMut<&'a str> for NbtElement {
	fn index_mut(&mut self, index: &str) -> &mut Self::Output {
		fn try_index<'b>(this: &'b mut NbtElement, index: &str) -> Option<&'b mut NbtElement> {
			let map = match this.as_pattern_mut() {
				NbtPatternMut::Compound(compound) => &mut *compound.entries,
				NbtPatternMut::Chunk(chunk) => &mut *chunk.entries,
				_ => return None,
			};

			if let Some(idx) = map.idx_of(index) && let Some((_, value)) = map.get_idx_mut(idx) {
				Some(value)
			} else {
				None
			}
		}

		let mut this = self;
		polonius!(|this| -> &'polonius mut NbtElement {
			if let Some(x) = try_index(this, index) {
				polonius_return!(x);
			}
		});
		this
	}
}

impl Index<usize> for NbtElement {
	type Output = NbtElement;

	fn index(&self, idx: usize) -> &Self::Output {
		self.get(idx).unwrap_or(Self::NULL_REF)
	}
}

impl IndexMut<usize> for NbtElement {
	fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
		let mut this = self;
		polonius!(|this| -> &'polonius mut NbtElement {
			if let Some(x) = this.get_mut(idx) {
				polonius_return!(x);
			}
		});
		this
	}
}

#[inline]
#[must_use]
pub fn id_to_string_name(id: u8) -> (&'static str, &'static str) {
	match id {
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
		NbtNull::ID => ("entry", "entries"),
		_ => panic!("Invalid id"),
	}
}

pub enum NbtPattern<'a> {
	Byte(&'a NbtByte),
	Short(&'a NbtShort),
	Int(&'a NbtInt),
	Long(&'a NbtLong),
	Float(&'a NbtFloat),
	Double(&'a NbtDouble),
	ByteArray(&'a NbtByteArray),
	String(&'a NbtString),
	List(&'a NbtList),
	Compound(&'a NbtCompound),
	IntArray(&'a NbtIntArray),
	LongArray(&'a NbtLongArray),
	Chunk(&'a NbtChunk),
	Region(&'a NbtRegion),
	Null(&'a NbtNull),
}

pub enum NbtPatternMut<'a> {
	Byte(&'a mut NbtByte),
	Short(&'a mut NbtShort),
	Int(&'a mut NbtInt),
	Long(&'a mut NbtLong),
	Float(&'a mut NbtFloat),
	Double(&'a mut NbtDouble),
	ByteArray(&'a mut NbtByteArray),
	String(&'a mut NbtString),
	List(&'a mut NbtList),
	Compound(&'a mut NbtCompound),
	IntArray(&'a mut NbtIntArray),
	LongArray(&'a mut NbtLongArray),
	Chunk(&'a mut NbtChunk),
	Region(&'a mut NbtRegion),
	Null(&'a mut NbtNull),
}

impl NbtElement {
	#[must_use]
	pub fn as_pattern(&self) -> NbtPattern {
		match self.id() {
			NbtByte::ID => NbtPattern::Byte(unsafe { self.as_byte_unchecked() }),
			NbtShort::ID => NbtPattern::Short(unsafe { self.as_short_unchecked() }),
			NbtInt::ID => NbtPattern::Int(unsafe { self.as_int_unchecked() }),
			NbtLong::ID => NbtPattern::Long(unsafe { self.as_long_unchecked() }),
			NbtFloat::ID => NbtPattern::Float(unsafe { self.as_float_unchecked() }),
			NbtDouble::ID => NbtPattern::Double(unsafe { self.as_double_unchecked() }),
			NbtByteArray::ID => NbtPattern::ByteArray(unsafe { self.as_byte_array_unchecked() }),
			NbtString::ID => NbtPattern::String(unsafe { self.as_string_unchecked() }),
			NbtList::ID => NbtPattern::List(unsafe { self.as_list_unchecked() }),
			NbtCompound::ID => NbtPattern::Compound(unsafe { self.as_compound_unchecked() }),
			NbtIntArray::ID => NbtPattern::IntArray(unsafe { self.as_int_array_unchecked() }),
			NbtLongArray::ID => NbtPattern::LongArray(unsafe { self.as_long_array_unchecked() }),
			NbtChunk::ID => NbtPattern::Chunk(unsafe { self.as_chunk_unchecked() }),
			NbtRegion::ID => NbtPattern::Region(unsafe { self.as_region_unchecked() }),
			NbtNull::ID => NbtPattern::Null(unsafe { self.as_null_unchecked() }),
			_ => panic!("variant wasn't known"),
		}
	}

	pub fn as_pattern_mut(&mut self) -> NbtPatternMut {
		match self.id() {
			NbtByte::ID => NbtPatternMut::Byte(unsafe { self.as_byte_unchecked_mut() }),
			NbtShort::ID => NbtPatternMut::Short(unsafe { self.as_short_unchecked_mut() }),
			NbtInt::ID => NbtPatternMut::Int(unsafe { self.as_int_unchecked_mut() }),
			NbtLong::ID => NbtPatternMut::Long(unsafe { self.as_long_unchecked_mut() }),
			NbtFloat::ID => NbtPatternMut::Float(unsafe { self.as_float_unchecked_mut() }),
			NbtDouble::ID => NbtPatternMut::Double(unsafe { self.as_double_unchecked_mut() }),
			NbtByteArray::ID => NbtPatternMut::ByteArray(unsafe { self.as_byte_array_unchecked_mut() }),
			NbtString::ID => NbtPatternMut::String(unsafe { self.as_string_unchecked_mut() }),
			NbtList::ID => NbtPatternMut::List(unsafe { self.as_list_unchecked_mut() }),
			NbtCompound::ID => NbtPatternMut::Compound(unsafe { self.as_compound_unchecked_mut() }),
			NbtIntArray::ID => NbtPatternMut::IntArray(unsafe { self.as_int_array_unchecked_mut() }),
			NbtLongArray::ID => NbtPatternMut::LongArray(unsafe { self.as_long_array_unchecked_mut() }),
			NbtChunk::ID => NbtPatternMut::Chunk(unsafe { self.as_chunk_unchecked_mut() }),
			NbtRegion::ID => NbtPatternMut::Region(unsafe { self.as_region_unchecked_mut() }),
			NbtNull::ID => NbtPatternMut::Null(unsafe { self.as_null_unchecked_mut() }),
			_ => panic!("variant wasn't known"),
		}
	}
}

macro_rules! type_conversions {
    ($t:ty, $field:ident, $is:ident, $into_unchecked:ident, $as_unchecked:ident, $as_unchecked_mut:ident, $into:ident, $r#as:ident, $as_mut:ident) => {
		#[allow(dead_code)]
		impl NbtElement {
			#[inline]
			#[must_use]
			pub unsafe fn $into_unchecked(self) -> $t {
				let result = core::ptr::read(core::ptr::addr_of!(*self.$field));
				core::mem::forget(self);
				result
			}

			#[inline]
			#[must_use]
			pub unsafe fn $as_unchecked(&self) -> &$t {
				&self.$field
			}

			#[inline]
			#[must_use]
			pub unsafe fn $as_unchecked_mut(&mut self) -> &mut $t {
				&mut self.$field
			}

			#[inline]
			#[must_use]
			pub fn $into(self) -> Option<$t> {
				unsafe {
					if self.$is() {
						Some(self.$into_unchecked())
					} else {
						None
					}
				}
			}

			#[inline]
			#[must_use]
			pub fn $r#as(&self) -> Option<&$t> {
				unsafe {
					if self.$is() {
						Some(self.$as_unchecked())
					} else {
						None
					}
				}
			}

			#[inline]
			#[must_use]
			pub fn $as_mut(&mut self) -> Option<&mut $t> {
				unsafe {
					if self.$is() {
						Some(self.$as_unchecked_mut())
					} else {
						None
					}
				}
			}

			#[inline]
			#[must_use]
			pub fn $is(&self) -> bool {
				self.id() == <$t>::ID
			}
		}
	}
}

type_conversions! { NbtByte, byte, is_byte, into_byte_unchecked, as_byte_unchecked, as_byte_unchecked_mut, into_byte, as_byte, as_byte_mut }
type_conversions! { NbtShort, short, is_short, into_short_unchecked, as_short_unchecked, as_short_unchecked_mut, into_short, as_short, as_short_mut }
type_conversions! { NbtInt, int, is_int, into_int_unchecked, as_int_unchecked, as_int_unchecked_mut, into_int, as_int, as_int_mut }
type_conversions! { NbtLong, long, is_long, into_long_unchecked, as_long_unchecked, as_long_unchecked_mut, into_long, as_long, as_long_mut }
type_conversions! { NbtFloat, float, is_float, into_float_unchecked, as_float_unchecked, as_float_unchecked_mut, into_float, as_float, as_float_mut }
type_conversions! { NbtDouble, double, is_double, into_double_unchecked, as_double_unchecked, as_double_unchecked_mut, into_double, as_double, as_double_mut }
type_conversions! { NbtByteArray, byte_array, is_byte_array, into_byte_array_unchecked, as_byte_array_unchecked, as_byte_array_unchecked_mut, into_byte_array, as_byte_array, as_byte_array_mut }
type_conversions! { NbtString, string, is_string, into_string_unchecked, as_string_unchecked, as_string_unchecked_mut, into_string, as_string, as_string_mut }
type_conversions! { NbtList, list, is_list, into_list_unchecked, as_list_unchecked, as_list_unchecked_mut, into_list, as_list, as_list_mut }
type_conversions! { NbtCompound, compound, is_compound, into_compound_unchecked, as_compound_unchecked, as_compound_unchecked_mut, into_compound, as_compound, as_compound_mut }
type_conversions! { NbtIntArray, int_array, is_int_array, into_int_array_unchecked, as_int_array_unchecked, as_int_array_unchecked_mut, into_int_array, as_int_array, as_int_array_mut }
type_conversions! { NbtLongArray, long_array, is_long_array, into_long_array_unchecked, as_long_array_unchecked, as_long_array_unchecked_mut, into_long_array, as_long_array, as_long_array_mut }
type_conversions! { NbtChunk, chunk, is_chunk, into_chunk_unchecked, as_chunk_unchecked, as_chunk_unchecked_mut, into_chunk, as_chunk, as_chunk_mut }
type_conversions! { NbtRegion, region, is_region, into_region_unchecked, as_region_unchecked, as_region_unchecked_mut, into_region, as_region, as_region_mut }
type_conversions! { NbtNull, null, is_null, into_null_unchecked, as_null_unchecked, as_null_unchecked_mut, into_null, as_null, as_null_mut }
