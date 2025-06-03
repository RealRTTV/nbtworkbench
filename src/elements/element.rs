use std::borrow::Cow;
use std::fmt::{Debug, Display, Error, Formatter};
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::{Index, IndexMut};
use std::slice::{Iter, IterMut};
#[cfg(not(target_arch = "wasm32"))] use std::thread::Scope;

use compact_str::CompactString;

use crate::elements::result::NbtParseResult;
use crate::elements::{
	ComplexNbtElementVariant, CompoundEntry, Matches, NbtByte, NbtByteArray, NbtChunk, NbtCompound, NbtDouble, NbtElementAndKey, NbtElementAndKeyRef, NbtElementAndKeyRefMut, NbtElementVariant, NbtFloat, NbtInt, NbtIntArray, NbtList, NbtLong,
	NbtLongArray, NbtRegion, NbtShort, NbtString, PrimitiveNbtElementVariant,
};
use crate::render::{RenderContext, TextColor, VertexBufferBuilder};
use crate::serialization::{BigEndianDecoder, Decoder, LittleEndianDecoder, PrettyDisplay, PrettyFormatter, UncheckedBufWriter};
use crate::tree::{
	Indices, IterativeNavigationInformationMut, NavigationInformation, NavigationInformationMut, OwnedIndices, ParentIterativeNavigationInformationMut, ParentNavigationInformation, ParentNavigationInformationMut, TraversalInformation,
	TraversalInformationMut,
};
use crate::util;
use crate::util::{StrExt, Vec2u, width_ascii};
#[cfg(target_arch = "wasm32")] use crate::wasm::FakeScope as Scope;
use crate::workbench::{DropResult, ElementAction, MarkedLines};

#[repr(C)]
pub union NbtElement {
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
	region: ManuallyDrop<NbtRegion>,
	chunk: ManuallyDrop<NbtChunk>,
	id: NbtElementId,
}

#[repr(C)]
#[derive(Copy, Clone)]
struct NbtElementId {
	_pad: [u8; 23],
	id: u8,
}

/// Matches
impl Matches for NbtElement {
	fn matches(&self, other: &Self) -> bool {
		use NbtPattern as Nbt;

		match (self.as_pattern(), other.as_pattern()) {
			(Nbt::Byte(a), Nbt::Byte(b)) => a.matches(b),
			(Nbt::Short(a), Nbt::Short(b)) => a.matches(b),
			(Nbt::Int(a), Nbt::Int(b)) => a.matches(b),
			(Nbt::Long(a), Nbt::Long(b)) => a.matches(b),
			(Nbt::Float(a), Nbt::Float(b)) => a.matches(b),
			(Nbt::Double(a), Nbt::Double(b)) => a.matches(b),
			(Nbt::ByteArray(a), Nbt::ByteArray(b)) => a.matches(b),
			(Nbt::String(a), Nbt::String(b)) => a.matches(b),
			(Nbt::List(a), Nbt::List(b)) => a.matches(b),
			(Nbt::Compound(a), Nbt::Compound(b)) => a.matches(b),
			(Nbt::IntArray(a), Nbt::IntArray(b)) => a.matches(b),
			(Nbt::LongArray(a), Nbt::LongArray(b)) => a.matches(b),
			(Nbt::Chunk(a), Nbt::Chunk(b)) => a.matches(b),
			(Nbt::Region(a), Nbt::Region(b)) => a.matches(b),
			_ => false,
		}
	}
}

impl PartialEq for NbtElement {
	fn eq(&self, other: &Self) -> bool {
		use NbtPattern as Nbt;

		match (self.as_pattern(), other.as_pattern()) {
			(Nbt::Byte(a), Nbt::Byte(b)) => a.eq(b),
			(Nbt::Short(a), Nbt::Short(b)) => a.eq(b),
			(Nbt::Int(a), Nbt::Int(b)) => a.eq(b),
			(Nbt::Long(a), Nbt::Long(b)) => a.eq(b),
			(Nbt::Float(a), Nbt::Float(b)) => a.eq(b),
			(Nbt::Double(a), Nbt::Double(b)) => a.eq(b),
			(Nbt::ByteArray(a), Nbt::ByteArray(b)) => a.eq(b),
			(Nbt::String(a), Nbt::String(b)) => a.eq(b),
			(Nbt::List(a), Nbt::List(b)) => a.eq(b),
			(Nbt::Compound(a), Nbt::Compound(b)) => a.eq(b),
			(Nbt::IntArray(a), Nbt::IntArray(b)) => a.eq(b),
			(Nbt::LongArray(a), Nbt::LongArray(b)) => a.eq(b),
			(Nbt::Chunk(a), Nbt::Chunk(b)) => a.eq(b),
			(Nbt::Region(a), Nbt::Region(b)) => a.eq(b),
			_ => false,
		}
	}
}

impl Clone for NbtElement {
	fn clone(&self) -> Self {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::Byte(byte) => Self::Byte(*byte),
			Nbt::Short(short) => Self::Short(*short),
			Nbt::Int(int) => Self::Int(*int),
			Nbt::Long(long) => Self::Long(*long),
			Nbt::Float(float) => Self::Float(*float),
			Nbt::Double(double) => Self::Double(*double),
			Nbt::ByteArray(byte_array) => Self::ByteArray(byte_array.clone()),
			Nbt::String(string) => Self::String(string.clone()),
			Nbt::List(list) => Self::List(list.clone()),
			Nbt::Compound(compound) => Self::Compound(compound.clone()),
			Nbt::IntArray(int_array) => Self::IntArray(int_array.clone()),
			Nbt::LongArray(long_array) => Self::LongArray(long_array.clone()),
			Nbt::Chunk(chunk) => Self::Chunk(chunk.clone()),
			Nbt::Region(region) => Self::Region(region.clone()),
		}
	}
}

/// Variant initialization from inner type
macro_rules! create_constructor {
	($snake_case:ident, $camel_case:ident, $ty:ty) => {
		#[must_use]
		pub const fn $camel_case($snake_case: $ty) -> Self {
			let mut this = Self::NULL;
			this.$snake_case = ManuallyDrop::new($snake_case);
			this.id.id = <$ty>::ID;
			this
		}
	};
}
#[allow(non_snake_case)]
impl NbtElement {
	pub const NULL_ID: u8 = 0;
	pub const NULL: NbtElement = Self {
		id: NbtElementId { _pad: [0_u8; 23], id: Self::NULL_ID },
	};
	pub const NULL_REF: &'static NbtElement = &Self::NULL;

	create_constructor!(byte, Byte, NbtByte);
	create_constructor!(short, Short, NbtShort);
	create_constructor!(int, Int, NbtInt);
	create_constructor!(long, Long, NbtLong);
	create_constructor!(float, Float, NbtFloat);
	create_constructor!(double, Double, NbtDouble);
	create_constructor!(byte_array, ByteArray, NbtByteArray);
	create_constructor!(string, String, NbtString);
	create_constructor!(list, List, NbtList);
	create_constructor!(compound, Compound, NbtCompound);
	create_constructor!(int_array, IntArray, NbtIntArray);
	create_constructor!(long_array, LongArray, NbtLongArray);
	create_constructor!(chunk, Chunk, NbtChunk);
	create_constructor!(region, Region, NbtRegion);
}

/// FromStr
impl NbtElement {
	pub fn from_str(mut s: &str) -> Result<NbtElementAndKey, usize> {
		let total_len = s.len();
		s = s.trim();

		if s.is_empty() {
			return Err(total_len - s.len())
		}

		let prefix = s
			.snbt_string_read()
			.ok()
			.and_then(|(prefix, s2)| {
				s2.trim_start()
					.strip_prefix(':')
					.filter(|s| !s.is_empty())
					.map(|s2| {
						s = s2.trim_start();
						prefix
					})
			});
		let (s, element) = Self::from_str0(s, Self::parse_int)
			.map(|(s, x)| (s.trim_start(), x))
			.map_err(|x| total_len - x)?;
		if !s.is_empty() {
			return Err(total_len - s.len())
		}
		Ok((prefix, element))
	}

	pub(super) fn from_str0(s: &str, parse_ambiguous_integer: impl FnOnce(&str, bool, bool, u32, &str) -> Result<Self, usize>) -> Result<(&str, Self), usize> {
		if let Some(s) = s.strip_prefix("false") {
			return Ok((s, Self::Byte(NbtByte { value: 0 })))
		} else if let Some(s) = s.strip_prefix("true") {
			return Ok((s, Self::Byte(NbtByte { value: 1 })))
		} else if let Ok((s, x)) = NbtByteArray::from_str0(s) {
			return Ok((s, Self::ByteArray(x)))
		} else if let Ok((s, x)) = NbtIntArray::from_str0(s) {
			return Ok((s, Self::IntArray(x)))
		} else if let Ok((s, x)) = NbtLongArray::from_str0(s) {
			return Ok((s, Self::LongArray(x)))
		} else if let Ok((s, x)) = NbtList::from_str0(s) {
			return Ok((s, Self::List(x)))
		} else if let Ok((s, x)) = NbtCompound::from_str0(s) {
			return Ok((s, Self::Compound(x)))
		} else if let Ok((s, x)) = NbtString::from_str0(s) {
			return Ok((s, Self::String(x)))
		}

		if let Some(result) = Self::try_parse_num(s, parse_ambiguous_integer)? {
			return Ok(result)
		}

		Err(s.len())
	}

	fn try_parse_num(mut s: &str, parse_ambiguous_integer: impl FnOnce(&str, bool, bool, u32, &str) -> Result<Self, usize>) -> Result<Option<(&str, Self)>, usize> {
		if let Some(s2) = s.strip_prefix("NaN") {
			s = s2.trim_start();
			return if let Some(s2) = s.strip_prefix('f') {
				Ok(Some((s2.trim_start(), Self::Float(NbtFloat { value: f32::NAN }))))
			} else {
				Ok(Some((s2.strip_prefix('d').unwrap_or(s2).trim_start(), Self::Double(NbtDouble { value: f64::NAN }))))
			};
		}

		if let Some(s2) = s
			.strip_prefix("Infinity")
			.or_else(|| s.strip_prefix("inf"))
		{
			s = s2.trim_start();
			return if let Some(s2) = s.strip_prefix('f') {
				Ok(Some((s2.trim_start(), Self::Float(NbtFloat { value: f32::INFINITY }))))
			} else {
				Ok(Some((s2.strip_prefix('d').unwrap_or(s2).trim_start(), Self::Double(NbtDouble { value: f64::INFINITY }))))
			};
		}

		if let Some(s2) = s
			.strip_prefix("-Infinity")
			.or_else(|| s.strip_prefix("-inf"))
		{
			s = s2.trim_start();
			return if let Some(s2) = s.strip_prefix('f') {
				Ok(Some((s2.trim_start(), Self::Float(NbtFloat { value: f32::NEG_INFINITY }))))
			} else if let Some(s2) = s.strip_prefix('d') {
				Ok(Some((s2.trim_start(), Self::Double(NbtDouble { value: f64::NEG_INFINITY }))))
			} else {
				Ok(Some((s2.trim_start(), Self::Double(NbtDouble { value: f64::NEG_INFINITY }))))
			};
		}

		let (num_end_idx, suffix_len, unsigned, base, positive) = 'a: {
			let mut d = s;
			let mut num_end_idx = 0;
			let mut suffix_len = 0;
			let mut positive = true;

			if let Some(d2) = d.strip_prefix('+').or(d.strip_prefix('-')) {
				if d.starts_with('-') {
					positive = false;
				}
				s = d2;
				d = s;
			}

			if let Some(d2) = d.strip_prefix("0x") {
				s = d2;
				d = s;
				let hex_part = d
					.bytes()
					.take_while(|&b| b.is_ascii_hexdigit() || b == b'_')
					.count();
				num_end_idx += hex_part;
				let unsigned = !d.starts_with('s');
				if d.starts_with('u') || d.starts_with('s') {
					suffix_len += 1;
				}
				(num_end_idx, suffix_len, unsigned, 16, positive)
			} else if let Some(d2) = d.strip_prefix("0b") {
				s = d2;
				d = s;
				let binary_part = d
					.bytes()
					.take_while(|&b| b == b'0' || b == b'1' || b == b'_')
					.count();
				num_end_idx += binary_part;
				let unsigned = !d.starts_with('s');
				if d.starts_with('u') || d.starts_with('s') {
					suffix_len += 1;
				}
				(num_end_idx, suffix_len, unsigned, 2, positive)
			} else {
				let int_part = d
					.bytes()
					.take_while(|&b| b.is_ascii_digit() || b == b'_')
					.count();
				d = &d[int_part..];
				num_end_idx += int_part;
				if int_part == 0 && !d.starts_with('.') {
					break 'a (num_end_idx, suffix_len, false, 10, true);
				}
				if let Some(d2) = d.strip_prefix('.') {
					// floats
					num_end_idx += 1;
					d = d2;
					let frac_part = d
						.bytes()
						.take_while(|&b| b.is_ascii_digit() || b == b'_')
						.count();
					num_end_idx += frac_part;
					if let Some(s2) = d.strip_prefix('e').or(d.strip_prefix('E')) {
						num_end_idx += 1;
						d = s2;
						if let Some(s2) = d.strip_prefix('+').or(d.strip_prefix('-')) {
							num_end_idx += 1;
							d = s2;
						}
						let exponent_part = d
							.bytes()
							.take_while(|&b| b.is_ascii_digit() || b == b'_')
							.count();
						num_end_idx += exponent_part;
					}
					(num_end_idx, suffix_len, false, 10, positive)
				} else {
					// ints
					let unsigned = d.starts_with('u');
					if d.starts_with('u') || d.starts_with('s') {
						suffix_len += 1;
					}
					(num_end_idx, suffix_len, unsigned, 10, positive)
				}
			}
		};
		if num_end_idx > 0 {
			let suffix = s[num_end_idx + suffix_len..]
				.trim_start()
				.as_bytes()
				.first()
				.map(u8::to_ascii_lowercase);
			let num_str = s[..num_end_idx].replace('_', "");
			return match suffix {
				Some(b'b') => Ok(Some((&s[num_end_idx + suffix_len + 1..], Self::parse_byte(&num_str, unsigned, positive, base, s)?))),
				Some(b's') => Ok(Some((&s[num_end_idx + suffix_len + 1..], Self::parse_short(&num_str, unsigned, positive, base, s)?))),
				Some(b'i') => Ok(Some((&s[num_end_idx + suffix_len + 1..], Self::parse_int(&num_str, unsigned, positive, base, s)?))),
				Some(b'l') => Ok(Some((&s[num_end_idx + suffix_len + 1..], Self::parse_long(&num_str, unsigned, positive, base, s)?))),
				Some(b'f') => Ok(Some((
					&s[num_end_idx + suffix_len + 1..],
					Self::Float(NbtFloat {
						value: {
							let value = num_str.parse().map_err(|_| s.len())?;
							if positive { value } else { -value }
						},
					}),
				))),
				Some(b'd') => Ok(Some((
					&s[num_end_idx + suffix_len + 1..],
					Self::Double(NbtDouble {
						value: {
							let value = num_str.parse().map_err(|_| s.len())?;
							if positive { value } else { -value }
						},
					}),
				))),
				_ => Ok(Some((&s[num_end_idx + suffix_len..], parse_ambiguous_integer(&num_str, unsigned, positive, base, s)?))),
			};
		}

		Ok(None)
	}

	pub(super) fn parse_byte(num_str: &str, unsigned: bool, positive: bool, base: u32, s: &str) -> Result<Self, usize> {
		let value = if unsigned {
			u8::from_str_radix(&num_str, base).map_err(|_| s.len())? as i8
		} else {
			i8::from_str_radix(&num_str, base).map_err(|_| s.len())?
		};
		Ok(Self::Byte(NbtByte { value: if positive { value } else { -value } }))
	}

	pub(super) fn parse_short(num_str: &str, unsigned: bool, positive: bool, base: u32, s: &str) -> Result<Self, usize> {
		let value = if unsigned {
			u16::from_str_radix(&num_str, base).map_err(|_| s.len())? as i16
		} else {
			i16::from_str_radix(&num_str, base).map_err(|_| s.len())?
		};
		Ok(Self::Short(NbtShort { value: if positive { value } else { -value } }))
	}

	pub(super) fn parse_int(num_str: &str, unsigned: bool, positive: bool, base: u32, s: &str) -> Result<Self, usize> {
		let value = if unsigned {
			u32::from_str_radix(&num_str, base).map_err(|_| s.len())? as i32
		} else {
			i32::from_str_radix(&num_str, base).map_err(|_| s.len())?
		};
		Ok(Self::Int(NbtInt { value: if positive { value } else { -value } }))
	}

	pub(super) fn parse_long(num_str: &str, unsigned: bool, positive: bool, base: u32, s: &str) -> Result<Self, usize> {
		let value = if unsigned {
			u64::from_str_radix(&num_str, base).map_err(|_| s.len())? as i64
		} else {
			i64::from_str_radix(&num_str, base).map_err(|_| s.len())?
		};
		Ok(Self::Long(NbtLong { value: if positive { value } else { -value } }))
	}

	pub(super) fn array_try_into_byte(self) -> Option<Self> {
		use NbtPattern as Nbt;

		Some(match self.as_pattern() {
			Nbt::Byte(_) => self,
			_ => return None,
		})
	}

	pub(super) fn array_try_into_int(self) -> Option<Self> {
		use NbtPattern as Nbt;

		Some(match self.as_pattern() {
			Nbt::Byte(&NbtByte { value }) => Self::Int(NbtInt { value: value as i32 }),
			Nbt::Short(&NbtShort { value }) => Self::Int(NbtInt { value: value as i32 }),
			Nbt::Int(_) => self,
			_ => return None,
		})
	}

	pub(super) fn array_try_into_long(self) -> Option<Self> {
		use NbtPattern as Nbt;

		Some(match self.as_pattern() {
			Nbt::Byte(&NbtByte { value }) => Self::Long(NbtLong { value: value as i64 }),
			Nbt::Short(&NbtShort { value }) => Self::Long(NbtLong { value: value as i64 }),
			Nbt::Int(&NbtInt { value }) => Self::Long(NbtLong { value: value as i64 }),
			_ => return None,
		})
	}
}

/// From Bytes
impl NbtElement {
	pub fn from_bytes<'a, D: Decoder<'a>>(element: u8, decoder: &mut D) -> NbtParseResult<Self> {
		use super::result::*;

		ok(match element {
			NbtByte::ID => Self::Byte(NbtByte::from_bytes(decoder, ())?),
			NbtShort::ID => Self::Short(NbtShort::from_bytes(decoder, ())?),
			NbtInt::ID => Self::Int(NbtInt::from_bytes(decoder, ())?),
			NbtLong::ID => Self::Long(NbtLong::from_bytes(decoder, ())?),
			NbtFloat::ID => Self::Float(NbtFloat::from_bytes(decoder, ())?),
			NbtDouble::ID => Self::Double(NbtDouble::from_bytes(decoder, ())?),
			NbtByteArray::ID => Self::ByteArray(NbtByteArray::from_bytes(decoder, ())?),
			NbtString::ID => Self::String(NbtString::from_bytes(decoder, ())?),
			NbtList::ID => Self::List(NbtList::from_bytes(decoder, ())?),
			NbtCompound::ID => Self::Compound(NbtCompound::from_bytes(decoder, ())?),
			NbtIntArray::ID => Self::IntArray(NbtIntArray::from_bytes(decoder, ())?),
			NbtLongArray::ID => Self::LongArray(NbtLongArray::from_bytes(decoder, ())?),
			_ => return err("Invalid NBT type"),
		})
	}

	#[must_use]
	pub fn from_id(id: u8) -> Option<Self> {
		Some(match id {
			NbtByte::ID => Self::Byte(NbtByte::default()),
			NbtShort::ID => Self::Short(NbtShort::default()),
			NbtInt::ID => Self::Int(NbtInt::default()),
			NbtLong::ID => Self::Long(NbtLong::default()),
			NbtFloat::ID => Self::Float(NbtFloat::default()),
			NbtDouble::ID => Self::Double(NbtDouble::default()),
			NbtByteArray::ID => Self::ByteArray(NbtByteArray::default()),
			NbtString::ID => Self::String(NbtString::default()),
			NbtList::ID => Self::List(NbtList::default()),
			NbtCompound::ID => Self::Compound(NbtCompound::default()),
			NbtIntArray::ID => Self::IntArray(NbtIntArray::default()),
			NbtLongArray::ID => Self::LongArray(NbtLongArray::default()),
			NbtRegion::ID => Self::Region(NbtRegion::default()),
			NbtChunk::ID => Self::Chunk(NbtChunk::default()),
			_ => return None,
		})
	}

	pub fn from_be_file(bytes: &[u8]) -> NbtParseResult<Self> {
		use super::result::*;

		let mut decoder = BigEndianDecoder::new(bytes);
		decoder.assert_len(1)?;
		if unsafe { decoder.u8() } != NbtCompound::ID {
			return err("Big-endian NBT file didn't start with Compound")
		}
		// fix for >= 1.20.2 protocol since they removed the empty field
		if is_ok(&decoder.assert_len(2)) && unsafe { decoder.u16() } != 0_u16.to_be() {
			decoder.skip(-2_isize as usize);
		}
		let nbt = Self::Compound(NbtCompound::from_bytes(&mut decoder, ())?);
		if is_ok(&decoder.assert_len(1)) {
			return err("Format should take all the bytes");
		}
		ok(nbt)
	}

	pub fn from_be_mca(bytes: &[u8]) -> NbtParseResult<Self> {
		let mut decoder = BigEndianDecoder::new(bytes);
		NbtRegion::from_bytes(&mut decoder, ()).map(Self::Region)
	}

	#[must_use]
	pub fn from_le_file(bytes: &[u8]) -> NbtParseResult<(Self, bool)> {
		use super::result::*;

		let mut decoder = LittleEndianDecoder::new(bytes);
		decoder.assert_len(1)?;
		let kind = unsafe { decoder.u8() };
		let result = match kind {
			NbtCompound::ID => {
				decoder.assert_len(2)?;
				let skip = unsafe { decoder.u16() } as usize;
				decoder.skip(skip);
				ok((Self::Compound(NbtCompound::from_bytes(&mut decoder, ())?), decoder.has_header()))
			}
			NbtList::ID => {
				decoder.assert_len(2)?;
				let skip = unsafe { decoder.u16() } as usize;
				decoder.skip(skip);
				ok((Self::List(NbtList::from_bytes(&mut decoder, ())?), decoder.has_header()))
			}
			_ => err("Little-endian should start with either Compound or List"),
		};
		if is_ok(&decoder.assert_len(1)) {
			return err("Format should take all the bytes");
		}
		result
	}
}

/// To Bytes
impl NbtElement {
	pub fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::Byte(byte) => byte.to_be_bytes(writer),
			Nbt::Short(short) => short.to_be_bytes(writer),
			Nbt::Int(int) => int.to_be_bytes(writer),
			Nbt::Long(long) => long.to_be_bytes(writer),
			Nbt::Float(float) => float.to_be_bytes(writer),
			Nbt::Double(double) => double.to_be_bytes(writer),
			Nbt::ByteArray(byte_array) => byte_array.to_be_bytes(writer),
			Nbt::String(string) => string.to_be_bytes(writer),
			Nbt::List(list) => list.to_be_bytes(writer),
			Nbt::Compound(compound) => compound.to_be_bytes(writer),
			Nbt::IntArray(int_array) => int_array.to_be_bytes(writer),
			Nbt::LongArray(long_array) => long_array.to_be_bytes(writer),
			Nbt::Chunk(chunk) => chunk.to_be_bytes(writer),
			Nbt::Region(region) => region.to_be_bytes(writer),
		}
	}

	pub fn to_le_bytes(&self, writer: &mut UncheckedBufWriter) {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::Byte(byte) => byte.to_le_bytes(writer),
			Nbt::Short(short) => short.to_le_bytes(writer),
			Nbt::Int(int) => int.to_le_bytes(writer),
			Nbt::Long(long) => long.to_le_bytes(writer),
			Nbt::Float(float) => float.to_le_bytes(writer),
			Nbt::Double(double) => double.to_le_bytes(writer),
			Nbt::ByteArray(byte_array) => byte_array.to_le_bytes(writer),
			Nbt::String(string) => string.to_le_bytes(writer),
			Nbt::List(list) => list.to_le_bytes(writer),
			Nbt::Compound(compound) => compound.to_le_bytes(writer),
			Nbt::IntArray(int_array) => int_array.to_le_bytes(writer),
			Nbt::LongArray(long_array) => long_array.to_le_bytes(writer),
			Nbt::Chunk(chunk) => chunk.to_le_bytes(writer),
			Nbt::Region(_) => { /* no */ }
		}
	}

	#[must_use]
	pub fn to_be_file(&self) -> Vec<u8> {
		let mut writer = UncheckedBufWriter::new();
		if self.is_compound() {
			writer.write(&[NbtCompound::ID, 0x00, 0x00]);
		}
		self.to_be_bytes(&mut writer);
		writer.finish()
	}

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
}

/// "Rendering" related functions
impl NbtElement {
	pub fn render(&self, remaining_scroll: &mut usize, builder: &mut VertexBufferBuilder, str: Option<&str>, tail: bool, ctx: &mut RenderContext) {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::Byte(byte) => byte.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::Short(short) => short.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::Int(int) => int.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::Long(long) => long.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::Float(float) => float.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::Double(double) => double.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::ByteArray(byte_array) => byte_array.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::String(string) => string.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::List(list) => list.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::Compound(compound) => compound.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::IntArray(int_array) => int_array.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::LongArray(long_array) => long_array.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::Chunk(chunk) => chunk.render(builder, str, remaining_scroll, tail, ctx),
			Nbt::Region(_) => { /* no impl */ }
		}
	}

	#[must_use]
	pub fn display_name(&self) -> &'static str {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::Byte(_) => "Byte",
			Nbt::Short(_) => "Short",
			Nbt::Int(_) => "Int",
			Nbt::Long(_) => "Long",
			Nbt::Float(_) => "Float",
			Nbt::Double(_) => "Double",
			Nbt::ByteArray(_) => "Byte Array",
			Nbt::String(_) => "String",
			Nbt::List(_) => "List",
			Nbt::Compound(_) => "Compound",
			Nbt::IntArray(_) => "Int Array",
			Nbt::LongArray(_) => "Long Array",
			Nbt::Chunk(_) => "Chunk",
			Nbt::Region(_) => "Region",
		}
	}

	#[must_use]
	pub fn uv(&self) -> Vec2u {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::Byte(_) => NbtByte::UV,
			Nbt::Short(_) => NbtShort::UV,
			Nbt::Int(_) => NbtInt::UV,
			Nbt::Long(_) => NbtLong::UV,
			Nbt::Float(_) => NbtFloat::UV,
			Nbt::Double(_) => NbtDouble::UV,
			Nbt::ByteArray(_) => NbtByteArray::UV,
			Nbt::String(_) => NbtString::UV,
			Nbt::List(_) => NbtList::UV,
			Nbt::Compound(_) => NbtCompound::UV,
			Nbt::IntArray(_) => NbtIntArray::UV,
			Nbt::LongArray(_) => NbtLongArray::UV,
			Nbt::Chunk(chunk) => chunk.uv(),
			Nbt::Region(region) => region.uv(),
		}
	}

	#[must_use]
	pub fn should_render_description(&self) -> bool {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::Chunk(chunk) => chunk.is_loaded() && !(chunk.x == 0 && chunk.z == 0 && chunk.is_empty()),
			_ => true,
		}
	}
}

/// Navigate & Traverse
#[allow(dead_code)]
impl NbtElement {
	#[must_use]
	pub fn navigate(&self, indices: &Indices) -> Option<NavigationInformation> { NavigationInformation::from(self, indices) }

	#[must_use]
	pub fn navigate_mut(&mut self, indices: &Indices) -> Option<NavigationInformationMut> { NavigationInformationMut::from(self, indices) }

	#[must_use]
	pub fn navigate_parent<'nbt, 'indices>(&'nbt self, indices: &'indices Indices) -> Option<ParentNavigationInformation<'nbt, 'indices>> { ParentNavigationInformation::from(self, indices) }

	#[must_use]
	pub fn navigate_parent_mut<'nbt, 'indices>(&'nbt mut self, indices: &'indices Indices) -> Option<ParentNavigationInformationMut<'nbt, 'indices>> { ParentNavigationInformationMut::from(self, indices) }

	/// # Safety
	///
	/// See [`IterativeNavigationInformationMut::new`]
	#[must_use]
	pub unsafe fn navigate_iteratively_mut<'nbt, 'indices>(&'nbt mut self, indices: &'indices Indices) -> IterativeNavigationInformationMut<'nbt, 'indices> { unsafe { IterativeNavigationInformationMut::new(self, indices) } }

	/// # Safety
	///
	/// See [`ParentIterativeNavigationInformationMut::new`]
	#[must_use]
	pub unsafe fn navigate_parents_iteratively_mut<'nbt, 'indices>(&'nbt mut self, indices: &'indices Indices) -> ParentIterativeNavigationInformationMut<'nbt, 'indices> { unsafe { ParentIterativeNavigationInformationMut::new(self, indices) } }

	#[must_use]
	pub fn traverse(&self, y: usize, x: Option<usize>) -> Option<TraversalInformation> { TraversalInformation::from(self, y, x) }

	#[must_use]
	pub fn traverse_mut(&mut self, y: usize, x: Option<usize>) -> Option<TraversalInformationMut> { TraversalInformationMut::from(self, y, x) }
}

/// Mutable Indices-based operations
impl NbtElement {
	pub fn recache(&mut self) {
		use NbtPatternMut as Nbt;

		match self.as_pattern_mut() {
			Nbt::ByteArray(byte_array) => byte_array.recache(),
			Nbt::List(list) => list.recache(),
			Nbt::Compound(compound) => compound.recache(),
			Nbt::IntArray(int_array) => int_array.recache(),
			Nbt::LongArray(long_array) => long_array.recache(),
			Nbt::Chunk(chunk) => chunk.recache(),
			Nbt::Region(region) => region.recache(),
			_ => (),
		}
	}

	pub fn recache_along_indices<'a>(&'a mut self, indices: &Indices) {
		// SAFETY: all recache does not change the children indices, this is just an optimization over using the stack with recursion
		let mut children: Box<[MaybeUninit<&'a mut NbtElement>]> = unsafe { Box::try_new_uninit_slice(indices.len()).unwrap_unchecked() };

		let mut current_child = unsafe { core::ptr::read(core::ptr::addr_of!(self)) };
		for (child_slot, idx) in children.iter_mut().zip(indices.iter()) {
			let child: &'a mut NbtElement = unsafe { core::mem::transmute(&mut current_child[idx]) };
			let child_clone: &'a mut NbtElement = unsafe { core::ptr::read(core::ptr::addr_of!(child)) };
			child_slot.write(child);
			current_child = child_clone;
		}

		for child in children.into_iter().rev() {
			// SAFETY: all values are initialized
			let child = unsafe { child.assume_init() };
			child.recache();
		}

		self.recache();
	}
}

/// Immutable "getter" operations
impl NbtElement {
	/// Please minimize usage of this and instead rely upon [`NbtElement::as_pattern`] and [`NbtElement::as_pattern_mut`] as they are more stable
	#[must_use]
	pub const fn id(&self) -> u8 { unsafe { self.id.id } }

	#[must_use]
	pub fn len(&self) -> Option<usize> {
		use NbtPattern as Nbt;

		Some(match self.as_pattern() {
			Nbt::ByteArray(x) => x.len(),
			Nbt::List(x) => x.len(),
			Nbt::Compound(x) => x.len(),
			Nbt::IntArray(x) => x.len(),
			Nbt::LongArray(x) => x.len(),
			Nbt::Chunk(x) => x.len(),
			Nbt::Region(x) => x.len(),
			_ => return None,
		})
	}

	#[must_use]
	pub fn is_empty(&self) -> bool { self.len().is_some_and(|x| x == 0) }

	#[must_use]
	pub fn height(&self) -> usize {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::ByteArray(x) => x.height(),
			Nbt::List(x) => x.height(),
			Nbt::Compound(x) => x.height(),
			Nbt::IntArray(x) => x.height(),
			Nbt::LongArray(x) => x.height(),
			Nbt::Chunk(x) => x.height(),
			Nbt::Region(x) => x.height(),
			_ => 1,
		}
	}

	#[must_use]
	pub fn true_height(&self) -> usize {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::ByteArray(x) => x.true_height(),
			Nbt::List(x) => x.true_height(),
			Nbt::Compound(x) => x.true_height(),
			Nbt::IntArray(x) => x.true_height(),
			Nbt::LongArray(x) => x.true_height(),
			Nbt::Chunk(x) => x.true_height(),
			Nbt::Region(x) => x.true_height(),
			_ => 1,
		}
	}

	#[must_use]
	pub fn heights(&self) -> (usize, usize) { (self.height(), self.true_height()) }

	#[must_use]
	pub fn has_keys(&self) -> bool {
		match self.id() {
			NbtCompound::ID | NbtChunk::ID => true,
			_ => false,
		}
	}

	#[must_use]
	#[allow(clippy::type_complexity)] // a type probably shouldn't abstract what this is, like... yeah
	pub fn children(&self) -> Option<Result<Iter<'_, NbtElement>, Iter<'_, CompoundEntry>>> {
		use NbtPattern as Nbt;

		Some(match self.as_pattern() {
			Nbt::ByteArray(x) => Ok(x.children()),
			Nbt::IntArray(x) => Ok(x.children()),
			Nbt::LongArray(x) => Ok(x.children()),
			Nbt::List(x) => Ok(x.children()),
			Nbt::Compound(x) => Err(x.children()),
			Nbt::Region(x) => Ok(x.children()),
			Nbt::Chunk(x) => Err(x.children()),
			_ => return None,
		})
	}

	#[must_use]
	pub fn values(&self) -> Option<NbtElementValues<'_>> {
		use NbtPattern as Nbt;

		Some(match self.as_pattern() {
			Nbt::ByteArray(x) => NbtElementValues::Iter(x.children()),
			Nbt::IntArray(x) => NbtElementValues::Iter(x.children()),
			Nbt::LongArray(x) => NbtElementValues::Iter(x.children()),
			Nbt::List(x) => NbtElementValues::Iter(x.children()),
			Nbt::Compound(x) => NbtElementValues::CompoundMapIter(x.children()),
			Nbt::Region(x) => NbtElementValues::Iter(x.children()),
			Nbt::Chunk(x) => NbtElementValues::CompoundMapIter(x.children()),
			_ => return None,
		})
	}

	#[must_use]
	pub fn get_kv_under_indices(&self, indices: &Indices) -> Option<NbtElementAndKeyRef> {
		let mut key = None;
		let mut value = self;

		for idx in indices {
			match value.get(idx) {
				Some((k, v)) => {
					key = k;
					value = v;
				}
				None => {
					std::hint::cold_path();
					return None
				}
			}
		}

		Some((key, value))
	}

	#[must_use]
	pub fn get(&self, idx: usize) -> Option<NbtElementAndKeyRef> {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::ByteArray(x) => x.get(idx).map(|x| x.into()),
			Nbt::IntArray(x) => x.get(idx).map(|x| x.into()),
			Nbt::LongArray(x) => x.get(idx).map(|x| x.into()),
			Nbt::List(x) => x.get(idx).map(|x| x.into()),
			Nbt::Compound(x) => x.get(idx).map(|x| x.into()),
			Nbt::Region(x) => x.get(idx).map(|x| x.into()),
			Nbt::Chunk(x) => x.get(idx).map(|x| x.into()),
			_ => {
				std::hint::cold_path();
				None
			}
		}
	}

	#[must_use]
	pub unsafe fn get_unchecked(&self, idx: usize) -> &NbtElement {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::ByteArray(x) => unsafe { x.get_unchecked(idx) },
			Nbt::IntArray(x) => unsafe { x.get_unchecked(idx) },
			Nbt::LongArray(x) => unsafe { x.get_unchecked(idx) },
			Nbt::List(x) => unsafe { x.get_unchecked(idx) },
			Nbt::Compound(x) => unsafe { &x.get_unchecked(idx).value },
			Nbt::Region(x) => unsafe { x.get_unchecked(idx) },
			Nbt::Chunk(x) => unsafe { &x.get_unchecked(idx).value },
			_ => unsafe { std::hint::unreachable_unchecked() },
		}
	}

	#[must_use]
	pub fn is_open(&self) -> bool {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::ByteArray(x) => x.is_open(),
			Nbt::IntArray(x) => x.is_open(),
			Nbt::LongArray(x) => x.is_open(),
			Nbt::List(x) => x.is_open(),
			Nbt::Compound(x) => x.is_open(),
			Nbt::Region(x) => x.is_open(),
			Nbt::Chunk(x) => x.is_open(),
			_ => false,
		}
	}

	#[must_use]
	pub fn is_primitive(&self) -> bool {
		use NbtPattern as Nbt;

		matches!(self.as_pattern(), Nbt::Byte(_) | Nbt::Short(_) | Nbt::Int(_) | Nbt::Long(_) | Nbt::Float(_) | Nbt::Double(_) | Nbt::String(_))
	}

	#[must_use]
	pub fn is_complex(&self) -> bool {
		use NbtPattern as Nbt;

		matches!(self.as_pattern(), Nbt::ByteArray(_) | Nbt::List(_) | Nbt::Compound(_) | Nbt::IntArray(_) | Nbt::LongArray(_) | Nbt::Chunk(_) | Nbt::Region(_))
	}

	#[must_use]
	pub fn is_default_state(&self) -> bool {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::Byte(x) => x.value == 0,
			Nbt::Short(x) => x.value == 0,
			Nbt::Int(x) => x.value == 0,
			Nbt::Long(x) => x.value == 0,
			Nbt::Float(x) => x.value == 0.0,
			Nbt::Double(x) => x.value == 0.0,
			Nbt::ByteArray(x) => x.is_empty(),
			Nbt::String(x) => x.str.as_str().is_empty(),
			Nbt::List(x) => x.is_empty(),
			Nbt::Compound(x) => x.is_empty(),
			Nbt::IntArray(x) => x.is_empty(),
			Nbt::LongArray(x) => x.is_empty(),
			Nbt::Chunk(x) => x.is_unloaded(),
			Nbt::Region(x) => x.loaded_chunks() == 0,
		}
	}

	#[must_use]
	pub fn value(&self) -> (Cow<'_, str>, TextColor) {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::Byte(x) => (x.value(), TextColor::TreePrimitive),
			Nbt::Short(x) => (x.value(), TextColor::TreePrimitive),
			Nbt::Int(x) => (x.value(), TextColor::TreePrimitive),
			Nbt::Long(x) => (x.value(), TextColor::TreePrimitive),
			Nbt::Float(x) => (x.value(), TextColor::TreePrimitive),
			Nbt::Double(x) => (x.value(), TextColor::TreePrimitive),
			Nbt::ByteArray(x) => (x.value(), TextColor::TreeKey),
			Nbt::String(x) => (x.value(), TextColor::TreeString),
			Nbt::List(x) => (x.value(), TextColor::TreeKey),
			Nbt::Compound(x) => (x.value(), TextColor::TreeKey),
			Nbt::IntArray(x) => (x.value(), TextColor::TreeKey),
			Nbt::LongArray(x) => (x.value(), TextColor::TreeKey),
			Nbt::Chunk(x) => (x.value(), TextColor::TreeKey),
			Nbt::Region(x) => (x.value(), TextColor::TreeKey),
		}
	}

	#[must_use]
	pub fn value_width(&self) -> usize {
		use NbtPattern as Nbt;
		use util::{f32_width, f64_width, i8_width, i16_width, i32_width, i64_width, u8_width, usize_width};

		match self.as_pattern() {
			Nbt::Byte(x) => i8_width(x.value),
			Nbt::Short(x) => i16_width(x.value),
			Nbt::Int(x) => i32_width(x.value),
			Nbt::Long(x) => i64_width(x.value),
			Nbt::Float(x) => f32_width(x.value),
			Nbt::Double(x) => f64_width(x.value),
			Nbt::ByteArray(x) => usize_width(x.len()) + const { width_ascii(" ") } + id_to_string_name_width(NbtByte::ID, x.len()),
			Nbt::String(x) => x.str.width(),
			Nbt::List(x) => usize_width(x.len()) + const { width_ascii(" ") } + id_to_string_name_width(x.child_id(), x.len()),
			Nbt::Compound(x) => usize_width(x.len()) + const { width_ascii(" ") } + id_to_string_name_width(Self::NULL_ID, x.len()),
			Nbt::IntArray(x) => usize_width(x.len()) + const { width_ascii(" ") } + id_to_string_name_width(NbtInt::ID, x.len()),
			Nbt::LongArray(x) => usize_width(x.len()) + const { width_ascii(" ") } + id_to_string_name_width(NbtLong::ID, x.len()),
			Nbt::Chunk(x) => u8_width(x.x) + const { width_ascii(", ") } + u8_width(x.z),
			Nbt::Region(x) => usize_width(x.loaded_chunks()) + id_to_string_name_width(NbtChunk::ID, x.loaded_chunks()),
		}
	}

	#[must_use]
	pub fn create_drop_indices(&self, kv: NbtElementAndKeyRef, mut y: usize, x: usize) -> Option<OwnedIndices> {
		let mut indices = OwnedIndices::new();
		match self.create_drop_indices0(kv, &mut y, 0, x, &mut indices) {
			DropResult::Dropped => Some(indices),
			DropResult::Missed | DropResult::Failed => None,
		}
	}

	#[must_use]
	pub(super) fn create_drop_indices0(&self, kv: NbtElementAndKeyRef, y: &mut usize, mut current_depth: usize, x: usize, indices: &mut OwnedIndices) -> DropResult {
		let height_px = self.height() * 16;
		if *y >= height_px + 8 {
			*y -= height_px;
			return DropResult::Missed
		}
		if let Some(iter) = self.values() {
			let can_insert = self.can_insert(kv.1);
			let is_open = self.is_open();
			let len = self.len().unwrap_or(0);

			if *y < 16 && *y >= 8 && current_depth == x && can_insert {
				indices.push(0);
				return DropResult::Dropped;
			}

			if !is_open && *y < 24 && *y >= 16 && current_depth == x && can_insert {
				indices.push(len);
				return DropResult::Dropped;
			}

			if *y < 16 {
				return DropResult::Missed;
			}

			*y -= 16;

			if is_open && !self.is_empty() {
				current_depth += 1;
				indices.push(0);
				let ptr_idx = indices.len() - 1;
				for (idx, child) in iter.enumerate() {
					indices[ptr_idx] = idx;
					if *y < 8 && current_depth == x && can_insert {
						return DropResult::Dropped;
					} else if *y >= child.height() * 16 - 8 && *y < child.height() * 16 && current_depth == x && can_insert {
						indices[ptr_idx] = idx + 1;
						return DropResult::Dropped;
					}

					match child.create_drop_indices0(kv, y, current_depth, x, indices) {
						DropResult::Missed => (),
						x @ (DropResult::Dropped | DropResult::Failed) => return x,
					}
				}
				indices.pop();
			}
			DropResult::Missed
		} else {
			*y = y.saturating_sub(16);
			DropResult::Missed
		}
	}

	#[must_use]
	pub fn max_depth(&self) -> usize {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::ByteArray(x) => x.max_depth(),
			Nbt::IntArray(x) => x.max_depth(),
			Nbt::LongArray(x) => x.max_depth(),
			Nbt::List(x) => x.max_depth(),
			Nbt::Compound(x) => x.max_depth(),
			Nbt::Region(x) => x.max_depth(),
			Nbt::Chunk(x) => x.max_depth(),
			_ => 0,
		}
	}

	#[must_use]
	pub fn actions(&self) -> &[ElementAction] {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::Byte(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			Nbt::Short(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			Nbt::Int(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			Nbt::Long(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			Nbt::Float(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			Nbt::Double(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			Nbt::ByteArray(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenArrayInHex,
				ElementAction::InvertBookmarks,
				ElementAction::InsertFromClipboard,
			],
			Nbt::String(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			#[cfg(not(target_arch = "wasm32"))]
			Nbt::List(x) => {
				const FULL: [ElementAction; 6] = [
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					ElementAction::OpenInTxt,
					ElementAction::InsertFromClipboard,
					ElementAction::InvertBookmarks,
					ElementAction::OpenArrayInHex,
				];
				let id = x.child_id();
				if matches!(id, NbtByte::ID | NbtShort::ID | NbtInt::ID | NbtLong::ID) { &FULL } else { &FULL[..FULL.len() - 1] }
			}
			#[cfg(target_arch = "wasm32")]
			Nbt::List(_) => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::InsertFromClipboard, ElementAction::InvertBookmarks],
			Nbt::Compound(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::SortCompoundByName,
				ElementAction::SortCompoundByType,
				ElementAction::InsertFromClipboard,
				ElementAction::InvertBookmarks,
			],
			Nbt::IntArray(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenArrayInHex,
				ElementAction::InsertFromClipboard,
				ElementAction::InvertBookmarks,
			],
			Nbt::LongArray(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenArrayInHex,
				ElementAction::InsertFromClipboard,
				ElementAction::InvertBookmarks,
			],
			Nbt::Chunk(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::SortCompoundByName,
				ElementAction::SortCompoundByType,
				ElementAction::InsertFromClipboard,
				ElementAction::InvertBookmarks,
			],
			Nbt::Region(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
		}
	}

	#[must_use]
	pub fn can_insert(&self, value: &NbtElement) -> bool {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::ByteArray(x) => x.can_insert(value),
			Nbt::List(x) => x.can_insert(value),
			Nbt::Compound(x) => x.can_insert(value),
			Nbt::IntArray(x) => x.can_insert(value),
			Nbt::LongArray(x) => x.can_insert(value),
			Nbt::Chunk(x) => x.can_insert(value),
			Nbt::Region(x) => x.can_insert(value),
			_ => false,
		}
	}
}

/// Operations
impl NbtElement {
	pub fn on_style_change(&mut self, bookmarks: &mut MarkedLines) -> bool {
		use NbtPatternMut as Nbt;

		match self.as_pattern_mut() {
			Nbt::Region(region) => region.on_style_change(bookmarks),
			_ => false,
		}
	}

	/// # Safety
	/// - must be assured to update valid caches
	pub unsafe fn toggle(&mut self) -> bool {
		use NbtPatternMut as Nbt;

		match self.as_pattern_mut() {
			Nbt::ByteArray(byte_array) => unsafe { byte_array.toggle() },
			Nbt::IntArray(int_array) => unsafe { int_array.toggle() },
			Nbt::LongArray(long_array) => unsafe { long_array.toggle() },
			Nbt::List(list) => unsafe { list.toggle() },
			Nbt::Compound(compound) => unsafe { compound.toggle() },
			Nbt::Region(region) => unsafe { region.toggle() },
			Nbt::Chunk(chunk) => unsafe { chunk.toggle() },
			_ => return false,
		}

		true
	}

	/// # Safety
	/// - must be assured to update valid caches (bookmarks)
	pub unsafe fn shut<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		use NbtPatternMut as Nbt;

		match self.as_pattern_mut() {
			Nbt::ByteArray(byte_array) => unsafe { byte_array.shut(scope) },
			Nbt::IntArray(int_array) => unsafe { int_array.shut(scope) },
			Nbt::LongArray(long_array) => unsafe { long_array.shut(scope) },
			Nbt::List(list) => unsafe { list.shut(scope) },
			Nbt::Compound(compound) => unsafe { compound.shut(scope) },
			Nbt::Region(region) => unsafe { region.shut(scope) },
			Nbt::Chunk(chunk) => unsafe { chunk.shut(scope) },
			_ => (),
		}
	}

	/// # Safety
	/// - must be assured to update valid caches (bookmarks)
	pub unsafe fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		use NbtPatternMut as Nbt;

		match self.as_pattern_mut() {
			Nbt::ByteArray(byte_array) => unsafe { byte_array.expand(scope) },
			Nbt::IntArray(int_array) => unsafe { int_array.expand(scope) },
			Nbt::LongArray(long_array) => unsafe { long_array.expand(scope) },
			Nbt::List(list) => unsafe { list.expand(scope) },
			Nbt::Compound(compound) => unsafe { compound.expand(scope) },
			Nbt::Region(region) => unsafe { region.expand(scope) },
			Nbt::Chunk(chunk) => unsafe { chunk.expand(scope) },
			_ => (),
		}
	}

	/// # Safety
	/// * must be assured to update valid caches
	pub unsafe fn insert(&mut self, idx: usize, kv: NbtElementAndKey) -> Result<Option<NbtElementAndKey>, NbtElementAndKey> {
		use NbtPatternMut as Nbt;

		Ok(match self.as_pattern_mut() {
			Nbt::ByteArray(byte_array) => unsafe {
				byte_array
					.insert(idx, kv.1)?
					.map(NbtElement::into)
			},
			Nbt::List(list) => unsafe { list.insert(idx, kv.1)?.map(NbtElement::into) },
			Nbt::Compound(compound) => unsafe {
				compound
					.insert(idx, CompoundEntry::new(kv.0.unwrap_or(CompactString::const_new("_")), kv.1))?
					.map(CompoundEntry::into)
			},
			Nbt::IntArray(int_array) => unsafe { int_array.insert(idx, kv.1)?.map(NbtElement::into) },
			Nbt::LongArray(long_array) => unsafe {
				long_array
					.insert(idx, kv.1)?
					.map(NbtElement::into)
			},
			Nbt::Region(region) => unsafe { region.insert(idx, kv.1)?.map(NbtElement::into) },
			Nbt::Chunk(chunk) => unsafe {
				chunk
					.insert(idx, CompoundEntry::new(kv.0.unwrap_or(CompactString::const_new("_")), kv.1))?
					.map(CompoundEntry::into)
			},
			_ => return Err(kv),
		})
	}

	/// # Safety
	/// - must be assured to update valid caches
	pub unsafe fn replace_key_value(&mut self, idx: usize, kv: NbtElementAndKey) -> Result<Option<NbtElementAndKey>, NbtElementAndKey> {
		use NbtPatternMut as Nbt;

		Ok(match self.as_pattern_mut() {
			Nbt::ByteArray(byte_array) => unsafe {
				byte_array
					.replace(idx, kv.1)?
					.map(NbtElement::into)
			},
			Nbt::IntArray(int_array) => unsafe {
				int_array
					.replace(idx, kv.1)?
					.map(NbtElement::into)
			},
			Nbt::LongArray(long_array) => unsafe {
				long_array
					.replace(idx, kv.1)?
					.map(NbtElement::into)
			},
			Nbt::List(list) => unsafe { list.replace(idx, kv.1)?.map(NbtElement::into) },
			Nbt::Compound(compound) => unsafe {
				compound
					.replace(idx, CompoundEntry::new(kv.0.unwrap_or(CompactString::const_new("_")), kv.1))?
					.map(CompoundEntry::into)
			},
			Nbt::Chunk(chunk) => unsafe {
				chunk
					.replace(idx, CompoundEntry::new(kv.0.unwrap_or(CompactString::const_new("_")), kv.1))?
					.map(CompoundEntry::into)
			},
			Nbt::Region(region) => unsafe { region.replace(idx, kv.1)?.map(NbtElement::into) },
			_ => return Err(kv),
		})
	}

	/// # Safety
	/// - must be assured to update valid caches
	pub unsafe fn remove(&mut self, idx: usize) -> Option<NbtElementAndKey> {
		use NbtPatternMut as Nbt;

		Some(match self.as_pattern_mut() {
			Nbt::ByteArray(byte_array) => byte_array.remove(idx)?.into(),
			Nbt::IntArray(int_array) => int_array.remove(idx)?.into(),
			Nbt::LongArray(long_array) => long_array.remove(idx)?.into(),
			Nbt::List(list) => list.remove(idx)?.into(),
			Nbt::Compound(compound) => compound.remove(idx)?.into(),
			Nbt::Region(region) => region.remove(idx)?.into(),
			Nbt::Chunk(chunk) => chunk.remove(idx)?.into(),
			_ => return None,
		})
	}

	/// # Safety
	/// - must be assured to update valid caches
	pub unsafe fn swap(&mut self, a: usize, b: usize) {
		use NbtPatternMut as Nbt;

		match self.as_pattern_mut() {
			Nbt::ByteArray(byte_array) => unsafe { byte_array.swap(a, b) },
			Nbt::IntArray(int_array) => unsafe { int_array.swap(a, b) },
			Nbt::LongArray(long_array) => unsafe { long_array.swap(a, b) },
			Nbt::List(list) => unsafe { list.swap(a, b) },
			Nbt::Compound(compound) => unsafe { compound.swap(a, b) },
			Nbt::Region(region) => unsafe { region.swap(a, b) },
			Nbt::Chunk(chunk) => unsafe { chunk.swap(a, b) },
			_ => {}
		}
	}
}

/// Mutable "getter" / "setter" operations
impl NbtElement {
	#[must_use]
	#[allow(clippy::type_complexity)] // a type probably shouldn't abstract what this is, like... yeah
	pub fn children_mut(&mut self) -> Option<Result<IterMut<'_, NbtElement>, IterMut<'_, CompoundEntry>>> {
		use NbtPatternMut as Nbt;

		Some(match self.as_pattern_mut() {
			Nbt::ByteArray(byte_array) => Ok(byte_array.children_mut()),
			Nbt::IntArray(int_array) => Ok(int_array.children_mut()),
			Nbt::LongArray(long_array) => Ok(long_array.children_mut()),
			Nbt::List(list) => Ok(list.children_mut()),
			Nbt::Compound(compound) => Err(compound.children_mut()),
			Nbt::Chunk(chunk) => Err(chunk.children_mut()),
			Nbt::Region(region) => Ok(region.children_mut()),
			_ => return None,
		})
	}

	pub fn set_value(&mut self, value: String) -> Option<(String, bool)> {
		use NbtPatternMut as Nbt;

		Some(match self.as_pattern_mut() {
			Nbt::Byte(byte) => {
				let before = byte.value().into_owned();
				let success = value.parse().map(|x| byte.value = x).is_ok();
				(before, success)
			}
			Nbt::Short(short) => {
				let before = short.value().into_owned();
				let success = value.parse().map(|x| short.value = x).is_ok();
				(before, success)
			}
			Nbt::Int(int) => {
				let before = int.value().into_owned();
				let success = value.parse().map(|x| int.value = x).is_ok();
				(before, success)
			}
			Nbt::Long(long) => {
				let before = long.value().into_owned();
				let success = value.parse().map(|x| long.value = x).is_ok();
				(before, success)
			}
			Nbt::Float(float) => {
				let before = float.value().into_owned();
				let success = value.parse().map(|x| float.value = x).is_ok();
				(before, success)
			}
			Nbt::Double(double) => {
				let before = double.value().into_owned();
				let success = value.parse().map(|x| double.value = x).is_ok();
				(before, success)
			}
			Nbt::String(string) => (
				core::mem::replace(string, NbtString::new(value.into()))
					.str
					.as_str()
					.to_owned(),
				true,
			),
			_ => {
				std::hint::cold_path();
				return None
			}
		})
	}

	#[must_use]
	pub fn update_key(&mut self, idx: usize, key: CompactString) -> Option<Option<CompactString>> {
		use NbtPatternMut as Nbt;

		Some(match self.as_pattern_mut() {
			Nbt::Compound(compound) => compound.map.update_key(idx, key),
			Nbt::Chunk(chunk) => chunk.map.update_key(idx, key),
			_ => return None,
		})
	}

	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<NbtElementAndKeyRefMut> {
		use NbtPatternMut as Nbt;

		match self.as_pattern_mut() {
			Nbt::ByteArray(byte_array) => byte_array.get_mut(idx).map(|x| x.into()),
			Nbt::List(list) => list.get_mut(idx).map(|x| x.into()),
			Nbt::Compound(compound) => compound.get_mut(idx).map(|x| x.into()),
			Nbt::IntArray(int_array) => int_array.get_mut(idx).map(|x| x.into()),
			Nbt::LongArray(long_array) => long_array.get_mut(idx).map(|x| x.into()),
			Nbt::Chunk(chunk) => chunk.get_mut(idx).map(|x| x.into()),
			Nbt::Region(region) => region.get_mut(idx).map(|x| x.into()),
			_ => {
				std::hint::cold_path();
				None
			}
		}
	}

	#[must_use]
	pub unsafe fn get_unchecked_mut(&mut self, idx: usize) -> &mut NbtElement {
		use NbtPatternMut as Nbt;

		match self.as_pattern_mut() {
			Nbt::ByteArray(byte_array) => unsafe { byte_array.get_unchecked_mut(idx) },
			Nbt::List(list) => unsafe { list.get_unchecked_mut(idx) },
			Nbt::Compound(compound) => unsafe { &mut compound.get_unchecked_mut(idx).value },
			Nbt::IntArray(int_array) => unsafe { int_array.get_unchecked_mut(idx) },
			Nbt::LongArray(long_array) => unsafe { long_array.get_unchecked_mut(idx) },
			Nbt::Chunk(chunk) => unsafe { &mut chunk.get_unchecked_mut(idx).value },
			Nbt::Region(region) => unsafe { region.get_unchecked_mut(idx) },
			_ => unsafe { core::hint::unreachable_unchecked() },
		}
	}

	pub unsafe fn try_compound_singleton_into_inner(mut self) -> Result<Self, Self> {
		if let Some(compound) = self.as_compound_mut()
			&& compound.len() == 1
			&& compound
				.get(0)
				.is_some_and(|entry| entry.key.is_empty())
			&& let Some(CompoundEntry { key: _, value }) = unsafe { compound.remove(0) }
		{
			Ok(value)
		} else {
			Err(self)
		}
	}
}

impl Display for NbtElement {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::Byte(byte) => write!(f, "{byte}"),
			Nbt::Short(short) => write!(f, "{short}"),
			Nbt::Int(int) => write!(f, "{int}"),
			Nbt::Long(long) => write!(f, "{long}"),
			Nbt::Float(float) => write!(f, "{float}"),
			Nbt::Double(double) => write!(f, "{double}"),
			Nbt::ByteArray(byte_array) => write!(f, "{byte_array}"),
			Nbt::String(string) => write!(f, "{string}"),
			Nbt::List(list) => write!(f, "{list}"),
			Nbt::Compound(compound) => write!(f, "{compound}"),
			Nbt::IntArray(int_array) => write!(f, "{int_array}"),
			Nbt::LongArray(long_array) => write!(f, "{long_array}"),
			Nbt::Chunk(chunk) => write!(f, "{chunk}"),
			Nbt::Region(_) => Err(Error),
		}
	}
}

impl PrettyDisplay for NbtElement {
	fn pretty_fmt(&self, f: &mut PrettyFormatter) {
		use NbtPattern as Nbt;

		match self.as_pattern() {
			Nbt::Byte(byte) => byte.pretty_fmt(f),
			Nbt::Short(short) => short.pretty_fmt(f),
			Nbt::Int(int) => int.pretty_fmt(f),
			Nbt::Long(long) => long.pretty_fmt(f),
			Nbt::Float(float) => float.pretty_fmt(f),
			Nbt::Double(double) => double.pretty_fmt(f),
			Nbt::ByteArray(byte_array) => byte_array.pretty_fmt(f),
			Nbt::String(string) => string.pretty_fmt(f),
			Nbt::List(list) => list.pretty_fmt(f),
			Nbt::Compound(compound) => compound.pretty_fmt(f),
			Nbt::IntArray(int_array) => int_array.pretty_fmt(f),
			Nbt::LongArray(long_array) => long_array.pretty_fmt(f),
			Nbt::Chunk(chunk) => chunk.pretty_fmt(f),
			Nbt::Region(region) => region.pretty_fmt(f),
		}
	}
}

impl Debug for NbtElement {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		let mut formatter = PrettyFormatter::new();
		self.pretty_fmt(&mut formatter);
		write!(f, "{}", formatter.finish())
	}
}

impl Drop for NbtElement {
	fn drop(&mut self) {
		use NbtPatternMut as Nbt;

		match self.as_pattern_mut() {
			Nbt::Byte(inner) => unsafe { (inner as *mut NbtByte).drop_in_place() },
			Nbt::Short(inner) => unsafe { (inner as *mut NbtShort).drop_in_place() },
			Nbt::Int(inner) => unsafe { (inner as *mut NbtInt).drop_in_place() },
			Nbt::Long(inner) => unsafe { (inner as *mut NbtLong).drop_in_place() },
			Nbt::Float(inner) => unsafe { (inner as *mut NbtFloat).drop_in_place() },
			Nbt::Double(inner) => unsafe { (inner as *mut NbtDouble).drop_in_place() },
			Nbt::ByteArray(inner) => unsafe { (inner as *mut NbtByteArray).drop_in_place() },
			Nbt::String(inner) => unsafe { (inner as *mut NbtString).drop_in_place() },
			Nbt::List(inner) => unsafe { (inner as *mut NbtList).drop_in_place() },
			Nbt::Compound(inner) => unsafe { (inner as *mut NbtCompound).drop_in_place() },
			Nbt::IntArray(inner) => unsafe { (inner as *mut NbtIntArray).drop_in_place() },
			Nbt::LongArray(inner) => unsafe { (inner as *mut NbtLongArray).drop_in_place() },
			Nbt::Chunk(inner) => unsafe { (inner as *mut NbtChunk).drop_in_place() },
			Nbt::Region(inner) => unsafe { (inner as *mut NbtRegion).drop_in_place() },
		}
	}
}

impl<'a> Index<&'a str> for NbtElement {
	type Output = NbtElement;

	fn index(&self, index: &'a str) -> &Self::Output {
		use NbtPattern as Nbt;

		let map = match self.as_pattern() {
			Nbt::Compound(compound) => compound.map.as_ref(),
			Nbt::Chunk(chunk) => chunk.map.as_ref(),
			_ => {
				std::hint::cold_path();
				return const { &Self::NULL }
			}
		};

		if let Some(idx) = map.idx_of(index)
			&& let Some(CompoundEntry { key: _, value }) = map.entries.get(idx)
		{
			value
		} else {
			std::hint::cold_path();
			const { &Self::NULL }
		}
	}
}

impl<'a> IndexMut<&'a str> for NbtElement {
	fn index_mut(&mut self, index: &str) -> &mut Self::Output {
		use NbtPatternMut as Nbt;

		pub static mut NULL_MUT: NbtElement = NbtElement::NULL;

		let result = 'a: {
			let map = match self.as_pattern_mut() {
				Nbt::Compound(compound) => &mut *compound.map,
				Nbt::Chunk(chunk) => &mut *chunk.map,
				_ => {
					std::hint::cold_path();
					break 'a None
				}
			};

			if let Some(idx) = map.idx_of(index)
				&& let Some(CompoundEntry { key: _, value }) = map.entries.get_mut(idx)
			{
				Some(value)
			} else {
				std::hint::cold_path();
				None
			}
		};

		result.unwrap_or_else(|| {
			std::hint::cold_path();
			unsafe {
				NULL_MUT = Self::NULL;
			}
			#[allow(static_mut_refs)]
			unsafe {
				&mut NULL_MUT
			}
		})
	}
}

impl Index<usize> for NbtElement {
	type Output = NbtElement;

	fn index(&self, idx: usize) -> &Self::Output {
		match self.get(idx) {
			Some((_, b)) => b,
			None => {
				std::hint::cold_path();
				Self::NULL_REF
			}
		}
	}
}

impl IndexMut<usize> for NbtElement {
	fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
		pub static mut NULL_MUT: NbtElement = NbtElement::NULL;

		self.get_mut(idx)
			.map(|(_, b)| b)
			.unwrap_or_else(|| {
				std::hint::cold_path();
				unsafe {
					NULL_MUT = NbtElement::NULL;
				}
				#[allow(static_mut_refs)]
				unsafe {
					&mut NULL_MUT
				}
			})
	}
}

#[must_use]
pub const fn id_to_string_name(id: u8, len: usize) -> &'static str {
	#[must_use]
	const fn id_to_string_name0(id: u8) -> (&'static str, &'static str) {
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
			_ => ("entry", "entries"),
		}
	}

	let (single, multiple) = id_to_string_name0(id);
	if len == 1 { single } else { multiple }
}

pub const fn id_to_string_name_width(id: u8, len: usize) -> usize {
	#[must_use]
	const fn id_to_string_name_width0(id: u8) -> (usize, usize) {
		match id {
			NbtByte::ID => (width_ascii("byte"), width_ascii("bytes")),
			NbtShort::ID => (width_ascii("short"), width_ascii("shorts")),
			NbtInt::ID => (width_ascii("int"), width_ascii("ints")),
			NbtLong::ID => (width_ascii("long"), width_ascii("longs")),
			NbtFloat::ID => (width_ascii("float"), width_ascii("floats")),
			NbtDouble::ID => (width_ascii("double"), width_ascii("doubles")),
			NbtByteArray::ID => (width_ascii("byte array"), width_ascii("byte arrays")),
			NbtString::ID => (width_ascii("string"), width_ascii("strings")),
			NbtList::ID => (width_ascii("list"), width_ascii("lists")),
			NbtCompound::ID => (width_ascii("compound"), width_ascii("compounds")),
			NbtIntArray::ID => (width_ascii("int array"), width_ascii("int arrays")),
			NbtLongArray::ID => (width_ascii("long array"), width_ascii("long arrays")),
			NbtChunk::ID => (width_ascii("chunk"), width_ascii("chunks")),
			NbtRegion::ID => (width_ascii("region"), width_ascii("regions")),
			NbtElement::NULL_ID => (width_ascii("entry"), width_ascii("entries")),
			_ => (0, 0),
		}
	}

	let (single, other) = id_to_string_name_width0(id);
	if len == 1 { single } else { other }
}

pub enum NbtElementValues<'a> {
	Iter(Iter<'a, NbtElement>),
	CompoundMapIter(Iter<'a, CompoundEntry>),
}

impl<'a> Iterator for NbtElementValues<'a> {
	type Item = &'a NbtElement;

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			Self::Iter(iter) => iter.next(),
			Self::CompoundMapIter(iter) => iter.next().map(|entry| &entry.value),
		}
	}
}

impl<'a> DoubleEndedIterator for NbtElementValues<'a> {
	fn next_back(&mut self) -> Option<Self::Item> {
		match self {
			Self::Iter(iter) => iter.next_back(),
			Self::CompoundMapIter(iter) => iter.next_back().map(|entry| &entry.value),
		}
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
}

/// Patterns
impl NbtElement {
	// if this inline isn't here, the match isn't inlined and the code isn't nearly as performant
	#[inline(always)]
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
			_ => unsafe { core::hint::unreachable_unchecked() },
		}
	}

	// if this inline isn't here, the match isn't inlined and the code isn't nearly as performant
	#[inline(always)]
	#[must_use]
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
			_ => unsafe { core::hint::unreachable_unchecked() },
		}
	}
}

macro_rules! type_conversions {
	($t:ty, $field:ident, $is:ident, $into_unchecked:ident, $as_unchecked:ident, $as_unchecked_mut:ident, $into:ident, $as_ref:ident, $as_mut:ident) => {
		#[allow(dead_code)]
		impl NbtElement {
			/// # Safety
			///
			/// Variant must be of type
			#[doc = stringify!($t)]
			#[must_use]
			pub unsafe fn $into_unchecked(self) -> $t {
				let inner = unsafe { ::std::ptr::read((&raw const self.$field).cast::<$t>()) };
				::std::mem::forget(self);
				inner
			}

			/// # Safety
			///
			/// Variant must be of type
			#[doc = stringify!($t)]
			#[must_use]
			pub unsafe fn $as_unchecked(&self) -> &$t { unsafe { &self.$field } }

			/// # Safety
			///
			/// Variant must be of type
			#[doc = stringify!($t)]
			#[must_use]
			pub unsafe fn $as_unchecked_mut(&mut self) -> &mut $t { unsafe { &mut self.$field } }

			#[must_use]
			pub fn $into(self) -> Option<$t> { unsafe { self.$is().then_some(self.$into_unchecked()) } }

			#[must_use]
			pub fn $as_ref(&self) -> Option<&$t> { unsafe { self.$is().then_some(self.$as_unchecked()) } }

			#[must_use]
			pub fn $as_mut(&mut self) -> Option<&mut $t> { unsafe { self.$is().then_some(self.$as_unchecked_mut()) } }

			#[must_use]
			pub fn $is(&self) -> bool { self.id() == <$t>::ID }
		}
	};
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

/// Nonnull
impl NbtElement {
	#[must_use]
	pub fn is_null(&self) -> bool { self.id() == Self::NULL_ID }

	#[must_use]
	pub fn as_nonnull(&self) -> Option<&Self> { (!self.is_null()).then_some(self) }

	#[must_use]
	pub fn as_nonnull_mut(&mut self) -> Option<&mut Self> { (!self.is_null()).then_some(self) }

	#[must_use]
	pub fn into_nonnull(self) -> Option<Self> { (!self.is_null()).then_some(self) }
}
