use std::alloc::{Layout, dealloc};
use std::fmt::{Debug, Display, Error, Formatter};
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::{Index, IndexMut};
use std::slice::{Iter, IterMut};
#[cfg(not(target_arch = "wasm32"))] use std::thread::Scope;

use NbtPattern::*;
use compact_str::{CompactString, ToCompactString};
use hashbrown::HashTable;

use crate::assets::ZOffset;
use crate::elements::result::NbtParseResult;
use crate::elements::{
	CompoundMap, CompoundMapIter, CompoundMapIterMut, Entry, NbtByte, NbtByteArray, NbtChunk, NbtCompound, NbtDouble, NbtElementAndKey, NbtElementAndKeyRef, NbtFloat, NbtInt, NbtIntArray, NbtList, NbtLong, NbtLongArray, NbtNull, NbtRegion, NbtShort,
	NbtString,
};
use crate::render::{RenderContext, TextColor, VertexBufferBuilder};
use crate::serialization::{BigEndianDecoder, Decoder, LittleEndianDecoder, PrettyFormatter, UncheckedBufWriter};
use crate::tree::{Indices, NavigationInformation, NavigationInformationMut, OwnedIndices, ParentNavigationInformation, ParentNavigationInformationMut, TraversalInformation, TraversalInformationMut};
use crate::util;
use crate::util::{StrExt, now, width_ascii};
#[cfg(target_arch = "wasm32")] use crate::wasm::FakeScope as Scope;
use crate::workbench::{DropResult, ElementAction, FileFormat, MarkedLines};

#[repr(C)]
pub union NbtElement {
	pub(super) chunk: ManuallyDrop<NbtChunk>,
	pub(super) region: ManuallyDrop<NbtRegion>,
	pub(super) byte: ManuallyDrop<NbtByte>,
	pub(super) short: ManuallyDrop<NbtShort>,
	pub(super) int: ManuallyDrop<NbtInt>,
	pub(super) long: ManuallyDrop<NbtLong>,
	pub(super) float: ManuallyDrop<NbtFloat>,
	pub(super) double: ManuallyDrop<NbtDouble>,
	pub(super) byte_array: ManuallyDrop<NbtByteArray>,
	pub(super) string: ManuallyDrop<NbtString>,
	pub(super) list: ManuallyDrop<NbtList>,
	pub(super) compound: ManuallyDrop<NbtCompound>,
	pub(super) int_array: ManuallyDrop<NbtIntArray>,
	pub(super) long_array: ManuallyDrop<NbtLongArray>,
	pub(super) null: ManuallyDrop<NbtNull>,
	id: NbtElementId,
}

#[repr(C)]
#[derive(Copy, Clone)]
struct NbtElementId {
	_pad: [MaybeUninit<u8>; 23],
	id: u8,
}

/// Matches
impl NbtElement {
	#[must_use]
	pub fn matches(&self, other: &Self) -> bool {
		match (self.as_pattern(), other.as_pattern()) {
			(Byte(a), Byte(b)) => a.matches(b),
			(Short(a), Short(b)) => a.matches(b),
			(Int(a), Int(b)) => a.matches(b),
			(Long(a), Long(b)) => a.matches(b),
			(Float(a), Float(b)) => a.matches(b),
			(Double(a), Double(b)) => a.matches(b),
			(ByteArray(a), ByteArray(b)) => a.matches(b),
			(String(a), String(b)) => a.matches(b),
			(List(a), List(b)) => a.matches(b),
			(Compound(a), Compound(b)) => a.matches(b),
			(IntArray(a), IntArray(b)) => a.matches(b),
			(LongArray(a), LongArray(b)) => a.matches(b),
			(Chunk(a), Chunk(b)) => a.matches(b),
			(Region(a), Region(b)) => a.matches(b),
			(Null(a), Null(b)) => a.matches(b),
			_ => false,
		}
	}
}

impl PartialEq for NbtElement {
	fn eq(&self, other: &Self) -> bool {
		match (self.as_pattern(), other.as_pattern()) {
			(Byte(a), Byte(b)) => a.eq(b),
			(Short(a), Short(b)) => a.eq(b),
			(Int(a), Int(b)) => a.eq(b),
			(Long(a), Long(b)) => a.eq(b),
			(Float(a), Float(b)) => a.eq(b),
			(Double(a), Double(b)) => a.eq(b),
			(ByteArray(a), ByteArray(b)) => a.eq(b),
			(String(a), String(b)) => a.eq(b),
			(List(a), List(b)) => a.eq(b),
			(Compound(a), Compound(b)) => a.eq(b),
			(IntArray(a), IntArray(b)) => a.eq(b),
			(LongArray(a), LongArray(b)) => a.eq(b),
			(Chunk(a), Chunk(b)) => a.eq(b),
			(Region(a), Region(b)) => a.eq(b),
			(Null(a), Null(b)) => a.eq(b),
			_ => false,
		}
	}
}

impl Clone for NbtElement {
	fn clone(&self) -> Self {
		match self.as_pattern() {
			Byte(byte) => Self::Byte(byte.clone()),
			Short(short) => Self::Short(short.clone()),
			Int(int) => Self::Int(int.clone()),
			Long(long) => Self::Long(long.clone()),
			Float(float) => Self::Float(float.clone()),
			Double(double) => Self::Double(double.clone()),
			ByteArray(byte_array) => Self::ByteArray(byte_array.clone()),
			String(string) => Self::String(string.clone()),
			List(list) => Self::List(list.clone()),
			Compound(compound) => Self::Compound(compound.clone()),
			IntArray(int_array) => Self::IntArray(int_array.clone()),
			LongArray(long_array) => Self::LongArray(long_array.clone()),
			Chunk(chunk) => Self::Chunk(chunk.clone()),
			Region(region) => Self::Region(region.clone()),
			Null(null) => Self::Null(null.clone()),
		}
	}
}

/// Variant initialization from inner type
macro_rules! create_constructor {
	($field:ident, $name:ident, $ty:ty) => {
		#[must_use]
		pub const fn $name(this: $ty) -> Self {
			let mut this = Self { $field: core::mem::ManuallyDrop::new(this) };
			unsafe { this.id.id = <$ty>::ID };
			this
		}
	};
}
#[allow(non_snake_case)]
impl NbtElement {
	pub const NULL: NbtElement = Self::Null(NbtNull);
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
	create_constructor!(null, Null, NbtNull);
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

	pub(super) fn from_str0<F: FnOnce(&str, bool, bool, u32, &str) -> Result<Self, usize>>(mut s: &str, parse_ambiguous_integer: F) -> Result<(&str, Self), usize> {
		if let Some(s2) = s.strip_prefix("false") {
			return Ok((s2, Self::Byte(NbtByte { value: 0 })))
		}
		if let Some(s2) = s.strip_prefix("true") {
			return Ok((s2, Self::Byte(NbtByte { value: 1 })))
		}
		if s.starts_with("[B;") {
			return NbtByteArray::from_str0(s).map(|(s, x)| (s, Self::ByteArray(x)))
		}
		if s.starts_with("[I;") {
			return NbtIntArray::from_str0(s).map(|(s, x)| (s, Self::IntArray(x)))
		}
		if s.starts_with("[L;") {
			return NbtLongArray::from_str0(s).map(|(s, x)| (s, Self::LongArray(x)))
		}
		if s.starts_with('[') {
			return NbtList::from_str0(s).map(|(s, x)| (s, Self::List(x)))
		}
		if s.starts_with('{') {
			return NbtCompound::from_str0(s).map(|(s, x)| (s, Self::Compound(x)))
		}
		if s.starts_with('"') {
			return NbtString::from_str0(s).map(|(s, x)| (s, Self::String(x)))
		}

		if let Some(s2) = s.strip_prefix("NaN") {
			s = s2.trim_start();
			return if let Some(s2) = s.strip_prefix('f') {
				Ok((s2.trim_start(), Self::Float(NbtFloat { value: f32::NAN })))
			} else {
				Ok((s2.strip_prefix('d').unwrap_or(s2).trim_start(), Self::Double(NbtDouble { value: f64::NAN })))
			};
		}

		if let Some(s2) = s
			.strip_prefix("Infinity")
			.or_else(|| s.strip_prefix("inf"))
		{
			s = s2.trim_start();
			return if let Some(s2) = s.strip_prefix('f') {
				Ok((s2.trim_start(), Self::Float(NbtFloat { value: f32::INFINITY })))
			} else {
				Ok((s2.strip_prefix('d').unwrap_or(s2).trim_start(), Self::Double(NbtDouble { value: f64::INFINITY })))
			};
		}

		if let Some(s2) = s
			.strip_prefix("-Infinity")
			.or_else(|| s.strip_prefix("-inf"))
		{
			s = s2.trim_start();
			return if let Some(s2) = s.strip_prefix('f') {
				Ok((s2.trim_start(), Self::Float(NbtFloat { value: f32::NEG_INFINITY })))
			} else if let Some(s2) = s.strip_prefix('d') {
				Ok((s2.trim_start(), Self::Double(NbtDouble { value: f64::NEG_INFINITY })))
			} else {
				Ok((s2.trim_start(), Self::Double(NbtDouble { value: f64::NEG_INFINITY })))
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
				Some(b'b') => Ok((&s[num_end_idx + suffix_len + 1..], Self::parse_byte(&num_str, unsigned, positive, base, s)?)),
				Some(b's') => Ok((&s[num_end_idx + suffix_len + 1..], Self::parse_short(&num_str, unsigned, positive, base, s)?)),
				Some(b'i') => Ok((&s[num_end_idx + suffix_len + 1..], Self::parse_int(&num_str, unsigned, positive, base, s)?)),
				Some(b'l') => Ok((&s[num_end_idx + suffix_len + 1..], Self::parse_long(&num_str, unsigned, positive, base, s)?)),
				Some(b'f') => Ok((
					&s[num_end_idx + suffix_len + 1..],
					Self::Float(NbtFloat {
						value: {
							let value = num_str.parse().map_err(|_| s.len())?;
							if positive { value } else { -value }
						},
					}),
				)),
				Some(b'd') => Ok((
					&s[num_end_idx + suffix_len + 1..],
					Self::Double(NbtDouble {
						value: {
							let value = num_str.parse().map_err(|_| s.len())?;
							if positive { value } else { -value }
						},
					}),
				)),
				Some(b'|') => Ok({
					let mut s = s;
					let Ok(x @ 0..=31) = num_str.parse::<u8>() else {
						return Err(s.len());
					};
					s = s[num_end_idx..]
						.trim_start()
						.split_at(1)
						.1
						.trim_start();
					let num_end_idx = s
						.bytes()
						.position(|x| !x.is_ascii_digit())
						.ok_or(s.len())?;
					let Ok(z @ 0..=31) = s[..num_end_idx].replace('_', "").parse::<u8>() else {
						return Err(s.len());
					};
					s = s[num_end_idx..].trim_start();
					let (s, inner) = NbtCompound::from_str0(s)?;
					(s, Self::Chunk(NbtChunk::from_compound(inner, (x, z), FileFormat::Zlib, now().as_secs() as u32)))
				}),
				_ => Ok((&s[num_end_idx + suffix_len..], parse_ambiguous_integer(&num_str, unsigned, positive, base, s)?)),
			};
		}

		NbtString::from_str0(s).map(|(s, x)| (s, Self::String(x)))
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
		Some(match self.id() {
			Byte(_) => self,
			_ => return None,
		})
	}

	pub(super) fn array_try_into_int(self) -> Option<Self> {
		Some(match self.as_pattern() {
			Byte(&NbtByte { value }) => Self::Int(NbtInt { value: value as i32 }),
			Short(&NbtShort { value }) => Self::Int(NbtInt { value: value as i32 }),
			Int(_) => self,
			_ => return None,
		})
	}

	pub(super) fn array_try_into_long(self) -> Option<Self> {
		Some(match self.as_pattern() {
			Byte(&NbtByte { value }) => Self::Long(NbtLong { value: value as i64 }),
			Short(&NbtShort { value }) => Self::Long(NbtLong { value: value as i64 }),
			Int(&NbtInt { value }) => Self::Long(NbtLong { value: value as i64 }),
			_ => return None,
		})
	}
}

/// From Bytes
impl NbtElement {
	pub fn from_bytes<'a, D: Decoder<'a>>(element: u8, decoder: &mut D) -> NbtParseResult<Self> {
		use super::result::*;

		ok(match element {
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
			_ => return err("Invalid NBT type"),
		})
	}

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
			NbtList::ID => Self::List(NbtList::new(vec![])),
			NbtCompound::ID => Self::Compound(NbtCompound::new()),
			NbtIntArray::ID => Self::IntArray(NbtIntArray::new()),
			NbtLongArray::ID => Self::LongArray(NbtLongArray::new()),
			NbtChunk::ID => Self::Chunk(NbtChunk::from_compound(NbtCompound::new(), (0, 0), FileFormat::Zlib, now().as_secs() as u32)),
			_ => Self::Null(NbtNull),
		}
	}

	#[must_use]
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
		let nbt = Self::Compound(NbtCompound::from_bytes(&mut decoder)?);
		if is_ok(&decoder.assert_len(1)) {
			return err("Format should take all the bytes");
		}
		ok(nbt)
	}

	#[must_use]
	pub fn from_be_mca(bytes: &[u8]) -> NbtParseResult<Self> { NbtRegion::from_be_bytes(bytes).map(Self::Region) }

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
				ok((Self::Compound(NbtCompound::from_bytes(&mut decoder)?), decoder.header()))
			}
			NbtList::ID => {
				decoder.assert_len(2)?;
				let skip = unsafe { decoder.u16() } as usize;
				decoder.skip(skip);
				ok((Self::List(NbtList::from_bytes(&mut decoder)?), decoder.header()))
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
		match self.as_pattern() {
			Byte(byte) => byte.to_be_bytes(writer),
			Short(short) => short.to_be_bytes(writer),
			Int(int) => int.to_be_bytes(writer),
			Long(long) => long.to_be_bytes(writer),
			Float(float) => float.to_be_bytes(writer),
			Double(double) => double.to_be_bytes(writer),
			ByteArray(byte_array) => byte_array.to_be_bytes(writer),
			String(string) => string.to_be_bytes(writer),
			List(list) => list.to_be_bytes(writer),
			Compound(compound) => compound.to_be_bytes(writer),
			IntArray(int_array) => int_array.to_be_bytes(writer),
			LongArray(long_array) => long_array.to_be_bytes(writer),
			Chunk(chunk) => chunk.to_be_bytes(writer),
			Region(region) => region.to_be_bytes(writer),
			Null(null) => null.to_be_bytes(writer),
		}
	}

	pub fn to_le_bytes(&self, writer: &mut UncheckedBufWriter) {
		match self.as_pattern() {
			Byte(byte) => byte.to_le_bytes(writer),
			Short(short) => short.to_le_bytes(writer),
			Int(int) => int.to_le_bytes(writer),
			Long(long) => long.to_le_bytes(writer),
			Float(float) => float.to_le_bytes(writer),
			Double(double) => double.to_le_bytes(writer),
			ByteArray(byte_array) => byte_array.to_le_bytes(writer),
			String(string) => string.to_le_bytes(writer),
			List(list) => list.to_le_bytes(writer),
			Compound(compound) => compound.to_le_bytes(writer),
			IntArray(int_array) => int_array.to_le_bytes(writer),
			LongArray(long_array) => long_array.to_le_bytes(writer),
			Chunk(chunk) => chunk.to_le_bytes(writer),
			Region(_) => { /* no */ }
			Null(null) => null.to_le_bytes(writer),
		}
	}

	#[must_use]
	pub fn to_be_file(&self) -> Vec<u8> {
		let mut writer = UncheckedBufWriter::new();
		if self.id() == NbtCompound::ID {
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
		match self.as_pattern() {
			Byte(byte) => byte.render(builder, str, ctx),
			Short(short) => short.render(builder, str, ctx),
			Int(int) => int.render(builder, str, ctx),
			Long(long) => long.render(builder, str, ctx),
			Float(float) => float.render(builder, str, ctx),
			Double(double) => double.render(builder, str, ctx),
			ByteArray(byte_array) => byte_array.render(builder, str, remaining_scroll, tail, ctx),
			String(string) => string.render(builder, str, ctx),
			List(list) => list.render(builder, str, remaining_scroll, tail, ctx),
			Compound(compound) => compound.render(builder, str, remaining_scroll, tail, ctx),
			IntArray(int_array) => int_array.render(builder, str, remaining_scroll, tail, ctx),
			LongArray(long_array) => long_array.render(builder, str, remaining_scroll, tail, ctx),
			Chunk(chunk) => chunk.render(builder, remaining_scroll, tail, ctx),
			Region(_) => { /* no impl */ }
			Null(null) => null.render(builder, str, ctx),
		}
	}

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
			_ => "null",
		}
	}

	pub fn render_icon(&self, pos: impl Into<(usize, usize)>, z: ZOffset, builder: &mut VertexBufferBuilder) {
		match self.as_pattern() {
			Byte(byte) => byte.render_icon(pos, z, builder),
			Short(short) => short.render_icon(pos, z, builder),
			Int(int) => int.render_icon(pos, z, builder),
			Long(long) => long.render_icon(pos, z, builder),
			Float(float) => float.render_icon(pos, z, builder),
			Double(double) => double.render_icon(pos, z, builder),
			ByteArray(byte_array) => byte_array.render_icon(pos, z, builder),
			String(string) => string.render_icon(pos, z, builder),
			List(list) => list.render_icon(pos, z, builder),
			Compound(compound) => compound.render_icon(pos, z, builder),
			IntArray(int_array) => int_array.render_icon(pos, z, builder),
			LongArray(long_array) => long_array.render_icon(pos, z, builder),
			Chunk(chunk) => chunk.render_icon(pos, z, builder),
			Region(region) => region.render_icon(pos, z, builder),
			Null(null) => null.render_icon(pos, z, builder),
		}
	}

	#[must_use]
	pub fn should_render_description(&self) -> bool {
		match self.as_pattern() {
			Chunk(chunk) => chunk.is_loaded() && !(chunk.x == 0 && chunk.z == 0 && chunk.is_empty()),
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

	#[must_use]
	pub fn traverse(&self, y: usize, x: Option<usize>) -> Option<TraversalInformation> { TraversalInformation::from(self, y, x) }

	#[must_use]
	pub fn traverse_mut(&mut self, y: usize, x: Option<usize>) -> Option<TraversalInformationMut> { TraversalInformationMut::from(self, y, x) }
}

/// Mutable Indices-based operations
impl NbtElement {
	pub fn expand_to_indices(&mut self, indices: &Indices) {
		fn inner(mut element: &mut NbtElement, indices: &Indices) {
			for idx in indices {
				if element.is_complex() && !element.is_open() {
					element.toggle();
				}
				element = &mut element[idx];
			}
		}

		inner(self, indices);

		self.recache_along_indices(indices);
	}

	pub fn expand_through_indices(&mut self, indices: &Indices) {
		if self.is_complex() && !self.is_open() {
			self.toggle();
		}
		self.expand_to_indices(indices);
	}

	pub fn recache(&mut self) {
		use NbtPatternMut::*;

		match self.as_pattern_mut() {
			ByteArray(byte_array) => byte_array.recache(),
			List(list) => list.recache(),
			Compound(compound) => compound.recache(),
			IntArray(int_array) => int_array.recache(),
			LongArray(long_array) => long_array.recache(),
			Chunk(chunk) => chunk.recache(),
			Region(region) => region.recache(),
			_ => (),
		}
	}

	pub fn recache_along_indices(&mut self, indices: &Indices) {
		let mut this = self;
		this.recache();
		for idx in indices {
			this = &mut this[idx];
			this.recache();
		}
	}
}

/// Immutable "getter" operations
impl NbtElement {
	#[must_use]
	pub const fn id(&self) -> u8 { unsafe { self.id.id } }

	#[must_use]
	pub fn len(&self) -> Option<usize> {
		Some(match self.as_pattern() {
			ByteArray(x) => x.len(),
			List(x) => x.len(),
			Compound(x) => x.len(),
			IntArray(x) => x.len(),
			LongArray(x) => x.len(),
			Chunk(x) => x.len(),
			Region(x) => x.len(),
			_ => return None,
		})
	}

	#[must_use]
	pub fn is_empty(&self) -> bool { self.len().is_some_and(|x| x == 0) }

	#[must_use]
	pub fn height(&self) -> usize {
		match self.as_pattern() {
			ByteArray(x) => x.height(),
			List(x) => x.height(),
			Compound(x) => x.height(),
			IntArray(x) => x.height(),
			LongArray(x) => x.height(),
			Chunk(x) => x.height(),
			Region(x) => x.height(),
			_ => 1,
		}
	}

	#[must_use]
	pub fn true_height(&self) -> usize {
		match self.as_pattern() {
			ByteArray(x) => x.true_height(),
			List(x) => x.true_height(),
			Compound(x) => x.true_height(),
			IntArray(x) => x.true_height(),
			LongArray(x) => x.true_height(),
			Chunk(x) => x.true_height(),
			Region(x) => x.true_height(),
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
	pub fn children(&self) -> Option<Result<Iter<'_, NbtElement>, CompoundMapIter<'_>>> {
		Some(match self.as_pattern() {
			ByteArray(x) => Ok(x.children()),
			IntArray(x) => Ok(x.children()),
			LongArray(x) => Ok(x.children()),
			List(x) => Ok(x.children()),
			Compound(x) => Err(x.children()),
			Region(x) => Ok(x.children()),
			Chunk(x) => Err(x.children()),
			_ => return None,
		})
	}

	#[must_use]
	pub fn values(&self) -> Option<NbtElementValues<'_>> {
		Some(match self.as_pattern() {
			ByteArray(x) => NbtElementValues::Iter(x.children()),
			IntArray(x) => NbtElementValues::Iter(x.children()),
			LongArray(x) => NbtElementValues::Iter(x.children()),
			List(x) => NbtElementValues::Iter(x.children()),
			Compound(x) => NbtElementValues::CompoundMapIter(x.children()),
			Region(x) => NbtElementValues::Iter(x.children()),
			Chunk(x) => NbtElementValues::CompoundMapIter(x.children()),
			_ => return None,
		})
	}

	#[must_use]
	pub fn get_kv_under_indices(&self, indices: &Indices) -> Option<NbtElementAndKeyRef> {
		let mut key = None;
		let mut value = self;

		for idx in indices {
			match value.get_kv(idx) {
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
	pub fn get_kv(&self, idx: usize) -> Option<NbtElementAndKeyRef> {
		match self.as_pattern() {
			ByteArray(x) => x.get(idx).map(|x| (None, x)),
			IntArray(x) => x.get(idx).map(|x| (None, x)),
			LongArray(x) => x.get(idx).map(|x| (None, x)),
			List(x) => x.get(idx).map(|x| (None, x)),
			Compound(x) => x.get_kv(idx).map(|(a, b)| (Some(a), b)),
			Region(x) => x.get(idx).map(|x| (None, x)),
			Chunk(x) => x.get_kv(idx).map(|(a, b)| (Some(a), b)),
			_ => {
				std::hint::cold_path();
				None
			}
		}
	}

	#[must_use]
	pub unsafe fn get_unchecked(&self, idx: usize) -> &NbtElement {
		match self.as_pattern() {
			ByteArray(x) => unsafe { x.get_unchecked(idx) },
			IntArray(x) => unsafe { x.get_unchecked(idx) },
			LongArray(x) => unsafe { x.get_unchecked(idx) },
			List(x) => unsafe { x.get_unchecked(idx) },
			Compound(x) => unsafe { x.get_unchecked(idx) },
			Region(x) => unsafe { x.get_unchecked(idx) },
			Chunk(x) => unsafe { x.get_unchecked(idx) },
			_ => unsafe { std::hint::unreachable_unchecked() },
		}
	}

	#[must_use]
	pub fn is_open(&self) -> bool {
		match self.as_pattern() {
			ByteArray(x) => x.open(),
			IntArray(x) => x.open(),
			LongArray(x) => x.open(),
			List(x) => x.open(),
			Compound(x) => x.is_open(),
			Region(x) => x.is_open(),
			Chunk(x) => x.is_open(),
			_ => false,
		}
	}

	#[must_use]
	pub fn is_primitive(&self) -> bool { matches!(self.id(), Byte(_) | Short(_) | Int(_) | Long(_) | Float(_) | Double(_) | String(_)) }

	#[must_use]
	pub fn is_complex(&self) -> bool { matches!(self.as_pattern(), ByteArray(_) | List(_) | Compound(_) | IntArray(_) | LongArray(_) | Chunk(_) | Region(_)) }

	#[must_use]
	pub fn is_default_state(&self) -> bool {
		match self.as_pattern() {
			Byte(x) => x.value == 0,
			Short(x) => x.value == 0,
			Int(x) => x.value == 0,
			Long(x) => x.value == 0,
			Float(x) => x.value == 0.0,
			Double(x) => x.value == 0.0,
			ByteArray(x) => x.is_empty(),
			String(x) => x.str.as_str().is_empty(),
			List(x) => x.is_empty(),
			Compound(x) => x.is_empty(),
			IntArray(x) => x.is_empty(),
			LongArray(x) => x.is_empty(),
			Chunk(x) => x.is_unloaded(),
			Region(x) => x.loaded_chunks() == 0,
			_ => true,
		}
	}

	#[must_use]
	pub fn value(&self) -> (CompactString, TextColor) {
		match self.as_pattern() {
			Byte(x) => (x.value(), TextColor::TreePrimitive),
			Short(x) => (x.value(), TextColor::TreePrimitive),
			Int(x) => (x.value(), TextColor::TreePrimitive),
			Long(x) => (x.value(), TextColor::TreePrimitive),
			Float(x) => (x.value(), TextColor::TreePrimitive),
			Double(x) => (x.value(), TextColor::TreePrimitive),
			ByteArray(x) => (x.value(), TextColor::TreeKey),
			String(x) => (x.str.as_str().to_compact_string(), TextColor::TreeString),
			List(x) => (x.value(), TextColor::TreeKey),
			Compound(x) => (x.value(), TextColor::TreeKey),
			IntArray(x) => (x.value(), TextColor::TreeKey),
			LongArray(x) => (x.value(), TextColor::TreeKey),
			Chunk(x) => (x.value(), TextColor::TreeKey),
			Region(x) => (x.value(), TextColor::TreeKey),
			Null(_) => (CompactString::const_new("null"), TextColor::TreeKey),
		}
	}

	#[must_use]
	pub fn value_width(&self) -> usize {
		match self.as_pattern() {
			Byte(x) => util::i8_width(x.value),
			Short(x) => util::i16_width(x.value),
			Int(x) => util::i32_width(x.value),
			Long(x) => util::i64_width(x.value),
			Float(x) => util::f32_width(x.value),
			Double(x) => util::f64_width(x.value),
			ByteArray(x) => util::usize_width(x.len()) + const { width_ascii(" ") } + id_to_string_name_width(NbtByte::ID, x.len()),
			String(x) => x.str.width(),
			List(x) => util::usize_width(x.len()) + const { width_ascii(" ") } + id_to_string_name_width(x.id(), x.len()),
			Compound(x) => util::usize_width(x.len()) + const { width_ascii(" ") } + id_to_string_name_width(NbtNull::ID, x.len()),
			IntArray(x) => util::usize_width(x.len()) + const { width_ascii(" ") } + id_to_string_name_width(NbtInt::ID, x.len()),
			LongArray(x) => util::usize_width(x.len()) + const { width_ascii(" ") } + id_to_string_name_width(NbtLong::ID, x.len()),
			Chunk(x) => util::u8_width(x.x) + const { width_ascii(", ") } + util::u8_width(x.z),
			Region(x) => util::usize_width(x.loaded_chunks()) + id_to_string_name_width(NbtChunk::ID, x.loaded_chunks()),
			_ => 0,
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
			} else {
				*y -= 16;
			}

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
		match self.as_pattern() {
			ByteArray(x) => x.max_depth(),
			IntArray(x) => x.max_depth(),
			LongArray(x) => x.max_depth(),
			List(x) => x.max_depth(),
			Compound(x) => x.max_depth(),
			Region(x) => x.max_depth(),
			Chunk(x) => x.max_depth(),
			_ => 0,
		}
	}

	#[must_use]
	pub fn actions(&self) -> &[ElementAction] {
		match self.as_pattern() {
			Byte(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			Short(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			Int(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			Long(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			Float(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			Double(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			ByteArray(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenArrayInHex,
				ElementAction::InvertBookmarks,
				ElementAction::InsertFromClipboard,
			],
			String(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			#[cfg(not(target_arch = "wasm32"))]
			List(x) => {
				const FULL: [ElementAction; 6] = [
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					ElementAction::OpenInTxt,
					ElementAction::InsertFromClipboard,
					ElementAction::InvertBookmarks,
					ElementAction::OpenArrayInHex,
				];
				let id = x.id();
				if matches!(id, NbtByte::ID | NbtShort::ID | NbtInt::ID | NbtLong::ID) { &FULL } else { &FULL[..FULL.len() - 1] }
			}
			#[cfg(target_arch = "wasm32")]
			List(_) => &[ElementAction::CopyRaw, ElementAction::CopyFormatted, ElementAction::InsertFromClipboard, ElementAction::InvertBookmarks],
			Compound(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::SortCompoundByName,
				ElementAction::SortCompoundByType,
				ElementAction::InsertFromClipboard,
				ElementAction::InvertBookmarks,
			],
			IntArray(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenArrayInHex,
				ElementAction::InsertFromClipboard,
				ElementAction::InvertBookmarks,
			],
			LongArray(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenArrayInHex,
				ElementAction::InsertFromClipboard,
				ElementAction::InvertBookmarks,
			],
			Chunk(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::SortCompoundByName,
				ElementAction::SortCompoundByType,
				ElementAction::InsertFromClipboard,
				ElementAction::InvertBookmarks,
			],
			Region(_) => &[
				ElementAction::CopyRaw,
				ElementAction::CopyFormatted,
				#[cfg(not(target_arch = "wasm32"))]
				ElementAction::OpenInTxt,
				ElementAction::InvertBookmarks,
			],
			Null(_) => &[],
		}
	}

	#[must_use]
	pub fn can_insert(&self, value: &NbtElement) -> bool {
		match self.as_pattern() {
			ByteArray(x) => x.can_insert(value),
			List(x) => x.can_insert(value),
			Compound(x) => x.can_insert(value),
			IntArray(x) => x.can_insert(value),
			LongArray(x) => x.can_insert(value),
			Chunk(x) => x.can_insert(value),
			Region(x) => x.can_insert(value),
			_ => false,
		}
	}
}

/// Operations
impl NbtElement {
	pub fn on_root_style_change(&mut self, bookmarks: &mut MarkedLines) {
		use NbtPatternMut::*;

		match self.as_pattern_mut() {
			Region(region) => region.on_root_style_change(bookmarks),
			_ => {}
		}
	}

	pub fn toggle(&mut self) -> bool {
		use NbtPatternMut::*;

		match self.as_pattern_mut() {
			ByteArray(byte_array) => byte_array.toggle(),
			IntArray(int_array) => int_array.toggle(),
			LongArray(long_array) => long_array.toggle(),
			List(list) => list.toggle(),
			Compound(compound) => compound.toggle(),
			Region(region) => region.toggle(),
			Chunk(chunk) => chunk.toggle(),
			_ => return false,
		}
		true
	}

	// todo: add wasm32 and non-wasm32 editions for scope expands on region-files
	pub fn shut<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		use NbtPatternMut::*;

		match self.as_pattern_mut() {
			ByteArray(byte_array) => byte_array.shut(),
			IntArray(int_array) => int_array.shut(),
			LongArray(long_array) => long_array.shut(),
			List(list) => list.shut(scope),
			Compound(compound) => compound.shut(scope),
			Region(region) => region.shut(scope),
			Chunk(chunk) => chunk.shut(scope),
			_ => (),
		}
	}

	pub fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		use NbtPatternMut::*;

		match self.as_pattern_mut() {
			ByteArray(byte_array) => byte_array.expand(),
			IntArray(int_array) => int_array.expand(),
			LongArray(long_array) => long_array.expand(),
			List(list) => list.expand(scope),
			Compound(compound) => compound.expand(scope),
			Region(region) => region.expand(scope),
			Chunk(chunk) => chunk.expand(scope),
			_ => (),
		}
	}

	/// # Safety
	/// - must be assured to update valid indices
	pub unsafe fn insert(&mut self, idx: usize, value: NbtElementAndKey) -> Result<Option<Self>, Self> {
		use NbtPatternMut::*;

		match self.as_pattern_mut() {
			ByteArray(byte_array) => byte_array.insert(idx, value.1),
			List(list) => list.insert(idx, value.1),
			Compound(compound) => {
				compound.insert(idx, value.0.unwrap_or(CompactString::const_new("_")), value.1);
				Ok(None)
			}
			IntArray(int_array) => int_array.insert(idx, value.1),
			LongArray(long_array) => long_array.insert(idx, value.1),
			Region(region) => region.insert(idx, value.1),
			Chunk(chunk) => {
				chunk.insert(idx, value.0.unwrap_or(CompactString::const_new("_")), value.1);
				Ok(None)
			}
			_ => Err(value.1),
		}
	}

	/// # Safety
	/// - must be assured to update valid indices
	pub unsafe fn replace_key_value(&mut self, idx: usize, kv: NbtElementAndKey) -> Option<NbtElementAndKey> {
		use NbtPatternMut::*;

		match self.as_pattern_mut() {
			ByteArray(byte_array) => Some((None, byte_array.replace(idx, kv.1)?)),
			IntArray(int_array) => Some((None, int_array.replace(idx, kv.1)?)),
			LongArray(long_array) => Some((None, long_array.replace(idx, kv.1)?)),
			List(list) => Some((None, list.replace(idx, kv.1)?)),
			Compound(compound) => compound.replace(idx, kv.0?, kv.1),
			Chunk(chunk) => chunk.replace(idx, kv.0?, kv.1),
			Region(region) => Some((None, region.replace(idx, kv.1).ok()??)),
			_ => None,
		}
	}

	/// # Safety
	/// - must be assured to update valid indices
	pub unsafe fn remove(&mut self, idx: usize) -> Option<(Option<CompactString>, Self)> {
		use NbtPatternMut::*;

		Some(match self.as_pattern_mut() {
			ByteArray(byte_array) => (None, byte_array.remove(idx)),
			IntArray(int_array) => (None, int_array.remove(idx)),
			LongArray(long_array) => (None, long_array.remove(idx)),
			List(list) => (None, list.remove(idx)),
			Compound(compound) => return compound.remove(idx).map(|(a, b)| (Some(a), b)),
			Region(region) => (None, region.replace_with_empty(idx)),
			Chunk(chunk) => return chunk.remove(idx).map(|(a, b)| (Some(a), b)),
			_ => return None,
		})
	}

	pub unsafe fn swap(&mut self, a: usize, b: usize) {
		use NbtPatternMut::*;

		match self.as_pattern_mut() {
			ByteArray(byte_array) => byte_array.values.swap(a, b),
			IntArray(int_array) => int_array.values.swap(a, b),
			LongArray(long_array) => long_array.values.swap(a, b),
			List(list) => list.elements.swap(a, b),
			Compound(compound) => compound.entries.swap(a, b),
			Region(region) => region.swap(a, b),
			Chunk(chunk) => chunk.entries.swap(a, b),
			_ => {}
		}
	}
}

/// Mutable "getter" / "setter" operations
impl NbtElement {
	#[must_use]
	#[allow(clippy::type_complexity)] // a type probably shouldn't abstract what this is, like... yeah
	pub fn children_mut(&mut self) -> Option<Result<IterMut<'_, NbtElement>, CompoundMapIterMut<'_>>> {
		use NbtPatternMut::*;

		Some(match self.as_pattern_mut() {
			ByteArray(byte_array) => Ok(byte_array.children_mut()),
			IntArray(int_array) => Ok(int_array.children_mut()),
			LongArray(long_array) => Ok(long_array.children_mut()),
			List(list) => Ok(list.children_mut()),
			Compound(compound) => Err(compound.children_mut()),
			Chunk(chunk) => Err(chunk.children_mut()),
			Region(region) => Ok(region.children_mut()),
			_ => return None,
		})
	}

	pub fn set_value(&mut self, value: CompactString) -> Option<(CompactString, bool)> {
		use NbtPatternMut::*;

		Some(match self.as_pattern_mut() {
			Byte(byte) => {
				let before = byte.value();
				let success = value.parse().map(|x| byte.value = x).is_ok();
				(before, success)
			}
			Short(short) => {
				let before = short.value();
				let success = value.parse().map(|x| short.value = x).is_ok();
				(before, success)
			}
			Int(int) => {
				let before = int.value();
				let success = value.parse().map(|x| int.value = x).is_ok();
				(before, success)
			}
			Long(long) => {
				let before = long.value();
				let success = value.parse().map(|x| long.value = x).is_ok();
				(before, success)
			}
			Float(float) => {
				let before = float.value();
				let success = value.parse().map(|x| float.value = x).is_ok();
				(before, success)
			}
			Double(double) => {
				let before = double.value();
				let success = value.parse().map(|x| double.value = x).is_ok();
				(before, success)
			}
			String(string) => (
				core::mem::replace(string, NbtString::new(value))
					.str
					.as_str()
					.to_compact_string(),
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
		use NbtPatternMut::*;

		Some(match self.as_pattern_mut() {
			Compound(compound) => compound.entries.update_key(idx, key),
			Chunk(chunk) => chunk.entries.update_key(idx, key),
			_ => return None,
		})
	}

	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<(Option<&str>, &mut Self)> {
		use NbtPatternMut::*;

		match self.as_pattern_mut() {
			ByteArray(byte_array) => byte_array.get_mut(idx).map(|x| (None, x)),
			List(list) => list.get_mut(idx).map(|x| (None, x)),
			Compound(compound) => compound
				.get_kv_mut(idx)
				.map(|(a, b)| (Some(a), b)),
			IntArray(int_array) => int_array.get_mut(idx).map(|x| (None, x)),
			LongArray(long_array) => long_array.get_mut(idx).map(|x| (None, x)),
			Chunk(chunk) => chunk.get_kv_mut(idx).map(|(a, b)| (Some(a), b)),
			Region(region) => region.get_mut(idx).map(|x| (None, x)),
			_ => {
				std::hint::cold_path();
				None
			}
		}
	}

	#[must_use]
	pub unsafe fn get_unchecked_mut(&mut self, idx: usize) -> &mut NbtElement {
		use NbtPatternMut::*;

		match self.as_pattern_mut() {
			ByteArray(byte_array) => byte_array.get_unchecked_mut(idx),
			List(list) => list.get_unchecked_mut(idx),
			Compound(compound) => compound.get_unchecked_mut(idx),
			IntArray(int_array) => int_array.get_unchecked_mut(idx),
			LongArray(long_array) => long_array.get_unchecked_mut(idx),
			Chunk(chunk) => chunk.get_unchecked_mut(idx),
			Region(region) => region.get_unchecked_mut(idx),
			_ => unsafe { core::hint::unreachable_unchecked() },
		}
	}

	pub fn try_compound_singleton_into_inner(mut self) -> Result<Self, Self> {
		if let Some(compound) = self.as_compound_mut()
			&& compound.len() == 1
			&& compound
				.get_kv(0)
				.is_some_and(|(key, _)| key.is_empty())
			&& let Some((_, inner)) = compound.remove(0)
		{
			Ok(inner)
		} else {
			Err(self)
		}
	}
}

impl Display for NbtElement {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self.as_pattern() {
			Byte(byte) => write!(f, "{byte}"),
			Short(short) => write!(f, "{short}"),
			Int(int) => write!(f, "{int}"),
			Long(long) => write!(f, "{long}"),
			Float(float) => write!(f, "{float}"),
			Double(double) => write!(f, "{double}"),
			ByteArray(byte_array) => write!(f, "{byte_array}"),
			String(string) => write!(f, "{string}"),
			List(list) => write!(f, "{list}"),
			Compound(compound) => write!(f, "{compound}"),
			IntArray(int_array) => write!(f, "{int_array}"),
			LongArray(long_array) => write!(f, "{long_array}"),
			Chunk(chunk) => write!(f, "{chunk}"),
			Region(_) => Err(Error),
			Null(null) => write!(f, "{null}"),
		}
	}
}

/// Pretty Formatter
impl NbtElement {
	pub fn pretty_fmt(&self, f: &mut PrettyFormatter) {
		match self.as_pattern() {
			Byte(byte) => byte.pretty_fmt(f),
			Short(short) => short.pretty_fmt(f),
			Int(int) => int.pretty_fmt(f),
			Long(long) => long.pretty_fmt(f),
			Float(float) => float.pretty_fmt(f),
			Double(double) => double.pretty_fmt(f),
			ByteArray(byte_array) => byte_array.pretty_fmt(f),
			String(string) => string.pretty_fmt(f),
			List(list) => list.pretty_fmt(f),
			Compound(compound) => compound.pretty_fmt(f),
			IntArray(int_array) => int_array.pretty_fmt(f),
			LongArray(long_array) => long_array.pretty_fmt(f),
			Chunk(chunk) => chunk.pretty_fmt(f),
			Region(region) => region.pretty_fmt(f),
			Null(null) => null.pretty_fmt(f),
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
		unsafe {
			let id = self.id();
			match id {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => {
					let vec = &mut *self.byte_array.values;
					if !vec.is_empty() {
						dealloc(vec.as_mut_ptr().cast(), Layout::array::<Self>(vec.capacity()).unwrap_unchecked());
					}
					dealloc((vec as *mut Vec<Self>).cast(), Layout::new::<Vec<Self>>());
				}
				NbtString::ID => {
					core::ptr::addr_of_mut!(self.string.str).drop_in_place();
				}
				NbtList::ID => {
					let list = &mut *self.list.elements;
					for entry in &mut *list {
						if entry.id() > NbtDouble::ID {
							(entry as *mut Self).drop_in_place();
						}
					}
					if !list.is_empty() {
						dealloc(list.as_mut_ptr().cast(), Layout::array::<Self>(list.capacity()).unwrap_unchecked());
					}
					dealloc((list as *mut Vec<Self>).cast(), Layout::new::<Vec<Self>>());
				}
				NbtCompound::ID => {
					let map = &mut *self.compound.entries;
					let CompoundMap { indices, entries } = map;
					(indices as *mut HashTable<usize>).drop_in_place();
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
					let map = &mut *self.chunk.entries;
					let CompoundMap { indices, entries } = map;
					(indices as *mut HashTable<usize>).drop_in_place();
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
					let chunks = *core::ptr::addr_of_mut!(self.region.chunks).read();
					for mut chunk in core::mem::transmute::<_, [ManuallyDrop<Self>; 1024]>(chunks) {
						let ptr = &mut **chunk.as_chunk_unchecked_mut();
						let map = &mut *ptr.entries;
						let CompoundMap { indices, entries } = map;
						(indices as *mut HashTable<usize>).drop_in_place();
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
				NbtByte::ID | NbtShort::ID | NbtInt::ID | NbtLong::ID | NbtFloat::ID | NbtDouble::ID | NbtNull::ID => {}
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}
}

impl<'a> Index<&'a str> for NbtElement {
	type Output = NbtElement;

	fn index(&self, index: &'a str) -> &Self::Output {
		let map = match self.as_pattern() {
			Compound(compound) => compound.entries.as_ref(),
			Chunk(chunk) => chunk.entries.as_ref(),
			_ => {
				std::hint::cold_path();
				return Self::NULL_REF
			}
		};

		if let Some(idx) = map.idx_of(index)
			&& let Some((_, value)) = map.get_kv_idx(idx)
		{
			value
		} else {
			std::hint::cold_path();
			Self::NULL_REF
		}
	}
}

impl<'a> IndexMut<&'a str> for NbtElement {
	fn index_mut(&mut self, index: &str) -> &mut Self::Output {
		pub static mut NULL_MUT: NbtElement = NbtElement::NULL;

		let result = 'a: {
			let map = match self.as_pattern_mut() {
				NbtPatternMut::Compound(compound) => &mut *compound.entries,
				NbtPatternMut::Chunk(chunk) => &mut *chunk.entries,
				_ => {
					std::hint::cold_path();
					break 'a None
				}
			};

			if let Some(idx) = map.idx_of(index)
				&& let Some((_, value)) = map.get_kv_idx_mut(idx)
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
		match self.get_kv(idx) {
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
			NbtNull::ID => (width_ascii("entry"), width_ascii("entries")),
			_ => (0, 0),
		}
	}

	let (single, other) = id_to_string_name_width0(id);
	if len == 1 { single } else { other }
}

pub enum NbtElementValues<'a> {
	Iter(Iter<'a, NbtElement>),
	CompoundMapIter(CompoundMapIter<'a>),
}

impl<'a> Iterator for NbtElementValues<'a> {
	type Item = &'a NbtElement;

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			Self::Iter(iter) => iter.next(),
			Self::CompoundMapIter(iter) => iter.next().map(|(_, b)| b),
		}
	}
}

impl<'a> DoubleEndedIterator for NbtElementValues<'a> {
	fn next_back(&mut self) -> Option<Self::Item> {
		match self {
			Self::Iter(iter) => iter.next_back(),
			Self::CompoundMapIter(iter) => iter.next_back().map(|(_, b)| b),
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

/// Patterns
impl NbtElement {
	// if this inline isn't here, the match isn't inlined and the code isn't nearly as performant
	#[allow(unused_qualifications)]
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
			NbtNull::ID => NbtPattern::Null(unsafe { self.as_null_unchecked() }),
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
			NbtNull::ID => NbtPatternMut::Null(unsafe { self.as_null_unchecked_mut() }),
			_ => unsafe { core::hint::unreachable_unchecked() },
		}
	}
}

macro_rules! type_conversions {
	($t:ty, $field:ident, $is:ident, $into_unchecked:ident, $as_unchecked:ident, $as_unchecked_mut:ident, $into:ident, $as_ref:ident, $as_mut:ident) => {
		#[allow(dead_code)]
		impl NbtElement {
			#[must_use]
			pub unsafe fn $into_unchecked(self) -> $t {
				let result = core::ptr::read(core::ptr::addr_of!(*self.$field));
				core::mem::forget(self);
				result
			}

			#[must_use]
			pub unsafe fn $as_unchecked(&self) -> &$t { &self.$field }

			#[must_use]
			pub unsafe fn $as_unchecked_mut(&mut self) -> &mut $t { &mut self.$field }

			#[must_use]
			pub fn $into(self) -> Option<$t> { unsafe { if self.$is() { Some(self.$into_unchecked()) } else { None } } }

			#[must_use]
			pub fn $as_ref(&self) -> Option<&$t> { unsafe { if self.$is() { Some(self.$as_unchecked()) } else { None } } }

			#[must_use]
			pub fn $as_mut(&mut self) -> Option<&mut $t> { unsafe { if self.$is() { Some(self.$as_unchecked_mut()) } else { None } } }

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
type_conversions! { NbtNull, null, is_null, into_null_unchecked, as_null_unchecked, as_null_unchecked_mut, into_null, as_null, as_null_mut }

/// Nonnull
impl NbtElement {
	#[must_use]
	pub fn as_nonnull(&self) -> Option<&Self> { if self.is_null() { None } else { Some(self) } }

	#[must_use]
	pub fn as_nonnull_mut(&mut self) -> Option<&mut Self> { if self.is_null() { None } else { Some(self) } }

	#[must_use]
	pub fn into_nonnull(self) -> Option<Self> { if self.is_null() { None } else { Some(self) } }
}
