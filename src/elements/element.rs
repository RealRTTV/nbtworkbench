pub(in crate::elements)
use std::alloc::{dealloc, Layout};
use std::fmt::{Debug, Display, Error, Formatter};
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::{Index, IndexMut};
use std::slice::{Iter, IterMut};
#[cfg(not(target_arch = "wasm32"))]
use std::thread::Scope;

use compact_str::{CompactString, ToCompactString};
use hashbrown::HashTable;
use polonius_the_crab::{polonius, polonius_return};

use crate::assets::ZOffset;
use crate::elements::{CompoundMap, CompoundMapIter, CompoundMapIterMut, Entry, NbtByte, NbtByteArray, NbtChunk, NbtCompound, NbtDouble, NbtElementAndKey, NbtFloat, NbtInt, NbtIntArray, NbtList, NbtLong, NbtLongArray, NbtNull, NbtRegion, NbtShort, NbtString};
use crate::render::{RenderContext, TextColor, VertexBufferBuilder};
use crate::serialization::{BigEndianDecoder, Decoder, LittleEndianDecoder, PrettyFormatter, UncheckedBufWriter};
use crate::tree;
use crate::tree::{Indices, NavigationInformation, NavigationInformationMut, OwnedIndices, ParentNavigationInformation, ParentNavigationInformationMut, TraversalInformation, TraversalInformationMut};
use crate::util::{now, width_ascii, StrExt};
use crate::workbench::{DropResult, ElementAction, FileFormat, MarkedLines};

#[repr(C)]
#[derive(Copy, Clone)]
pub struct NbtElementId {
	_pad: [MaybeUninit<u8>; 23],
	id: u8,
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

impl PartialEq for NbtElement {
	fn eq(&self, other: &Self) -> bool {
		if self.id() != other.id() { return false }

		unsafe {
			match self.id() {
				NbtChunk::ID => self.chunk.eq(&other.chunk),
				NbtRegion::ID => self.region.eq(&other.region),
				NbtByte::ID => self.byte.eq(&other.byte),
				NbtShort::ID => self.short.eq(&other.short),
				NbtInt::ID => self.int.eq(&other.int),
				NbtLong::ID => self.long.eq(&other.long),
				NbtFloat::ID => self.float.eq(&other.float),
				NbtDouble::ID => self.double.eq(&other.double),
				NbtByteArray::ID => self.byte_array.eq(&other.byte_array),
				NbtString::ID => self.string.eq(&other.string),
				NbtList::ID => self.list.eq(&other.list),
				NbtCompound::ID => self.compound.eq(&other.compound),
				NbtIntArray::ID => self.int_array.eq(&other.int_array),
				NbtLongArray::ID => self.long_array.eq(&other.long_array),
				NbtNull::ID => self.null.eq(&other.null),
				_ => core::hint::unreachable_unchecked(),
			}
		}
	}
}

impl Clone for NbtElement {
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
		let (s, element) = Self::from_str0(s, Self::parse_int).map(|(s, x)| (s.trim_start(), x)).map_err(|x| total_len - x)?;
		if !s.is_empty() { return Err(total_len - s.len()) }
		Ok((prefix, element))
	}

	#[allow(clippy::too_many_lines)]
	pub(in crate::elements) fn from_str0<F: FnOnce(&str, bool, bool, u32, &str) -> Result<Self, usize>>(mut s: &str, parse_ambiguous_integer: F) -> Result<(&str, Self), usize> {
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
				let hex_part = d.bytes().take_while(|&b| b.is_ascii_hexdigit() || b == b'_').count();
				num_end_idx += hex_part;
				let unsigned = !d.starts_with('s');
				if d.starts_with('u') || d.starts_with('s') {
					suffix_len += 1;
				}
				(num_end_idx, suffix_len, unsigned, 16, positive)
			} else if let Some(d2) = d.strip_prefix("0b") {
				s = d2;
				d = s;
				let binary_part = d.bytes().take_while(|&b| b == b'0' || b == b'1' || b == b'_').count();
				num_end_idx += binary_part;
				let unsigned = !d.starts_with('s');
				if d.starts_with('u') || d.starts_with('s') {
					suffix_len += 1;
				}
				(num_end_idx, suffix_len, unsigned, 2, positive)
			} else {
				let int_part = d.bytes().take_while(|&b| b.is_ascii_digit() || b == b'_').count();
				d = &d[int_part..];
				num_end_idx += int_part;
				if int_part == 0 && !d.starts_with('.') {
					break 'a (num_end_idx, suffix_len, false, 10, true);
				}
				if let Some(d2) = d.strip_prefix('.') {
					// floats
					num_end_idx += 1;
					d = d2;
					let frac_part = d.bytes().take_while(|&b| b.is_ascii_digit() || b == b'_').count();
					num_end_idx += frac_part;
					if let Some(s2) = d.strip_prefix('e').or(d.strip_prefix('E')) {
						num_end_idx += 1;
						d = s2;
						if let Some(s2) = d.strip_prefix('+').or(d.strip_prefix('-')) {
							num_end_idx += 1;
							d = s2;
						}
						let exponent_part = d.bytes().take_while(|&b| b.is_ascii_digit() || b == b'_').count();
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
				Some(b'b') => Ok((
					&s[num_end_idx + suffix_len + 1..],
					Self::parse_byte(&num_str, unsigned, positive, base, s)?,
				)),
				Some(b's') => Ok((
					&s[num_end_idx + suffix_len + 1..],
					Self::parse_short(&num_str, unsigned, positive, base, s)?,
				)),
				Some(b'i') => Ok((
					&s[num_end_idx + suffix_len + 1..],
					Self::parse_int(&num_str, unsigned, positive, base, s)?,
				)),
				Some(b'l') => Ok((
					&s[num_end_idx + suffix_len + 1..],
					Self::parse_long(&num_str, unsigned, positive, base, s)?,
				)),
				Some(b'f') => Ok((
					&s[num_end_idx + suffix_len + 1..],
					Self::Float(NbtFloat {
						value: {
							let value = num_str.parse().map_err(|_| s.len())?;
							if positive { value } else { -value }
						}
					}),
				)),
				Some(b'd') => Ok((
					&s[num_end_idx + suffix_len + 1..],
					Self::Double(NbtDouble {
						value: {
							let value = num_str.parse().map_err(|_| s.len())?;
							if positive { value } else { -value }
						}
					}),
				)),
				Some(b'|') => Ok({
					let mut s = s;
					let Ok(x @ 0..=31) = num_str.parse::<u8>() else {
						return Err(s.len());
					};
					s = s[num_end_idx..].trim_start().split_at(1).1.trim_start();
					let num_end_idx = s.bytes().position(|x| !x.is_ascii_digit()).ok_or(s.len())?;
					let Ok(z @ 0..=31) = s[..num_end_idx].replace('_', "").parse::<u8>() else {
						return Err(s.len());
					};
					s = s[num_end_idx..].trim_start();
					let (s, inner) = NbtCompound::from_str0(s)?;
					(
						s,
						Self::Chunk(NbtChunk::from_compound(
							inner,
							(x, z),
							FileFormat::Zlib,
							now().as_secs() as u32,
						)),
					)
				}),
				_ => Ok((
					&s[num_end_idx + suffix_len..],
					parse_ambiguous_integer(&num_str, unsigned, positive, base, s)?,
				)),
			};
		}

		NbtString::from_str0(s).map(|(s, x)| (s, Self::String(x)))
	}

	pub(in crate::elements) fn parse_byte(num_str: &str, unsigned: bool, positive: bool, base: u32, s: &str) -> Result<Self, usize> {
		let value = if unsigned {
			u8::from_str_radix(&num_str, base).map_err(|_| s.len())? as i8
		} else {
			i8::from_str_radix(&num_str, base).map_err(|_| s.len())?
		};
		Ok(Self::Byte(NbtByte { value: if positive { value } else { -value }, }))
	}

	pub(in crate::elements) fn parse_short(num_str: &str, unsigned: bool, positive: bool, base: u32, s: &str) -> Result<Self, usize> {
		let value = if unsigned {
			u16::from_str_radix(&num_str, base).map_err(|_| s.len())? as i16
		} else {
			i16::from_str_radix(&num_str, base).map_err(|_| s.len())?
		};
		Ok(Self::Short(NbtShort { value: if positive { value } else { -value }, }))
	}

	pub(in crate::elements) fn parse_int(num_str: &str, unsigned: bool, positive: bool, base: u32, s: &str) -> Result<Self, usize> {
		let value = if unsigned {
			u32::from_str_radix(&num_str, base).map_err(|_| s.len())? as i32
		} else {
			i32::from_str_radix(&num_str, base).map_err(|_| s.len())?
		};
		Ok(Self::Int(NbtInt { value: if positive { value } else { -value }, }))
	}

	pub(in crate::elements) fn parse_long(num_str: &str, unsigned: bool, positive: bool, base: u32, s: &str) -> Result<Self, usize> {
		let value = if unsigned {
			u64::from_str_radix(&num_str, base).map_err(|_| s.len())? as i64
		} else {
			i64::from_str_radix(&num_str, base).map_err(|_| s.len())?
		};
		Ok(Self::Long(NbtLong { value: if positive { value } else { -value }, }))
	}

	pub(in crate::elements) fn array_try_into_byte(self) -> Option<Self> {
		match self.id() {
			NbtByte::ID => Some(self),
			_ => None,
		}
	}

	pub(in crate::elements) fn array_try_into_int(self) -> Option<Self> {
		match self.id() {
			NbtByte::ID => Some(Self::Int(NbtInt { value: unsafe { self.byte.value } as i32 })),
			NbtShort::ID => Some(Self::Int(NbtInt { value: unsafe { self.short.value } as i32 })),
			NbtInt::ID => Some(self),
			_ => None,
		}
	}

	pub(in crate::elements) fn array_try_into_long(self) -> Option<Self> {
		match self.id() {
			NbtByte::ID => Some(Self::Long(NbtLong { value: unsafe { self.byte.value } as i64 })),
			NbtShort::ID => Some(Self::Long(NbtLong { value: unsafe { self.short.value } as i64 })),
			NbtInt::ID => Some(Self::Long(NbtLong { value: unsafe { self.int.value } as i64 })),
			NbtLong::ID => Some(self),
			_ => None,
		}
	}

	pub fn from_bytes<'a, D: Decoder<'a>>(element: u8, decoder: &mut D) -> Option<Self> {
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
			NbtList::ID => Self::List(NbtList::new(vec![])),
			NbtCompound::ID => Self::Compound(NbtCompound::new()),
			NbtIntArray::ID => Self::IntArray(NbtIntArray::new()),
			NbtLongArray::ID => Self::LongArray(NbtLongArray::new()),
			NbtChunk::ID => Self::Chunk(NbtChunk::from_compound(
				NbtCompound::new(),
				(0, 0),
				FileFormat::Zlib,
				now().as_secs() as u32,
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
			if decoder.assert_len(2).is_some() && decoder.u16() != 0_u16.to_be() {
				decoder.skip(-2_isize as usize);
			}
		}
		let nbt = Self::Compound(NbtCompound::from_bytes(&mut decoder)?);
		if decoder.assert_len(1).is_some() {
			return None;
		}
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
			let result = match kind {
				NbtCompound::ID => {
					decoder.assert_len(2)?;
					let skip = decoder.u16() as usize;
					decoder.skip(skip);
					Some((Self::Compound(NbtCompound::from_bytes(&mut decoder)?), decoder.header()))
				},
				NbtList::ID => {
					decoder.assert_len(2)?;
					let skip = decoder.u16() as usize;
					decoder.skip(skip);
					Some((Self::List(NbtList::from_bytes(&mut decoder)?), decoder.header()))
				},
				_ => None,
			};
			if decoder.assert_len(1).is_some() {
				return None;
			}
			result
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
				NbtNull::ID => 0,
				_ => 1,
			}
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
				NbtNull::ID => 0,
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
	
	#[must_use]
	pub fn values(&self) -> Option<NbtElementValues<'_>> {
		unsafe {
			Some(match self.id() {
				NbtByteArray::ID => NbtElementValues::Iter(self.byte_array.children()),
				NbtIntArray::ID => NbtElementValues::Iter(self.int_array.children()),
				NbtLongArray::ID => NbtElementValues::Iter(self.long_array.children()),
				NbtList::ID => NbtElementValues::Iter(self.list.children()),
				NbtCompound::ID => NbtElementValues::CompoundMapIter(self.compound.children()),
				NbtChunk::ID => NbtElementValues::CompoundMapIter(self.chunk.children()),
				NbtRegion::ID => NbtElementValues::Iter(self.region.children()),
				_ => return None,
			})
		}
	}

	#[must_use]
	#[allow(clippy::type_complexity)] // a type probably shouldn't abstract what this is, like... yeah
	pub fn children_mut(&mut self) -> Option<Result<IterMut<'_, NbtElement>, CompoundMapIterMut<'_>>> {
		unsafe {
			Some(match self.id() {
				NbtByteArray::ID => Ok(self.byte_array.children_mut()),
				NbtIntArray::ID => Ok(self.int_array.children_mut()),
				NbtLongArray::ID => Ok(self.long_array.children_mut()),
				NbtList::ID => Ok(self.list.children_mut()),
				NbtCompound::ID => Err(self.compound.children_mut()),
				NbtChunk::ID => Err(self.chunk.children_mut()),
				NbtRegion::ID => Ok(self.region.children_mut()),
				_ => return None,
			})
		}
	}

	#[must_use]
	#[allow(dead_code)]
	pub fn navigate<'a>(&'a self, indices: &Indices) -> Option<NavigationInformation<'a>> {
		NavigationInformation::from(self, indices)
	}

	#[must_use]
	#[allow(dead_code)]
	pub fn navigate_mut<'a>(&'a mut self, indices: &Indices) -> Option<NavigationInformationMut<'a>> {
		NavigationInformationMut::from(self, indices)
	}

	#[must_use]
	#[allow(dead_code)]
	pub fn navigate_parent<'nbt, 'indices>(&'nbt self, indices: &'indices Indices) -> Option<ParentNavigationInformation<'nbt, 'indices>> {
		ParentNavigationInformation::from(self, indices)
	}

	#[must_use]
	#[allow(dead_code)]
	pub fn navigate_parent_mut<'nbt, 'indices>(&'nbt mut self, indices: &'indices Indices) -> Option<ParentNavigationInformationMut<'nbt, 'indices>> {
		ParentNavigationInformationMut::from(self, indices)
	}

	#[must_use]
	#[allow(dead_code)]
	pub fn traverse(&self, y: usize, x: Option<usize>) -> Option<TraversalInformation> {
		TraversalInformation::from(self, y, x)
	}

	#[must_use]
	#[allow(dead_code)]
	pub fn traverse_mut(&mut self, y: usize, x: Option<usize>) -> Option<TraversalInformationMut> {
		TraversalInformationMut::from(self, y, x)
	}

	pub fn recache(&mut self) {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.recache(),
				NbtIntArray::ID => self.int_array.recache(),
				NbtLongArray::ID => self.long_array.recache(),
				NbtList::ID => self.list.recache(),
				NbtCompound::ID => self.compound.recache(),
				NbtChunk::ID => self.chunk.recache(),
				NbtRegion::ID => self.region.recache(),
				_ => {}
			}
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
	
	#[must_use]
	pub fn update_key(&mut self, idx: usize, key: CompactString) -> Option<Option<CompactString>> {
		unsafe {
			match self.id() {
				NbtCompound::ID => Some(self.compound.entries.update_key(idx, key)),
				NbtChunk::ID => Some(self.chunk.entries.update_key(idx, key)),
				_ => None,
			}
		}
	}

	#[must_use]
	pub fn as_nonnull(&self) -> Option<&Self> {
		if self.is_null() {
			None
		} else {
			Some(self)
		}
	}

	#[must_use]
	pub fn as_nonnull_mut(&mut self) -> Option<&mut Self> {
		if self.is_null() {
			None
		} else {
			Some(self)
		}
	}

	#[must_use]
	pub fn into_nonnull(self) -> Option<Self> {
		if self.is_null() {
			None
		} else {
			Some(self)
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

	#[inline]
	#[must_use]
	pub fn toggle(&mut self) -> Option<()> {
		unsafe {
			Some(match self.id() {
				NbtByteArray::ID => self.byte_array.toggle(),
				NbtIntArray::ID => self.int_array.toggle(),
				NbtLongArray::ID => self.long_array.toggle(),
				NbtList::ID => self.list.toggle(),
				NbtCompound::ID => self.compound.toggle(),
				NbtRegion::ID => self.region.toggle(),
				NbtChunk::ID => self.chunk.toggle(),
				_ => return None,
			})
		}
	}

	#[inline]
	#[must_use]
	pub fn is_open(&self) -> bool {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.open(),
				NbtIntArray::ID => self.int_array.open(),
				NbtLongArray::ID => self.long_array.open(),
				NbtList::ID => self.list.open(),
				NbtCompound::ID => self.compound.is_open(),
				NbtRegion::ID => self.region.is_open(),
				NbtChunk::ID => self.chunk.is_open(),
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
	pub fn value_width(&self) -> usize {
		unsafe {
			match self.id() {
				NbtByte::ID => {
					(const { width_ascii("1") }) * self.byte.value.abs().checked_ilog10().map_or(1, |x| x as usize + 1)
						+ (self.byte.value < 0) as usize * const { width_ascii("-") }
				},
				NbtShort::ID => {
					(const { width_ascii("1") }) * self.short.value.abs().checked_ilog10().map_or(1, |x| x as usize + 1)
						+ (self.short.value < 0) as usize * const { width_ascii("-") }
				},
				NbtInt::ID => {
					(const { width_ascii("1") }) * self.int.value.abs().checked_ilog10().map_or(1, |x| x as usize + 1)
						+ (self.int.value < 0) as usize * const { width_ascii("-") }
				},
				NbtLong::ID => {
					(const { width_ascii("1") }) * self.long.value.abs().checked_ilog10().map_or(1, |x| x as usize + 1)
						+ (self.long.value < 0) as usize * const { width_ascii("-") }
				},
				NbtFloat::ID => self.float.value().width(), // optimizations to this wouldn't do much because the internal BigInteger would still need a heap alloc
				NbtDouble::ID => self.double.value().width(), // optimizations to this wouldn't do much because the internal BigInteger would still need a heap alloc
				NbtByteArray::ID => {
					(const { width_ascii("1") }) * self.byte_array.len().checked_ilog10().map_or(1, |x| x as usize + 1)
						+ const { width_ascii(" ") }
						+ if self.byte_array.len() == 1 { const { id_to_string_name_width(NbtByte::ID) }.0 } else { const { id_to_string_name_width(NbtByte::ID) }.1 }
				},
				NbtString::ID => self.string.str.width(),
				NbtList::ID => {
					(const { width_ascii("1") }) * self.list.len().checked_ilog10().map_or(1, |x| x as usize + 1)
						+ const { width_ascii(" ") }
						+ if self.list.len() == 1 { id_to_string_name_width(self.list.id()).0 } else { id_to_string_name_width(self.list.id()).1 }
				},
				NbtCompound::ID => {
					(const { width_ascii("1") }) * self.compound.len().checked_ilog10().map_or(1, |x| x as usize + 1)
						+ const { width_ascii(" ") }
						+ if self.compound.len() == 1 { const { id_to_string_name_width(NbtNull::ID) }.0 } else { const { id_to_string_name_width(NbtNull::ID) }.1 }
				},
				NbtIntArray::ID => {
					(const { width_ascii("1") }) * self.int_array.len().checked_ilog10().map_or(1, |x| x as usize + 1)
						+ const { width_ascii(" ") }
						+ if self.int_array.len() == 1 { const { id_to_string_name_width(NbtInt::ID) }.0 } else { const { id_to_string_name_width(NbtInt::ID) }.1 }
				},
				NbtLongArray::ID => {
					(const { width_ascii("1") }) * self.long_array.len().checked_ilog10().map_or(1, |x| x as usize + 1)
						+ const { width_ascii(" ") }
						+ if self.long_array.len() == 1 { const { id_to_string_name_width(NbtLong::ID) }.0 } else { const { id_to_string_name_width(NbtLong::ID) }.1 }
				},
				NbtChunk::ID => {
					(const { width_ascii("1") }) * (self.chunk.x.checked_ilog10().map_or(1, |x| x as usize + 1) + self.chunk.z.checked_ilog10().map_or(1, |x| x as usize + 1))
						+ const { width_ascii(", ") }
				},
				NbtRegion::ID => {
					(const { width_ascii("1") }) * self.region.loaded_chunks().checked_ilog10().map_or(1, |x| x as usize + 1)
						+ const { width_ascii(" chunk") }
						+ (self.region.loaded_chunks() == 1) as usize * const { width_ascii("s") }
				},
				_ => 0,
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn is_primitive(&self) -> bool {
		matches!(self.id(), NbtByte::ID | NbtShort::ID | NbtInt::ID | NbtLong::ID | NbtFloat::ID | NbtDouble::ID | NbtString::ID)
	}
	
	#[must_use]
	pub fn is_complex(&self) -> bool {
		matches!(self.id(), NbtByteArray::ID | NbtList::ID | NbtCompound::ID | NbtIntArray::ID | NbtLongArray::ID | NbtChunk::ID | NbtRegion::ID)
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
	pub fn try_into_inner(mut self) -> Result<Self, Self> {
		if let Some(compound) = self.as_compound_mut() && compound.len() == 1 && compound.get(0).is_some_and(|(key, _)| key.is_empty()) && let Some((_, inner)) = compound.remove(0) {
			Ok(inner)
		} else {
			Err(self)
		}
	}

	#[must_use]
	pub fn create_drop_indices(&self, key: Option<&str>, value: &Self, mut y: usize, x: usize) -> Option<OwnedIndices> {
		let mut indices = OwnedIndices::new();
		match self.create_drop_indices0(key, value, &mut y, x, 0, &mut indices) {
			DropResult::Dropped => Some(indices),
			DropResult::Missed => None,
			DropResult::Failed => None,
		}
	}

	#[must_use]
	pub(super) fn create_drop_indices0(&self, key: Option<&str>, value: &Self, y: &mut usize, current_depth: usize, x: usize, indices: &mut OwnedIndices) -> DropResult {
		let height_px = self.height() * 16;
		if *y >= height_px + 8 {
			*y -= height_px;
			return DropResult::Missed
		}
		if let Some(iter) = self.values() {
			let can_insert = self.can_insert(&value);
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

					match child.create_drop_indices0(key, value, y, current_depth + 1, x, indices) {
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

	/// # Safety
	/// - must be assured to update valid indices
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

	/// # Safety
	/// - must be assured to update valid indices
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

	/// # Safety
	/// - must be assured to update valid indices
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
	pub fn get(&self, idx: usize) -> Option<(Option<&str>, &Self)> {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.get(idx).map(|x| (None, x)),
				NbtIntArray::ID => self.int_array.get(idx).map(|x| (None, x)),
				NbtLongArray::ID => self.long_array.get(idx).map(|x| (None, x)),
				NbtList::ID => self.list.get(idx).map(|x| (None, x)),
				NbtCompound::ID => self.compound.get(idx).map(|(a, b)| (Some(a), b)),
				NbtRegion::ID => self.region.get(idx).map(|x| (None, x)),
				NbtChunk::ID => self.chunk.get(idx).map(|(a, b)| (Some(a), b)),
				_ => None,
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<(Option<&str>, &mut Self)> {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.get_mut(idx).map(|x| (None, x)),
				NbtIntArray::ID => self.int_array.get_mut(idx).map(|x| (None, x)),
				NbtLongArray::ID => self.long_array.get_mut(idx).map(|x| (None, x)),
				NbtList::ID => self.list.get_mut(idx).map(|x| (None, x)),
				NbtCompound::ID => self.compound.get_mut(idx).map(|(a, b)| (Some(a), b)),
				NbtRegion::ID => self.region.get_mut(idx).map(|x| (None, x)),
				NbtChunk::ID => self.chunk.get_mut(idx).map(|(a, b)| (Some(a), b)),
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
					let id = self.as_list_unchecked().id();
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
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
					let list = &mut *self.list.elements;
					for entry in &mut *list {
						if entry.id() > NbtDouble::ID {
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
					(indices as *mut HashTable<usize>).drop_in_place();
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
					(indices as *mut HashTable<usize>).drop_in_place();
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
						(indices as *mut HashTable<usize>).drop_in_place();
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
		pub static mut NULL_MUT: NbtElement = NbtElement::NULL;

		let result = 'a: {
			let map = match self.as_pattern_mut() {
				NbtPatternMut::Compound(compound) => &mut *compound.entries,
				NbtPatternMut::Chunk(chunk) => &mut *chunk.entries,
				_ => break 'a None,
			};

			if let Some(idx) = map.idx_of(index) && let Some((_, value)) = map.get_idx_mut(idx) {
				Some(value)
			} else {
				None
			}
		};

		result.unwrap_or_else(|| {
			unsafe { NULL_MUT = Self::NULL; }
			unsafe { &mut NULL_MUT }
		})
	}
}

impl Index<usize> for NbtElement {
	type Output = NbtElement;

	fn index(&self, idx: usize) -> &Self::Output {
		self.get(idx)
			.map(|(a, b)| b)
			.unwrap_or(Self::NULL_REF)
	}
}

impl IndexMut<usize> for NbtElement {
	fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
		pub static mut NULL_MUT: NbtElement = NbtElement::NULL;

		self.get_mut(idx)
			.map(|(a, b)| b)
			.unwrap_or_else(|| {
			unsafe { NULL_MUT = NbtElement::NULL; }
			unsafe { &mut NULL_MUT }
		})
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

#[inline]
#[must_use]
pub const fn id_to_string_name_width(id: u8) -> (usize, usize) {
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

pub enum NbtElementValues<'a> {
	Iter(Iter<'a, NbtElement>),
	CompoundMapIter(CompoundMapIter<'a>),
}

impl<'a> Iterator for NbtElementValues<'a> {
	type Item = &'a NbtElement;

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			Self::Iter(iter) => iter.next(),
			Self::CompoundMapIter(iter) => iter.next().map(|(a, b)| b),
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