use std::alloc::{alloc, dealloc, Layout};
use std::fmt::{Debug, Display, Error, Formatter};
use std::intrinsics::likely;
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::Deref;
#[cfg(not(target_arch = "wasm32"))]
use std::thread::Scope;
use std::{fmt, fmt::Write};

use compact_str::{format_compact, CompactString, ToCompactString};
use hashbrown::raw::RawTable;

use crate::assets::{BASE_Z, BYTE_ARRAY_UV, BYTE_UV, CONNECTION_UV, DOUBLE_UV, FLOAT_UV, INT_ARRAY_UV, INT_UV, LONG_ARRAY_UV, LONG_UV, SHORT_UV, ZOffset};
use crate::be_decoder::BigEndianDecoder;
use crate::elements::chunk::{NbtChunk, NbtRegion};
use crate::elements::compound::{CompoundMap, CompoundMapIter, Entry, NbtCompound};
use crate::element_action::ElementAction;
use crate::elements::list::{NbtList, ValueIterator, ValueMutIterator};
use crate::elements::string::NbtString;
use crate::encoder::UncheckedBufWriter;
use crate::{panic_unchecked, since_epoch, SortAlgorithm, array, primitive, DropFn, RenderContext, StrExt, VertexBufferBuilder, TextColor, assets::JUST_OVERLAPPING_BASE_TEXT_Z};
use crate::formatter::PrettyFormatter;
use crate::le_decoder::LittleEndianDecoder;
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
				_ => core::hint::unreachable_unchecked(),
			};
			element.id.id = self.id.id;
			element
		}
	}
}

#[allow(non_snake_case)]
impl NbtElement {
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
}

impl NbtElement {
	#[must_use]
	#[allow(clippy::should_implement_trait)] // i can't, sorry :(
	pub fn from_str(mut s: &str, sort: SortAlgorithm) -> Option<(Option<CompactString>, Self)> {
		s = s.trim_start();

		if s.is_empty() { return None }

		let prefix = s.snbt_string_read().and_then(|(prefix, s2)| {
			s2.trim_start().strip_prefix(':').filter(|s| !s.is_empty()).map(|s2| {
				s = s2.trim_start();
				prefix
			})
		});
		let (s, element) = Self::from_str0(s, sort).map(|(s, x)| (s.trim_start(), x))?;
		if !s.is_empty() { return None }
		Some((prefix, element))
	}

	#[allow(clippy::too_many_lines)]
	pub(in crate::elements) fn from_str0(mut s: &str, sort: SortAlgorithm) -> Option<(&str, Self)> {
		if let Some(s2) = s.strip_prefix("false") { return Some((s2, Self::Byte(NbtByte { value: 0 }))) }
		if let Some(s2) = s.strip_prefix("true") { return Some((s2, Self::Byte(NbtByte { value: 1 }))) }
		if s.starts_with("[B;") { return NbtByteArray::from_str0(s, sort).map(|(s, x)| (s, Self::ByteArray(x))) }
		if s.starts_with("[I;") { return NbtIntArray::from_str0(s, sort).map(|(s, x)| (s, Self::IntArray(x))) }
		if s.starts_with("[L;") { return NbtLongArray::from_str0(s, sort).map(|(s, x)| (s, Self::LongArray(x))) }
		if s.starts_with('[') { return NbtList::from_str0(s, sort).map(|(s, x)| (s, Self::List(x))) }
		if s.starts_with('{') { return NbtCompound::from_str0(s, sort).map(|(s, x)| (s, Self::Compound(x))) }
		if s.starts_with('"') { return NbtString::from_str0(s).map(|(s, x)| (s, Self::String(x))) }

		if let Some(s2) = s.strip_prefix("NaN") {
			s = s2.trim_start();
			return if let Some(s2) = s.strip_prefix('f') {
				Some((s2.trim_start(), Self::Float(NbtFloat { value: f32::NAN })))
			} else {
				Some((s2.strip_prefix('d').unwrap_or(s2).trim_start(), Self::Double(NbtDouble { value: f64::NAN })))
			};
		}

		if let Some(s2) = s.strip_prefix("Infinity").or_else(|| s.strip_prefix("inf")) {
			s = s2.trim_start();
			return if let Some(s2) = s.strip_prefix('f') {
				Some((
					s2.trim_start(),
					Self::Float(NbtFloat {
						value: f32::INFINITY,
					}),
				))
			} else {
				Some((
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
				Some((
					s2.trim_start(),
					Self::Float(NbtFloat {
						value: f32::NEG_INFINITY,
					}),
				))
			} else if let Some(s2) = s.strip_prefix('d') {
				Some((
					s2.trim_start(),
					Self::Double(NbtDouble {
						value: f64::NEG_INFINITY,
					}),
				))
			} else {
				Some((
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
				Some(b'b') => Some((
					&s[(digit_end_idx + 1)..],
					Self::Byte(NbtByte {
						value: s[..digit_end_idx].parse().ok()?,
					}),
				)),
				Some(b's') => Some((
					&s[(digit_end_idx + 1)..],
					Self::Short(NbtShort {
						value: s[..digit_end_idx].parse().ok()?,
					}),
				)),
				Some(b'l') => Some((
					&s[(digit_end_idx + 1)..],
					Self::Long(NbtLong {
						value: s[..digit_end_idx].parse().ok()?,
					}),
				)),
				Some(b'f') => Some((
					&s[(digit_end_idx + 1)..],
					Self::Float(NbtFloat {
						value: s[..digit_end_idx].parse().ok()?,
					}),
				)),
				Some(b'd') => Some((
					&s[(digit_end_idx + 1)..],
					Self::Double(NbtDouble {
						value: s[..digit_end_idx].parse().ok()?,
					}),
				)),
				Some(b'|') => Some({
					let mut s = s;
					let Ok(x @ 0..=31) = s[..digit_end_idx].parse::<u8>() else {
						return None;
					};
					s = s[digit_end_idx..].trim_start().split_at(1).1.trim_start();
					let digit_end_idx = s.bytes().position(|x| !x.is_ascii_digit())?;
					let Ok(z @ 0..=31) = s[..digit_end_idx].parse::<u8>() else {
						return None;
					};
					s = s[digit_end_idx..].trim_start();
					let (s, inner) = NbtCompound::from_str0(s, sort)?;
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
				_ => Some((
					&s[digit_end_idx..],
					Self::Int(NbtInt {
						value: s[..digit_end_idx].parse().ok()?,
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
				_ => core::hint::unreachable_unchecked(),
			};
		}
	}

	#[inline]
	#[must_use]
	pub const fn id(&self) -> u8 { unsafe { self.id.id } }

	#[inline]
	#[must_use]
	pub const fn is_null(&self) -> bool { self.id() == 0 }

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
			NbtString::ID => Self::String(NbtString::new(CompactString::new_inline(""))),
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
			_ => unsafe { core::mem::zeroed() },
		}
	}

	#[inline]
	#[must_use]
	pub fn from_be_file(bytes: &[u8], sort: SortAlgorithm) -> Option<Self> {
		let mut decoder = BigEndianDecoder::new(bytes, sort);
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
	pub fn from_be_mca(bytes: &[u8], sort: SortAlgorithm) -> Option<Self> {
		NbtRegion::from_be_bytes(bytes, sort).map(Self::Region)
	}

	#[inline]
	#[must_use]
	pub fn from_le_file(bytes: &[u8], sort: SortAlgorithm) -> Option<(Self, bool)> {
		let mut decoder = LittleEndianDecoder::new(bytes, sort);
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
				NbtCompound::ID => self.compound.len(),
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID | NbtList::ID => self.list.len(),
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
			_ => unsafe { panic_unchecked("Invalid element id") },
		}
	}

	#[inline]
	pub fn render_icon(id: u8, pos: impl Into<(usize, usize)>, z: ZOffset, builder: &mut VertexBufferBuilder) {
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
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.byte_array.height(),
				NbtList::ID => self.list.height(),
				NbtCompound::ID => self.compound.height(),
				NbtChunk::ID => self.chunk.height(),
				NbtRegion::ID => self.region.height(),
				_ => 1,
			}
		}
	}

	#[inline]
	pub fn swap(&mut self, a: usize, b: usize) {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID | NbtList::ID => self.list.elements.swap(a, b),
				NbtCompound::ID => self.compound.entries.swap(a, b),
				NbtChunk::ID => self.chunk.entries.swap(a, b),
				NbtRegion::ID => self.region.chunks.as_mut().0.swap(a, b),
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
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID | NbtList::ID => Ok(self.list.children()),
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
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.byte_array.true_height(),
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
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.byte_array.toggle(),
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
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.byte_array.open(),
				NbtList::ID => self.list.open(),
				NbtCompound::ID => self.compound.open(),
				NbtRegion::ID => self.region.open(),
				NbtChunk::ID => self.chunk.open(),
				_ => false,
			}
		}
	}

	#[inline]
	pub fn increment(&mut self, amount: usize, true_amount: usize) {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.byte_array.increment(amount, true_amount),
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
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.byte_array.decrement(amount, true_amount),
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
				NbtChunk::ID => (self.chunk.z.to_compact_string(), TextColor::TreePrimitive),
				NbtRegion::ID => (self.region.value(), TextColor::TreeKey),
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
					DropFn::Missed(key, element)
				}
			}
		}
	}

	#[inline]
	pub fn shut(&mut self) {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.byte_array.shut(),
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
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.byte_array.expand(),
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
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.byte_array.expand(),
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
	pub fn insert(&mut self, idx: usize, value: Self) -> Result<(), Self> {
		unsafe {
			match self.id() {
				NbtByteArray::ID => self.byte_array.insert(idx, value),
				NbtList::ID => self.list.insert(idx, value),
				NbtCompound::ID => {
					self.compound
						.insert(idx, CompactString::new_inline("_"), value);
					Ok(())
				}
				NbtIntArray::ID => self.int_array.insert(idx, value),
				NbtLongArray::ID => self.long_array.insert(idx, value),
				NbtRegion::ID => self.region.insert(idx, value),
				NbtChunk::ID => {
					self.chunk
						.insert(idx, CompactString::new_inline("_"), value);
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
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => (None, self.byte_array.remove(idx)),
				NbtList::ID => (None, self.list.remove(idx)),
				NbtCompound::ID => return self.compound.remove_idx(idx).map(|(a, b)| (Some(a), b)),
				NbtRegion::ID => (None, self.region.remove(idx)),
				NbtChunk::ID => return self.chunk.remove_idx(idx).map(|(a, b)| (Some(a), b)),
				_ => return None,
			})
		}
	}

	#[inline]
	#[must_use]
	pub fn get(&self, idx: usize) -> Option<&Self> {
		unsafe {
			match self.id() {
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.byte_array.get(idx),
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
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.byte_array.get_mut(idx),
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
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => self.byte_array.max_depth(),
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
				],
				NbtString::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
				],
				#[cfg(not(target_arch = "wasm32"))]
				NbtList::ID => {
					const FULL: [ElementAction; 4] = [
						ElementAction::CopyRaw,
						ElementAction::CopyFormatted,
						ElementAction::OpenInTxt,
						ElementAction::OpenArrayInHex
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
				],
				NbtCompound::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
					ElementAction::SortCompoundByName,
					ElementAction::SortCompoundByType,
				],
				NbtIntArray::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenArrayInHex,
				],
				NbtLongArray::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenArrayInHex,
				],
				NbtChunk::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
					ElementAction::SortCompoundByName,
					ElementAction::SortCompoundByType,
				],
				NbtRegion::ID => &[
					ElementAction::CopyRaw,
					ElementAction::CopyFormatted,
					#[cfg(not(target_arch = "wasm32"))]
					ElementAction::OpenInTxt,
				],
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
					let (map, chunks) = *core::ptr::addr_of_mut!(self.region.chunks).read();
					drop(map);
					for mut chunk in core::mem::transmute::<_, [ManuallyDrop<Self>; 1024]>(chunks) {
						if !chunk.is_null() {
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
			_ => unsafe { panic_unchecked("variant wasn't known") },
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
			_ => unsafe { panic_unchecked("variant wasn't known") },
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Byte`
	#[inline]
	#[must_use]
	pub unsafe fn into_byte_unchecked(self) -> NbtByte { core::ptr::addr_of!(*self.byte).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Byte`
	#[inline]
	#[must_use]
	pub unsafe fn as_byte_unchecked(&self) -> &NbtByte { &self.byte }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Byte`
	#[inline]
	#[must_use]
	pub unsafe fn as_byte_unchecked_mut(&mut self) -> &mut NbtByte { &mut self.byte }

	#[inline]
	#[must_use]
	pub fn into_byte(self) -> Option<NbtByte> {
		unsafe {
			if self.id() == NbtByte::ID {
				Some(core::ptr::addr_of!(*self.byte).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_byte(&self) -> Option<&NbtByte> {
		unsafe {
			if self.id() == NbtByte::ID {
				Some(&self.byte)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_byte_mut(&mut self) -> Option<&mut NbtByte> {
		unsafe {
			if self.id() == NbtByte::ID {
				Some(&mut self.byte)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Short`
	#[inline]
	#[must_use]
	pub unsafe fn into_short_unchecked(self) -> NbtShort { core::ptr::addr_of!(*self.short).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Short`
	#[inline]
	#[must_use]
	pub unsafe fn as_short_unchecked(&self) -> &NbtShort { &self.short }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Short`
	#[inline]
	#[must_use]
	pub unsafe fn as_short_unchecked_mut(&mut self) -> &mut NbtShort { &mut self.short }

	#[inline]
	#[must_use]
	pub fn into_short(self) -> Option<NbtShort> {
		unsafe {
			if self.id() == NbtShort::ID {
				Some(core::ptr::addr_of!(*self.short).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_short(&self) -> Option<&NbtShort> {
		unsafe {
			if self.id() == NbtShort::ID {
				Some(&self.short)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_short_mut(&mut self) -> Option<&mut NbtShort> {
		unsafe {
			if self.id() == NbtShort::ID {
				Some(&mut self.short)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Int`
	#[inline]
	#[must_use]
	pub unsafe fn into_int_unchecked(self) -> NbtInt { core::ptr::addr_of!(*self.int).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Int`
	#[inline]
	#[must_use]
	pub unsafe fn as_int_unchecked(&self) -> &NbtInt { &self.int }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Int`
	#[inline]
	#[must_use]
	pub unsafe fn as_int_unchecked_mut(&mut self) -> &mut NbtInt { &mut self.int }

	#[inline]
	#[must_use]
	pub fn into_int(self) -> Option<NbtInt> {
		unsafe {
			if self.id() == NbtInt::ID {
				Some(core::ptr::addr_of!(*self.int).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_int(&self) -> Option<&NbtInt> {
		unsafe {
			if self.id() == NbtInt::ID {
				Some(&self.int)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_int_mut(&mut self) -> Option<&mut NbtInt> {
		unsafe {
			if self.id() == NbtInt::ID {
				Some(&mut self.int)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Long`
	#[inline]
	#[must_use]
	pub unsafe fn into_long_unchecked(self) -> NbtLong { core::ptr::addr_of!(*self.long).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Long`
	#[inline]
	#[must_use]
	pub unsafe fn as_long_unchecked(&self) -> &NbtLong { &self.long }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Long`
	#[inline]
	#[must_use]
	pub unsafe fn as_long_unchecked_mut(&mut self) -> &mut NbtLong { &mut self.long }

	#[inline]
	#[must_use]
	pub fn into_long(self) -> Option<NbtLong> {
		unsafe {
			if self.id() == NbtLong::ID {
				Some(core::ptr::addr_of!(*self.long).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_long(&self) -> Option<&NbtLong> {
		unsafe {
			if self.id() == NbtLong::ID {
				Some(&self.long)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_long_mut(&mut self) -> Option<&mut NbtLong> {
		unsafe {
			if self.id() == NbtLong::ID {
				Some(&mut self.long)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Float`
	#[inline]
	#[must_use]
	pub unsafe fn into_float_unchecked(self) -> NbtFloat { core::ptr::addr_of!(*self.float).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Float`
	#[inline]
	#[must_use]
	pub unsafe fn as_float_unchecked(&self) -> &NbtFloat { &self.float }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Float`
	#[inline]
	#[must_use]
	pub unsafe fn as_float_unchecked_mut(&mut self) -> &mut NbtFloat { &mut self.float }

	#[inline]
	#[must_use]
	pub fn into_float(self) -> Option<NbtFloat> {
		unsafe {
			if self.id() == NbtFloat::ID {
				Some(core::ptr::addr_of!(*self.float).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_float(&self) -> Option<&NbtFloat> {
		unsafe {
			if self.id() == NbtFloat::ID {
				Some(&self.float)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_float_mut(&mut self) -> Option<&mut NbtFloat> {
		unsafe {
			if self.id() == NbtFloat::ID {
				Some(&mut self.float)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Double`
	#[inline]
	#[must_use]
	pub unsafe fn into_double_unchecked(self) -> NbtDouble { core::ptr::addr_of!(*self.double).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Double`
	#[inline]
	#[must_use]
	pub unsafe fn as_double_unchecked(&self) -> &NbtDouble { &self.double }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Double`
	#[inline]
	#[must_use]
	pub unsafe fn as_double_unchecked_mut(&mut self) -> &mut NbtDouble { &mut self.double }

	#[inline]
	#[must_use]
	pub fn into_double(self) -> Option<NbtDouble> {
		unsafe {
			if self.id() == NbtDouble::ID {
				Some(core::ptr::addr_of!(*self.double).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_double(&self) -> Option<&NbtDouble> {
		unsafe {
			if self.id() == NbtDouble::ID {
				Some(&self.double)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_double_mut(&mut self) -> Option<&mut NbtDouble> {
		unsafe {
			if self.id() == NbtDouble::ID {
				Some(&mut self.double)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::ByteArray`
	#[inline]
	#[must_use]
	pub unsafe fn into_byte_array_unchecked(self) -> NbtByteArray { core::ptr::addr_of!(*self.byte_array).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::ByteArray`
	#[inline]
	#[must_use]
	pub unsafe fn as_byte_array_unchecked(&self) -> &NbtByteArray { &self.byte_array }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::ByteArray`
	#[inline]
	#[must_use]
	pub unsafe fn as_byte_array_unchecked_mut(&mut self) -> &mut NbtByteArray { &mut self.byte_array }

	#[inline]
	#[must_use]
	pub fn into_byte_array(self) -> Option<NbtByteArray> {
		unsafe {
			if self.id() == NbtByteArray::ID {
				Some(core::ptr::addr_of!(*self.byte_array).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_byte_array(&self) -> Option<&NbtByteArray> {
		unsafe {
			if self.id() == NbtByteArray::ID {
				Some(&self.byte_array)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_byte_array_mut(&mut self) -> Option<&mut NbtByteArray> {
		unsafe {
			if self.id() == NbtByteArray::ID {
				Some(&mut self.byte_array)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::String`
	#[inline]
	#[must_use]
	pub unsafe fn into_string_unchecked(self) -> NbtString { core::ptr::addr_of!(*self.string).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::String`
	#[inline]
	#[must_use]
	pub unsafe fn as_string_unchecked(&self) -> &NbtString { &self.string }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::String`
	#[inline]
	#[must_use]
	pub unsafe fn as_string_unchecked_mut(&mut self) -> &mut NbtString { &mut self.string }

	#[inline]
	#[must_use]
	pub fn into_string(self) -> Option<NbtString> {
		unsafe {
			if self.id() == NbtString::ID {
				Some(core::ptr::addr_of!(*self.string).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_string(&self) -> Option<&NbtString> {
		unsafe {
			if self.id() == NbtString::ID {
				Some(&self.string)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_string_mut(&mut self) -> Option<&mut NbtString> {
		unsafe {
			if self.id() == NbtString::ID {
				Some(&mut self.string)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::List`
	#[inline]
	#[must_use]
	pub unsafe fn into_list_unchecked(self) -> NbtList { core::ptr::addr_of!(*self.list).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::List`
	#[inline]
	#[must_use]
	pub unsafe fn as_list_unchecked(&self) -> &NbtList { &self.list }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::List`
	#[inline]
	#[must_use]
	pub unsafe fn as_list_unchecked_mut(&mut self) -> &mut NbtList { &mut self.list }

	#[inline]
	#[must_use]
	pub fn into_list(self) -> Option<NbtList> {
		unsafe {
			if self.id() == NbtList::ID {
				Some(core::ptr::addr_of!(*self.list).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_list(&self) -> Option<&NbtList> {
		unsafe {
			if self.id() == NbtList::ID {
				Some(&self.list)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_list_mut(&mut self) -> Option<&mut NbtList> {
		unsafe {
			if self.id() == NbtList::ID {
				Some(&mut self.list)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Compound`
	#[inline]
	#[must_use]
	pub unsafe fn into_compound_unchecked(self) -> NbtCompound { core::ptr::addr_of!(*self.compound).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Compound`
	#[inline]
	#[must_use]
	pub unsafe fn as_compound_unchecked(&self) -> &NbtCompound { &self.compound }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Compound`
	#[inline]
	#[must_use]
	pub unsafe fn as_compound_unchecked_mut(&mut self) -> &mut NbtCompound { &mut self.compound }

	#[inline]
	#[must_use]
	pub fn into_compound(self) -> Option<NbtCompound> {
		unsafe {
			if self.id() == NbtCompound::ID {
				Some(core::ptr::addr_of!(*self.compound).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_compound(&self) -> Option<&NbtCompound> {
		unsafe {
			if self.id() == NbtCompound::ID {
				Some(&self.compound)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_compound_mut(&mut self) -> Option<&mut NbtCompound> {
		unsafe {
			if self.id() == NbtCompound::ID {
				Some(&mut self.compound)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::IntArray`
	#[inline]
	#[must_use]
	pub unsafe fn into_int_array_unchecked(self) -> NbtIntArray { core::ptr::addr_of!(*self.int_array).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::IntArray`
	#[inline]
	#[must_use]
	pub unsafe fn as_int_array_unchecked(&self) -> &NbtIntArray { &self.int_array }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::IntArray`
	#[inline]
	#[must_use]
	pub unsafe fn as_int_array_unchecked_mut(&mut self) -> &mut NbtIntArray { &mut self.int_array }

	#[inline]
	#[must_use]
	pub fn into_int_array(self) -> Option<NbtIntArray> {
		unsafe {
			if self.id() == NbtIntArray::ID {
				Some(core::ptr::addr_of!(*self.int_array).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_int_array(&self) -> Option<&NbtIntArray> {
		unsafe {
			if self.id() == NbtIntArray::ID {
				Some(&self.int_array)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_int_array_mut(&mut self) -> Option<&mut NbtIntArray> {
		unsafe {
			if self.id() == NbtIntArray::ID {
				Some(&mut self.int_array)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::LongArray`
	#[inline]
	#[must_use]
	pub unsafe fn into_long_array_unchecked(self) -> NbtLongArray { core::ptr::addr_of!(*self.long_array).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::LongArray`
	#[inline]
	#[must_use]
	pub unsafe fn as_long_array_unchecked(&self) -> &NbtLongArray { &self.long_array }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::LongArray`
	#[inline]
	#[must_use]
	pub unsafe fn as_long_array_unchecked_mut(&mut self) -> &mut NbtLongArray { &mut self.long_array }

	#[inline]
	#[must_use]
	pub fn into_long_array(self) -> Option<NbtLongArray> {
		unsafe {
			if self.id() == NbtLongArray::ID {
				Some(core::ptr::addr_of!(*self.long_array).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_long_array(&self) -> Option<&NbtLongArray> {
		unsafe {
			if self.id() == NbtLongArray::ID {
				Some(&self.long_array)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_long_array_mut(&mut self) -> Option<&mut NbtLongArray> {
		unsafe {
			if self.id() == NbtLongArray::ID {
				Some(&mut self.long_array)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Chunk`
	#[inline]
	#[must_use]
	pub unsafe fn into_chunk_unchecked(self) -> NbtChunk { core::ptr::addr_of!(*self.chunk).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Chunk`
	#[inline]
	#[must_use]
	pub unsafe fn as_chunk_unchecked(&self) -> &NbtChunk { &self.chunk }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Chunk`
	#[inline]
	#[must_use]
	pub unsafe fn as_chunk_unchecked_mut(&mut self) -> &mut NbtChunk { &mut self.chunk }

	#[inline]
	#[must_use]
	pub fn into_chunk(self) -> Option<NbtChunk> {
		unsafe {
			if self.id() == NbtChunk::ID {
				Some(core::ptr::addr_of!(*self.chunk).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_chunk(&self) -> Option<&NbtChunk> {
		unsafe {
			if self.id() == NbtChunk::ID {
				Some(&self.chunk)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_chunk_mut(&mut self) -> Option<&mut NbtChunk> {
		unsafe {
			if self.id() == NbtChunk::ID {
				Some(&mut self.chunk)
			} else {
				None
			}
		}
	}
}

#[allow(dead_code)]
impl NbtElement {
	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Region`
	#[inline]
	#[must_use]
	pub unsafe fn into_region_unchecked(self) -> NbtRegion { core::ptr::addr_of!(*self.region).read() }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Region`
	#[inline]
	#[must_use]
	pub unsafe fn as_region_unchecked(&self) -> &NbtRegion { &self.region }

	/// # Safety
	///
	/// * `self` must be of variant `NbtElement::Region`
	#[inline]
	#[must_use]
	pub unsafe fn as_region_unchecked_mut(&mut self) -> &mut NbtRegion { &mut self.region }

	#[inline]
	#[must_use]
	pub fn into_region(self) -> Option<NbtRegion> {
		unsafe {
			if self.id() == NbtRegion::ID {
				Some(core::ptr::addr_of!(*self.region).read())
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_region(&self) -> Option<&NbtRegion> {
		unsafe {
			if self.id() == NbtRegion::ID {
				Some(&self.region)
			} else {
				None
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn as_region_mut(&mut self) -> Option<&mut NbtRegion> {
		unsafe {
			if self.id() == NbtRegion::ID {
				Some(&mut self.region)
			} else {
				None
			}
		}
	}
}
