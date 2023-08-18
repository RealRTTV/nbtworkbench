use std::fmt::{Debug, Display, Formatter};
use std::intrinsics::likely;
use std::io::BufWriter;
use std::string::String;
use std::{fmt, fmt::Write};

use crate::decoder::Decoder;
use crate::elements::chunk::{NbtChunk, NbtRegion};
use crate::elements::compound::NbtCompound;
use crate::elements::list::{NbtList, ValueIterator, ValueMutIterator};
use crate::elements::string::NbtString;
use crate::panic_unchecked;
use crate::{array, primitive, DropFn, RenderContext, StrExt, VertexBufferBuilder};
use crate::assets::{BYTE_UV, SHORT_UV, INT_UV, LONG_UV, FLOAT_UV, DOUBLE_UV, CONNECTION_UV, BYTE_ARRAY_UV, INT_ARRAY_UV, LONG_ARRAY_UV};
use core::fmt::DebugList;

primitive!(BYTE_UV, { Some('b') }, NbtByte, i8, 1);
primitive!(SHORT_UV, { Some('s') }, NbtShort, i16, 2);
primitive!(INT_UV, { None::<char> }, NbtInt, i32, 3);
primitive!(LONG_UV, { Some('L') }, NbtLong, i64, 4);
primitive!(FLOAT_UV, { Some('f') }, NbtFloat, f32, 5);
primitive!(DOUBLE_UV, { Some('d') }, NbtDouble, f64, 6);
array!(Byte, NbtByte, NbtByteArray, i8, 7, 1, 'B', BYTE_ARRAY_UV, BYTE_UV);
array!(Int, NbtInt, NbtIntArray, i32, 11, 3, 'I', INT_ARRAY_UV, INT_UV);
array!(Long, NbtLong, NbtLongArray, i64, 12, 4, 'L', LONG_ARRAY_UV, LONG_UV);

// todo, niche somehow with null variant
#[derive(Clone)]
pub enum NbtElement {
	/// This variant only exists in .mca files and exists here because refactoring `Tab` is too difficult due to the common use of &mut tab.value().
	/// The solution to that would be to refactor all of that to include a list of chunks.
	/// It's far easier to check if you're pulling a chunk from somewhere to somewhere it shouldn't be than to refactor everything.
	Chunk(NbtChunk),
	Region(NbtRegion),

	Null,
	Byte(NbtByte),
	Short(NbtShort),
	Int(NbtInt),
	Long(NbtLong),
	Float(NbtFloat),
	Double(NbtDouble),
	ByteArray(NbtByteArray),
	String(NbtString),
	List(NbtList),
	Compound(NbtCompound),
	IntArray(NbtIntArray),
	LongArray(NbtLongArray),
}

impl NbtElement {
	#[must_use]
	#[allow(clippy::should_implement_trait)] // i can't, sorry :(
	pub fn from_str(mut s: &str) -> Option<Result<(Box<str>, Self), Self>> {
		s = s.trim_start();

		if s.is_empty() {
			return None;
		}

		let prefix = s.snbt_string_read().and_then(|(prefix, s2)| {
			s2.strip_prefix(':').map(|s2| {
				s = s2;
				prefix.into_boxed_str()
			})
		});
		let element = Self::from_str0(s).map(|(_, x)| x)?;
		Some(match prefix {
			Some(key) => Ok((key, element)),
			None => Err(element),
		})
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
			if s.starts_with('.') {
				digit_end_idx += 1;
				s = &s[1..];
				let frac_part = s.bytes().take_while(u8::is_ascii_digit).count();
				digit_end_idx += frac_part;
				// s = &s[frac_part..];
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

		// please not nbt null, that's just, no.
		None
	}

	pub fn from_bytes(element: u8, decoder: &mut Decoder) -> Option<Self> {
		Some(match element {
			0 => Self::Null,
			1 => Self::Byte(NbtByte::from_bytes(decoder)?),
			2 => Self::Short(NbtShort::from_bytes(decoder)?),
			3 => Self::Int(NbtInt::from_bytes(decoder)?),
			4 => Self::Long(NbtLong::from_bytes(decoder)?),
			5 => Self::Float(NbtFloat::from_bytes(decoder)?),
			6 => Self::Double(NbtDouble::from_bytes(decoder)?),
			7 => Self::ByteArray(NbtByteArray::from_bytes(decoder)?),
			8 => Self::String(NbtString::from_bytes(decoder)?),
			9 => Self::List(NbtList::from_bytes(decoder)?),
			10 => Self::Compound(NbtCompound::from_bytes(decoder)?),
			11 => Self::IntArray(NbtIntArray::from_bytes(decoder)?),
			12 => Self::LongArray(NbtLongArray::from_bytes(decoder)?),
			_ => return None,
		})
	}

	pub fn to_bytes<W: std::io::Write>(&self, writer: &mut W) {
		match self {
			Self::Null => {
				let _ = writer.write(&[0x00]);
			}
			Self::Byte(byte) => byte.to_bytes(writer),
			Self::Short(short) => short.to_bytes(writer),
			Self::Int(int) => int.to_bytes(writer),
			Self::Long(long) => long.to_bytes(writer),
			Self::Float(float) => float.to_bytes(writer),
			Self::Double(double) => double.to_bytes(writer),
			Self::ByteArray(bytes) => bytes.to_bytes(writer),
			Self::String(string) => string.to_bytes(writer),
			Self::List(list) => list.to_bytes(writer),
			Self::Compound(compound) => compound.to_bytes(writer),
			Self::IntArray(ints) => ints.to_bytes(writer),
			Self::LongArray(longs) => longs.to_bytes(writer),
			Self::Chunk(chunk) => chunk.to_bytes(writer),
			Self::Region(region) => region.to_bytes(writer),
		};
	}

	#[inline]
	#[must_use]
	pub const fn id(&self) -> u8 {
		match self {
			Self::Null => 0,
			Self::Byte(_) => NbtByte::ID,
			Self::Short(_) => NbtShort::ID,
			Self::Int(_) => NbtInt::ID,
			Self::Long(_) => NbtLong::ID,
			Self::Float(_) => NbtFloat::ID,
			Self::Double(_) => NbtDouble::ID,
			Self::ByteArray(_) => NbtByteArray::ID,
			Self::String(_) => NbtString::ID,
			Self::List(_) => NbtList::ID,
			Self::Compound(_) => NbtCompound::ID,
			Self::IntArray(_) => NbtIntArray::ID,
			Self::LongArray(_) => NbtLongArray::ID,
			Self::Chunk(_) => NbtChunk::ID,
			Self::Region(_) => NbtRegion::ID,
		}
	}

	#[inline]
	#[must_use]
	pub fn from_id(id: u8) -> Option<Self> {
		Some(match id {
			0 => Self::Null,
			1 => Self::Byte(NbtByte::default()),
			2 => Self::Short(NbtShort::default()),
			3 => Self::Int(NbtInt::default()),
			4 => Self::Long(NbtLong::default()),
			5 => Self::Float(NbtFloat::default()),
			6 => Self::Double(NbtDouble::default()),
			7 => Self::ByteArray(NbtByteArray::new()),
			8 => Self::String(NbtString::new(String::new().into_boxed_str())),
			9 => Self::List(NbtList::new(vec![], 0x00)),
			10 => Self::Compound(NbtCompound::new()),
			11 => Self::IntArray(NbtIntArray::new()),
			12 => Self::LongArray(NbtLongArray::new()),
			_ => return None,
		})
	}

	#[inline]
	#[must_use]
	pub fn from_file(bytes: &[u8]) -> Option<Self> {
		#[cfg(windows_subsystem = "console")]
		let start = std::time::Instant::now();
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
		#[cfg(windows_subsystem = "console")]
		println!("{}ms for file read", start.elapsed().as_nanos() as f64 / 1_000_000.0);
		Some(nbt)
	}

	#[inline]
	#[must_use]
	pub fn to_file(&self) -> Option<Vec<u8>> {
		use std::io::Write;

		#[cfg(windows_subsystem = "console")]
		let start = std::time::Instant::now();
		for _ in 0..10_000_000 {
			let mut writer = BufWriter::with_capacity(65536, vec![]);
			let _ = writer.write(&[0x0A, 0x00, 0x00]);
			self.to_bytes(&mut writer);
			core::hint::black_box(writer);
		}
		#[cfg(windows_subsystem = "console")]
		println!("{}ms for file write", start.elapsed().as_nanos());
		let mut writer = BufWriter::with_capacity(65536, vec![]);
		let _ = writer.write(&[0x0A, 0x00, 0x00]);
		self.to_bytes(&mut writer);
		writer.into_inner().ok()
	}

	#[inline]
	#[must_use]
	pub fn from_mca(bytes: &[u8]) -> Option<Self> {
		NbtRegion::from_bytes(bytes).map(NbtElement::Region)
	}

	#[inline]
	pub fn render(&self, x: &mut usize, y: &mut usize, remaining_scroll: &mut usize, builder: &mut VertexBufferBuilder, str: Option<&str>, tail: bool, ctx: &mut RenderContext) {
		match self {
			Self::Null => *y += 16,
			Self::Byte(byte) => byte.render(builder, x, y, str, ctx),
			Self::Short(short) => short.render(builder, x, y, str, ctx),
			Self::Int(int) => int.render(builder, x, y, str, ctx),
			Self::Long(long) => long.render(builder, x, y, str, ctx),
			Self::Float(float) => float.render(builder, x, y, str, ctx),
			Self::Double(double) => double.render(builder, x, y, str, ctx),
			Self::ByteArray(byte_array) => {
				byte_array.render(builder, x, y, str, remaining_scroll, tail, ctx);
			}
			Self::String(string) => string.render(builder, x, y, str, ctx),
			Self::List(list) => {
				list.render(builder, x, y, str, remaining_scroll, tail, ctx);
			}
			Self::Compound(compound) => {
				compound.render(builder, x, y, str, remaining_scroll, tail, ctx);
			}
			Self::IntArray(int_array) => {
				int_array.render(builder, x, y, str, remaining_scroll, tail, ctx);
			}
			Self::LongArray(long_array) => {
				long_array.render(builder, x, y, str, remaining_scroll, tail, ctx);
			}
			// can't be done at all
			Self::Chunk(_) => {}
			Self::Region(_) => {}
		}
	}

	#[inline]
	#[must_use]
	pub fn len(&self) -> Option<usize> {
		Some(match self {
			Self::ByteArray(bytes) => bytes.len(),
			Self::List(list) => list.len(),
			Self::Compound(compound) => compound.len(),
			Self::IntArray(ints) => ints.len(),
			Self::LongArray(longs) => longs.len(),
			Self::Region(region) => region.len(),
			Self::Chunk(chunk) => chunk.len(),
			_ => return None,
		})
	}

	#[inline]
	#[must_use]
	pub fn is_empty(&self) -> bool {
		self.len().is_some_and(|x| x > 0)
	}

	#[inline]
	pub fn render_icon(id: u8, x: usize, y: usize, builder: &mut VertexBufferBuilder) {
		match id {
			0 => {}
			NbtByte::ID => NbtByte::render_icon(x, y, builder),
			NbtShort::ID => NbtShort::render_icon(x, y, builder),
			NbtInt::ID => NbtInt::render_icon(x, y, builder),
			NbtLong::ID => NbtLong::render_icon(x, y, builder),
			NbtFloat::ID => NbtFloat::render_icon(x, y, builder),
			NbtDouble::ID => NbtDouble::render_icon(x, y, builder),
			NbtByteArray::ID => NbtByteArray::render_icon(x, y, builder),
			NbtString::ID => NbtString::render_icon(x, y, builder),
			NbtList::ID => NbtList::render_icon(x, y, builder),
			NbtCompound::ID  => NbtCompound::render_icon(x, y, builder),
			NbtIntArray::ID  => NbtIntArray::render_icon(x, y, builder),
			NbtLongArray::ID  => NbtLongArray::render_icon(x, y, builder),
			NbtChunk::ID => NbtChunk::render_icon(x, y, builder),
			NbtRegion::ID => NbtRegion::render_icon(x, y, builder),
			_ => unsafe { panic_unchecked("Invalid element id") }
		}
	}

	#[inline]
	#[must_use]
	pub const fn height(&self) -> usize {
		match self {
			Self::ByteArray(bytes) => bytes.height(),
			Self::List(list) => list.height(),
			Self::Compound(compound) => compound.height(),
			Self::IntArray(ints) => ints.height(),
			Self::LongArray(longs) => longs.height(),
			Self::Region(region) => region.height(),
			Self::Chunk(chunk) => chunk.height(),
			_ => 1,
		}
	}

	#[inline]
	pub fn swap(&mut self, a: usize, b: usize) {
		match self {
			Self::ByteArray(bytes) => bytes.values.swap(a, b),
			Self::List(list) => list.elements.swap(a, b),
			Self::Compound(compound) => compound.entries.swap_indices(a, b),
			Self::IntArray(ints) => ints.values.swap(a, b),
			Self::LongArray(longs) => longs.values.swap(a, b),
			Self::Region(region) => region.map.swap(a, b),
			Self::Chunk(chunk) => chunk.inner.entries.swap_indices(a, b),
			_ => {}
		}
	}

	#[inline]
	#[must_use]
	#[allow(clippy::type_complexity)] // a type probably shouldn't abstract what this is, like... yeah
	pub fn children(&self) -> Option<Result<ValueIterator, indexmap::map::Iter<'_, Box<str>, Self>>> {
		Some(match self {
			Self::ByteArray(array) => Ok(array.children()),
			Self::List(list) => Ok(list.children()),
			Self::Compound(compound) => Err(compound.children()),
			Self::IntArray(array) => Ok(array.children()),
			Self::LongArray(array) => Ok(array.children()),
			Self::Region(region) => Ok(region.children()),
			Self::Chunk(chunk) => Err(chunk.children()),
			_ => return None,
		})
	}

	#[inline]
	pub fn set_value(&mut self, value: Box<str>) -> Option<Box<str>> {
		Some(match self {
			Self::Byte(byte) => {
				let before = byte.value.to_string().into();
				byte.set(value.parse().ok());
				before
			}
			Self::Short(short) => {
				let before = short.value.to_string().into();
				short.set(value.parse().ok());
				before
			}
			Self::Int(int) => {
				let before = int.value.to_string().into();
				int.set(value.parse().ok());
				before
			}
			Self::Long(long) => {
				let before = long.value.to_string().into();
				long.set(value.parse().ok());
				before
			}
			Self::Float(float) => {
				let before = float.value.to_string().into();
				float.set(value.parse().ok());
				before
			}
			Self::Double(double) => {
				let before = double.value.to_string().into();
				double.set(value.parse().ok());
				before
			}
			Self::String(string) => {
				// SAFETY: String is repr(transparent) and field is Box<str>
				unsafe { core::mem::transmute::<_, Box<str>>(core::mem::replace(string, NbtString::new(value))) }
			}
			_ => return None,
		})
	}

	#[must_use]
	pub const fn true_height(&self) -> usize {
		match self {
			Self::ByteArray(bytes) => bytes.true_height(),
			Self::List(list) => list.true_height(),
			Self::Compound(compound) => compound.true_height(),
			Self::IntArray(ints) => ints.true_height(),
			Self::LongArray(longs) => longs.true_height(),
			Self::Region(region) => region.true_height(),
			Self::Chunk(chunk) => chunk.true_height(),
			_ => 1,
		}
	}

	#[inline]
	#[must_use]
	pub fn toggle(&mut self) -> Option<()> {
		match self {
			Self::ByteArray(bytes) => bytes.toggle(),
			Self::List(list) => list.toggle(),
			Self::Compound(compound) => compound.toggle(),
			Self::IntArray(ints) => ints.toggle(),
			Self::LongArray(longs) => longs.toggle(),
			Self::Region(region) => region.toggle(),
			Self::Chunk(chunk) => chunk.toggle(),
			_ => None,
		}
	}

	#[inline]
	#[must_use]
	pub const fn open(&self) -> bool {
		match self {
			Self::ByteArray(bytes) => bytes.open(),
			Self::List(list) => list.open(),
			Self::Compound(compound) => compound.open(),
			Self::IntArray(ints) => ints.open(),
			Self::LongArray(longs) => longs.open(),
			Self::Region(region) => region.open(),
			Self::Chunk(chunk) => chunk.open(),
			_ => false,
		}
	}

	#[inline]
	pub fn increment(&mut self, amount: usize, true_amount: usize) {
		match self {
			Self::ByteArray(bytes) => bytes.increment(amount, true_amount),
			Self::List(list) => list.increment(amount, true_amount),
			Self::Compound(compound) => compound.increment(amount, true_amount),
			Self::IntArray(ints) => ints.increment(amount, true_amount),
			Self::LongArray(longs) => longs.increment(amount, true_amount),
			Self::Region(region) => region.increment(amount, true_amount),
			Self::Chunk(chunk) => chunk.increment(amount, true_amount),
			_ => {}
		}
	}

	#[inline]
	pub fn decrement(&mut self, amount: usize, true_amount: usize) {
		match self {
			Self::ByteArray(bytes) => bytes.decrement(amount, true_amount),
			Self::List(list) => list.decrement(amount, true_amount),
			Self::Compound(compound) => compound.decrement(amount, true_amount),
			Self::IntArray(ints) => ints.decrement(amount, true_amount),
			Self::LongArray(longs) => longs.decrement(amount, true_amount),
			Self::Region(region) => region.decrement(amount, true_amount),
			Self::Chunk(chunk) => chunk.decrement(amount, true_amount),
			_ => {}
		}
	}

	#[inline]
	#[must_use]
	pub fn value(&self) -> (Box<str>, bool) {
		match self {
			Self::Null => (String::new().into_boxed_str(), false),
			Self::Byte(x) => (x.value.to_string().into_boxed_str(), true),
			Self::Short(x) => (x.value.to_string().into_boxed_str(), true),
			Self::Int(x) => (x.value.to_string().into_boxed_str(), true),
			Self::Long(x) => (x.value.to_string().into_boxed_str(), true),
			Self::Float(x) => (x.value.to_string().into_boxed_str(), true),
			Self::Double(x) => (x.value.to_string().into_boxed_str(), true),
			Self::ByteArray(array) => (array.value().into_boxed_str(), false),
			Self::String(string) => (string.unwrap().to_owned().into_boxed_str(), true),
			Self::List(list) => (list.value().into_boxed_str(), false),
			Self::Compound(compound) => (compound.value().into_boxed_str(), false),
			Self::IntArray(array) => (array.value().into_boxed_str(), false),
			Self::LongArray(array) => (array.value().into_boxed_str(), false),
			Self::Chunk(chunk) => (chunk.z.to_string().into_boxed_str(), true),
			Self::Region(region) => (region.value().into_boxed_str(), false),
		}
	}

	#[inline]
	pub fn drop(&mut self, key: Option<Box<str>>, element: Self, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> DropFn {
		let height = self.height() * 16;
		match self {
			_ if *y >= height + 8 => {
				*y -= height;
				DropFn::Missed(key, element)
			}
			Self::ByteArray(bytes) => bytes.drop(key, element, y, depth, target_depth, indices),
			Self::List(list) => list.drop(key, element, y, depth, target_depth, indices),
			Self::Compound(compound) | Self::Chunk(NbtChunk { inner: compound, .. }) => compound.drop(key, element, y, depth, target_depth, indices),
			Self::IntArray(ints) => ints.drop(key, element, y, depth, target_depth, indices),
			Self::LongArray(longs) => longs.drop(key, element, y, depth, target_depth, indices),
			Self::Region(region) => region.drop(key, element, y, depth, target_depth, indices),
			_ => {
				*y = y.saturating_sub(16);
				DropFn::Missed(key, element)
			}
		}
	}

	#[inline]
	pub fn shut(&mut self) {
		match self {
			Self::ByteArray(array) => array.shut(),
			Self::List(list) => list.shut(),
			Self::Compound(compound) | Self::Chunk(NbtChunk { inner: compound, .. }) => compound.shut(),
			Self::IntArray(array) => array.shut(),
			Self::LongArray(array) => array.shut(),
			Self::Region(region) => region.shut(),
			_ => {}
		}
	}

	/// # Errors
	///
	/// * `self` cannot contain that specific variant of `Self`, i.e. `Self::NbtByte` in an `Self::NbtIntArray`
	///
	/// If any changes are made to this error list then duplicate may have to be updated as it relies on this never occurring
	#[inline]
	pub fn insert(&mut self, idx: usize, value: Self) -> Result<(), Self> {
		match self {
			Self::ByteArray(bytes) => bytes.insert(idx, value),
			Self::List(list) => list.insert(idx, value),
			Self::Compound(compound) => {
				compound.insert_full(idx, "_".to_owned(), value);
				Ok(())
			}
			Self::IntArray(ints) => ints.insert(idx, value),
			Self::LongArray(longs) => longs.insert(idx, value),
			Self::Region(region) => region.insert(idx, value),
			Self::Chunk(chunk) => {
				chunk.insert_full(idx, "_".to_owned(), value);
				Ok(())
			}
			_ => Err(value),
		}
	}

	#[inline]
	pub fn remove(&mut self, idx: usize) -> Option<Result<(Box<str>, Self), Self>> {
		Some(match self {
			Self::ByteArray(bytes) => Err(bytes.remove(idx)),
			Self::List(list) => Err(list.remove(idx)),
			Self::Compound(compound) => Ok(compound.remove_idx(idx)?),
			Self::IntArray(ints) => Err(ints.remove(idx)),
			Self::LongArray(longs) => Err(longs.remove(idx)),
			Self::Region(region) => Err(region.remove(idx)),
			Self::Chunk(chunk) => Ok(chunk.remove_idx(idx)?),
			_ => return None,
		})
	}

	#[inline]
	#[must_use]
	pub fn get(&self, idx: usize) -> Option<&Self> {
		match self {
			Self::ByteArray(bytes) => bytes.get(idx),
			Self::List(list) => list.get(idx),
			Self::Compound(compound) => compound.get(idx).map(|(_, x)| x),
			Self::IntArray(ints) => ints.get(idx),
			Self::LongArray(longs) => longs.get(idx),
			Self::Region(region) => region.get(idx),
			Self::Chunk(chunk) => chunk.get(idx).map(|(_, x)| x),
			_ => None,
		}
	}

	#[inline]
	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<&mut Self> {
		match self {
			Self::ByteArray(bytes) => bytes.get_mut(idx),
			Self::List(list) => list.get_mut(idx),
			Self::Compound(compound) => compound.get_mut(idx).map(|(_, x)| x),
			Self::IntArray(ints) => ints.get_mut(idx),
			Self::LongArray(longs) => longs.get_mut(idx),
			Self::Region(region) => region.get_mut(idx),
			Self::Chunk(chunk) => chunk.get_mut(idx).map(|(_, x)| x),
			_ => None,
		}
	}

	#[inline]
	#[must_use]
	pub unsafe fn into_chunk_unchecked(self) -> NbtChunk {
		match self {
			Self::Chunk(chunk) => chunk,
			_ => panic_unchecked("YOU SAID IT WAS A CHUNK, STOP LYING CALLER FUNCTION :((("),
		}
	}

	#[inline]
	#[must_use]
	pub unsafe fn as_chunk_unchecked(&self) -> &NbtChunk {
		match self {
			Self::Chunk(chunk) => chunk,
			_ => panic_unchecked("YOU SAID IT WAS A CHUNK, STOP LYING CALLER FUNCTION :((("),
		}
	}

	#[inline]
	#[must_use]
	pub unsafe fn as_chunk_unchecked_mut(&mut self) -> &mut NbtChunk {
		match self {
			Self::Chunk(chunk) => chunk,
			_ => panic_unchecked("YOU SAID IT WAS A CHUNK, STOP LYING CALLER FUNCTION :((("),
		}
	}

	#[inline]
	#[must_use]
	pub fn depth(&self, key: Option<&str>) -> usize {
		(match self {
			Self::Region(region) => {
				let mut max_width = region.value().width() + 4 + key.map_or(0, |x| x.width() + ": ".width());
				if region.open() {
					for child in region.children() {
						let chunk = unsafe { child.as_chunk_unchecked() };
						max_width = usize::max(max_width, child.depth(Some(&format!("{}{}", chunk.x, chunk.z))) + 4);
					}
				}
				max_width
			}
			Self::ByteArray(array) => {
				let mut max_width = array.value().width() + 4 + key.map_or(0, |x| x.width() + ": ".width());
				if array.open() {
					for child in array.children() {
						max_width = usize::max(max_width, child.depth(None) + 4);
					}
				}
				max_width
			},
			Self::List(list) => {
				let mut max_width = list.value().width() + 4 + key.map_or(0, |x| x.width() + ": ".width());
				if list.open() {
					for child in list.children() {
						max_width = usize::max(max_width, child.depth(None) + 4);
					}
				}
				max_width
			}
			Self::Chunk(NbtChunk { inner: compound, .. }) | Self::Compound(compound) => {
				let mut max_width = compound.value().width() + 4 + key.map_or(0, |x| x.width() + ": ".width());
				if compound.open() {
					for (key, child) in compound.children() {
						max_width = usize::max(max_width, child.depth(Some(key.as_ref())) + 4);
					}
				}
				max_width
			}
			Self::IntArray(array) => {
				let mut max_width = array.value().width() + 4 + key.map_or(0, |x| x.width() + ": ".width());
				if array.open() {
					for child in array.children() {
						max_width = usize::max(max_width, child.depth(None) + 4);
					}
				}
				max_width
			},
			Self::LongArray(array) => {
				let mut max_width = array.value().width() + 4 + key.map_or(0, |x| x.width() + ": ".width());
				if array.open() {
					for child in array.children() {
						max_width = usize::max(max_width, child.depth(None) + 4);
					}
				}
				max_width
			},
			Self::String(NbtString { str }) => str.width() + key.map_or(0, |x| x.width() + ": ".width()),
			_ => self.value().0.width() + key.map_or(0, |x| x.width() + ": ".width()),
		}) + 16
	}
}

impl Display for NbtElement {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::Null => write!(f, "null"),
			Self::Byte(byte) => write!(f, "{byte}"),
			Self::Short(short) => write!(f, "{short}"),
			Self::Int(int) => write!(f, "{int}"),
			Self::Long(long) => write!(f, "{long}"),
			Self::Float(float) => write!(f, "{float}"),
			Self::Double(double) => write!(f, "{double}"),
			Self::ByteArray(bytes) => write!(f, "{bytes}"),
			Self::String(string) => write!(f, "{string}"),
			Self::List(list) => write!(f, "{list}"),
			Self::Compound(compound) => write!(f, "{compound}"),
			Self::IntArray(ints) => write!(f, "{ints}"),
			Self::LongArray(longs) => write!(f, "{longs}"),
			Self::Chunk(chunk) => todo!(),
			Self::Region(region) => todo!(),
		}
	}
}

impl Debug for NbtElement {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::Null => f.write_str("null"),
			Self::Byte(byte) => Debug::fmt(byte, f),
			Self::Short(short) => Debug::fmt(short, f),
			Self::Int(int) => Debug::fmt(int, f),
			Self::Long(long) => Debug::fmt(long, f),
			Self::Float(float) => Debug::fmt(float, f),
			Self::Double(double) => Debug::fmt(double, f),
			Self::ByteArray(array) => Debug::fmt(array, f),
			Self::String(string) => Debug::fmt(string, f),
			Self::List(list) => Debug::fmt(list, f),
			Self::Compound(compound) => Debug::fmt(compound, f),
			Self::IntArray(array) => Debug::fmt(array, f),
			Self::LongArray(array) => Debug::fmt(array, f),
			Self::Chunk(chunk) => Debug::fmt(chunk, f),
			Self::Region(region) => Debug::fmt(region, f),
		}
	}
}
