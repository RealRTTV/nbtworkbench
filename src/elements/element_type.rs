use core::slice::{Iter, IterMut};
use std::fmt::{Debug, Display, Formatter};
use std::intrinsics::likely;
use std::io::BufWriter;
use std::string::String;
#[cfg(debug_assertions)]
use std::time::Instant;
use std::{fmt, fmt::Write};

use crate::decoder::Decoder;
use crate::elements::compound::NbtCompound;
use crate::elements::list::NbtList;
use crate::elements::string::NbtString;
use crate::panic_unchecked;
use crate::{array, primitive, DropFn, RenderContext, StrExt, VertexBufferBuilder};
use core::fmt::DebugList;

primitive!((0 0), { Some('b') }, NbtByte, i8, 1);
primitive!((16 0), { Some('s') }, NbtShort, i16, 2);
primitive!((32 0), { None::<char> }, NbtInt, i32, 3);
primitive!((48 0), { Some('L') }, NbtLong, i64, 4);
primitive!((64 0), { Some('f') }, NbtFloat, f32, 5);
primitive!((80 0), { Some('d') }, NbtDouble, f64, 6);
array!(Byte, NbtByte, NbtByteArray, i8, 7, 1, 'B', (96 0), (0 0));
array!(Int, NbtInt, NbtIntArray, i32, 11, 3, 'I', (112 0), (32 0));
array!(Long, NbtLong, NbtLongArray, i64, 12, 4, 'L', (0 16), (48 0));

#[derive(Clone)]
pub enum NbtElement {
	/// This variant only exists in .mca files and exists here because refactoring `Tab` is too difficult due to the common use of &mut tab.value().
	/// The solution to that would be to refactor all of that to include a list of chunks.
	/// It's far easier to check if you're pulling a chunk from somewhere to somewhere it shouldn't be than to refactor everything.
	/// Also currently this is todo
	// Chunk(NbtChunk),

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
		dbg!();
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
		#[cfg(debug_assertions)]
		let start = Instant::now();
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
		#[cfg(debug_assertions)]
		println!("{}ms for file read", Instant::now().duration_since(start).as_nanos() as f64 / 1_000_000.0);
		Some(nbt)
	}

	#[inline]
	#[must_use]
	pub fn to_file(&self) -> Option<Vec<u8>> {
		use std::io::Write;

		#[cfg(debug_assertions)]
		let start = Instant::now();
		let mut writer = BufWriter::with_capacity(65536, vec![]);
		let _ = writer.write(&[0x0A, 0x00, 0x00]);
		self.to_bytes(&mut writer);
		#[cfg(debug_assertions)]
		println!("{}ms for file write", Instant::now().duration_since(start).as_nanos() as f64 / 1_000_000.0);
		writer.into_inner().ok()
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
			_ => return None,
		})
	}

	#[inline]
	#[must_use]
	pub fn is_empty(&self) -> bool {
		self.len().map_or(false, |x| x > 0)
	}

	#[inline]
	pub fn render_icon(id: u8, x: usize, y: usize, builder: &mut VertexBufferBuilder) {
		match id {
			0 => {}
			1 => NbtByte::render_icon(x, y, builder),
			2 => NbtShort::render_icon(x, y, builder),
			3 => NbtInt::render_icon(x, y, builder),
			4 => NbtLong::render_icon(x, y, builder),
			5 => NbtFloat::render_icon(x, y, builder),
			6 => NbtDouble::render_icon(x, y, builder),
			7 => NbtByteArray::render_icon(x, y, builder),
			8 => NbtString::render_icon(x, y, builder),
			9 => NbtList::render_icon(x, y, builder),
			10 => NbtCompound::render_icon(x, y, builder),
			11 => NbtIntArray::render_icon(x, y, builder),
			_ => NbtLongArray::render_icon(x, y, builder),
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
			_ => {}
		}
	}

	#[inline]
	#[must_use]
	#[allow(clippy::type_complexity)] // a type probably shouldn't abstract what this is, like... yeah
	pub fn children(&self) -> Option<Result<Iter<'_, Self>, indexmap::map::Iter<'_, Box<str>, Self>>> {
		Some(match self {
			Self::ByteArray(array) => Ok(array.children()),
			Self::List(list) => Ok(list.children()),
			Self::Compound(compound) => Err(compound.children()),
			Self::IntArray(array) => Ok(array.children()),
			Self::LongArray(array) => Ok(array.children()),
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
			Self::Compound(compound) => compound.drop(key, element, y, depth, target_depth, indices),
			Self::IntArray(ints) => ints.drop(key, element, y, depth, target_depth, indices),
			Self::LongArray(longs) => longs.drop(key, element, y, depth, target_depth, indices),
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
			Self::Compound(compound) => compound.shut(),
			Self::IntArray(array) => array.shut(),
			Self::LongArray(array) => array.shut(),
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
			_ => return None,
		})
	}

	#[inline]
	#[must_use]
	pub fn get(&self, idx: usize) -> Option<&Self> {
		match self {
			Self::ByteArray(bytes) => bytes.children().nth(idx),
			Self::List(list) => list.children().nth(idx),
			Self::Compound(compound) => compound.children().nth(idx).map(|(_, x)| x),
			Self::IntArray(ints) => ints.children().nth(idx),
			Self::LongArray(longs) => longs.children().nth(idx),
			_ => None,
		}
	}

	#[inline]
	#[must_use]
	pub fn get_mut(&mut self, idx: usize) -> Option<&mut Self> {
		match self {
			Self::ByteArray(bytes) => bytes.children_mut().nth(idx),
			Self::List(list) => list.children_mut().nth(idx),
			Self::Compound(compound) => compound.children_mut().nth(idx).map(|(_, x)| x),
			Self::IntArray(ints) => ints.children_mut().nth(idx),
			Self::LongArray(longs) => longs.children_mut().nth(idx),
			_ => None,
		}
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
		}
	}
}
