use compact_str::{CompactString, ToCompactString};
use regex::{Regex, RegexBuilder};
use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};
use std::hint::likely;
use std::mem::MaybeUninit;
use crate::render::VertexBufferBuilder;

#[cfg(target_arch = "wasm32")]
pub use crate::wasm::{get_clipboard, now, set_clipboard};

#[must_use]
#[cfg(not(target_arch = "wasm32"))]
pub fn get_clipboard() -> Option<String> {
	cli_clipboard::get_contents().ok()
}

#[cfg(not(target_arch = "wasm32"))]
pub fn set_clipboard(value: String) -> bool {
	cli_clipboard::set_contents(value).is_ok()
}

#[must_use]
#[cfg(not(target_arch = "wasm32"))]
pub fn now() -> std::time::Duration {
    std::time::SystemTime::UNIX_EPOCH.elapsed().unwrap_or_else(|e| e.duration())
}

#[must_use]
pub fn create_regex(mut str: String, case_sensitive: bool) -> Option<Regex> {
	let flags = 'a: {
		if !str.starts_with("/") {
			break 'a 0;
		}

		str = str.split_off(1);

		let mut flags = 0_u8;
		while let Some(char) = str.pop() {
			match char {
				'i' => flags |= 0b000001,
				'g' => flags |= 0b000010,
				'm' => flags |= 0b000100,
				's' => flags |= 0b001000,
				'u' => flags |= 0b010000,
				'y' => flags |= 0b100000,
				'/' => break,
				_ => return None
			}
		}
		flags
	};

	RegexBuilder::new(&str)
		.case_insensitive((flags & 0b1 > 0) || !case_sensitive)
		.multi_line(flags & 0b100 > 0)
		.dot_matches_new_line(flags & 0b1000 > 0)
		.unicode(flags & 0b10000 > 0)
		.swap_greed(flags & 0b10000 > 0)
		.build()
		.ok()
}

#[must_use]
pub fn split_lines<const MAX_WIDTH: usize>(s: String) -> Vec<String> {
	let mut lines = Vec::new();
	let mut current_line = String::new();
	let mut is_previous_byte_ascii_whitespace = true;
	for word in s.as_bytes().split_inclusive(|byte| {
		let is_ascii_whitespace = byte.is_ascii_whitespace();
		let is_previous_byte_ascii_whitespace = core::mem::replace(&mut is_previous_byte_ascii_whitespace, is_ascii_whitespace);
		!is_previous_byte_ascii_whitespace && is_ascii_whitespace
	}).filter(|slice| !slice.is_empty()).map(|slice| unsafe { std::str::from_utf8_unchecked(slice) }) {
		if current_line.width() + word.width() > MAX_WIDTH {
			lines.push(current_line.trim_ascii_end().to_string());
			current_line = String::new();
		}
		current_line += word;
	}
	let trimmed = current_line.trim_ascii_end();
	if !trimmed.is_empty() {
		lines.push(trimmed.to_string());
	}

	lines
}

#[must_use]
pub fn nth(n: usize) -> String {
    use std::fmt::Write as _;

	let mut buf = String::with_capacity(n.checked_ilog10().map_or(1, |x| x + 1) as usize + 2);
	let _ = write!(&mut buf, "{n}");
	if n / 10 % 10 == 1 {
		buf.push_str("th");
	} else {
		match n % 10 {
			1 => buf.push_str("st"),
			2 => buf.push_str("nd"),
			3 => buf.push_str("rd"),
			_ => buf.push_str("th"),
		}
	}
	buf
}

#[must_use]
pub fn encompasses_or_equal<T: Ord>(outer: &[T], inner: &[T]) -> bool {
	outer.len() <= inner.len() && outer == &inner[..outer.len()]
}

#[must_use]
pub fn encompasses<T: Ord>(outer: &[T], inner: &[T]) -> bool {
	outer.len() < inner.len() && outer == &inner[..outer.len()]
}

#[must_use]
pub const fn is_utf8_char_boundary(x: u8) -> bool { (x as i8) >= -0x40 }

// importantly, no underscores
#[must_use]
pub fn is_jump_char_boundary(x: u8) -> bool { b" \t\r\n/\\()\"'-.,:;<>~!@#$%^&*|+=[]{}~?|".contains(&x) }

pub struct SinglyLinkedNode<T> {
	value: T,
	prev: Option<Box<SinglyLinkedNode<T>>>,
}

#[must_use]
pub fn smoothstep64(x: f64) -> f64 {
	let x = x.clamp(0.0, 1.0);
	3.0 * x * x - 2.0 * x * x * x
}

#[must_use]
pub const fn valid_unescaped_char(byte: u8) -> bool { matches!(byte, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'-' | b'.' | b'+') }

#[must_use]
pub const fn valid_starting_char(byte: u8) -> bool { matches!(byte, b'A'..=b'Z' | b'a'..=b'z' | b'_') }

#[must_use]
pub fn combined_two_sorted<T: Ord>(a: Box<[T]>, b: Box<[T]>) -> Vec<T> {
	let mut a = unsafe { core::mem::transmute::<_, Box<[MaybeUninit<T>]>>(a) };
	let mut a_idx = 0;
	let mut b = unsafe { core::mem::transmute::<_, Box<[MaybeUninit<T>]>>(b) };
	let mut b_idx = 0;
	let mut out = Vec::with_capacity(a.len() + b.len());
	let spare = out.spare_capacity_mut();
	let mut idx = 0;

	while a_idx < a.len() && b_idx < b.len() {
		let a = &mut a[a_idx];
		let b = &mut b[b_idx];

		// SAFETY: the values are all initialized initially, once this is uninit memory, we go to the next `idx` so we never read it again
		match unsafe { a.assume_init_ref().cmp(b.assume_init_ref()) } {
			Ordering::Less => {
				spare[idx].write(unsafe { core::mem::replace(a, MaybeUninit::uninit()).assume_init() });
				a_idx += 1;
				idx += 1;
			}
			Ordering::Equal => {
				spare[idx].write(unsafe { core::mem::replace(a, MaybeUninit::uninit()).assume_init() });
				drop(unsafe { core::mem::replace(b, MaybeUninit::uninit()).assume_init() });
				a_idx += 1;
				b_idx += 1;
				idx += 1;
			}
			Ordering::Greater => {
				spare[idx].write(unsafe { core::mem::replace(b, MaybeUninit::uninit()).assume_init() });
				b_idx += 1;
				idx += 1;
			}
		}
	}

	unsafe { spare.as_mut_ptr().add(idx).copy_from_nonoverlapping(a.as_ptr().add(a_idx), a.len() - a_idx) }
	idx += a.len() - a_idx;
	unsafe { spare.as_mut_ptr().add(idx).copy_from_nonoverlapping(b.as_ptr().add(b_idx), b.len() - b_idx) }
	idx += b.len() - b_idx;

	// SAFETY: all the values used have been copied over, all the unused values have been dropped.
	drop(a);
	drop(b);

	// we have written `idx` times
	unsafe { out.set_len(idx); }
	out
}

pub struct LinkedQueue<T> {
	tail: Option<Box<SinglyLinkedNode<T>>>,
	len: usize,
}

impl<'a, T> IntoIterator for &'a LinkedQueue<T> {
	type Item = &'a T;
	type IntoIter = LinkedQueueIter<'a, T>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}

impl<T: Debug> Debug for LinkedQueue<T> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "LinkedQueue [")?;
		for element in self {
			write!(f, "{element:?}")?;
		}
		write!(f, "]")
	}
}

// perf enhancement
impl<T> Drop for LinkedQueue<T> {
	fn drop(&mut self) {
		while let Some(box SinglyLinkedNode { value: _, mut prev }) = self.tail.take() {
			// take is not required, but then intellij gets upset.
			self.tail = prev.take();
		}
	}
}

impl<T: Clone> Clone for LinkedQueue<T> {
	fn clone(&self) -> Self {
		let mut new = Self::new();
		for t in self.iter().cloned().collect::<Vec<_>>() {
			new.push(t);
		}
		new
	}
}

impl<T> LinkedQueue<T> {
	#[must_use]
	pub const fn new() -> Self { Self { tail: None, len: 0 } }

	pub fn push(&mut self, value: T) {
		self.tail = Some(Box::new(SinglyLinkedNode {
			value,
			prev: self.tail.take(),
		}));
		self.len += 1;
	}

	#[must_use]
	pub fn pop(&mut self) -> Option<T> {
		if let Some(box SinglyLinkedNode { value, prev: tail }) = self.tail.take() {
			self.tail = tail;
			self.len -= 1;
			Some(value)
		} else {
			None
		}
	}

	#[must_use]
	pub fn get(&self) -> Option<&T> { self.tail.as_ref().map(|x| &x.value) }

	#[must_use]
	pub fn get_mut(&mut self) -> Option<&mut T> { self.tail.as_mut().map(|x| &mut x.value) }

	#[must_use]
	pub const fn is_empty(&self) -> bool { self.len == 0 }

	#[must_use]
	pub const fn len(&self) -> usize { self.len }

	pub fn clear(&mut self) {
		while let Some(box SinglyLinkedNode { value: _, mut prev }) = self.tail.take() {
			// take is not required, but then intellij gets upset.
			self.tail = prev.take();
		}
		self.len = 0;
	}

	#[must_use]
	pub fn iter(&self) -> LinkedQueueIter<'_, T> {
		LinkedQueueIter {
			tail: &self.tail,
		}
	}
}

pub struct LinkedQueueIter<'a, T> {
	tail: &'a Option<Box<SinglyLinkedNode<T>>>,
}

impl<'a, T> Iterator for LinkedQueueIter<'a, T> {
	type Item = &'a T;

	fn next(&mut self) -> Option<Self::Item> {
		if let Some(SinglyLinkedNode { value, prev }) = self.tail.as_deref() {
			self.tail = prev;
			Some(value)
		} else {
			None
		}
	}
}

pub trait StrExt {
	fn snbt_string_read(&self) -> Result<(CompactString, &str), usize>;

	#[must_use]
	fn needs_escape(&self) -> bool;

	#[must_use]
	fn width(&self) -> usize;

	#[must_use]
	fn contains_ignore_ascii_case(&self, other: &Self) -> bool;

	#[must_use]
	fn replace_ignore_ascii_case(&self, from: &Self, to: &Self) -> String;
}

impl StrExt for str {

	#[optimize(speed)]
	#[allow(clippy::too_many_lines)]
	fn snbt_string_read(mut self: &Self) -> Result<(CompactString, &Self), usize> {
		const MAPPING: [Option<u8>; 256] = {
			let mut initial = [Option::<u8>::None; 256];
			initial[b'0' as usize] = Some(0);
			initial[b'1' as usize] = Some(1);
			initial[b'2' as usize] = Some(2);
			initial[b'3' as usize] = Some(3);
			initial[b'4' as usize] = Some(4);
			initial[b'5' as usize] = Some(5);
			initial[b'6' as usize] = Some(6);
			initial[b'7' as usize] = Some(7);
			initial[b'8' as usize] = Some(8);
			initial[b'9' as usize] = Some(9);
			initial[b'a' as usize] = Some(10);
			initial[b'b' as usize] = Some(11);
			initial[b'c' as usize] = Some(12);
			initial[b'd' as usize] = Some(13);
			initial[b'e' as usize] = Some(14);
			initial[b'f' as usize] = Some(15);
			initial[b'A' as usize] = Some(10);
			initial[b'B' as usize] = Some(11);
			initial[b'C' as usize] = Some(12);
			initial[b'D' as usize] = Some(13);
			initial[b'E' as usize] = Some(14);
			initial[b'F' as usize] = Some(15);
			initial
		};

		if !self.starts_with('"') && !self.starts_with('\'') {
			let end_idx = self
				.char_indices()
				.find(|(_, c)| !valid_unescaped_char(*c as u8))
				.map_or(self.len(), |(idx, _)| idx);
			let (s, s2) = unsafe {
				(
					self.get_unchecked(..end_idx),
					self.get_unchecked(end_idx..self.len()),
				)
			};
			if s.needs_escape() { return Err(s2.len()) }
			Ok((s.to_compact_string(), s2))
		} else {
			let enclosing = self.as_bytes().first().copied().ok_or(self.len())?;
			self = unsafe { self.get_unchecked(1..) };
			let (end, len) = 'a: {
				let mut backslash = false;
				let mut sub = 0;
				let mut iter = self.bytes().enumerate();
				while let Some((idx, byte)) = iter.next() {
					if backslash {
						if byte == b'x' {
							if let Ok([(_, a), _]) = iter.next_chunk::<2>()
								&& let Some(a) = MAPPING[a as usize]
							{
								if a < 8 {
									sub += 3;
								} else {
									sub += 2;
								}
							} else {
								return Err(self.len() - idx);
							}
						} else if byte == b'u' {
							if let Ok([(_, _), (_, b), (_, c), _]) = iter.next_chunk::<4>()
								&& let Some(b) = MAPPING[b as usize]
								&& let Some(c) = MAPPING[c as usize]
							{
								if b < 8 {
									if c < 8 {
										sub += 5;
									} else {
										sub += 4;
									}
								} else {
									sub += 3;
								}
							} else {
								return Err(self.len() - idx);
							}
						} else {
							sub += 1;
						}
					}
					if byte == enclosing {
						if backslash {
							backslash = false;
						} else {
							break 'a (idx, idx - sub);
						}
					} else if byte == b'\\' {
						backslash = !backslash;
					} else {
						backslash = false;
					}
				}
				return Err(self.len());
			};
			let mut out = CompactString::with_capacity(len);
			let ptr = out.as_mut_ptr();
			unsafe {
				out.set_len(len);
			}
			let mut buf_len = 0;
			let mut backslash = false;
			let mut iter = self.bytes();
			while let Some(mut byte) = iter.next() {
				if byte == b'\\' {
					if backslash {
						backslash = false;
					} else {
						backslash = true;
						continue;
					}
				} else if byte == enclosing {
					if backslash {
						backslash = false;
					} else {
						break;
					}
				} else if byte == b'n' {
					if backslash {
						backslash = false;
						byte = b'\n';
					}
				} else if byte == b'r' {
					if backslash {
						backslash = false;
						byte = b'\r';
					}
				} else if byte == b'0' {
					if backslash {
						backslash = false;
						byte = b'\0';
					}
				} else if byte == b'b' {
					if backslash {
						backslash = false;
						byte = b'\x08';
					}
				} else if byte == b's' {
					if backslash {
						backslash = false;
						byte = b'\x20';
					}
				} else if byte == b't' {
					if backslash {
						backslash = false;
						byte = b'\x09';
					}
				} else if byte == b'f' {
					if backslash {
						backslash = false;
						byte = b'\x0C';
					}
				} else if byte == b'x' {
					if backslash {
						backslash = false;
						if let Ok([a, b]) = iter.next_chunk::<2>()
							&& let Some(a) = MAPPING[a as usize]
							&& let Some(b) = MAPPING[b as usize]
						{
							let char = ((a << 4) | b) as char;
							let len = char.len_utf8();
							char.encode_utf8(unsafe { core::slice::from_raw_parts_mut(ptr.add(buf_len), len) });
							buf_len += len;
							continue;
						} else {
							return Err(self.len());
						}
					}
				} else if byte == b'u' {
					if backslash {
						backslash = false;
						if let Ok([a, b, c, d]) = iter.next_chunk::<4>()
							&& let Some(a) = MAPPING[a as usize]
							&& let Some(b) = MAPPING[b as usize]
							&& let Some(c) = MAPPING[c as usize]
							&& let Some(d) = MAPPING[d as usize]
							&& let Some(char) = char::from_u32(((a as u32) << 12) | ((b as u32) << 8) | ((c as u32) << 4) | (d as u32))
						{
							let len = char.len_utf8();
							char.encode_utf8(unsafe { core::slice::from_raw_parts_mut(ptr.add(buf_len), len) });
							buf_len += len;
							continue;
						} else {
							return Err(self.len());
						}
					}
				} else if byte == b'U' {
					if backslash {
						backslash = false;
						if let Ok([a, b, c, d, e, f, g, h]) = iter.next_chunk::<8>()
							&& let Some(a) = MAPPING[a as usize]
							&& let Some(b) = MAPPING[b as usize]
							&& let Some(c) = MAPPING[c as usize]
							&& let Some(d) = MAPPING[d as usize]
							&& let Some(e) = MAPPING[e as usize]
							&& let Some(f) = MAPPING[f as usize]
							&& let Some(g) = MAPPING[g as usize]
							&& let Some(h) = MAPPING[h as usize]
							&& let Some(char) = char::from_u32(((a as u32) << 28) | ((b as u32) << 24) | ((c as u32) << 20) | ((d as u32) << 16) | ((e as u32) << 12) | ((f as u32) << 8) | ((g as u32) << 4) | (h as u32))
						{
							let len = char.len_utf8();
							char.encode_utf8(unsafe { core::slice::from_raw_parts_mut(ptr.add(buf_len), len) });
							buf_len += len;
							continue;
						} else {
							return Err(self.len());
						}
					}
				} else if backslash {
					return Err(self.len());
				}

				unsafe {
					*ptr.add(buf_len) = byte;
					buf_len += 1;
				}
			}

			if self.len() < end + 1 { return Err(self.len()) };
			unsafe { Ok((out, self.get_unchecked((end + 1)..))) }
		}
	}

	fn needs_escape(&self) -> bool { self.as_bytes().first().copied().is_some_and(valid_starting_char) || !self.bytes().all(valid_unescaped_char) }

	fn width(&self) -> usize {
		self.chars().map(CharExt::width).sum()
	}

	fn contains_ignore_ascii_case(&self, other: &Self) -> bool {
		let haystack = self.as_bytes();
		let needle = other.as_bytes();

		if needle.len() > haystack.len() {
			return false;
		}

		for offset in 0..(haystack.len() - needle.len() + 1) {
			let haystack_partition = &haystack[offset..offset + needle.len()];
			if haystack_partition.eq_ignore_ascii_case(needle) {
				return true;
			}
		}

		false
	}

	fn replace_ignore_ascii_case(&self, from: &Self, to: &Self) -> String {
		let haystack = self.as_bytes();
		let needle = from.as_bytes();
		let replacement = to.as_bytes();

		if needle.len() > haystack.len() {
			return self.to_owned();
		}

		let mut buf = Vec::with_capacity(if replacement.len() >= needle.len() { haystack.len() } else { 0 });

		let mut offset = 0;
		while offset < haystack.len() - needle.len() + 1 {
			let haystack_partition = &haystack[offset..offset + needle.len()];
			if haystack_partition.eq_ignore_ascii_case(needle) {
				buf.extend_from_slice(replacement);
				offset += needle.len();
			} else {
				buf.push(haystack[offset]);
				offset += 1;
			}
		}

		unsafe { String::from_utf8_unchecked(buf) }
	}
}

pub trait CharExt {
	#[must_use]
	fn width(self) -> usize;
}

impl CharExt for char {
	fn width(self) -> usize {
		if (self as u32) < 56832 {
			VertexBufferBuilder::CHAR_WIDTH[self as usize] as usize
		} else {
			VertexBufferBuilder::CHAR_WIDTH[56829] as usize
		}
	}
}

#[must_use]
pub const fn width_ascii(s: &str) -> usize {
	let mut width = 0;
	let mut i = 0;
	while i < s.len() {
		if s.as_bytes()[i].is_ascii() {
			width += VertexBufferBuilder::CHAR_WIDTH[s.as_bytes()[i] as usize] as usize;
			i += 1;
		}
	}
	width
}

pub fn drop_on_separate_thread<T: 'static + Send>(t: T) {
	#[cfg(not(target_arch = "wasm32"))]
	std::thread::Builder::new().stack_size(1_048_576 * 64 /*64MiB*/).spawn(move || drop(t)).expect("Failed to spawn thread");
	#[cfg(target_arch = "wasm32")]
	drop(t)
}

#[derive(Copy, Clone, Eq)]
pub struct Vec2u {
	pub x: usize,
	pub y: usize,
}

impl Debug for Vec2u {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "({x},{y})", x = self.x, y = self.y)
	}
}

impl Vec2u {
	pub const fn new(x: usize, y: usize) -> Self { Self { x, y } }

	pub const fn wrapping_sub(self, rhs: Self) -> Self {
		Self {
			x: self.x.wrapping_sub(rhs.x),
			y: self.y.wrapping_sub(rhs.y),
		}
	}

	pub const fn saturating_sub(self, rhs: Self) -> Self {
		Self {
			x: self.x.saturating_sub(rhs.x),
			y: self.y.saturating_sub(rhs.y),
		}
	}
}

impl<T: Into<(usize, usize)> + Clone> PartialEq<T> for Vec2u {
	fn eq(&self, other: &T) -> bool {
		let (x, y) = other.clone().into();
		(self.x == x) & (self.y == y)
	}
}

impl From<(usize, usize)> for Vec2u {
	fn from(value: (usize, usize)) -> Self {
		let (x, y) = value;
		Self::new(x, y)
	}
}

impl From<Vec2u> for (usize, usize) {
	fn from(val: Vec2u) -> Self { (val.x, val.y) }
}

impl<T: Into<(usize, usize)>> std::ops::Add<T> for Vec2u {
	type Output = Self;

	fn add(self, rhs: T) -> Self::Output {
		let (x, y) = rhs.into();
		Self {
			x: self.x + x,
			y: self.y + y,
		}
	}
}

impl<T: Into<(usize, usize)>> std::ops::AddAssign<T> for Vec2u {
	fn add_assign(&mut self, rhs: T) {
		let (x, y) = rhs.into();
		self.x += x;
		self.y += y;
	}
}

impl<T: Into<(usize, usize)>> std::ops::Sub<T> for Vec2u {
	type Output = Self;

	fn sub(self, rhs: T) -> Self::Output {
		let (x, y) = rhs.into();
		Self {
			x: self.x - x,
			y: self.y - y,
		}
	}
}

impl<T: Into<(usize, usize)>> std::ops::SubAssign<T> for Vec2u {
	fn sub_assign(&mut self, rhs: T) {
		let (x, y) = rhs.into();
		self.x = self.x.wrapping_sub(x);
		self.y = self.x.wrapping_sub(y);
	}
}

#[derive(Copy, Clone)]
pub struct AxisAlignedBoundingBox {
	low: Vec2u,
	high: Vec2u,
}

impl AxisAlignedBoundingBox {
	#[must_use]
	pub fn new(x0: usize, x1: usize, y0: usize, y1: usize) -> Self {
		let (x0, x1) = if likely(x0 < x1) { (x0, x1) } else { (x1, x0) };
		let (y0, y1) = if likely(y0 < y1) { (y0, y1) } else { (y1, y0) };
		Self {
			low: Vec2u::new(x0, y0),
			high: Vec2u::new(x1, y1),
		}
	}
	
	#[must_use]
	pub fn contains(self, point: Vec2u) -> bool {
		let Self { low: Vec2u { x: x0, y: y0 }, high: Vec2u { x: x1, y: y1 } } = self;
		let Vec2u { x, y } = point;
		x0 <= x && x <= x1 && y0 <= y && y <= y1
	}
	
	#[must_use]
	pub fn low(self) -> Vec2u {
		self.low
	}

	#[must_use]
	pub fn high(self) -> Vec2u {
		self.high
	}
}
