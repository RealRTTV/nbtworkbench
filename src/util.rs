use core::time::Duration;
use std::alloc::{Allocator, Layout};
use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};
use std::hint::likely;
use std::{io, iter};
use std::mem::MaybeUninit;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};

use compact_str::{CompactString, ToCompactString};
use regex::{Regex, RegexBuilder};
use thiserror::Error;
use winit::dpi::{PhysicalPosition, PhysicalSize};

use crate::render::vertex_buffer_builder::VertexBufferBuilder;
#[cfg(target_arch = "wasm32")]
pub use crate::wasm::{get_clipboard, set_clipboard};

#[derive(Debug, Error)]
pub enum ClipboardError {
	#[error("Failed to get clipboard: {0}")]
	ClipboardFailed(String),
}

#[cfg(not(target_arch = "wasm32"))]
pub fn get_clipboard() -> Result<String, ClipboardError> { cli_clipboard::get_contents().map_err(|e| ClipboardError::ClipboardFailed(e.to_string())) }

#[cfg(not(target_arch = "wasm32"))]
pub fn set_clipboard(value: String) -> bool { cli_clipboard::set_contents(value).is_ok() }

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Timestamp {
	since_epoch: Duration,
}

impl Timestamp {
	pub const UNIX_EPOCH: Self = Self { since_epoch: Duration::ZERO };

	#[must_use]
	#[cfg(not(target_arch = "wasm32"))]
	pub fn now() -> Self {
		Self {
			since_epoch: std::time::SystemTime::UNIX_EPOCH.elapsed().unwrap_or_else(|e| e.duration()),
		}
	}

	#[must_use]
	#[cfg(target_arch = "wasm32")]
	pub fn now() -> Self {
		Self {
			since_epoch: Duration::from_millis(web_sys::js_sys::Date::now() as u64),
		}
	}

	#[must_use]
	pub fn elapsed(self) -> Duration { Self::now() - self }

	#[must_use]
	pub const fn saturating_sub(self, rhs: Self) -> Duration { self.since_epoch.saturating_sub(rhs.since_epoch) }
}

impl Sub for Timestamp {
	type Output = Duration;

	fn sub(self, rhs: Self) -> Self::Output { self.since_epoch.sub(rhs.since_epoch) }
}

impl Sub<Duration> for Timestamp {
	type Output = Self;

	fn sub(self, rhs: Duration) -> Self::Output { Self { since_epoch: self.since_epoch - rhs } }
}

impl Add<Duration> for Timestamp {
	type Output = Self;

	fn add(self, rhs: Duration) -> Self::Output { Self { since_epoch: self.since_epoch + rhs } }
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
				_ => return None,
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
	for word in s
		.as_bytes()
		.split_inclusive(|byte| {
			let is_ascii_whitespace = byte.is_ascii_whitespace();
			let is_previous_byte_ascii_whitespace = core::mem::replace(&mut is_previous_byte_ascii_whitespace, is_ascii_whitespace);
			!is_previous_byte_ascii_whitespace && is_ascii_whitespace
		})
		.filter(|slice| !slice.is_empty())
		.map(|slice| unsafe { std::str::from_utf8_unchecked(slice) })
	{
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
pub fn encompasses_or_equal<T: Ord>(outer: &[T], inner: &[T]) -> bool { outer.len() <= inner.len() && outer == &inner[..outer.len()] }

#[must_use]
pub fn encompasses<T: Ord>(outer: &[T], inner: &[T]) -> bool { outer.len() < inner.len() && outer == &inner[..outer.len()] }

#[must_use]
pub fn is_parent_for<T: Ord>(parent: &[T], child: &[T]) -> bool { parent.len() == child.len() + 1 && &parent[..child.len()] == child }

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
pub fn smoothstep(x: f64) -> f64 {
	let x = x.clamp(0.0, 1.0);
	3.0 * x * x - 2.0 * x * x * x
}

#[must_use]
pub const fn valid_unescaped_char(byte: u8) -> bool { matches!(byte, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'-' | b'.' | b'+') }

#[must_use]
pub const fn valid_starting_char(byte: u8) -> bool { matches!(byte, b'A'..=b'Z' | b'a'..=b'z' | b'_') }

/// # Safety
/// `a` or `b` must not contain values repeated (such that `Ord::cmp()` returns Ordering::Equal) between elements within their own set
#[must_use]
pub unsafe fn union_two_sorted_no_duplicates<T: Ord>(a: Vec<T>, b: Vec<T>) -> Vec<T> {
	let (a_root_ptr, a_len, a_cap, a_alloc) = a.into_parts_with_alloc();
	let mut a_ptr = a_root_ptr.as_ptr();
	let a_end_ptr = unsafe { a_ptr.add(a_len) };

	let (b_root_ptr, b_len, b_cap, b_alloc) = b.into_parts_with_alloc();
	let mut b_ptr = b_root_ptr.as_ptr();
	let b_end_ptr = unsafe { b_ptr.add(b_len) };

	let mut out = unsafe { Vec::try_with_capacity(a_len + b_len).unwrap_unchecked() };

	while a_ptr < a_end_ptr && b_ptr < b_end_ptr {
		let a = &mut *a_ptr;
		let b = &mut *b_ptr;

		match Ord::cmp(a, b) {
			Ordering::Less => {
				out.push(a_ptr.cast::<MaybeUninit<T>>().replace(MaybeUninit::uninit()).assume_init());
				a_ptr = a_ptr.add(1);
			}
			Ordering::Equal => {
				out.push(a_ptr.cast::<MaybeUninit<T>>().replace(MaybeUninit::uninit()).assume_init());
				b_ptr.cast::<MaybeUninit<T>>().replace(MaybeUninit::uninit()).assume_init_drop();
				a_ptr = a_ptr.add(1);
				b_ptr = b_ptr.add(1);
			}
			Ordering::Greater => {
				out.push(b_ptr.cast::<MaybeUninit<T>>().replace(MaybeUninit::uninit()).assume_init());
				b_ptr = b_ptr.add(1);
			}
		}
	}

	if a_ptr < a_end_ptr {
		let remaining = a_end_ptr.offset_from_unsigned(a_ptr);
		out.as_mut_ptr().copy_from_nonoverlapping(a_ptr, remaining);
		out.set_len(out.len() + remaining);
	} else {
		let remaining = b_end_ptr.offset_from_unsigned(b_ptr);
		out.as_mut_ptr().copy_from_nonoverlapping(b_ptr, remaining);
		out.set_len(out.len() + remaining);
	}

	a_alloc.deallocate(a_root_ptr.cast::<u8>(), Layout::array::<T>(a_cap).unwrap_unchecked());
	b_alloc.deallocate(b_root_ptr.cast::<u8>(), Layout::array::<T>(b_cap).unwrap_unchecked());

	out
}

/// # Safety
/// `a` or `b` must not contain values repeated (such that `Ord::cmp()` returns Ordering::Equal) between elements within their own set
#[must_use]
pub unsafe fn intersection_two_sorted_no_duplicates<T: Ord>(a: Vec<T>, b: Vec<T>) -> Vec<T> {
	let (a_root_ptr, a_len, a_cap, a_alloc) = a.into_parts_with_alloc();
	let mut a_ptr = a_root_ptr.as_ptr();
	let a_end_ptr = a_ptr.add(a_len);

	let (b_root_ptr, b_len, b_cap, b_alloc) = b.into_parts_with_alloc();
	let mut b_ptr = b_root_ptr.as_ptr();
	let b_end_ptr = b_ptr.add(b_len);

	let mut out = Vec::with_capacity(a_len + b_len);

	while a_ptr < a_end_ptr && b_ptr < b_end_ptr {
		let a = &mut *a_ptr;
		let b = &mut *b_ptr;

		match Ord::cmp(a, b) {
			Ordering::Less => {
				a_ptr.cast::<MaybeUninit<T>>().replace(MaybeUninit::uninit()).assume_init_drop();
				a_ptr = a_ptr.add(1);
			}
			Ordering::Equal => {
				out.push(a_ptr.cast::<MaybeUninit<T>>().replace(MaybeUninit::uninit()).assume_init());
				b_ptr.cast::<MaybeUninit<T>>().replace(MaybeUninit::uninit()).assume_init_drop();
				a_ptr = a_ptr.add(1);
				b_ptr = b_ptr.add(1);
			}
			Ordering::Greater => {
				b_ptr.cast::<MaybeUninit<T>>().replace(MaybeUninit::uninit()).assume_init_drop();
				b_ptr = b_ptr.add(1);
			}
		}
	}

	a_alloc.deallocate(a_root_ptr.cast::<u8>(), Layout::array::<T>(a_cap).unwrap_unchecked());
	b_alloc.deallocate(b_root_ptr.cast::<u8>(), Layout::array::<T>(b_cap).unwrap_unchecked());

	out
}

/// # Safety
/// `a` or `b` must not contain values repeated (such that `Ord::cmp()` returns Ordering::Equal) between elements within their own set
#[must_use]
pub unsafe fn symmetric_difference_two_sorted_no_duplicates<T: Ord>(a: Vec<T>, b: Vec<T>) -> Vec<T> {
	let (a_root_ptr, a_len, a_cap, a_alloc) = a.into_parts_with_alloc();
	let mut a_ptr = a_root_ptr.as_ptr();
	let a_end_ptr = a_ptr.add(a_len);

	let (b_root_ptr, b_len, b_cap, b_alloc) = b.into_parts_with_alloc();
	let mut b_ptr = b_root_ptr.as_ptr();
	let b_end_ptr = b_ptr.add(b_len);

	let mut out = Vec::with_capacity(a_len + b_len);

	while a_ptr < a_end_ptr && b_ptr < b_end_ptr {
		let a = &mut *a_ptr;
		let b = &mut *b_ptr;

		match Ord::cmp(a, b) {
			Ordering::Less => {
				out.push(a_ptr.cast::<MaybeUninit<T>>().replace(MaybeUninit::uninit()).assume_init());
				a_ptr = a_ptr.add(1);
			}
			Ordering::Equal => {
				a_ptr.cast::<MaybeUninit<T>>().replace(MaybeUninit::uninit()).assume_init_drop();
				b_ptr.cast::<MaybeUninit<T>>().replace(MaybeUninit::uninit()).assume_init_drop();
				a_ptr = a_ptr.add(1);
				b_ptr = b_ptr.add(1);
			}
			Ordering::Greater => {
				out.push(b_ptr.cast::<MaybeUninit<T>>().replace(MaybeUninit::uninit()).assume_init());
				b_ptr = b_ptr.add(1);
			}
		}
	}

	if a_ptr < a_end_ptr {
		let remaining = a_end_ptr.offset_from_unsigned(a_ptr);
		out.as_mut_ptr().copy_from_nonoverlapping(a_ptr, remaining);
		out.set_len(out.len() + remaining);
	} else {
		let remaining = b_end_ptr.offset_from_unsigned(b_ptr);
		out.as_mut_ptr().copy_from_nonoverlapping(b_ptr, remaining);
		out.set_len(out.len() + remaining);
	}

	a_alloc.deallocate(a_root_ptr.cast::<u8>(), Layout::array::<T>(a_cap).unwrap_unchecked());
	b_alloc.deallocate(b_root_ptr.cast::<u8>(), Layout::array::<T>(b_cap).unwrap_unchecked());

	out
}

pub struct LinkedQueue<T> {
	tail: Option<Box<SinglyLinkedNode<T>>>,
	len: usize,
}

impl<'a, T> IntoIterator for &'a LinkedQueue<T> {
	type Item = &'a T;
	type IntoIter = LinkedQueueIter<'a, T>;

	fn into_iter(self) -> Self::IntoIter { self.iter() }
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
		self.tail = Some(Box::new(SinglyLinkedNode { value, prev: self.tail.take() }));
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
	pub fn iter(&self) -> LinkedQueueIter<'_, T> { LinkedQueueIter { tail: &self.tail } }
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
	fn snbt_string_read(mut self: &Self) -> Result<(CompactString, &Self), usize> {
		const MAPPING: [Option<u8>; 256] = {
			let mut mapping = [Option::<u8>::None; 256];
			mapping[b'0' as usize] = Some(0);
			mapping[b'1' as usize] = Some(1);
			mapping[b'2' as usize] = Some(2);
			mapping[b'3' as usize] = Some(3);
			mapping[b'4' as usize] = Some(4);
			mapping[b'5' as usize] = Some(5);
			mapping[b'6' as usize] = Some(6);
			mapping[b'7' as usize] = Some(7);
			mapping[b'8' as usize] = Some(8);
			mapping[b'9' as usize] = Some(9);
			mapping[b'a' as usize] = Some(10);
			mapping[b'b' as usize] = Some(11);
			mapping[b'c' as usize] = Some(12);
			mapping[b'd' as usize] = Some(13);
			mapping[b'e' as usize] = Some(14);
			mapping[b'f' as usize] = Some(15);
			mapping[b'A' as usize] = Some(10);
			mapping[b'B' as usize] = Some(11);
			mapping[b'C' as usize] = Some(12);
			mapping[b'D' as usize] = Some(13);
			mapping[b'E' as usize] = Some(14);
			mapping[b'F' as usize] = Some(15);
			mapping
		};

		if !self.starts_with('"') && !self.starts_with('\'') {
			let end_idx = self.char_indices().find(|(_, c)| !valid_unescaped_char(*c as u8)).map_or(self.len(), |(idx, _)| idx);
			let (s, s2) = unsafe { (self.get_unchecked(..end_idx), self.get_unchecked(end_idx..self.len())) };
			if s.needs_escape() {
				return Err(s2.len())
			}
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

			if self.len() < end + 1 {
				return Err(self.len())
			};
			unsafe { Ok((out, self.get_unchecked((end + 1)..))) }
		}
	}

	fn needs_escape(&self) -> bool { self.as_bytes().first().copied().is_some_and(valid_starting_char) || !self.bytes().all(valid_unescaped_char) }

	fn width(&self) -> usize { self.chars().map(CharExt::width).sum() }

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

pub fn reorder<T>(data: &mut [T], mapping: impl Into<Box<[usize]>>) -> Result<(), ReorderMappingError> {
	let mut mapping = mapping.into();
	if data.len() != mapping.len() {
		return Err(ReorderMappingError::InequalLength {
			mapping_len: mapping.len(),
			data_len: data.len(),
		})
	}
	let len = data.len();

	for &mapped_idx in &mapping {
		if mapped_idx >= len {
			return Err(ReorderMappingError::NumberOutOfBounds { num: mapped_idx, len });
		}
	}

	for current_idx in 0..len {
		let old_idx = current_idx;
		let new_idx = mapping[current_idx];

		unsafe { core::hint::assert_unchecked(new_idx < len) }
		unsafe { core::hint::assert_unchecked(old_idx < len) }

		data.swap(old_idx, new_idx);
		mapping.swap(old_idx, new_idx);
	}

	Ok(())
}

#[derive(Debug, Error)]
pub enum ReorderMappingError {
	#[error("Mapping index ({num}) was out of bounds of length {len}.")]
	NumberOutOfBounds { num: usize, len: usize },
	#[error("Inequal length; mapping length ({mapping_len}) != data length ({data_len}).")]
	InequalLength { mapping_len: usize, data_len: usize },
}

/// Mappings are defined such that `mapping[n]` is where the `n`th element should be moved to.\
/// The human intuition is that the `n`th element should be moved to the `mapping[n]`th index.\
/// Therefore, we invert it for you.
pub fn invert_mapping(mapping: &[usize]) -> Result<Box<[usize]>, InvertMappingError> {
	let mut new_mapping = iter::repeat_n(None, mapping.len()).collect::<Box<[Option<usize>]>>();
	for (new_idx, &old_idx) in mapping.iter().enumerate() {
		let reference = new_mapping.get_mut(old_idx).ok_or_else(|| InvertMappingError::IndexOutOfBounds { idx: old_idx, len: mapping.len() })?;
		if reference.is_some() {
			return Err(InvertMappingError::DuplicateNumber)
		} else {
			*reference = Some(new_idx);
		}
	}
	new_mapping.into_iter().collect::<Option<Box<[usize]>>>().ok_or_else(|| InvertMappingError::MissingNumber)
}

#[derive(Debug, Error)]
pub enum InvertMappingError {
	#[error("Duplicate number found in invalid mapping.")]
	DuplicateNumber,
	#[error("Missing number in invalid mapping.")]
	MissingNumber,
	#[error("Index {idx} out of bounds for length {len}")]
	IndexOutOfBounds { idx: usize, len: usize },
}

#[must_use]
pub unsafe fn invert_mapping_unchecked(mapping: &[usize]) -> Box<[usize]> {
	let mut new_mapping = Box::<[usize]>::new_uninit_slice(mapping.len());
	for (new_idx, &old_idx) in mapping.iter().enumerate() {
		unsafe { new_mapping.get_mut(old_idx).unwrap_unchecked() }.write(new_idx);
	}
	unsafe { new_mapping.assume_init() }
}

macro_rules! unsigned_num_width {
	($name:ident, $ty:ty) => {
		#[allow(dead_code)]
		#[must_use]
		pub const fn $name(x: $ty) -> usize {
			(match x.checked_ilog10() {
				Some(n) => n as usize + 1,
				None => 1,
			}) * width_ascii("1")
		}
	};
}

macro_rules! signed_num_width {
	($name:ident, $ty:ty) => {
		#[allow(dead_code)]
		#[must_use]
		pub const fn $name(x: $ty) -> usize {
			(match x.abs().checked_ilog10() {
				Some(n) => n as usize + 1,
				None => 1,
			}) * width_ascii("1")
				+ if x < 0 { width_ascii("-") } else { width_ascii("") }
		}
	};
}

macro_rules! float_num_width {
	($name:ident, $ty:ty) => {
		#[allow(dead_code)]
		#[must_use]
		pub fn $name(x: $ty) -> usize { $crate::util::StrExt::width(x.to_string().as_str()) }
	};
}

unsigned_num_width!(u8_width, u8);
unsigned_num_width!(u16_width, u16);
unsigned_num_width!(u32_width, u32);
unsigned_num_width!(u64_width, u64);
unsigned_num_width!(usize_width, usize);

signed_num_width!(i8_width, i8);
signed_num_width!(i16_width, i16);
signed_num_width!(i32_width, i32);
signed_num_width!(i64_width, i64);
signed_num_width!(isize_width, isize);

float_num_width!(f32_width, f32);
float_num_width!(f64_width, f64);

pub fn drop_on_separate_thread<T: 'static + Send>(t: T) {
	#[cfg(not(target_arch = "wasm32"))]
	std::thread::Builder::new().stack_size(1_048_576 * 64 /* 64MiB */).spawn(move || drop(t)).expect("Failed to spawn thread");
	#[cfg(target_arch = "wasm32")]
	drop(t)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn open_file(str: &str) -> Result<std::process::ExitStatus, io::Error> {
	#[cfg(target_os = "windows")]
	return Ok(std::process::Command::new("cmd").args(["/c", "start", str]).status()?);
	#[cfg(target_os = "macos")]
	return Ok(std::process::Command::new("open").arg(str).status()?);
	#[cfg(target_os = "linux")]
	return Ok(std::process::Command::new("xdg-open").arg(str).status()?);
}

#[derive(Copy, Clone, Eq)]
pub struct Vec2u {
	pub x: usize,
	pub y: usize,
}

impl Debug for Vec2u {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "({x},{y})", x = self.x, y = self.y) }
}

impl Vec2u {
	#[must_use]
	pub const fn new(x: usize, y: usize) -> Self { Self { x, y } }

	#[must_use]
	pub const fn wrapping_sub(self, rhs: Self) -> Self {
		Self {
			x: self.x.wrapping_sub(rhs.x),
			y: self.y.wrapping_sub(rhs.y),
		}
	}

	#[must_use]
	pub const fn saturating_sub(self, rhs: Self) -> Self {
		Self {
			x: self.x.saturating_sub(rhs.x),
			y: self.y.saturating_sub(rhs.y),
		}
	}

	#[must_use]
	pub fn angle(self) -> f64 { f64::atan2(self.y as f64, self.x as f64) }

	#[must_use]
	pub fn relative_to(self, aabb: AABB) -> Option<Self> {
		if aabb.contains(self) {
			Some(self - aabb.low())
		} else {
			None
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

impl From<PhysicalSize<u32>> for Vec2u {
	fn from(value: PhysicalSize<u32>) -> Self {
		Self {
			x: value.width as usize,
			y: value.height as usize,
		}
	}
}

impl From<Vec2u> for (usize, usize) {
	fn from(val: Vec2u) -> Self { (val.x, val.y) }
}

impl<T: Into<(usize, usize)>> Add<T> for Vec2u {
	type Output = Self;

	fn add(self, rhs: T) -> Self::Output {
		let (x, y) = rhs.into();
		Self {
			x: self.x.wrapping_add(x),
			y: self.y.wrapping_add(y),
		}
	}
}

impl<T: Into<(usize, usize)>> AddAssign<T> for Vec2u {
	fn add_assign(&mut self, rhs: T) {
		let (x, y) = rhs.into();
		self.x = self.x.wrapping_add(x);
		self.y = self.y.wrapping_add(y);
	}
}

impl<T: Into<(usize, usize)>> Sub<T> for Vec2u {
	type Output = Self;

	fn sub(self, rhs: T) -> Self::Output {
		let (x, y) = rhs.into();
		Self {
			x: self.x.wrapping_sub(x),
			y: self.y.wrapping_sub(y),
		}
	}
}

impl<T: Into<(usize, usize)>> SubAssign<T> for Vec2u {
	fn sub_assign(&mut self, rhs: T) {
		let (x, y) = rhs.into();
		self.x = self.x.wrapping_sub(x);
		self.y = self.y.wrapping_sub(y);
	}
}

impl Mul for Vec2u {
	type Output = Self;

	fn mul(self, rhs: Self) -> Self::Output { Self { x: self.x * rhs.x, y: self.y * rhs.y } }
}

impl MulAssign for Vec2u {
	fn mul_assign(&mut self, rhs: Self) {
		self.x *= rhs.x;
		self.y *= rhs.y;
	}
}

impl Mul<usize> for Vec2u {
	type Output = Self;

	fn mul(self, rhs: usize) -> Self::Output { Self::new(self.x * rhs, self.y * rhs) }
}

impl Div for Vec2u {
	type Output = Self;

	fn div(self, rhs: Self) -> Self::Output { Self { x: self.x / rhs.x, y: self.y / rhs.y } }
}

impl Div<usize> for Vec2u {
	type Output = Self;

	fn div(self, rhs: usize) -> Self::Output { Self::new(self.x / rhs, self.y / rhs) }
}

impl DivAssign for Vec2u {
	fn div_assign(&mut self, rhs: Self) {
		self.x /= rhs.x;
		self.y /= rhs.y;
	}
}

#[derive(Copy, Clone, PartialEq)]
pub struct Vec2d {
	pub x: f64,
	pub y: f64,
}

impl Vec2d {
	#[must_use]
	pub const fn new(x: f64, y: f64) -> Self { Self { x, y } }

	#[must_use]
	pub fn round(self) -> Self { Self { x: self.x.round(), y: self.y.round() } }

	#[must_use]
	pub fn distance_squared(self) -> f64 { self.x * self.x + self.y * self.y }
}

impl Debug for Vec2d {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "({x},{y})", x = self.x, y = self.y) }
}

impl From<(f64, f64)> for Vec2d {
	fn from(value: (f64, f64)) -> Self { Self::new(value.0, value.1) }
}

impl From<Vec2d> for (f64, f64) {
	fn from(val: Vec2d) -> Self { (val.x, val.y) }
}

impl From<Vec2d> for Vec2u {
	fn from(vec: Vec2d) -> Self { Self { x: vec.x as usize, y: vec.y as usize } }
}

impl From<Vec2u> for Vec2d {
	fn from(vec: Vec2u) -> Self { Self { x: vec.x as f64, y: vec.y as f64 } }
}

impl From<PhysicalSize<f64>> for Vec2d {
	fn from(value: PhysicalSize<f64>) -> Self { Self { x: value.width, y: value.height } }
}

impl From<PhysicalPosition<f64>> for Vec2d {
	fn from(value: PhysicalPosition<f64>) -> Self { Self { x: value.x, y: value.y } }
}

impl<T: Into<(f64, f64)>> Add<T> for Vec2d {
	type Output = Self;

	fn add(self, rhs: T) -> Self::Output {
		let (x, y) = rhs.into();
		Self { x: self.x + x, y: self.y + y }
	}
}

impl<T: Into<(f64, f64)>> AddAssign<T> for Vec2d {
	fn add_assign(&mut self, rhs: T) {
		let (x, y) = rhs.into();
		self.x += x;
		self.y += y;
	}
}

impl<T: Into<(f64, f64)>> Sub<T> for Vec2d {
	type Output = Self;

	fn sub(self, rhs: T) -> Self::Output {
		let (x, y) = rhs.into();
		Self { x: self.x - x, y: self.y - y }
	}
}

impl<T: Into<(f64, f64)>> SubAssign<T> for Vec2d {
	fn sub_assign(&mut self, rhs: T) {
		let (x, y) = rhs.into();
		self.x = self.x - x;
		self.y = self.x - y;
	}
}

impl Mul for Vec2d {
	type Output = Self;

	fn mul(self, rhs: Self) -> Self::Output { Self { x: self.x * rhs.x, y: self.y * rhs.y } }
}

impl MulAssign for Vec2d {
	fn mul_assign(&mut self, rhs: Self) {
		self.x *= rhs.x;
		self.y *= rhs.y;
	}
}

impl Div for Vec2d {
	type Output = Self;

	fn div(self, rhs: Self) -> Self::Output { Self { x: self.x / rhs.x, y: self.y / rhs.y } }
}

impl DivAssign for Vec2d {
	fn div_assign(&mut self, rhs: Self) {
		self.x /= rhs.x;
		self.y /= rhs.y;
	}
}

impl Mul<f64> for Vec2d {
	type Output = Self;

	fn mul(self, rhs: f64) -> Self::Output { Self { x: self.x * rhs, y: self.y * rhs } }
}

impl MulAssign<f64> for Vec2d {
	fn mul_assign(&mut self, rhs: f64) {
		self.x *= rhs;
		self.y *= rhs;
	}
}

impl Div<f64> for Vec2d {
	type Output = Self;

	fn div(self, rhs: f64) -> Self::Output { Self { x: self.x / rhs, y: self.y / rhs } }
}

impl DivAssign<f64> for Vec2d {
	fn div_assign(&mut self, rhs: f64) {
		self.x /= rhs;
		self.y /= rhs;
	}
}

#[derive(Copy, Clone)]
pub struct AABB {
	low: Vec2u,
	high: Vec2u,
}

impl AABB {
	pub const NIL: Self = Self::new(0, 0, 0, 0);

	#[must_use]
	pub const fn new(x0: usize, x1: usize, y0: usize, y1: usize) -> Self {
		let (x0, x1) = if likely(x0 < x1) { (x0, x1) } else { (x1, x0) };
		let (y0, y1) = if likely(y0 < y1) { (y0, y1) } else { (y1, y0) };
		Self {
			low: Vec2u::new(x0, y0),
			high: Vec2u::new(x1, y1),
		}
	}

	#[must_use]
	pub const fn from_pos_and_dims(pos: Vec2u, dims: PhysicalSize<u32>) -> Self {
		Self {
			low: pos,
			high: Vec2u::new(pos.x + dims.width as usize, pos.y + dims.height as usize),
		}
	}

	#[must_use]
	pub fn contains(self, point: Vec2u) -> bool {
		let Self {
			low: Vec2u { x: x0, y: y0 },
			high: Vec2u { x: x1, y: y1 },
		} = self;
		let Vec2u { x, y } = point;
		x0 <= x && x < x1 && y0 <= y && y < y1
	}

	#[must_use]
	pub fn low(self) -> Vec2u { self.low }

	#[must_use]
	pub fn high(self) -> Vec2u { self.high }

	#[must_use]
	pub fn width(self) -> usize { self.high.x - self.low.x }

	#[must_use]
	pub fn height(self) -> usize { self.high.y - self.low.y }
	
	#[must_use]
	pub fn dims(self) -> PhysicalSize<u32> { PhysicalSize::new(self.width() as _, self.height() as _) }
}

#[cfg(test)]
mod tests {
	use std::assert_matches::assert_matches;

	#[test]
	fn test_reorder() {
		fn reorder<T>(mut data: Vec<T>, mapping: &[usize]) -> Vec<T> {
			let result = super::reorder(&mut data, mapping);
			assert_matches!(result, Ok(()), "Reordering was unsuccessful");
			data
		}

		assert_eq!(reorder(vec![1, 2, 3], &[2, 0, 1]), vec![2, 3, 1]);
		assert_eq!(reorder(vec![1, 2, 3, 4, 5], &[0, 1, 2, 3, 4]), vec![1, 2, 3, 4, 5]);
		assert_eq!(reorder(vec![1, 2, 3, 4], &[3, 2, 1, 0]), vec![4, 3, 2, 1]);
	}
}
