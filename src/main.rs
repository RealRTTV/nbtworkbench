#![warn(
	clippy::pedantic,
	clippy::nursery,
	clippy::perf,
	clippy::correctness,
	clippy::cargo,
	clippy::style,
	clippy::complexity,
	clippy::unused_unit,
	clippy::unused_async,
	clippy::unused_peekable,
	clippy::unused_self,
	clippy::unused_format_specs,
	clippy::unused_io_amount,
	clippy::unused_rounding,
	clippy::extra_unused_lifetimes,
	clippy::extra_unused_type_parameters,
	clippy::unwrap_used
)]
#![allow(
	clippy::unreadable_literal,
	clippy::cast_precision_loss,
	clippy::cast_lossless,
	clippy::cast_sign_loss,
	clippy::cast_possible_truncation,
	clippy::collapsible_if,
	clippy::collapsible_else_if,
	clippy::redundant_else,
	clippy::cast_possible_wrap,
	clippy::multiple_crate_versions
)]
#![feature(unchecked_math)]
#![feature(let_chains)]
#![feature(core_intrinsics)]
#![feature(stmt_expr_attributes)]
#![feature(slice_first_last_chunk)]
#![feature(maybe_uninit_uninit_array)]
#![feature(const_maybe_uninit_uninit_array)]
#![feature(new_uninit)]
#![feature(stdsimd)]
#![feature(box_patterns)]
#![feature(split_array)]
#![feature(array_chunks)]
#![feature(maybe_uninit_array_assume_init)]
#![feature(iter_array_chunks)]
#![feature(thread_id_value)]
#![feature(optimize_attribute)]
#![feature(allocator_api)]
#![feature(slice_ptr_get)]
#![feature(slice_swap_unchecked)]
#![feature(inline_const)]
#![feature(alloc_error_hook)]
#![feature(alloc_error_handler)]
#![feature(iter_next_chunk)]
#![feature(const_collections_with_hasher)]
#![cfg_attr(all(windows, not(debug_assertions)), windows_subsystem = "windows")]

use std::convert::identity;
use std::fmt::Write;

use compact_str::{CompactString, ToCompactString};
use notify::PollWatcher;
use static_assertions::const_assert_eq;
use uuid::Uuid;

use elements::element::NbtElement;
use vertex_buffer_builder::VertexBufferBuilder;

use crate::assets::{
	BASE_TEXT_Z, BASE_Z, BOOKMARK_UV, BOOKMARK_Z, BYTE_ARRAY_GHOST_UV, BYTE_GHOST_UV, CHUNK_GHOST_UV, COMPOUND_GHOST_UV, DOUBLE_GHOST_UV, END_LINE_NUMBER_SEPARATOR_UV, FLOAT_GHOST_UV, HEADER_SIZE,
	HIDDEN_BOOKMARK_UV, INT_ARRAY_GHOST_UV, INT_GHOST_UV, INVALID_STRIPE_UV, LINE_NUMBER_SEPARATOR_UV, LINE_NUMBER_Z, LIST_GHOST_UV, LONG_ARRAY_GHOST_UV, LONG_GHOST_UV, REGION_GHOST_UV,
	SELECTED_TOGGLE_OFF_UV, SELECTED_TOGGLE_ON_UV, SHORT_GHOST_UV, STRING_GHOST_UV, TEXT_UNDERLINE_UV, TOGGLE_Z, UNSELECTED_TOGGLE_OFF_UV, UNSELECTED_TOGGLE_ON_UV,
};
use crate::elements::chunk::{NbtChunk, NbtRegion};
use crate::elements::compound::NbtCompound;
use crate::elements::element::{NbtByte, NbtByteArray, NbtDouble, NbtFloat, NbtInt, NbtIntArray, NbtLong, NbtLongArray, NbtShort};
use crate::elements::list::NbtList;
use crate::elements::string::NbtString;
use crate::tree_travel::Navigate;
use crate::vertex_buffer_builder::Vec2u;
mod assets;
mod decoder;
mod encoder;
mod selected_text;
mod tab;
mod tree_travel;
mod vertex_buffer_builder;
mod window;
pub mod workbench;
mod workbench_action;
mod shader;
mod text_shader;

#[macro_export]
macro_rules! flags {
	() => {
		0b000_u8
	};
	(Ctrl) => {
		0b001_u8
	};
	(Shift) => {
		0b010_u8
	};
	(Ctrl + Shift) => {
		0b011_u8
	};
	(Alt) => {
		0b100_u8
	};
	(Ctrl + Alt) => {
		0b101_u8
	};
	(Shift + Alt) => {
		0b110_u8
	};
	(Ctrl + Shift + Alt) => {
		0b111_u8
	};
}

#[macro_export]
macro_rules! hash {
	($key:expr) => {{
		let mut hasher = FxHasher::default();
		hasher.write($key.as_bytes());
		hasher.finish()
	}};
}

/// # Refactor
/// # Long Term Goals
/// * web assembly ver
/// * smart screen
/// * wiki page for docs on minecraft's format of stuff
/// * [chunk](NbtChunk) section rendering
/// # Minor Features
/// * __ctrl + h__, open a playground `nbt` file to help with user interaction (bonus points if I have some way to tell if you haven't used this editor before)
/// * bookmark positions on the scrollbar section (i'd suggest a `Bookmark` struct and height manipulation on toggles and drops and stuff)
/// * [`last_modified`] field actually gets some impl
/// * warnings for closing files that have unsaved changes (and closing the program with unsaved changes)
/// * autosave
/// * gear icon to swap toolbar with settings panel
/// * blur behind tooltip
/// * alerts for a failed file subscription update
/// # Major Features
/// * macros
/// * keyboard-based element dropping (press numbers before to specify count for move operations, right shift to enable mode)
/// * animations!!!!
fn main() -> ! {
	window::run()
}

pub enum DropFn {
	Dropped(usize, usize, Option<CompactString>, usize),
	Missed(Option<CompactString>, NbtElement),
	InvalidType(Option<CompactString>, NbtElement),
}

/// Yoinked from `itertools`.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Position {
	Only,
	First,
	Middle,
	Last,
}

pub enum HeldEntry {
	Empty,
	FromAether((Option<CompactString>, NbtElement)),
	FromKnown((Option<CompactString>, NbtElement), Box<[usize]>),
}

impl HeldEntry {
	#[must_use]
	pub const fn element(&self) -> Option<&NbtElement> {
		match self {
			Self::Empty => None,
			Self::FromAether((_, element)) | Self::FromKnown((_, element), _) => Some(element),
		}
	}

	#[must_use]
	pub const fn is_empty(&self) -> bool {
		matches!(self, Self::Empty)
	}
}

pub fn sum_indices<I: Iterator<Item = usize>>(indices: I, mut root: &NbtElement) -> usize {
	unsafe {
		let mut total = 0;
		let mut indices = indices.peekable();
		while let Some(idx) = indices.next() {
			root = if let NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID = root.id() {
				total += 1 + idx;
				break;
			} else if let Some(list) = root.as_list() {
				total += 1 + list.children().take(idx).map(NbtElement::height).sum::<usize>();
				if let Some(root) = list.get(idx) {
					root
				} else {
					break;
				}
			} else if let Some(compound) = root.as_compound() {
				total += 1 + compound.children().take(idx).map(|(_, b)| b).map(NbtElement::height).sum::<usize>();
				if let Some((_, root)) = compound.get(idx) {
					root
				} else {
					break;
				}
			} else if let Some(chunk) = root.as_chunk() {
				total += 1 + chunk.children().take(idx).map(|(_, b)| b).map(NbtElement::height).sum::<usize>();
				if let Some((_, root)) = chunk.get(idx) {
					root
				} else {
					break;
				}
			} else if let Some(region) = root.as_region() {
				total += 1 + region.children().take(idx).map(NbtElement::height).sum::<usize>();
				if let Some(root) = region.get(idx) {
					root
				} else {
					break;
				}
			} else {
				total += root.height();
				if indices.peek().is_some() {
					panic_unchecked("tried to index non-indexable")
				} else {
					break;
				}
			};
		}
		total
	}
}

pub fn recache_along_indices(indices: &[usize], root: &mut NbtElement) {
	if let Some(region) = root.as_region_mut() {
		if let Some((&idx, rest)) = indices.split_first() {
			recache_along_indices(rest, unsafe { region.get_mut(idx).panic_unchecked("expected valid index") });
		}
		region.recache_depth();
	} else if let Some(array) = root.as_byte_array_mut() {
		array.recache_depth();
	} else if let Some(array) = root.as_int_array_mut() {
		array.recache_depth();
	} else if let Some(array) = root.as_long_array_mut() {
		array.recache_depth();
	} else if let Some(list) = root.as_list_mut() {
		if let Some((&idx, rest)) = indices.split_first() {
			recache_along_indices(rest, unsafe { list.get_mut(idx).panic_unchecked("expected valid index") });
		}
		list.recache_depth();
	} else if let Some(compound) = root.as_compound_mut() {
		if let Some((&idx, rest)) = indices.split_first() {
			recache_along_indices(rest, unsafe { compound.get_mut(idx).panic_unchecked("expected valid index") }.1);
		}
		compound.recache_depth();
	} else if let Some(chunk) = root.as_chunk_mut() {
		if let Some((&idx, rest)) = indices.split_first() {
			recache_along_indices(rest, unsafe { chunk.get_mut(idx).panic_unchecked("expected valid index") }.1);
		}
		chunk.recache_depth();
	}
}

#[inline]
#[must_use]
pub fn encompasses_or_equal(outer: &[usize], inner: &[usize]) -> bool {
	let outer_len = outer.len();
	let inner_len = inner.len();
	if outer_len <= inner_len {
		outer == &inner[..outer_len]
	} else {
		false
	}
}

#[inline]
#[must_use]
pub fn encompasses(outer: &[usize], inner: &[usize]) -> bool {
	let outer_len = outer.len();
	let inner_len = inner.len();
	if outer_len < inner_len {
		outer == &inner[..outer_len]
	} else {
		false
	}
}

#[inline]
#[must_use]
pub fn either_encompass(a: &[usize], b: &[usize]) -> bool {
	let min = usize::min(a.len(), b.len());
	a[..min] == b[..min]
}

#[inline]
#[must_use]
pub const fn is_utf8_char_boundary(x: u8) -> bool {
	(x as i8) >= -0x40
}

#[inline]
#[must_use]
pub fn is_jump_char_boundary(x: u8) -> bool {
	b" \t\r\n/\\()\"'-.,:;<>~!@#$%^&*|+=[]{}~?|".contains(&x)
}

pub struct FileUpdateSubscription {
	subscription_type: FileUpdateSubscriptionType,
	indices: Box<[usize]>,
	rx: std::sync::mpsc::Receiver<Vec<u8>>,
	watcher: PollWatcher,
	tab_uuid: Uuid,
}

pub enum FileUpdateSubscriptionType {
	Snbt,
	ByteArray,
	IntArray,
	LongArray,
}

pub struct RenderContext {
	forbidden_y: usize,
	forbidden_key: Option<(Box<str>, Box<str>)>,
	extend_forbid_for_value: bool,
	key_invalid: bool,
	ghost: Option<(u8, usize, usize, usize)>,
	ghost_line_number: Option<(usize, usize)>,
	left_margin: usize,
	mouse_x: usize,
	mouse_y: usize,
	line_number: usize,
	red_line_numbers: [usize; 2],
	pub x_offset: usize,
	pub y_offset: usize,
	// must be sorted least to greatest
	line_numbers: Vec<usize>,
	window_width: usize,
}

impl RenderContext {
	#[must_use]
	#[allow(clippy::type_complexity)] // forbidden is fine to be like that, c'mon
	pub fn new(forbidden: (usize, Option<(Box<str>, Box<str>)>), ghost: Option<(u8, usize, usize, usize)>, left_margin: usize, mouse: (usize, usize), window_width: usize) -> Self {
		Self {
			forbidden_y: forbidden.0,
			forbidden_key: forbidden.1,
			extend_forbid_for_value: false,
			key_invalid: false,
			ghost,
			left_margin,
			mouse_x: mouse.0,
			mouse_y: mouse.1,
			line_number: 1,
			red_line_numbers: [0, 0],
			x_offset: 16 + left_margin,
			y_offset: HEADER_SIZE,
			line_numbers: vec![],
			ghost_line_number: None,
			window_width,
		}
	}

	#[must_use]
	pub const fn pos(&self) -> Vec2u {
		Vec2u::new(self.x_offset, self.y_offset)
	}

	#[inline]
	pub fn check_key<F: FnOnce(&str, &str) -> bool>(&mut self, f: F, extend: bool) {
		if let Some((forbidden_key, forbidden_value)) = self.forbidden_key.as_ref() {
			self.key_invalid = f(forbidden_key, forbidden_value);
			self.extend_forbid_for_value = extend;
		}
	}

	#[inline]
	pub fn draw_toggle(&self, pos: impl Into<(usize, usize)>, open: bool, builder: &mut VertexBufferBuilder) {
		let pos = pos.into();
		let x = (pos.0 - self.left_margin) / 16;
		let y = (pos.1 - HEADER_SIZE) / 16;
		let hovered = if (self.mouse_x >= self.left_margin) & (self.mouse_y >= HEADER_SIZE) {
			(x == (self.mouse_x - self.left_margin) / 16) & (y == (self.mouse_y - HEADER_SIZE) / 16)
		} else {
			false
		};
		let uv = match (open, hovered) {
			(false, false) => UNSELECTED_TOGGLE_OFF_UV,
			(false, true) => SELECTED_TOGGLE_OFF_UV,
			(true, false) => UNSELECTED_TOGGLE_ON_UV,
			(true, true) => SELECTED_TOGGLE_ON_UV,
		};
		builder.draw_texture_z(Vec2u::from(pos) + (3, 5), TOGGLE_Z, uv, (8, 8));
	}

	#[inline]
	#[must_use]
	pub fn forbid(&self, pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder) -> bool {
		let (x, y) = pos.into();
		if y == self.forbidden_y {
			if self.key_invalid {
				self.draw_forbid_underline(x, y, builder);
			}
			false
		} else {
			true
		}
	}

	#[inline]
	pub fn draw_forbid_underline_width(&self, x: usize, y: usize, overridden_width: usize, builder: &mut VertexBufferBuilder) {
		if self.forbidden_key.is_some() {
			builder.draw_texture_region_z((x, y), BASE_Z, INVALID_STRIPE_UV + (1, 1), (self.window_width, 16), (14, 14));
			builder.draw_texture_region_z((x + 20, y + 14), BASE_Z, TEXT_UNDERLINE_UV, (overridden_width, 2), (16, 2));
		}
	}

	#[inline]
	pub fn draw_forbid_underline(&self, x: usize, y: usize, builder: &mut VertexBufferBuilder) {
		if let Some((forbidden_key, forbidden_value)) = self.forbidden_key.as_ref() {
			self.draw_forbid_underline_width(x, y, forbidden_key.width() + (": ".width() + forbidden_value.width()) * self.extend_forbid_for_value as usize, builder);
		}
	}

	#[inline]
	pub fn skip_line_numbers(&mut self, n: usize) {
		self.line_number += n;
	}

	#[inline]
	pub fn line_number(&mut self) {
		self.line_numbers.push(self.line_number);
		self.line_number += 1;
	}

	#[inline]
	pub fn render_line_numbers(&self, builder: &mut VertexBufferBuilder, mut bookmarks: &[usize]) {
		let start = self.line_numbers.first();
		while let Some((&first, rest)) = bookmarks.split_first() {
			if start.is_some_and(|&start| start > first) {
				bookmarks = rest;
			} else {
				break;
			}
		}
		let mut y = HEADER_SIZE;
		for (idx, &render_line_number) in self.line_numbers.iter().enumerate() {
			let actual_line_number = if let Some((offset, height)) = self.ghost_line_number && render_line_number > offset {
				render_line_number - height
			} else {
				render_line_number
			};
			let next_line_number = self.line_numbers.get(idx + 1).copied().map(|next_line_number| {
				if let Some((offset, height)) = self.ghost_line_number && next_line_number > offset { next_line_number - height } else { next_line_number }
			});

			let color = if (self.red_line_numbers[0] == y) | (self.red_line_numbers[1] == y) {
				if idx % 2 == 0 {
					0xC33C3C
				} else {
					0xFF5555
				}
			} else {
				if idx % 2 == 0 {
					0x777777
				} else {
					0xAAAAAA
				}
			};
			let color = core::mem::replace(&mut builder.color, color);
			builder.settings((self.left_margin - render_line_number.ilog10() as usize * 8 - 16, y), false, BASE_TEXT_Z);
			let _ = write!(builder, "{render_line_number}");
			builder.color = color;

			if let Some((&first, rest)) = bookmarks.split_first() && self.ghost_line_number.is_none_or(|(offset, _)| render_line_number != offset) && actual_line_number == first {
				bookmarks = rest;
				builder.draw_texture_region_z((2, y + 2), BOOKMARK_Z, BOOKMARK_UV, (builder.text_coords.0, 12), (16, 16));
			}
			if let Some((&first, rest)) = bookmarks.split_first() && next_line_number.is_none_or(|next_line_number| self.ghost_line_number.is_none_or(|(offset, _)| next_line_number != offset) && actual_line_number <= first && first < next_line_number) {
				bookmarks = rest;
				builder.draw_texture_region_z((2, y + 15), BOOKMARK_Z, HIDDEN_BOOKMARK_UV, (builder.text_coords.0, 2), (16, 16));
			}

			let uv = if idx + 1 == self.line_numbers.len() {
				END_LINE_NUMBER_SEPARATOR_UV
			} else {
				LINE_NUMBER_SEPARATOR_UV
			};
			builder.draw_texture_z((builder.text_coords.0 + 4, y), LINE_NUMBER_Z, uv, (2, 16));
			y += 16;
		}
	}

	#[must_use]
	pub fn ghost<F: FnOnce(usize, usize) -> bool, G: FnOnce(u8) -> bool>(&mut self, pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder, f: F, g: G) -> bool {
		let (x_offset, y_offset) = pos.into();
		if self.ghost_line_number.is_none() && let Some((id, x, y, true_height)) = self.ghost && f(x, y) && g(id) {
			builder.draw_texture((x_offset, y_offset), match id {
				NbtByte::ID => BYTE_GHOST_UV,
				NbtShort::ID => SHORT_GHOST_UV,
				NbtInt::ID => INT_GHOST_UV,
				NbtLong::ID => LONG_GHOST_UV,
				NbtFloat::ID => FLOAT_GHOST_UV,
				NbtDouble::ID => DOUBLE_GHOST_UV,
				NbtByteArray::ID => BYTE_ARRAY_GHOST_UV,
				NbtString::ID => STRING_GHOST_UV,
				NbtList::ID => LIST_GHOST_UV,
				NbtCompound::ID => COMPOUND_GHOST_UV,
				NbtIntArray::ID => INT_ARRAY_GHOST_UV,
				NbtLongArray::ID => LONG_ARRAY_GHOST_UV,
				NbtChunk::ID => CHUNK_GHOST_UV,
				NbtRegion::ID => REGION_GHOST_UV,
				_ => unsafe { panic_unchecked("Invalid element id") },
			}, (16, 16));
			self.ghost_line_number = Some((self.line_number, true_height));
			self.line_number();
			self.skip_line_numbers(true_height - 1);
			true
		} else {
			false
		}
	}
}

pub struct LinkedQueue<T> {
	tail: Option<Box<SinglyLinkedNode<T>>>,
	len: usize,
}

impl<T: Clone> Clone for LinkedQueue<T> {
	fn clone(&self) -> Self {
		Self {
			tail: self.tail.clone(),
			len: self.len,
		}
	}
}

// perf enhancement
impl<T> Drop for LinkedQueue<T> {
	fn drop(&mut self) {
		while let Some(box SinglyLinkedNode { value: _, prev }) = self.tail.take() {
			self.tail = prev;
		}
	}
}

impl<T> LinkedQueue<T> {
	#[must_use]
	pub const fn new() -> Self {
		Self { tail: None, len: 0 }
	}

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
	pub fn get(&self) -> Option<&T> {
		self.tail.as_ref().map(|x| &x.value)
	}

	#[must_use]
	pub fn get_mut(&mut self) -> Option<&mut T> {
		self.tail.as_mut().map(|x| &mut x.value)
	}

	#[must_use]
	pub const fn is_empty(&self) -> bool {
		self.len == 0
	}

	pub fn clear(&mut self) {
		while let Some(box SinglyLinkedNode { value: _, prev }) = self.tail.take() {
			self.tail = prev;
		}
		self.len = 0;
	}
}

pub struct SinglyLinkedNode<T> {
	value: T,
	prev: Option<Box<SinglyLinkedNode<T>>>,
}

impl<T: Clone> Clone for SinglyLinkedNode<T> {
	fn clone(&self) -> Self {
		Self {
			value: self.value.clone(),
			prev: self.prev.clone(),
		}
	}
}

pub trait StrExt {
	fn snbt_string_read(&self) -> Option<(CompactString, &str)>;

	fn needs_escape(&self) -> bool;

	fn width(&self) -> usize;
}

#[must_use]
pub const fn valid_unescaped_char(byte: u8) -> bool {
	matches!(byte, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'-' | b'.' | b'+')
}

impl StrExt for str {
	#[inline]
	#[optimize(speed)]
	#[allow(clippy::too_many_lines)]
	fn snbt_string_read(mut self: &Self) -> Option<(CompactString, &Self)> {
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
			let end_idx = self.char_indices().find(|(_, c)| !valid_unescaped_char(*c as u8)).map(|(idx, _)| idx)?;
			let (s, s2) = unsafe { (self.get_unchecked(..end_idx), self.get_unchecked(end_idx..self.len())) };
			if s.needs_escape() {
				return None;
			};
			Some((s.to_compact_string(), s2))
		} else {
			let enclosing = self.as_bytes().first().copied()?;
			self = unsafe { self.get_unchecked(1..) };
			let (end, len) = 'a: {
				let mut backslash = false;
				let mut sub = 0;
				let mut iter = self.bytes().enumerate();
				while let Some((idx, byte)) = iter.next() {
					if backslash {
						if byte == b'x' {
							if let Ok([(_, a), _]) = iter.next_chunk::<2>() && let Some(a) = MAPPING[a as usize] {
								if a < 8 {
									sub += 3;
								} else {
									sub += 2;
								}
							} else {
								return None;
							}
						} else if byte == b'u' {
							if let Ok([(_, _), (_, b), (_, c), _]) = iter.next_chunk::<4>() && let Some(b) = MAPPING[b as usize] && let Some(c) = MAPPING[c as usize] {
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
								return None;
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
				return None;
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
				} else if byte == b'x' {
					if backslash {
						backslash = false;
						if let Ok([a, b]) = iter.next_chunk::<2>() && let Some(a) = MAPPING[a as usize] && let Some(b) = MAPPING[b as usize] {
							let char = ((a << 4) | b) as char;
							let len = char.len_utf8();
							char.encode_utf8(unsafe { core::slice::from_raw_parts_mut(ptr.add(buf_len), len) });
							buf_len += len;
							continue;
						} else {
							return None;
						}
					}
				} else if byte == b'u' {
					if backslash {
						backslash = false;
						if let Ok([a, b, c, d]) = iter.next_chunk::<4>() && let Some(a) = MAPPING[a as usize] && let Some(b) = MAPPING[b as usize] && let Some(c) = MAPPING[c as usize] && let Some(d) = MAPPING[d as usize] && let Some(char) = char::from_u32(((a as u32) << 12) | ((b as u32) << 8) | ((c as u32) << 4) | (d as u32)) {
							let len = char.len_utf8();
							char.encode_utf8(unsafe { core::slice::from_raw_parts_mut(ptr.add(buf_len), len) });
							buf_len += len;
							continue;
						} else {
							return None;
						}
					}
				} else if backslash {
					return None;
				}

				unsafe {
					*ptr.add(buf_len) = byte;
					buf_len += 1;
				}
			}

			if self.len() < end + 1 {
				return None;
			};
			unsafe { Some((out, self.get_unchecked((end + 1)..))) }
		}
	}

	fn needs_escape(&self) -> bool {
		self.as_bytes().first().is_some_and(u8::is_ascii_digit) || !self.bytes().all(valid_unescaped_char)
	}

	fn width(&self) -> usize {
		self.chars().map(|x| if (x as u32) < 56832 { VertexBufferBuilder::CHAR_WIDTH[x as usize] as usize } else { 0 }).sum()
	}
}

pub trait OptionExt<T> {
	/// # Safety
	///
	/// * This code better be unreachable otherwise it's UB without `debug_assertions`, just a panic with them however.
	unsafe fn panic_unchecked(self, msg: &str) -> T;

	#[allow(clippy::wrong_self_convention)] // then why is is_some_and like that, huh?
	fn is_none_or(self, f: impl FnOnce(T) -> bool) -> bool;
}

impl<T> OptionExt<T> for Option<T> {
	unsafe fn panic_unchecked(self, msg: &str) -> T {
		self.map_or_else(|| panic_unchecked(msg), identity)
	}

	fn is_none_or(self, f: impl FnOnce(T) -> bool) -> bool {
		self.map_or(true, f)
	}
}

/// # Safety
///
/// * This code better be unreachable otherwise it's UB without `debug_assertions`, just a panic with them however.
///
/// # Panics
///
/// * When `debug_assertions` is true, it panics with the respective `msg`
#[allow(unused_variables)] // intellij being freaky
pub unsafe fn panic_unchecked(msg: &str) -> ! {
	#[cfg(debug_assertions)]
	panic!("{msg}");

	#[cfg(not(debug_assertions))]
	core::hint::unreachable_unchecked()
}

pub mod elements {
	pub mod array;
	pub mod chunk;
	pub mod compound;
	pub mod element_action;
	pub mod element;
	pub mod list;
	pub mod primitive;
	pub mod string;
}

const_assert_eq!(VertexBufferBuilder::CHAR_WIDTH[b':' as usize], VertexBufferBuilder::CHAR_WIDTH[b',' as usize]);
