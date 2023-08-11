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
clippy::extra_unused_type_parameters
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
#![feature(prelude_2024)]
#![feature(maybe_uninit_uninit_array)]
#![feature(new_uninit)]
#![feature(stdsimd)]
#![feature(box_patterns)]
#![cfg_attr(all(windows, not(debug_assertions)), windows_subsystem = "windows")]

use std::fmt::Write;
use std::fs::{read, write};
use std::intrinsics::{likely, unlikely};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::string::String;
use std::time::SystemTime;

use flate2::Compression;
use fxhash::{FxBuildHasher, FxHashSet};
use pollster::FutureExt;
use winit::dpi::PhysicalPosition;
use winit::event::{ElementState, KeyboardInput, MouseButton, MouseScrollDelta, VirtualKeyCode};
use zune_inflate::DeflateDecoder;

use elements::element_type::NbtElement;
use vertex_buffer_builder::VertexBufferBuilder;

use crate::assets::HEADER_SIZE;
use crate::elements::compound::NbtCompound;
use crate::elements::element_type::{NbtByte, NbtByteArray, NbtDouble, NbtFloat, NbtInt, NbtIntArray, NbtLong, NbtLongArray, NbtShort};
use crate::elements::list::NbtList;
use crate::elements::string::NbtString;
use crate::tree_travel::{Navigate, Traverse, TraverseParents};
use crate::window::WINDOW_HEIGHT;
use crate::workbench_action::WorkbenchAction;

mod assets;
mod decoder;
mod encoder;
mod tree_travel;
mod vertex_buffer_builder;
mod window;
mod workbench_action;

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
macro_rules! tab_mut_unchecked {
	($this:expr) => {
		unsafe { $this.tabs.get_mut($this.tab).panic_unchecked("") }
	};
}

pub enum DropFn {
	Dropped(usize, usize, Option<Box<str>>),
	Missed(Option<Box<str>>, NbtElement),
	InvalidType(Option<Box<str>>, NbtElement),
}

pub type DeleteFn = (Option<Box<str>>, NbtElement, (usize, usize));

pub type ToggleFn = Result<usize, ()>;

pub type StealFn = Option<(Option<Box<str>>, NbtElement, (usize, usize))>;

pub type TrySelectTextFn = (Option<(Box<str>, bool)>, Option<(Box<str>, bool)>);

fn main() -> ! {
	{
		// todo, web assembly ver
		// todo, smart screen
		// todo, 24x24 for toolbar icons
		// todo, horizontal scrolling maximum value
		// todo, usable scrollbar
		// todo, ctrl + h, open a playground nbt file to help with user interaction
		// todo, ctrl + r, reload current page / currently hovered entry
		// todo, bookmarking line numbers
		// todo, mca
		// todo, if RenderContext::invalid_key, highlight the other entry that you're duplicating the name of
		// todo, tab scrolling
		// todo, keyboard-based element dropping (press nums before to specify count for move operations, right shift to enable)
		// todo, icons for each workbench action, and a 5 most recent actions for undo and redo respectively
		// todo, editing a key in a compound to one that already exists makes a red underline for that other value
		// todo, macros
	}

	window::run().block_on();
}

pub struct NbtWorkbench {
	tabs: Vec<Tab>,
	tab: usize,
	mouse_x: usize,
	mouse_y: usize,
	window_height: usize,
	held_mouse_keys: FxHashSet<MouseButton>,
	held_keys: FxHashSet<VirtualKeyCode>,
	held_entry: HeldEntry,
	selected_text: Option<SelectedText>,
	cache_cursor_x: Option<usize>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Position {
	/// Only element present in the iterator, all following elements will only appear with len > 1
	Only,

	/// First element present in the iterator
	First,

	/// Middle element(s) present in the iterator, will only occur for len > 2
	Middle,

	/// Tail element, aka the `Last` element.
	Last,
}

pub enum HeldEntry {
	Empty,
	FromAether(Result<(Box<str>, NbtElement), NbtElement>),
	FromKnown(Result<(Box<str>, NbtElement), NbtElement>, Box<[usize]>),
}

impl HeldEntry {
	#[must_use]
	pub fn element(&self) -> Option<&NbtElement> {
		match self {
			Self::Empty => None,
			Self::FromAether(element) | Self::FromKnown(element, _) => Some(element.as_ref().map_or_else(|x| x, |(_, x)| x)),
		}
	}

	#[must_use]
	pub const fn is_empty(&self) -> bool {
		matches!(self, Self::Empty)
	}
}

pub fn sum_indices<I: Iterator<Item = usize>>(indices: I, mut root: &NbtElement) -> usize {
	let mut total = 0;
	let mut indices = indices.peekable();
	while let Some(idx) = indices.next() {
		root = match root {
			NbtElement::ByteArray(array) => {
				if idx >= array.len() {
					unsafe { panic_unchecked("byte array oob") }
				} else {
					total += 1 + idx;
					break;
				}
			}
			NbtElement::List(list) => {
				if idx >= list.len() {
					unsafe { panic_unchecked("list oob") }
				} else {
					total += 1;
					for jdx in 0..idx {
						// SAFETY: n < len (is valid bounds) implies n - m (where m is a positive integer <= n) < len (is valid bounds)
						total += unsafe { list.get(jdx).panic_unchecked("index oob") }.height();
					}
					// SAFETY: asserted beforehand
					unsafe { list.get(idx).panic_unchecked("index oob") }
				}
			}
			NbtElement::Compound(compound) => {
				if idx >= compound.len() {
					unsafe { panic_unchecked("compound oob") }
				} else {
					total += 1;
					for jdx in 0..idx {
						// SAFETY: n < len (is valid bounds) implies n - m (where m is a positive integer <= n) < len (is valid bounds)
						total += unsafe { compound.get(jdx).panic_unchecked("index oob") }.1.height();
					}
					// SAFETY: asserted beforehand
					unsafe { compound.get(idx).panic_unchecked("index oob") }.1
				}
			}
			NbtElement::IntArray(array) => {
				if idx >= array.len() {
					unsafe { panic_unchecked("int array oob") }
				} else {
					total += 1 + idx;
					break;
				}
			}
			NbtElement::LongArray(array) => {
				if idx >= array.len() {
					unsafe { panic_unchecked("long array oob") }
				} else {
					total += 1 + idx;
					break;
				}
			}
			x => {
				total += x.height();
				if indices.peek().is_some() {
					unsafe { panic_unchecked("tried to index non-indexable") }
				} else {
					break;
				}
			}
		}
	}
	total
}

#[derive(Clone, Debug)]
pub struct SelectedTextCache {
	keyfix: Option<Box<str>>,
	value: Box<str>,
	valuefix: Option<Box<str>>,
	cursor: usize,
	selection: Option<usize>,
}

impl SelectedTextCache {
	pub fn ne(&self, text: &SelectedText) -> bool {
		(self.value.as_ref() != text.value.as_str()) | (self.keyfix.as_ref().map(Box::as_ref) != text.keyfix.as_deref()) | (self.valuefix.as_ref().map(Box::as_ref) != text.valuefix.as_deref())
	}
}

#[derive(Clone)]
pub struct SelectedText {
	y: usize,
	indices: Box<[usize]>,
	cursor: usize,
	value: String,
	selection: Option<usize>,
	keyfix: Option<String>,
	prefix: String,
	suffix: String,
	valuefix: Option<String>,
	editable: bool,
	last_interaction: SystemTime,
	undos: LinkedQueue<SelectedTextCache>,
	redos: LinkedQueue<SelectedTextCache>,
}

#[repr(u8)]
pub enum KeyResult {
	Failed,
	NothingSpecial,
	Revert,
	Finish,
	Keyfix,
	Valuefix,
	Up(bool),
	Down(bool),
	ForceClose,
	ForceOpen,
	ShiftUp,
	ShiftDown,
}

impl SelectedText {
	#[inline]
	#[must_use]
	#[allow(clippy::too_many_lines)]
	pub fn new(target_x: usize, mouse_x: usize, y: usize, key: Option<(Box<str>, bool)>, value: Option<(Box<str>, bool)>, indices: Vec<usize>) -> Option<Self> {
		let key_width = if let Some((key, true)) = key.clone() {
			let key_width = key.width();

			if mouse_x + 4 >= target_x {
				let (suffix, valuefix) = if let Some((v, b)) = &value {
					if *b {
						(": ".to_owned(), Some(v.clone().into_string()))
					} else {
						(format!(": {v}"), None)
					}
				} else {
					(String::new(), None)
				};

				if mouse_x <= target_x {
					return Some(
						Self {
							y,
							indices: indices.into_boxed_slice(),
							cursor: 0,
							value: key.into_string(),
							selection: None,
							keyfix: None,
							prefix: String::new(),
							suffix,
							valuefix,
							editable: true,
							last_interaction: SystemTime::now(),
							undos: LinkedQueue::new(),
							redos: LinkedQueue::new(),
						}
							.post_process(),
					);
				}

				if target_x + key_width < mouse_x + key.chars().last().map_or(0, |char| VertexBufferBuilder::furthest_pixel(char as u16)) && mouse_x < target_x + key_width + 7 {
					return Some(
						Self {
							y,
							indices: indices.into_boxed_slice(),
							cursor: key.len(),
							value: key.into_string(),
							selection: None,
							keyfix: None,
							prefix: String::new(),
							suffix,
							valuefix,
							editable: true,
							last_interaction: SystemTime::now(),
							undos: LinkedQueue::new(),
							redos: LinkedQueue::new(),
						}
							.post_process(),
					);
				}

				let mut cursor = 0;
				let mut x = (mouse_x - target_x) as isize;
				let key_width = key_width as isize;

				for char in key.chars() {
					let width = VertexBufferBuilder::furthest_pixel(char as u16) as isize;
					if x * 2 >= width {
						// algebra, to understand, divide both sides by two
						cursor += char.len_utf8();
						x -= width;
					} else if x < key_width {
						return Some(
							Self {
								y,
								indices: indices.into_boxed_slice(),
								cursor,
								value: key.into_string(),
								selection: None,
								keyfix: None,
								prefix: String::new(),
								suffix,
								valuefix,
								editable: true,
								last_interaction: SystemTime::now(),
								undos: LinkedQueue::new(),
								redos: LinkedQueue::new(),
							}
								.post_process(),
						);
					}
				}
			}
			key_width + ": ".width()
		} else {
			0
		};

		if let Some((value, true)) = value.as_ref() {
			let value_x = target_x + key_width;
			if mouse_x + 4 >= value_x {
				let (keyfix, prefix) = if let Some((k, b)) = key.as_ref() {
					if *b {
						(Some(k.as_ref().to_owned()), ": ".to_owned())
					} else {
						(None, format!(": {k}"))
					}
				} else {
					(None, String::new())
				};

				if mouse_x <= value_x {
					return Some(
						Self {
							y,
							indices: indices.into_boxed_slice(),
							cursor: 0,
							value: value.as_ref().to_owned(),
							selection: None,
							keyfix,
							prefix,
							suffix: String::new(),
							valuefix: None,
							editable: true,
							last_interaction: SystemTime::now(),
							undos: LinkedQueue::new(),
							redos: LinkedQueue::new(),
						}
							.post_process(),
					);
				}

				let value_width = value.width();

				if value_x + value_width < mouse_x + value.chars().last().map_or(0, |char| VertexBufferBuilder::furthest_pixel(char as u16)) && mouse_x < value_x + value_width + 5 {
					return Some(
						Self {
							y,
							indices: indices.into_boxed_slice(),
							cursor: value.len(),
							value: value.as_ref().to_owned(),
							selection: None,
							keyfix,
							prefix,
							suffix: String::new(),
							valuefix: None,
							editable: true,
							last_interaction: SystemTime::now(),
							undos: LinkedQueue::new(),
							redos: LinkedQueue::new(),
						}
							.post_process(),
					);
				}

				let value_width = value_width as isize;
				let mut x = (mouse_x - value_x) as isize;
				let mut cursor = 0;

				for char in value.chars() {
					let width = VertexBufferBuilder::furthest_pixel(char as u16) as isize;
					if x * 2 >= width {
						// algebra, to understand, divide both sides by two
						cursor += char.len_utf8();
						x -= width;
					} else if x < value_width {
						return Some(
							Self {
								y,
								indices: indices.into_boxed_slice(),
								cursor,
								value: value.as_ref().to_owned(),
								selection: None,
								keyfix,
								prefix,
								suffix: String::new(),
								valuefix: None,
								editable: true,
								last_interaction: SystemTime::now(),
								undos: LinkedQueue::new(),
								redos: LinkedQueue::new(),
							}
								.post_process(),
						);
					}
				}
			}
		}

		if key.as_ref().is_none_or(|(_, display)| !*display) && value.as_ref().is_none_or(|(_, display)| !*display) {
			Some(
				Self {
					y,
					indices: indices.into_boxed_slice(),
					cursor: 0,
					value: if key.is_some() { ": ".to_owned() } else { String::new() },
					selection: None,
					keyfix: key.map(|(x, _)| x.into_string()),
					prefix: String::new(),
					suffix: String::new(),
					valuefix: value.map(|(x, _)| x.into_string()),
					editable: false,
					last_interaction: SystemTime::now(),
					undos: LinkedQueue::new(),
					redos: LinkedQueue::new(),
				}
					.post_process(),
			)
		} else {
			None
		}
	}

	#[must_use]
	pub fn post_process(mut self) -> Self {
		self.undos.push(SelectedTextCache {
			keyfix: self.keyfix.clone().map(String::into_boxed_str),
			value: self.value.clone().into_boxed_str(),
			valuefix: self.valuefix.clone().map(String::into_boxed_str),
			cursor: self.cursor,
			selection: self.selection,
		});

		self
	}

	pub fn handle_history(&mut self) {
		let should_cache = unsafe {
			core::mem::replace(&mut self.last_interaction, SystemTime::now())
				.elapsed()
				.ok()
				.panic_unchecked("Time can't go backwards")
		}
			.as_millis() >= 1_500;
		if should_cache && self.editable && self.undos.get().is_none_or(|x| x.ne(self)) {
			if self.redos.pop().is_none_or(|x| x.ne(self)) {
				self.redos = LinkedQueue::new();
			}

			self.undos.push(SelectedTextCache {
				keyfix: self.keyfix.clone().map(String::into_boxed_str),
				value: self.value.clone().into_boxed_str(),
				valuefix: self.valuefix.clone().map(String::into_boxed_str),
				cursor: self.cursor,
				selection: self.selection,
			});
		}
	}

	#[cfg_attr(not(debug_assertions), inline)]
	#[allow(clippy::cognitive_complexity, clippy::too_many_lines)] // i handled this fn well
	#[must_use]
	pub fn on_key_press(&mut self, key: VirtualKeyCode, mut char: Option<char>, flags: u8) -> KeyResult {
		use crate::KeyResult::{Down, Failed, Finish, ForceClose, ForceOpen, Keyfix, NothingSpecial, Revert, ShiftDown, ShiftUp, Up, Valuefix};

		if key == VirtualKeyCode::Escape && flags == flags!() {
			return Revert;
		}

		if let VirtualKeyCode::Return | VirtualKeyCode::NumpadEnter = key && flags == flags!() {
			return Finish;
		}

		if key == VirtualKeyCode::Z && flags == flags!(Ctrl) && self.editable {
			let SelectedTextCache {
				keyfix,
				value,
				valuefix,
				cursor,
				selection,
			} = 'a: {
				while let Some(cache) = self.undos.pop() {
					if unlikely(cache.ne(self)) {
						self.undos.push(cache.clone());
						break 'a cache;
					}
				}
				return Failed;
			};

			self.redos.push(SelectedTextCache {
				keyfix: core::mem::replace(&mut self.keyfix, keyfix.map(str::into_string)).map(String::into_boxed_str),
				value: core::mem::replace(&mut self.value, value.into_string()).into_boxed_str(),
				valuefix: core::mem::replace(&mut self.valuefix, valuefix.map(str::into_string)).map(String::into_boxed_str),
				cursor: core::mem::replace(&mut self.cursor, cursor),
				selection: core::mem::replace(&mut self.selection, selection),
			});
			self.last_interaction = SystemTime::UNIX_EPOCH;
			return NothingSpecial;
		}

		if (key == VirtualKeyCode::Y && flags == flags!(Ctrl) || key == VirtualKeyCode::Z && flags == flags!(Ctrl + Shift)) && self.editable {
			let SelectedTextCache {
				keyfix,
				value,
				valuefix,
				cursor,
				selection,
			} = 'a: {
				while let Some(cache) = self.redos.pop() {
					if likely(cache.ne(self)) {
						self.redos.push(cache.clone());
						break 'a cache;
					}
				}
				return Failed;
			};
			self.undos.push(SelectedTextCache {
				keyfix: core::mem::replace(&mut self.keyfix, keyfix.map(str::into_string)).map(String::into_boxed_str),
				value: core::mem::replace(&mut self.value, value.into_string()).into_boxed_str(),
				valuefix: core::mem::replace(&mut self.valuefix, valuefix.map(str::into_string)).map(String::into_boxed_str),
				cursor: core::mem::replace(&mut self.cursor, cursor),
				selection: core::mem::replace(&mut self.selection, selection),
			});
			self.last_interaction = SystemTime::UNIX_EPOCH;
			return NothingSpecial;
		}

		if key == VirtualKeyCode::A && flags == flags!(Ctrl) && self.editable {
			self.cursor = 0;
			self.selection = Some(self.value.len());
			return NothingSpecial;
		}

		if let Some(selection) = self.selection {
			if let VirtualKeyCode::Back | VirtualKeyCode::Delete = key && self.editable {
				let (low_selection, high_selection) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
				let (left, right) = self.value.split_at(low_selection);
				let (_, right) = right.split_at(high_selection - low_selection);
				self.value = format!("{left}{right}");
				self.selection = None;
				self.cursor = low_selection;
				return NothingSpecial;
			}
		}

		if key == VirtualKeyCode::X && flags == flags!(Ctrl) && self.editable {
			if let Some(selection) = self.selection {
				let (start, end) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
				let (low, right) = self.value.split_at(start);
				let (cut, high) = right.split_at(end - start);
				if cli_clipboard::set_contents(cut.to_owned()).is_ok() {
					self.value = format!("{low}{high}");
					self.selection = None;
				}
				return NothingSpecial;
			}
		}

		if key == VirtualKeyCode::C && flags == flags!(Ctrl) && self.editable {
			if let Some(selection) = self.selection {
				let (start, end) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
				let (_, right) = self.value.split_at(start);
				let (cut, _) = right.split_at(end - start);
				let _ = cli_clipboard::set_contents(cut.to_owned()).is_ok();
				return NothingSpecial;
			}
		}

		if key == VirtualKeyCode::V && flags == flags!(Ctrl) && self.editable {
			if let Ok(clipboard) = cli_clipboard::get_contents() {
				if let Some(selection) = self.selection.take() {
					let (start, end) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
					let (left, right) = self.value.split_at(start);
					self.cursor = left.len() + clipboard.len();
					let (_, right) = right.split_at(end - start);
					self.value = format!("{left}{clipboard}{right}");
				} else {
					let (left, right) = self.value.split_at(self.cursor);
					self.value = format!("{left}{clipboard}{right}");
					self.cursor += clipboard.len();
				}
				return NothingSpecial;
			}
		}

		if key == VirtualKeyCode::Home && flags == flags!() && self.editable {
			self.cursor = 0;
			return NothingSpecial;
		}

		if key == VirtualKeyCode::End && flags == flags!() && self.editable {
			self.cursor = self.value.len();
			return NothingSpecial;
		}

		if key == VirtualKeyCode::Back && flags < 2 && self.editable {
			if flags & flags!(Ctrl) > 0 {
				self.value = self.value.split_off(self.cursor);
				self.cursor = 0;
			} else {
				let (left, right) = self.value.split_at(self.cursor);

				if !left.is_empty() {
					let mut end = left.len() - 1;
					while end > 0 {
						if is_utf8_char_boundary(left.as_bytes()[end]) {
							break;
						}
						end -= 1;
					}
					let (left, _) = left.split_at(end);
					self.cursor = left.len();
					self.value = format!("{left}{right}");
				}
			}

			return NothingSpecial;
		}

		if key == VirtualKeyCode::Delete && self.editable {
			if flags & flags!(Ctrl) > 0 {
				self.value.truncate(self.cursor);
				self.cursor = self.value.len();
			} else {
				let (left, right) = self.value.split_at(self.cursor);

				if !right.is_empty() {
					let mut start = 1;
					while start < right.len() {
						if is_utf8_char_boundary(right.as_bytes()[start]) {
							break;
						}
						start += 1;
					}
					let (_, right) = right.split_at(start);
					self.cursor = left.len();
					self.value = format!("{left}{right}");
				}
			}
			return NothingSpecial;
		}

		if key == VirtualKeyCode::Up {
			if flags & !flags!(Ctrl) == 0 {
				return Up(flags == flags!(Ctrl));
			} else if flags == flags!(Ctrl + Shift) {
				return ShiftUp;
			}
		}

		if key == VirtualKeyCode::Down {
			if flags & !flags!(Ctrl) == 0 {
				return Down(flags == flags!(Ctrl));
			} else if flags == flags!(Ctrl + Shift) {
				return ShiftDown;
			}
		}

		if key == VirtualKeyCode::Left {
			if flags == flags!(Alt) {
				return ForceClose;
			}

			if self.editable {
				if flags & flags!(Shift) == 0 && self.selection.is_none() && self.cursor == 0 && self.keyfix.is_some() {
					return Keyfix;
				}

				if flags & flags!(Ctrl) > 0 {
					if flags & flags!(Shift) > 0 {
						self.selection = Some(0);
					} else {
						self.cursor = 0;
						self.selection = None;
					}
				} else {
					let mut new = if let Some(selection) = self.selection && flags & flags!(Shift) > 0 { selection } else { self.cursor };
					if new > 0 {
						new -= 1;
						while new > 0 {
							if is_utf8_char_boundary(self.value.as_bytes()[new]) {
								break;
							}

							new -= 1;
						}
					}

					if flags & flags!(Shift) > 0 {
						self.selection = Some(new);
					} else if let Some(selection) = self.selection.take() {
						self.cursor = self.cursor.min(selection);
					} else {
						self.cursor = new;
					}
				}

				if self.selection.is_some_and(|x| x == self.cursor) {
					self.selection = None;
				}
			}

			return NothingSpecial;
		}

		if key == VirtualKeyCode::Right {
			if flags == flags!(Alt) {
				return ForceOpen;
			}

			if self.editable {
				if flags & flags!(Shift) == 0 && self.selection.is_none() && self.cursor == self.value.len() && self.valuefix.is_some() {
					return Valuefix;
				}

				if flags & flags!(Ctrl) > 0 {
					if flags & flags!(Shift) > 0 {
						self.selection = Some(self.value.len());
					} else {
						self.cursor = self.value.len();
						self.selection = None;
					}
				} else {
					let mut new = if let Some(selection) = self.selection && flags == flags!(Shift) { selection } else { self.cursor };
					if new < self.value.len() {
						new += 1;
						while new < self.value.len() {
							if is_utf8_char_boundary(self.value.as_bytes()[new]) {
								break;
							}

							new += 1;
						}
					}

					if flags & flags!(Shift) > 0 {
						self.selection = Some(new);
					} else if let Some(selection) = self.selection.take() {
						self.cursor = self.cursor.max(selection);
					} else {
						self.cursor = new;
					}
				}

				if self.selection.is_some_and(|x| x == self.cursor) {
					self.selection = None;
				}
			}

			return NothingSpecial;
		}

		if let VirtualKeyCode::Return | VirtualKeyCode::NumpadEnter = key && flags == flags!(Shift) && self.editable {
			char = Some('\n');
		}

		if let Some(char) = char && self.editable {
			if let Some(selection) = self.selection {
				let (low_selection, high_selection) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
				let (left, right) = self.value.split_at(low_selection);
				let (_, right) = right.split_at(high_selection - low_selection);
				self.value = format!("{left}{char}{right}");
				self.selection = None;
				self.cursor = low_selection + char.len_utf8();
			} else {
				let (left, right) = self.value.split_at(self.cursor);
				self.value = format!("{left}{char}{right}");
				self.cursor += char.len_utf8();
			}

			return NothingSpecial;
		}

		Failed
	}

	#[inline]
	pub fn render(&self, builder: &mut VertexBufferBuilder, left_margin: usize) {
		let x = self.indices.len() * 16 + 32 + 4 + left_margin;
		let y = if builder.scroll() > self.y {
			return;
		} else {
			self.y - builder.scroll()
		};

		if y < HEADER_SIZE {
			return;
		}

		let prefix_width = self.prefix.width() + self.keyfix.as_ref().map_or(0, StrExt::width);

		if self.editable {
			builder.draw_texture((x + self.value.split_at(self.cursor).0.width() + prefix_width, y), (0, 32), (2, 16));
		}

		builder.draw_texture((x - 4 - 16, y), (0, 32), (16, 16));

		builder.settings(x, y, false);
		let _ = write!(
			builder,
			"{}{}{}{}{}",
			self.keyfix.as_ref().map_or("", String::as_str),
			self.prefix,
			self.value,
			self.suffix,
			self.valuefix.as_ref().map_or("", String::as_str),
		);

		if let Some(selection) = self.selection && self.editable {
			let (start, end) = if self.cursor > selection { (selection, self.cursor) } else { (self.cursor, selection) };
			let mut start_x = x + self.value.split_at(start).0.width();
			let end_x = x + self.value.split_at(end).0.width();
			let mut remaining_width = end_x - start_x;
			while remaining_width > 0 {
				builder.draw_texture_z((start_x + prefix_width, y), 0.5, (1, 32), (remaining_width.min(14), 16));
				remaining_width = if remaining_width <= 14 { 0 } else { remaining_width - 14 };
				start_x += 14;
			}
		}
	}
}

#[inline]
#[must_use]
pub const fn is_utf8_char_boundary(x: u8) -> bool {
	(x as i8) >= -0x40
}

#[derive(Copy, Clone)]
pub enum FileFormat {
	Nbt = 0,
	Gzip = 1,
	Zlib = 2,
	Snbt = 3,
}

impl FileFormat {
	#[must_use]
	pub const fn cycle(self) -> Self {
		match self {
			Self::Nbt => Self::Gzip,
			Self::Gzip => Self::Zlib,
			Self::Zlib => Self::Snbt,
			Self::Snbt => Self::Nbt,
		}
	}

	#[must_use]
	pub fn encode(self, data: &NbtElement) -> Vec<u8> {
		match self {
			Self::Nbt => data.to_file().unwrap_or_default(),
			Self::Gzip => {
				let mut vec = vec![];
				let _ = flate2::read::GzEncoder::new(&*data.to_file().unwrap_or_default(), Compression::best()).read_to_end(&mut vec);
				vec
			}
			Self::Zlib => {
				let mut vec = vec![];
				let _ = flate2::read::ZlibEncoder::new(&*data.to_file().unwrap_or_default(), Compression::best()).read_to_end(&mut vec);
				vec
			}
			Self::Snbt => data.to_string().into_bytes(),
		}
	}
}

impl Default for NbtWorkbench {
	fn default() -> Self {
		let mut workbench = Self {
			tabs: vec![],
			tab: 0,
			mouse_x: 0,
			mouse_y: 0,
			window_height: WINDOW_HEIGHT,
			held_mouse_keys: FxHashSet::with_hasher(FxBuildHasher::default()),
			held_keys: FxHashSet::with_hasher(FxBuildHasher::default()),
			held_entry: HeldEntry::Empty,
			selected_text: None,
			cache_cursor_x: None,
		};
		if let Some(x) = &std::env::args().nth(1).and_then(|x| PathBuf::from_str(&x).ok()) {
			workbench.on_open_file(x);
		} else {
			workbench.tabs.push(Tab {
				#[cfg(debug_assertions)]
				value: Box::new(NbtElement::from_file(include_bytes!("assets/test.nbt")).unwrap()),
				#[cfg(debug_assertions)]
				name: "test.nbt".into(),
				#[cfg(not(debug_assertions))]
				value: Box::new(NbtElement::Compound(NbtCompound::new())),
				#[cfg(not(debug_assertions))]
				name: "new.nbt".into(),
				path: None,
				compression: FileFormat::Nbt,
				undos: LinkedQueue::new(),
				redos: LinkedQueue::new(),
				history_changed: false,
				scroll: 0,
				horizontal_scroll: 0,
				window_height: WINDOW_HEIGHT,
			});
		}
		workbench
	}
}

impl NbtWorkbench {
	#[inline]
	#[must_use]
	pub fn new() -> Self {
		Self::default()
	}

	#[inline]
	#[allow(clippy::equatable_if_let)]
	pub fn on_open_file(&mut self, path: &PathBuf) -> Option<()> {
		if let Ok(f) = read(path) {
			let (nbt, compressed) = {
				if let Some(0x1F8B) = f.first_chunk::<2>().copied().map(u16::from_be_bytes) {
					(NbtElement::from_file(&DeflateDecoder::new(f.as_slice()).decode_gzip().ok()?)?, FileFormat::Gzip)
				} else if let Some(0x7801 | 0x789C | 0x78DA) = f.first_chunk::<2>().copied().map(u16::from_be_bytes) {
					(NbtElement::from_file(&DeflateDecoder::new(f.as_slice()).decode_zlib().ok()?)?, FileFormat::Gzip)
				} else if let Some(str) = NbtElement::from_str(core::str::from_utf8(&f).ok()?) {
					(
						match str {
							Ok((_, x)) | Err(x) => x,
						},
						FileFormat::Snbt,
					)
				} else {
					(NbtElement::from_file(&f)?, FileFormat::Nbt)
				}
			};
			match Tab::new(nbt, path, compressed, self.window_height) {
				None => None,
				Some(entry) => {
					self.tabs.push(entry);
					self.tab = self.tabs.len() - 1;
					Some(())
				}
			}
		} else {
			None
		}
	}

	#[inline]
	pub fn on_scroll(&mut self, scroll: MouseScrollDelta) -> bool {
		match scroll {
			MouseScrollDelta::LineDelta(h, v) => {
				let horizontal = self.held_keys.contains(&VirtualKeyCode::LShift) | self.held_keys.contains(&VirtualKeyCode::RShift);
				if let Some(tab) = self.tab_mut() {
					if horizontal {
						tab.set_horizontal_scroll(-v);
						tab.set_scroll(-h);
					} else {
						tab.set_horizontal_scroll(-h);
						tab.set_scroll(-v);
					}
				}
			}
			MouseScrollDelta::PixelDelta(_) => {}
		}
		true
	}

	#[inline]
	#[allow(clippy::collapsible_if)]
	pub fn on_mouse_input(&mut self, state: ElementState, button: MouseButton) -> bool {
		let horizontal_scroll = self.horizontal_scroll();
		if state == ElementState::Released {
			self.held_mouse_keys.remove(&button);
			let x = self.mouse_x;
			let y = self.mouse_y;
			if y < 19 && x > 2 && y > 3 {
				self.click_tab(button);
			} else if y > HEADER_SIZE {
				'a: {
					match core::mem::replace(&mut self.held_entry, HeldEntry::Empty) {
						HeldEntry::Empty => {}
						HeldEntry::FromAether(x) => {
							self.drop(x, None);
							break 'a;
						}
						HeldEntry::FromKnown(x, indices) => {
							self.drop(x, Some(indices));
							break 'a;
						}
					}

					if (y - HEADER_SIZE) < 16 && x > 32 + self.left_margin() {
						if self.rename(x + horizontal_scroll) {
							break 'a;
						}
					}

					if let MouseButton::Left | MouseButton::Right = button {
						if self.toggle() {
							break 'a;
						}
						if self.try_select_text() {
							break 'a;
						}
					}
				}
			}
		} else {
			if !self.close_selected_text() {
				self.selected_text = None;
			}
			self.held_mouse_keys.insert(button);
			'a: {
				if self.held_entry.is_empty() && self.mouse_y >= 23 && self.mouse_y < 39 {
					self.hold_entry(button);
					break 'a;
				}
				if self.held_entry.is_empty() && self.mouse_y >= HEADER_SIZE + 16 && self.mouse_x >= self.left_margin() + 16 {
					if self.steal() {
						break 'a;
					}
				}
			}
		}
		true
	}

	#[inline]
	fn steal(&mut self) -> bool {
		let left_margin = self.left_margin();
		let horizontal_scroll = self.horizontal_scroll();
		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		let target_depth = (self.mouse_x + horizontal_scroll - left_margin - 16) / 16;
		if let Some(tab) = self.tab_mut() && y < tab.value.height() && y > 0 {
			let (depth, height, true_height) = {
				let (depth, (_, _, element)) = Traverse::new(y, &mut tab.value).enumerate().last();
				(depth, element.height(), element.true_height())
			};
			if depth != target_depth { return false }

			let mut indices = vec![];
			let mut iter = TraverseParents::new(y, &mut tab.value);
			let value = 'w: {
				while let Some( (position, idx, _, element) ) = iter.next() {
					indices.push(idx);
					element.decrement(height, true_height);
					match position {
						Position::Only | Position::Last => {
							break 'w (unsafe { element.remove(idx).panic_unchecked("we asserted above that this indeed does capture the final element") })
						}
						Position::First | Position::Middle => {}
					}
				}
				unsafe { panic_unchecked("so you're telling me, we had a node, that was **not** root, as confirmed by the let chain for `NbtWorkbench::tab_mut()` yet somehow no parent's even existed, seriously wtf") }
			};

			self.held_entry = HeldEntry::FromKnown(value, indices.into_boxed_slice());
			true
		} else {
			false
		}
	}

	#[inline]
	fn rename(&mut self, offset: usize) -> bool {
		if let Some(tab) = self.tab() {
			let name = tab.path.as_ref().and_then(|x| x.to_str()).map_or_else(|| tab.name.clone(), Into::into);
			self.selected_text = SelectedText::new(36 + self.left_margin(), offset, HEADER_SIZE, Some((name, true)), None, vec![]);
			self.selected_text.is_some()
		} else {
			false
		}
	}

	#[inline]
	fn duplicate(&mut self) -> bool {
		if self.mouse_y < HEADER_SIZE {
			return false;
		}

		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		if let Some(tab) = self.tab_mut() && y < tab.value.height() && y > 0 {
			let (height, true_height) = unsafe { Traverse::new(y, &mut tab.value).last().map(|(_, _, element)| (element.height(), element.true_height())).panic_unchecked("we've asserted that y > 0") };

			let mut indices = vec![];
			let mut iter = TraverseParents::new(y, &mut tab.value);
			while let Some( (position, idx, key, element) ) = iter.next() {
				match position {
					Position::First | Position::Middle => {
						element.increment(height, true_height);
						indices.push(idx);
					},
					Position::Only | Position::Last => {
						indices.push(idx + 1);
						let duplicate = unsafe { element.get(idx).panic_unchecked("it exists mate, let's stop playing around") }.clone();
						match element {
							NbtElement::Compound(compound) => compound.insert_full(idx + 1, unsafe { key.panic_unchecked("it's a compound, it **has** a key for every value") }.into_string(), duplicate),
							// this is the same thing, literally the same thing, i refuse to believe that it can't be added
							element => { let _ =  element.insert(idx + 1, duplicate); }
						}
					}
				}
			}
			tab.undos.push(WorkbenchAction::Add { indices: indices.into_boxed_slice() });
			tab.history_changed = true;
			true
		} else {
			false
		}
	}

	#[inline]
	fn copy(&mut self) -> bool {
		if self.mouse_y < HEADER_SIZE {
			return false;
		}
		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		if let Some(tab) = self.tab_mut() && y < tab.value.height() {
			let (_, key, element) = unsafe { Traverse::new(y, &mut tab.value).last().panic_unchecked("There is always at least one element - Master Oogway") };
			cli_clipboard::set_contents(format!("{}{}{element}", key.unwrap_or(""), if key.is_some() { ":" } else { "" })).is_ok()
		} else {
			false
		}
	}

	#[inline]
	fn delete(&mut self, clipboard: bool) -> bool {
		if self.mouse_y < HEADER_SIZE {
			return false;
		};

		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		if let Some(tab) = self.tab_mut() && y < tab.value.height() && y > 0 {
			let mut indices = vec![];
			let (height, true_height) = if let Some((_, key, element)) = Traverse::new(y, &mut tab.value).last() {
				if clipboard {
					let _ = cli_clipboard::set_contents(format!("{}{}{element}", key.unwrap_or(""), if key.is_some() { ":" } else { "" }));
				}
				(element.height(), element.true_height())
			} else {
				unsafe { panic_unchecked("dis ain't even possible i never f**k'd with the traverse") }
			};
			let mut iter = TraverseParents::new(y, &mut tab.value);
			let element = 'w: {
				while let Some( (position, idx, _, element) ) = iter.next() {
					indices.push(idx);
					element.decrement(height, true_height);
					match position {
						Position::First | Position::Middle => {},
						Position::Last | Position::Only => break 'w (unsafe { element.remove(idx).panic_unchecked("da hell mate") }),
					}
				}
				unsafe { panic_unchecked("parents were dodged") }
			};
			tab.undos.push(WorkbenchAction::Remove {
				element,
				indices: indices.into_boxed_slice(),
			});
			true
		} else {
			false
		}
	}

	#[inline]
	fn drop(&mut self, element: Result<(Box<str>, NbtElement), NbtElement>, from_indices: Option<Box<[usize]>>) -> bool {
		let left_margin = self.left_margin();
		let horizontal_scroll = self.horizontal_scroll();
		if self.mouse_y <= HEADER_SIZE || self.mouse_x + horizontal_scroll < left_margin {
			return false;
		}

		let y = self.mouse_y - HEADER_SIZE + self.scroll();
		let x = (self.mouse_x + horizontal_scroll - left_margin) / 16;
		if let Some(tab) = self.tab_mut() {
			let mut indices = vec![];
			let (key, element) = element.map_or_else(|element| (None, element), |(x, y)| (Some(x), y));
			let (value, value_selectable) = element.value();

			match NbtElement::drop(tab.value.as_mut(), key.clone(), element, &mut y.clone(), 2, x, &mut indices) {
				DropFn::InvalidType(key, element) | DropFn::Missed(key, element) => {
					if let Some(from_indices) = from_indices {
						tab.undos.push(WorkbenchAction::Remove {
							indices: from_indices,
							element: match key {
								Some(key) => Ok((key, element)),
								None => Err(element),
							},
						});
					}

					tab.history_changed = true;
					self.selected_text = None;
				}
				DropFn::Dropped(_, _, new_key) => {
					if let Some(from_indices) = from_indices {
						tab.undos.push(WorkbenchAction::Move {
							from: from_indices,
							to: indices.clone().into_boxed_slice(),
							original_key: key,
						});
					} else {
						tab.undos.push(WorkbenchAction::Add {
							indices: indices.clone().into_boxed_slice(),
						});
					}

					let value = if let Some(new_key) = new_key.clone() {
						Some((
							None,
							String::new(),
							new_key,
							if value_selectable { ": ".to_owned() } else { format!(": {value}") },
							if value_selectable { Some(value.into_string()) } else { None },
						))
					} else if value_selectable {
						Some((new_key.clone().map(str::into_string), new_key.map_or(String::new(), |_| ": ".to_owned()), value, String::new(), None))
					} else {
						None
					};

					tab.history_changed = true;
					if let Some((keyfix, prefix, value, suffix, valuefix)) = value {
						self.selected_text = Some(
							SelectedText {
								y: sum_indices(indices.iter().copied(), &tab.value) * 16 + HEADER_SIZE,
								indices: indices.into_boxed_slice(),
								cursor: 0,
								selection: Some(value.len()),
								value: value.into_string(),
								keyfix,
								prefix,
								suffix,
								valuefix,
								editable: true,
								last_interaction: SystemTime::now(),
								undos: LinkedQueue::new(),
								redos: LinkedQueue::new(),
							}
								.post_process(),
						);
					} else {
						self.selected_text = None;
					}
				}
			}
			true
		} else {
			false
		}
	}

	#[inline]
	fn hold_entry(&mut self, button: MouseButton) {
		if button == MouseButton::Left {
			if self.mouse_x & !0b1111 == 192 && let Some(element) = cli_clipboard::get_contents().ok().and_then(|x| NbtElement::from_str(&x)) {
				self.held_entry = HeldEntry::FromAether(element);
			} else {
				self.held_entry = unsafe { HeldEntry::FromAether(Err(NbtElement::from_id(match self.mouse_x / 16 {
					0 => 1,
					1 => 2,
					2 => 3,
					3 => 4,
					4 => 5,
					5 => 6,
					6 => 7,
					7 => 11,
					8 => 12,
					9 => 8,
					10 => 9,
					11 => 10,
					_ => return
				}).panic_unchecked("Type was invalid somehow, even though we map each one"))) };
			}
		}
	}

	#[inline]
	fn click_tab(&mut self, button: MouseButton) {
		// todo, probably bad somewhere, check with rendering to see if im stupid
		if self.mouse_x < 2 {
			return;
		}

		let mut x = self.mouse_x - 2;
		for (idx, tab) in self.tabs.iter_mut().enumerate() {
			let width = tab.name.width() + 37;

			if x <= width {
				if button == MouseButton::Middle {
					self.tabs.remove(idx);
					self.tab = idx.saturating_sub(1);
				} else if idx == self.tab && x > width - 16 && x < width {
					tab.compression = tab.compression.cycle();
				} else if idx == self.tab && x + 1 >= width - 32 && x < width - 16 {
					tab.save();
				} else if button == MouseButton::Left {
					self.tab = idx;
				}

				break;
			}

			x -= width;

			if x < 6 {
				break;
			} else {
				x -= 6;
			}
		}
	}

	#[inline]
	fn left_margin(&self) -> usize {
		self.tab().map_or(0, |tab| (tab.value.true_height().ilog10() as usize + 1) * 8 + 4) + 8
	}

	#[inline]
	fn toggle(&mut self) -> bool {
		let left_margin = self.left_margin();
		let horizontal_scroll = self.horizontal_scroll();
		if self.mouse_x + horizontal_scroll < left_margin {
			return false;
		}
		if self.mouse_y < HEADER_SIZE {
			return false;
		}

		let x = (self.mouse_x + horizontal_scroll - left_margin) / 16;
		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		if let Some(tab) = self.tab_mut() && y < tab.value.height() {
			let (depth, (_, _, element)) = Traverse::new(y, &mut tab.value).enumerate().last();
			if depth != x {
				return false;
			}
			let before = element.height();
			let _ = element.toggle();
			let increment = element.height().wrapping_sub(before);
			if increment == 0 {
				return true;
			}
			let mut iter = Traverse::new(y, &mut tab.value);
			while let Some((position, _, _, element)) = iter.next() {
				match position {
					Position::First | Position::Middle => element.increment(increment, 0),
					Position::Last | Position::Only => {} // our element already was updated nimrod
				}
			}

			tab.scroll = tab.scroll();
			true
		} else {
			false
		}
	}

	#[inline]
	fn try_select_text(&mut self) -> bool {
		let left_margin = self.left_margin();
		let horizontal_scroll = self.horizontal_scroll();
		if self.mouse_x + horizontal_scroll < left_margin {
			return false;
		}
		if self.mouse_y < HEADER_SIZE + 16 {
			return false;
		}

		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		if let Some(tab) = self.tab_mut() {
			if y >= tab.value.height() {
				return false;
			}

			let mut indices = vec![];
			let mut iter = Traverse::new(y, &mut tab.value);
			let (key, value) = 'w: {
				while let Some((position, idx, key, value)) = iter.next() {
					match position {
						Position::First | Position::Only => {} // do nothing
						Position::Middle => {
							indices.push(idx);
						}
						Position::Last => {
							indices.push(idx);
							break 'w (key.map(|x| (x.to_owned().into_boxed_str(), true)), Some(value.value()));
						}
					}
				}
				unsafe { panic_unchecked("Hit heat when trying to select text") }
			};

			self.selected_text = SelectedText::new(indices.len() * 16 + 32 + 4 + left_margin, self.mouse_x + horizontal_scroll, y * 16 + HEADER_SIZE, key, value, indices);
			self.selected_text.is_some()
		} else {
			false
		}
	}

	#[inline]
	pub fn keyfix(&mut self) {
		if let Some(SelectedText { y, indices, cursor, value, selection, keyfix, prefix, suffix, valuefix, editable: true, last_interaction: _, undos: _, redos: _ }) = self.selected_text.clone() && let Some(keyfix) = keyfix && valuefix.is_none() && suffix.is_empty() && cursor == 0 {
			if !self.close_selected_text() { return; } // we'll still render the new one, even if its invalid text. cool right?
			self.selected_text = Some(SelectedText {
				// x,
				y,
				indices,
				cursor: keyfix.len(),
				selection,
				keyfix: None,
				prefix: String::new(),
				suffix: prefix,
				valuefix: Some(value),
				value: keyfix,
				editable: true,
				last_interaction: SystemTime::now(),
				undos: LinkedQueue::new(),
				redos: LinkedQueue::new(),
			}.post_process());
		}
	}

	#[inline]
	pub fn valuefix(&mut self) {
		if let Some(SelectedText {y, indices, cursor, value, selection, keyfix, prefix, suffix, valuefix, editable: true, last_interaction: _, undos: _, redos: _ }) = self.selected_text.clone() && let Some(valuefix) = valuefix && keyfix.is_none() && prefix.is_empty() && cursor == value.len() {
			// normally won't occur, but im future proofing
			if !self.close_selected_text() { return; } // we'll still render the new one, even if its invalid text. cool right?
			self.selected_text = Some(SelectedText {
				y,
				indices,
				cursor: 0,
				selection,
				keyfix: Some(value),
				prefix: suffix,
				suffix: String::new(),
				valuefix: None,
				value: valuefix,
				editable: true,
				last_interaction: SystemTime::now(),
				undos: LinkedQueue::new(),
				redos: LinkedQueue::new(),
			}.post_process());
		}
	}

	#[inline]
	pub fn shift_selected_text_up(&mut self) {
		if let Some(SelectedText { y, indices, .. }) = &mut self.selected_text {
			if indices.is_empty() {
				return;
			} // well it could be empty
			let tab = tab_mut_unchecked!(self);
			let child_idx = unsafe { indices.last().copied().panic_unchecked("Indices list is never empty") };
			let parent = Navigate::new(indices.iter().copied().take(indices.len() - 1), &mut tab.value).last().2;
			if child_idx == 0 || parent.len() == Some(0) {
				return;
			}
			let original_key = match parent {
				NbtElement::Compound(compound) => unsafe { Some(compound.get(child_idx).panic_unchecked("Index obviously exists").0.to_owned().into_boxed_str()) },
				_ => None,
			};
			*y -= unsafe { parent.get(child_idx - 1).panic_unchecked("if i exist, the one before me exists") }.height() * 16;
			parent.swap(child_idx, child_idx - 1);
			let from = indices.clone();
			unsafe {
				*indices.last_mut().panic_unchecked("Indices list is never empty") -= 1;
			}
			let to = indices.clone();

			tab.undos.push(WorkbenchAction::Move { from, to, original_key });

			tab.scroll = tab.scroll.min((*y - HEADER_SIZE).saturating_sub(16));
			tab.scroll = tab.scroll();
		}
	}

	#[inline]
	pub fn shift_selected_text_down(&mut self) {
		if let Some(SelectedText { y, indices, .. }) = &mut self.selected_text {
			if indices.is_empty() {
				return;
			} // well it could be empty
			let tab = tab_mut_unchecked!(self);
			let child_idx = unsafe { indices.last().copied().panic_unchecked("Indices list is never empty") };
			let parent = Navigate::new(indices.iter().copied().take(indices.len() - 1), &mut tab.value).last().2;
			if parent.len().is_none_or(|x| x == child_idx + 1) {
				return;
			}
			let original_key = match parent {
				NbtElement::Compound(compound) => unsafe { Some(compound.get(child_idx).panic_unchecked("Index obviously exists").0.to_owned().into_boxed_str()) },
				_ => None,
			};
			*y += unsafe { parent.get(child_idx + 1).panic_unchecked("checked above") }.height() * 16;
			parent.swap(child_idx, child_idx + 1);
			let from = indices.clone();
			unsafe {
				*indices.last_mut().panic_unchecked("Indices list is never empty") += 1;
			}
			let to = indices.clone();

			tab.undos.push(WorkbenchAction::Move { from, to, original_key });

			if *y + 48 > tab.scroll + tab.window_height - HEADER_SIZE {
				tab.scroll = *y + 48 - (tab.window_height - HEADER_SIZE);
				tab.scroll = tab.scroll();
			}
		}
	}

	#[inline]
	pub fn selected_text_up(&mut self, ctrl: bool) {
		let left_margin = self.left_margin();
		if let Some(SelectedText {
			            y,
			            indices,
			            cursor,
			            keyfix,
			            prefix,
			            value: str_value,
			            ..
		            }) = self.selected_text.clone()
		{
			if indices.is_empty() {
				return;
			}

			if !self.close_selected_text() {
				return;
			}
			let cache_cursor_x = self.cache_cursor_x;
			if let Some(tab) = self.tab_mut() {
				let old_depth = indices.len();
				let mouse_x = cache_cursor_x.unwrap_or(old_depth * 16 + 32 + 4 + left_margin + keyfix.as_ref().map_or(0, StrExt::width) + prefix.width() + str_value.split_at(cursor).0.width());

				if y == HEADER_SIZE + 16 {
					let width = tab.path.as_ref().and_then(|x| x.to_str()).map_or_else(|| tab.name.width(), StrExt::width);
					self.rename(mouse_x.min(width + 32 + 4 + self.left_margin()));
					return;
				}

				let (k, v, indices, new_y) = if ctrl && indices.last().is_some_and(|x| *x > 0) {
					let mut indices = indices.into_vec();
					let tail = Navigate::new(indices.iter().copied().take(indices.len() - 1), &mut tab.value).last().2;
					unsafe {
						*indices.last_mut().panic_unchecked("indices cannot be empty, we're in it") = 0;
					}
					let iter = unsafe { tail.children().panic_unchecked("we are a child, some must exist") };
					let (k, v) = match iter {
						Ok(mut children) => (None, Some(unsafe { children.next().panic_unchecked("we are a child, some must exist") }.value())),
						Err(mut children) => unsafe {
							children
								.next()
								.map(|(k, v)| (Some((k.clone(), true)), Some(v.value())))
								.panic_unchecked("we are a child, some parent must exist")
						},
					};
					let new_y = sum_indices(indices.iter().copied(), &tab.value) * 16;
					(k, v, indices, new_y)
				} else {
					let total = sum_indices(indices.iter().copied(), &tab.value) - 1;
					let mut indices = vec![];
					// SAFETY: total is -1'd meaning that it's original range of 1..=root.height() is now ..root.height(), which is in range
					let (k, v) = 'w: {
						let mut iter = Traverse::new(total, &mut tab.value);
						let _ = iter.next();
						while let Some((position, idx, key, value)) = iter.next() {
							indices.push(idx);
							if position == Position::Last {
								break 'w (key.map(|k| (k.to_owned().into_boxed_str(), true)), Some(value.value()));
							}
						}
						unsafe { panic_unchecked("Iterator was empty, somehow") }
					};
					(k, v, indices, y - 16 - HEADER_SIZE)
				};
				tab.scroll = tab.scroll.min(new_y.saturating_sub(16));
				tab.scroll = tab.scroll();
				let low = indices.len() * 16 + 32 + 4 + left_margin;
				let high = low
					+ k.as_ref().map_or(0, |(x, display)| x.width() * (*display as usize))
					+ (k.as_ref().is_some_and(|(_, display)| *display) && v.as_ref().is_some_and(|(_, display)| *display)) as usize * (": ".width())
					+ v.as_ref().map_or(0, |(x, display)| (*display as usize) * x.width());
				self.cache_cursor_x = self.cache_cursor_x.or(Some(mouse_x));
				self.selected_text = SelectedText::new(low, mouse_x.clamp(low, high), new_y + HEADER_SIZE, k, v, indices);
			}
		}
	}

	#[inline]
	pub fn selected_text_down(&mut self, ctrl: bool) {
		let left_margin = self.left_margin();
		if let Some(SelectedText {
			            y,
			            indices,
			            cursor,
			            keyfix,
			            prefix,
			            value: str_value,
			            ..
		            }) = self.selected_text.clone()
		{
			let total = if let Some(tab) = self.tab() {
				let mut total = sum_indices(indices.iter().copied(), &tab.value);
				total += 1; // move down
				if total >= tab.value.height() {
					return;
				} // down needs a check that it doesn't encroach the bottom
				total
			} else {
				return;
			};
			if !self.close_selected_text() {
				return;
			}
			let cache_cursor_x = self.cache_cursor_x;
			if let Some(tab) = self.tab_mut() {
				let old_depth = indices.len();
				let (k, v, end_idx) = if ctrl && indices.len() > 0 {
					let iter = unsafe {
						Navigate::new(indices.iter().copied().take(indices.len() - 1), &mut tab.value)
							.last()
							.2
							.children()
							.panic_unchecked("we're the child")
					};
					unsafe {
						iter.map_or_else(
							|iter| {
								let tuple = iter.enumerate().last().panic_unchecked("we're the child, it can't be empty");
								(Some((tuple.1 .0.clone(), true)), Some(tuple.1 .1.value()), tuple.0)
							},
							|iter| {
								let tuple = iter.enumerate().last().panic_unchecked("we're the child, it can't be empty");
								(None, Some(tuple.1.value()), tuple.0)
							},
						)
					}
				} else {
					(None, None, 0)
				};
				let (k, v, indices, new_y) = if ctrl && indices.len() > 0 && !indices.last().is_some_and(|x| *x == end_idx) {
					let mut indices = indices.into_vec();
					unsafe { *indices.last_mut().panic_unchecked("it literally just can't be empty") = end_idx };
					let new_y = sum_indices(indices.iter().copied(), &tab.value) * 16;
					(k, v, indices, new_y)
				} else {
					'w: {
						let mut indices = vec![];
						let mut iter = Traverse::new(total, &mut tab.value);
						let _ = iter.next();
						while let Some((position, idx, key, value)) = iter.next() {
							indices.push(idx);
							if position == Position::Last {
								break 'w (key.map(|k| (k.to_owned().into_boxed_str(), true)), Some(value.value()), indices, y + 16 - HEADER_SIZE);
							}
						}
						unsafe { panic_unchecked("Iterator was empty, somehow") }
					}
				};
				let low = indices.len() * 16 + 32 + 4 + left_margin;
				let high = low
					+ k.as_ref().map_or(0, |(x, display)| x.width() * (*display as usize))
					+ (k.as_ref().is_some_and(|(_, display)| *display) && v.as_ref().is_some_and(|(_, display)| *display)) as usize * ": ".width()
					+ v.as_ref().map_or(0, |(x, display)| (*display as usize) * x.width());
				let mouse_x = cache_cursor_x.unwrap_or(old_depth * 16 + 32 + 4 + left_margin + keyfix.as_ref().map_or(0, StrExt::width) + prefix.width() + str_value.split_at(cursor).0.width());
				if new_y + 48 > tab.scroll + tab.window_height - HEADER_SIZE {
					tab.scroll = new_y + 48 - (tab.window_height - HEADER_SIZE);
					tab.scroll = tab.scroll();
				}
				self.cache_cursor_x = self.cache_cursor_x.or(Some(mouse_x));
				self.selected_text = SelectedText::new(low, mouse_x.clamp(low, high), new_y + HEADER_SIZE, k, v, indices);
			}
		}
	}

	#[inline]
	pub fn force_close(&mut self) {
		if let Some(SelectedText { indices, .. }) = self.selected_text.as_ref() {
			let indices = indices.clone();
			if let Some(tab) = self.tab_mut() {
				let element = Navigate::new(indices.iter().copied(), &mut tab.value).last().2;
				let decrement = element.height() - 1;
				if element.open() && element.toggle().is_some() {
					let mut iter = Navigate::new(indices.iter().copied(), &mut tab.value);
					while let Some((position, _, _, value)) = iter.next() {
						if let Position::First | Position::Middle = position {
							value.decrement(decrement, 0);
						}
					}
				}
			}
		}
	}

	#[inline]
	pub fn force_open(&mut self) {
		if let Some(SelectedText { indices, .. }) = self.selected_text.as_ref() {
			let indices = indices.clone();
			if let Some(tab) = self.tab_mut() {
				let element = Navigate::new(indices.iter().copied(), &mut tab.value).last().2;
				if !element.open() && element.toggle().is_some() {
					let increment = element.height() - 1;
					let mut iter = Navigate::new(indices.iter().copied(), &mut tab.value);
					while let Some((position, _, _, value)) = iter.next() {
						if let Position::First | Position::Middle = position {
							value.increment(increment, 0);
						}
					}
				}
			}
		}
	}

	#[inline]
	#[must_use]
	pub fn close_selected_text(&mut self) -> bool {
		if let Some(SelectedText {
			            indices,
			            value,
			            prefix,
			            suffix,
			            editable: true,
			            ..
		            }) = self.selected_text.clone()
		{
			if let Some(tab) = self.tab_mut() {
				if indices.is_empty() {
					let buf = PathBuf::from(value);
					if let Some(name) = buf.file_name().and_then(std::ffi::OsStr::to_str) {
						tab.name = name.into();
						tab.path = Some(buf);
					}
				} else {
					let value = value.into_boxed_str();
					let key = prefix.is_empty() && !suffix.is_empty();
					let (&last, rem) = unsafe { indices.split_last().panic_unchecked("Indices were empty somehow") };
					let previous = {
						let element = Navigate::new(rem.iter().copied(), &mut tab.value).last().2;
						if key {
							if let NbtElement::Compound(compound) = element {
								unsafe {
									if compound.get(last).panic_unchecked("this will exist").0 == value.as_ref() || compound.entries.contains_key(value.as_ref()) {
										return false;
									}
								}
								compound.update_key(last, value.clone()).unwrap_or(value)
							} else {
								unsafe { panic_unchecked("Expected key-value indices chain tail to be of type compound") }
							}
						} else {
							// no drops dw, well except for the value, but that's a simple thing dw
							unsafe {
								element
									.get_mut(last)
									.panic_unchecked("Last index was valid")
									.set_value(value)
									.panic_unchecked("Type of indices tail can accept value writes")
							}
						}
					};

					tab.undos.push(WorkbenchAction::Rename { indices, key, previous });

					tab.history_changed = true;
					self.selected_text = None;
				}
			}
		}
		true
	}

	#[cfg_attr(not(debug_assertions), inline)]
	#[allow(clippy::collapsible_if, clippy::too_many_lines)]
	pub fn on_key_input(&mut self, key: KeyboardInput) -> bool {
		match key.state {
			ElementState::Pressed => {
				if let Some(key) = key.virtual_keycode {
					self.held_keys.insert(key);
					let char = self.char_from_key(key);
					let flags = (self.held_keys.contains(&VirtualKeyCode::LControl) as u8 | self.held_keys.contains(&VirtualKeyCode::RControl) as u8)
						| ((self.held_keys.contains(&VirtualKeyCode::LShift) as u8 | self.held_keys.contains(&VirtualKeyCode::RShift) as u8) << 1)
						| ((self.held_keys.contains(&VirtualKeyCode::LAlt) as u8 | self.held_keys.contains(&VirtualKeyCode::RAlt) as u8) << 2);
					if let Some(selected_text) = &mut self.selected_text {
						match selected_text.on_key_press(key, char, flags) {
							KeyResult::NothingSpecial => {
								selected_text.handle_history();
								self.cache_cursor_x = None;
								return true;
							}
							KeyResult::Revert => {
								self.cache_cursor_x = None;
								self.selected_text = None;
								return true;
							}
							KeyResult::Finish => {
								self.cache_cursor_x = None;
								// we just won't let you leave if you didn't fix it ;)
								let _ = self.close_selected_text();
								return true;
							}
							KeyResult::Keyfix => {
								self.cache_cursor_x = None;
								self.keyfix();
								return true;
							}
							KeyResult::Valuefix => {
								self.cache_cursor_x = None;
								self.valuefix();
								return true;
							}
							KeyResult::Up(ctrl) => {
								self.selected_text_up(ctrl);
								return true;
							}
							KeyResult::Down(ctrl) => {
								self.selected_text_down(ctrl);
								return true;
							}
							KeyResult::ForceClose => {
								selected_text.handle_history();
								self.force_close();
								return true;
							}
							KeyResult::ForceOpen => {
								selected_text.handle_history();
								self.force_open();
								return true;
							}
							KeyResult::ShiftUp => {
								selected_text.handle_history();
								self.shift_selected_text_up();
								return true;
							}
							KeyResult::ShiftDown => {
								selected_text.handle_history();
								self.shift_selected_text_down();
								return true;
							}
							KeyResult::Failed => {} // next thing pls
						}
					}
					if !self.held_entry.is_empty() && key == VirtualKeyCode::Escape && flags == flags!() {
						self.held_entry = HeldEntry::Empty;
						return true;
					}
					{
						if key == VirtualKeyCode::Key1 {
							self.tab = 0.min(self.tabs.len().saturating_sub(1));
							return true;
						}
						if key == VirtualKeyCode::Key2 {
							self.tab = 1.min(self.tabs.len().saturating_sub(1));
							return true;
						}
						if key == VirtualKeyCode::Key3 {
							self.tab = 2.min(self.tabs.len().saturating_sub(1));
							return true;
						}
						if key == VirtualKeyCode::Key4 {
							self.tab = 3.min(self.tabs.len().saturating_sub(1));
							return true;
						}
						if key == VirtualKeyCode::Key5 {
							self.tab = 4.min(self.tabs.len().saturating_sub(1));
							return true;
						}
						if key == VirtualKeyCode::Key6 {
							self.tab = 5.min(self.tabs.len().saturating_sub(1));
							return true;
						}
						if key == VirtualKeyCode::Key7 {
							self.tab = 6.min(self.tabs.len().saturating_sub(1));
							return true;
						}
						if key == VirtualKeyCode::Key8 {
							self.tab = 7.min(self.tabs.len().saturating_sub(1));
							return true;
						}
						if key == VirtualKeyCode::Key9 {
							self.tab = self.tabs.len().saturating_sub(1);
							return true;
						}
					}
					if key == VirtualKeyCode::N && flags == flags!(Ctrl) {
						self.tabs.push(Tab {
							value: Box::new(NbtElement::Compound(NbtCompound::new())),
							name: "new.nbt".into(),
							path: None,
							compression: FileFormat::Nbt,
							undos: LinkedQueue::new(),
							redos: LinkedQueue::new(),
							history_changed: false,
							scroll: 0,
							horizontal_scroll: 0,
							window_height: WINDOW_HEIGHT,
						});
						self.tab = self.tabs.len() - 1;
						self.selected_text = None;
						return true;
					}
					if key == VirtualKeyCode::S && flags == flags!(Ctrl) {
						if let Some(tab) = self.tab_mut() {
							if tab.save() {
								self.selected_text = None;
								return true;
							}
						}
					}
					if key == VirtualKeyCode::W && flags == flags!(Ctrl) {
						if let Some(tab) = self.tab() && !tab.history_changed {
							self.tabs.remove(self.tab);
							if self.tab >= self.tabs.len() && self.tab > 0 {
								self.tab -= 1;
							}
							self.selected_text = None;
							return true;
						}
					}
					if key == VirtualKeyCode::Z && flags == flags!(Ctrl) {
						if let Some(tab) = self.tab_mut() {
							if let Some(action) = tab.undos.pop() {
								tab.redos.push(action.undo(&mut tab.value));
								self.selected_text = None;
								return true;
							}
						}
					}
					if key == VirtualKeyCode::Y && flags == flags!(Ctrl) {
						if let Some(tab) = self.tab_mut() {
							if let Some(action) = tab.redos.pop() {
								tab.undos.push(action.undo(&mut tab.value));
								self.selected_text = None;
								return true;
							}
						}
					}
					if ((key == VirtualKeyCode::Back || key == VirtualKeyCode::Delete) && flags == flags!()) || (key == VirtualKeyCode::X && flags == flags!(Ctrl)) {
						if self.delete(flags & flags!(Ctrl) > 0) {
							self.selected_text = None;
							return true;
						}
					}
					if key == VirtualKeyCode::D && flags == flags!(Ctrl) {
						if self.duplicate() {
							self.selected_text = None;
							return true;
						}
					}
					if key == VirtualKeyCode::C && flags == flags!(Ctrl) {
						if self.copy() {
							self.selected_text = None;
							return true;
						}
					}
					if let Some(element) = cli_clipboard::get_contents().ok().and_then(|x| NbtElement::from_str(&x)) && key == VirtualKeyCode::V && flags == flags!(Ctrl) {
						if self.drop(element, None) {
							return true;
						}
					}
				}
			}
			ElementState::Released => {
				if let Some(x) = key.virtual_keycode {
					self.held_keys.remove(&x);
				}
			}
		}

		false
	}

	#[inline]
	pub fn on_cursor_move(&mut self, pos: PhysicalPosition<f64>) -> bool {
		self.mouse_x = pos.x as usize;
		self.mouse_y = pos.y as usize;
		true
	}

	#[inline]
	pub fn window_height(&mut self, window_height: usize) {
		self.window_height = window_height;
	}

	#[inline]
	#[must_use]
	pub fn scroll(&self) -> usize {
		self.tab().map_or(0, Tab::scroll)
	}

	#[inline]
	#[must_use]
	pub fn horizontal_scroll(&self) -> usize {
		self.tab().map_or(0, |tab| tab.horizontal_scroll) & !0b1111
	}

	#[inline]
	fn tab(&self) -> Option<&Tab> {
		self.tabs.get(self.tab)
	}

	#[inline]
	fn tab_mut(&mut self) -> Option<&mut Tab> {
		self.tabs.get_mut(self.tab)
	}

	#[inline]
	pub fn render(&self, builder: &mut VertexBufferBuilder) {
		self.render_tabs(builder);
		self.render_icons(builder);
		if let Some(tab) = self.tab() {
			let left_margin = self.left_margin();
			let horizontal_scroll = tab.horizontal_scroll;
			let mut highlight = if self.mouse_y < HEADER_SIZE || self.mouse_x + horizontal_scroll < left_margin {
				(0, 0)
			} else {
				(self.mouse_x + horizontal_scroll, ((self.mouse_y - HEADER_SIZE) & !0b1111) + HEADER_SIZE)
			};
			let ghost = if self.mouse_x + horizontal_scroll >= left_margin && self.mouse_y >= HEADER_SIZE {
				self.held_entry.element().map(|x| {
					(
						x.id(),
						((self.mouse_x + horizontal_scroll - left_margin) & !0b1111) + left_margin,
						((self.mouse_y - HEADER_SIZE) & !0b0111) + HEADER_SIZE,
					)
				})
			} else {
				None
			};
			if self.selected_text.is_some() {
				highlight = (0, 0);
			}
			if !self.held_entry.is_empty() {
				highlight = (0, 0);
			}
			let forbidden = (
				self.selected_text.as_ref().map(|x| x.y).and_then(|x| x.checked_sub(builder.scroll())).unwrap_or(0),
				self.selected_text.as_ref().and_then(|text| {
					if text.keyfix.is_none() && text.valuefix.is_some() {
						Some(text.value.clone().into_boxed_str())
					} else if let Some(keyfix) = text.keyfix.as_ref() && text.valuefix.is_none() {
						Some(keyfix.clone().into_boxed_str())
					} else {
						None
					}
				}),
			);
			let mut ctx = RenderContext::new(forbidden, highlight, ghost, left_margin, (tab.value.height() - 1) * 16 + HEADER_SIZE - tab.scroll);
			tab.render(builder, &mut ctx);
			if let Some(selected_text) = &self.selected_text {
				builder.horizontal_scroll = horizontal_scroll;
				selected_text.render(builder, left_margin);
				builder.horizontal_scroll = 0;
			}
		}
		self.render_held_entry(builder);
	}

	#[inline]
	fn render_held_entry(&self, builder: &mut VertexBufferBuilder) {
		if let Some(element) = self.held_entry.element() {
			NbtElement::render_icon(element.id(), self.mouse_x.saturating_sub(8), self.mouse_y.saturating_sub(8), builder);
		}
	}

	#[inline]
	fn render_icons(&self, builder: &mut VertexBufferBuilder) {
		NbtByte::render_icon(0, 26, builder);
		NbtShort::render_icon(16, 26, builder);
		NbtInt::render_icon(32, 26, builder);
		NbtLong::render_icon(48, 26, builder);
		NbtFloat::render_icon(64, 26, builder);
		NbtDouble::render_icon(80, 26, builder);
		NbtByteArray::render_icon(96, 26, builder);
		NbtIntArray::render_icon(112, 26, builder);
		NbtLongArray::render_icon(128, 26, builder);
		NbtString::render_icon(144, 26, builder);
		NbtList::render_icon(160, 26, builder);
		NbtCompound::render_icon(176, 26, builder);
		builder.draw_texture((192, 26), (0, 48), (16, 16));

		if self.mouse_x < 208 && self.mouse_y >= 23 && self.mouse_y < 39 {
			builder.draw_texture((self.mouse_x & !0b1111, 26), (0, 32), (16, 16));
		}
	}

	#[inline]
	fn render_tabs(&self, builder: &mut VertexBufferBuilder) {
		let mut offset = 3;
		for (idx, tab) in self.tabs.iter().enumerate() {
			let u = (idx == self.tab) as usize * 16 + 16;
			builder.draw_texture((offset, 3), (u, 32), (3, 16));
			builder.settings(offset + 3, 3, false);
			let _ = write!(builder, "{}", tab.name);
			let remaining_width = tab.name.width() + 32 + 6;
			builder.draw_texture_region_z((offset, 3), 0.0, (u + 3, 32), (remaining_width, 16), (10, 16));
			offset += remaining_width;
			builder.draw_texture((offset, 3), (u + 13, 32), (3, 16));
			builder.draw_texture((offset - 32, 3), (96 - tab.history_changed as usize * 16, 32), (16, 16));
			builder.draw_texture((offset - 16, 3), (16 + tab.compression as usize * 16, 48), (16, 16));
			offset += 6;
		}
		builder.draw_texture_region_z((0, 21), 0.0, (48, 32), (builder.window_width(), 2), (16, 2));
		builder.draw_texture_region_z((0, 45), 0.0, (48, 32), (builder.window_width(), 2), (16, 2));
	}

	#[inline]
	#[allow(clippy::cognitive_complexity, clippy::match_same_arms, clippy::too_many_lines)]
	fn char_from_key(&self, key: VirtualKeyCode) -> Option<char> {
		if self.held_keys.contains(&VirtualKeyCode::LControl) || self.held_keys.contains(&VirtualKeyCode::RControl) {
			return None;
		}
		let shift = self.held_keys.contains(&VirtualKeyCode::LShift) || self.held_keys.contains(&VirtualKeyCode::RShift);
		Some(match key {
			VirtualKeyCode::Key1 => {
				if shift {
					'!'
				} else {
					'1'
				}
			}
			VirtualKeyCode::Key2 => {
				if shift {
					'@'
				} else {
					'2'
				}
			}
			VirtualKeyCode::Key3 => {
				if shift {
					'#'
				} else {
					'3'
				}
			}
			VirtualKeyCode::Key4 => {
				if shift {
					'$'
				} else {
					'4'
				}
			}
			VirtualKeyCode::Key5 => {
				if shift {
					'%'
				} else {
					'5'
				}
			}
			VirtualKeyCode::Key6 => {
				if shift {
					'^'
				} else {
					'6'
				}
			}
			VirtualKeyCode::Key7 => {
				if shift {
					'&'
				} else {
					'7'
				}
			}
			VirtualKeyCode::Key8 => {
				if shift {
					'*'
				} else {
					'8'
				}
			}
			VirtualKeyCode::Key9 => {
				if shift {
					'('
				} else {
					'9'
				}
			}
			VirtualKeyCode::Key0 => {
				if shift {
					')'
				} else {
					'0'
				}
			}
			VirtualKeyCode::A => {
				if shift {
					'A'
				} else {
					'a'
				}
			}
			VirtualKeyCode::B => {
				if shift {
					'B'
				} else {
					'b'
				}
			}
			VirtualKeyCode::C => {
				if shift {
					'C'
				} else {
					'c'
				}
			}
			VirtualKeyCode::D => {
				if shift {
					'D'
				} else {
					'd'
				}
			}
			VirtualKeyCode::E => {
				if shift {
					'E'
				} else {
					'e'
				}
			}
			VirtualKeyCode::F => {
				if shift {
					'F'
				} else {
					'f'
				}
			}
			VirtualKeyCode::G => {
				if shift {
					'G'
				} else {
					'g'
				}
			}
			VirtualKeyCode::H => {
				if shift {
					'H'
				} else {
					'h'
				}
			}
			VirtualKeyCode::I => {
				if shift {
					'I'
				} else {
					'i'
				}
			}
			VirtualKeyCode::J => {
				if shift {
					'J'
				} else {
					'j'
				}
			}
			VirtualKeyCode::K => {
				if shift {
					'K'
				} else {
					'k'
				}
			}
			VirtualKeyCode::L => {
				if shift {
					'L'
				} else {
					'l'
				}
			}
			VirtualKeyCode::M => {
				if shift {
					'M'
				} else {
					'm'
				}
			}
			VirtualKeyCode::N => {
				if shift {
					'N'
				} else {
					'n'
				}
			}
			VirtualKeyCode::O => {
				if shift {
					'O'
				} else {
					'o'
				}
			}
			VirtualKeyCode::P => {
				if shift {
					'P'
				} else {
					'p'
				}
			}
			VirtualKeyCode::Q => {
				if shift {
					'Q'
				} else {
					'q'
				}
			}
			VirtualKeyCode::R => {
				if shift {
					'R'
				} else {
					'r'
				}
			}
			VirtualKeyCode::S => {
				if shift {
					'S'
				} else {
					's'
				}
			}
			VirtualKeyCode::T => {
				if shift {
					'T'
				} else {
					't'
				}
			}
			VirtualKeyCode::U => {
				if shift {
					'U'
				} else {
					'u'
				}
			}
			VirtualKeyCode::V => {
				if shift {
					'V'
				} else {
					'v'
				}
			}
			VirtualKeyCode::W => {
				if shift {
					'W'
				} else {
					'w'
				}
			}
			VirtualKeyCode::X => {
				if shift {
					'X'
				} else {
					'x'
				}
			}
			VirtualKeyCode::Y => {
				if shift {
					'Y'
				} else {
					'y'
				}
			}
			VirtualKeyCode::Z => {
				if shift {
					'Z'
				} else {
					'z'
				}
			}
			VirtualKeyCode::Space => ' ',
			VirtualKeyCode::Caret => '^',
			VirtualKeyCode::Numpad0 => '0',
			VirtualKeyCode::Numpad1 => '1',
			VirtualKeyCode::Numpad2 => '2',
			VirtualKeyCode::Numpad3 => '3',
			VirtualKeyCode::Numpad4 => '4',
			VirtualKeyCode::Numpad5 => '5',
			VirtualKeyCode::Numpad6 => '6',
			VirtualKeyCode::Numpad7 => '7',
			VirtualKeyCode::Numpad8 => '8',
			VirtualKeyCode::Numpad9 => '9',
			VirtualKeyCode::NumpadAdd => '+',
			VirtualKeyCode::NumpadDivide => '/',
			VirtualKeyCode::NumpadDecimal => '.',
			VirtualKeyCode::NumpadComma => ',',
			VirtualKeyCode::NumpadEquals => '=',
			VirtualKeyCode::NumpadMultiply => '*',
			VirtualKeyCode::NumpadSubtract => '-',
			VirtualKeyCode::Apostrophe => {
				if shift {
					'"'
				} else {
					'\''
				}
			}
			VirtualKeyCode::Asterisk => '*',
			VirtualKeyCode::Backslash => {
				if shift {
					'|'
				} else {
					'\\'
				}
			}
			VirtualKeyCode::Colon => ':',
			VirtualKeyCode::Comma => {
				if shift {
					'<'
				} else {
					','
				}
			}
			VirtualKeyCode::Equals => {
				if shift {
					'+'
				} else {
					'='
				}
			}
			VirtualKeyCode::Grave => {
				if shift {
					'~'
				} else {
					'`'
				}
			}
			VirtualKeyCode::LBracket => {
				if shift {
					'{'
				} else {
					'['
				}
			}
			VirtualKeyCode::Minus => {
				if shift {
					'_'
				} else {
					'-'
				}
			}
			VirtualKeyCode::Period => {
				if shift {
					'>'
				} else {
					'.'
				}
			}
			VirtualKeyCode::Plus => '+',
			VirtualKeyCode::RBracket => {
				if shift {
					'}'
				} else {
					']'
				}
			}
			VirtualKeyCode::Semicolon => {
				if shift {
					':'
				} else {
					';'
				}
			}
			VirtualKeyCode::Slash => {
				if shift {
					'?'
				} else {
					'/'
				}
			}
			VirtualKeyCode::Tab => '\t',
			_ => return None,
		})
	}
}

pub struct RenderContext {
	forbidden_y: usize,
	forbidden_key: Option<Box<str>>,
	key_invalid: bool,
	highlight: (usize, usize),
	ghost: Option<(u8, usize, usize)>,
	left_margin: usize,
	end_y: usize,
	pub line_number: usize,
}

impl RenderContext {
	#[must_use]
	pub fn new(forbidden: (usize, Option<Box<str>>), highlight: (usize, usize), ghost: Option<(u8, usize, usize)>, left_margin: usize, end_y: usize) -> Self {
		Self {
			forbidden_y: forbidden.0,
			forbidden_key: forbidden.1,
			key_invalid: false,
			highlight,
			ghost,
			left_margin,
			end_y,
			line_number: 1,
		}
	}

	pub fn highlight(&self, pos: (usize, usize), text_width: usize, builder: &mut VertexBufferBuilder) {
		if pos.1 == self.highlight.1 && self.highlight.0 <= pos.0 + if text_width > 0 { text_width + 4 } else { text_width } + 16 && self.highlight.0 >= pos.0 {
			builder.draw_texture(pos, (0, 32), (16, 16));
		}
	}

	pub fn check_key<F: FnOnce(&str) -> bool>(&mut self, f: F) {
		if let Some(forbidden_key) = self.forbidden_key.as_ref() {
			self.key_invalid = f(forbidden_key);
		}
	}

	#[must_use]
	pub fn forbid(&self, x: usize, y: usize, builder: &mut VertexBufferBuilder) -> bool {
		if y == self.forbidden_y {
			if let Some(forbidden_key) = self.forbidden_key.as_ref() && self.key_invalid {
				let mut width = forbidden_key.width();
				let mut offset = 20;
				// texture varies across x axis so we can't stretch this
				loop {
					builder.draw_texture((x + offset, y + 14), (64, 46), (width.min(16), 2));
					offset += width.min(16);
					if width == 0 { break }
					width = width.saturating_sub(16);
				}
			}
			false
		} else {
			true
		}
	}

	pub fn line_number(&mut self, y: usize, builder: &mut VertexBufferBuilder) {
		let line_number = self.line_number;
		let color = core::mem::replace(&mut builder.color, 0xAA);
		builder.settings(self.left_margin - 9 - (line_number.ilog10() as usize + 1) * 8, y, false);
		let _ = write!(builder, "{line_number}");
		builder.color = color;
		builder.draw_texture_z((builder.text_coords.0 + 4, y), 0.5, (115 + (y >= self.end_y) as usize * 2, 32), (2, 16));
		self.line_number += 1;
	}

	pub fn ghost<F: FnOnce(usize, usize) -> bool>(&self, x_offset: usize, y_offset: usize, builder: &mut VertexBufferBuilder, f: F) -> bool {
		if let Some((id, x, y)) = self.ghost && f(x, y) {
			builder.draw_texture((x_offset, y_offset), match id {
				1 => (0, 64),
				2 => (16, 64),
				3 => (32, 64),
				4 => (48, 64),
				5 => (64, 64),
				6 => (80, 64),
				7 => (96, 64),
				8 => (16, 80),
				9 => (32, 80),
				10 => (48, 80),
				11 => (112, 64),
				12 => (0, 80),
				_ => unsafe { panic_unchecked("Invalid element id") },
			}, (16, 16));
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
	pub const fn is_empty(&self) -> bool {
		self.len == 0
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

#[allow(clippy::linkedlist)] // I want O(1) insertions, people may work on projects for hours, not that amortized stuff.
pub struct Tab {
	value: Box<NbtElement>,
	name: Box<str>,
	path: Option<PathBuf>,
	compression: FileFormat,
	undos: LinkedQueue<WorkbenchAction>,
	redos: LinkedQueue<WorkbenchAction>,
	history_changed: bool,
	scroll: usize,
	horizontal_scroll: usize,
	window_height: usize,
}

impl Tab {
	#[must_use]
	pub fn new(nbt: NbtElement, path: &Path, compression: FileFormat, window_height: usize) -> Option<Self> {
		if nbt.id() != NbtCompound::ID {
			return None;
		}

		Some(Self {
			value: Box::new(nbt),
			name: path.file_name()?.to_str()?.into(),
			path: Some(path.to_path_buf()),
			compression,
			undos: LinkedQueue::new(),
			redos: LinkedQueue::new(),
			history_changed: false,
			scroll: 0,
			horizontal_scroll: 0,
			window_height,
		})
	}

	pub fn save(&mut self) -> bool {
		if let Some(dir) = &self.path {
			let _ = write(dir, self.compression.encode(&self.value));
			self.history_changed = false;
			true
		} else {
			false
		}
	}

	pub fn render(&self, builder: &mut VertexBufferBuilder, ctx: &mut RenderContext) {
		if let box NbtElement::Compound(compound) = &self.value {
			builder.horizontal_scroll = self.horizontal_scroll;
			compound.render_root(builder, &self.name, ctx);
			builder.horizontal_scroll = 0;
			if builder.window_height() >= HEADER_SIZE {
				let height = compound.height() * 16; // not adding 48 because that doesn't accurately represent
				let total = builder.window_height() - HEADER_SIZE;
				if height > total {
					let height = height;
					let offset = (total as f64 / (height as f64 / (self.scroll() & !0b1111) as f64)) as usize + HEADER_SIZE; // safe to grab scroll
					let height = (total * total) / height;
					if height > 1 {
						builder.draw_texture((builder.window_width() - 4, offset), (112, 32), (3, 1));
						let mut remaining = height - 1;
						while remaining > 0 {
							builder.draw_texture((builder.window_width() - 4, offset + (height - remaining)), (112, 33), (3, (remaining).min(14)));
							remaining = if remaining <= 14 { 0 } else { remaining - 14 };
						}
						builder.draw_texture((builder.window_width() - 4, offset + height), (112, 47), (3, 1));
					}
				}
			}
		}
	}

	#[must_use]
	pub fn scroll(&self) -> usize {
		let height = self.value.height() * 16 + 48;
		let scroll = self.scroll;
		let max = (height + HEADER_SIZE).saturating_sub(self.window_height);
		scroll.min(max) & !0b1111
	}

	pub fn set_scroll(&mut self, scroll: f32) {
		const SCROLL_MULTIPLIER: f32 = 48.0;

		if scroll.is_sign_negative() && self.scroll < (scroll * -SCROLL_MULTIPLIER) as usize {
			self.scroll = 0;
		} else if scroll.is_sign_negative() {
			self.scroll -= (scroll * -SCROLL_MULTIPLIER) as usize;
		} else {
			self.scroll += (scroll * SCROLL_MULTIPLIER) as usize;
		}
		self.scroll = self.scroll();
	}

	pub fn set_horizontal_scroll(&mut self, scroll: f32) {
		const SCROLL_MULTIPLIER: f32 = 48.0;

		if scroll.is_sign_negative() && self.horizontal_scroll < (scroll * -SCROLL_MULTIPLIER) as usize {
			self.horizontal_scroll = 0;
		} else if scroll.is_sign_negative() {
			self.horizontal_scroll -= (scroll * -SCROLL_MULTIPLIER) as usize;
		} else {
			self.horizontal_scroll += (scroll * SCROLL_MULTIPLIER) as usize;
		}
	}
}

pub trait StrExt {
	fn snbt_string_read(&self) -> Option<(String, &str)>;

	fn needs_escape(&self) -> bool;

	fn width(&self) -> usize;
}

#[must_use]
pub const fn valid_unescaped_char(byte: u8) -> bool {
	matches!(byte, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'-' | b'.' | b'+')
}

impl StrExt for str {
	fn snbt_string_read(mut self: &Self) -> Option<(String, &str)> {
		if !self.starts_with('"') && !self.starts_with('\'') {
			let colon_idx = self.char_indices().find(|(_, c)| *c == ':').map(|(idx, _)| idx)?;
			let (s, s2) = self.split_at(colon_idx);
			let mut s3 = String::with_capacity(s.len());
			for byte in s.bytes() {
				if valid_unescaped_char(byte) {
					s3.push(byte as char);
				} else {
					return None;
				}
			}
			Some((s3, s2))
		} else {
			let enclosing = self.chars().nth(0)?;
			self = &self[1..];
			let mut buf = String::new();
			let mut backslash = false;
			let end = 'a: {
				for (idx, mut char) in self.char_indices() {
					if char == '\\' {
						if backslash {
							backslash = false;
						} else {
							backslash = true;
							continue;
						}
					}

					if char == enclosing {
						if backslash {
							backslash = false;
						} else {
							break 'a idx;
						}
					}

					if char == 'n' {
						if backslash {
							backslash = false;
							char = '\n';
						}
					}

					if char == 'r' {
						if backslash {
							backslash = false;
							char = '\r';
						}
					}

					if char == '0' {
						if backslash {
							backslash = false;
							char = '\0';
						}
					}

					if backslash {
						return None;
					}

					buf.push(char);
				}
				return None;
			};
			Some((buf, &self[(end + 1)..]))
		}
	}

	fn needs_escape(&self) -> bool {
		!self.bytes().all(valid_unescaped_char)
	}

	fn width(&self) -> usize {
		self.chars().map(|x| if (x as u32) < 56832 { VertexBufferBuilder::CHAR_WIDTH[x as usize] as usize } else { 0 }).sum()
	}
}

impl StrExt for String {
	fn snbt_string_read(&self) -> Option<(String, &str)> {
		str::snbt_string_read(self)
	}

	fn needs_escape(&self) -> bool {
		str::needs_escape(self)
	}

	fn width(&self) -> usize {
		str::width(self)
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
		self.map_or_else(|| panic_unchecked(msg), core::convert::identity)
	}

	fn is_none_or(self, f: impl FnOnce(T) -> bool) -> bool {
		self.map_or(true, f)
	}
}

/// # Safety
///
/// * This code better be unreachable otherwise it's UB without `debug_assertions`, just a panic with them however.
#[allow(unused_variables)] // intellij being freaky
unsafe fn panic_unchecked(msg: &str) -> ! {
	#[cfg(debug_assertions)]
	panic!("{msg}");

	#[cfg(not(debug_assertions))]
	core::hint::unreachable_unchecked()
}

pub mod elements {
	pub mod array;
	pub mod compound;
	pub mod element_type;
	pub mod list;
	pub mod primitive;
	pub mod string;
}

#[cfg(all(test, miri))]
mod miri_tests {
	use crate::NbtWorkbench;

	#[test]
	fn miri_interpretation() {
		let mut workbench = NbtWorkbench::new();

		workbench.mouse_y = 54;
		workbench.mouse_x = 57;

		workbench.toggle();

		workbench.mouse_x = 66;
		workbench.mouse_y = 184;

		workbench.toggle();

		workbench.mouse_x = 100;
		workbench.mouse_y = 283;

		workbench.delete(false);

		if let Some(tab) = workbench.tab_mut() {
			if let Some(action) = tab.undos.pop() {
				tab.redos.push(action.undo(&mut tab.value));
				workbench.selected_text = None;
			}
		}
	}
}
