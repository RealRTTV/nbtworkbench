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
#![feature(split_array)]
#![feature(array_chunks)]
#![feature(maybe_uninit_array_assume_init)]
#![cfg_attr(all(windows, not(debug_assertions)), windows_subsystem = "windows")]

use std::convert::identity;
use std::ffi::OsStr;
use std::fmt::Write;
use std::fs::read;
use std::path::PathBuf;
use std::str::FromStr;
use std::string::String;
use std::time::SystemTime;

use fxhash::{FxBuildHasher, FxHashSet};
use winit::dpi::PhysicalPosition;
use winit::event::{ElementState, KeyboardInput, MouseButton, MouseScrollDelta, VirtualKeyCode};
use zune_inflate::DeflateDecoder;

use elements::element_type::NbtElement;
use vertex_buffer_builder::VertexBufferBuilder;

use crate::assets::{BYTE_ARRAY_GHOST_UV, BYTE_ARRAY_UV, BYTE_GHOST_UV, BYTE_UV, CHUNK_GHOST_UV, COMPOUND_GHOST_UV, COMPOUND_UV, DOUBLE_GHOST_UV, DOUBLE_UV, EDITED_UV, END_LINE_NUMBER_SEPARATOR_UV, FLOAT_GHOST_UV, FLOAT_UV, HEADER_SIZE, HORIZONTAL_SEPARATOR_UV, INT_ARRAY_GHOST_UV, INT_ARRAY_UV, INT_GHOST_UV, INT_UV, LINE_NUMBER_SEPARATOR_UV, LIST_GHOST_UV, LIST_UV, LONG_ARRAY_GHOST_UV, LONG_ARRAY_UV, LONG_GHOST_UV, LONG_UV, REGION_GHOST_UV, SELECTED_TOGGLE_OFF_UV, SELECTED_TOGGLE_ON_UV, SELECTED_WIDGET_UV, SELECTION_UV, SHORT_GHOST_UV, SHORT_UV, STRING_GHOST_UV, STRING_UV, TEXT_UNDERLINE_UV, UNEDITED_UV, UNKNOWN_NBT_GHOST_UV, UNKNOWN_NBT_UV, UNSELECTED_TOGGLE_OFF_UV, UNSELECTED_TOGGLE_ON_UV, UNSELECTED_WIDGET_UV};
use crate::elements::chunk::{NbtChunk, NbtRegion};
use crate::elements::compound::NbtCompound;
use crate::elements::element_type::{NbtByte, NbtByteArray, NbtDouble, NbtFloat, NbtInt, NbtIntArray, NbtLong, NbtLongArray, NbtShort};
use crate::elements::list::{NbtList, ValueIterator};
use crate::elements::string::NbtString;
use crate::selected_text::{KeyResult, SelectedText};
use crate::tab::{FileFormat, Tab};
use crate::tree_travel::{Navigate, Traverse, TraverseParents};
use crate::vertex_buffer_builder::Vec2u;
use crate::window::{WINDOW_HEIGHT, WINDOW_WIDTH};
use crate::workbench_action::WorkbenchAction;

mod assets;
mod decoder;
mod encoder;
mod selected_text;
mod tab;
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
		unsafe { $this.tabs.get_mut($this.tab).panic_unchecked("Tab should've existed") }
	};
}

#[macro_export]
macro_rules! tab_mut {
	($this:expr, $($t:tt)*) => {
		let Some(tab) = $this.tabs.get_mut($this.tab) else { $($t)* };
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
		// REFACTOR

		// LONG TERM GOALS
		// todo, web assembly ver
		// todo, smart screen
		// todo, wiki page for docs on minecraft's format of stuff
		// todo, chunk section rendering

		// MINOR FEATURES
		// todo, ctrl + h, open a playground nbt file to help with user interaction
		// todo, shift + alt + right arrow to open fully (or shift + left click/right click)
		// todo, bookmarking line numbers
		// todo, ability to open a nbt array of some kind in a hex editor to then be listened on it's temp file for updates, then to remake the list live, allowing integration with other programs
		// todo, [art] icons for each workbench action, and a 5 most recent actions for undo and redo respectively

		// MAJOR FEATURES
		// todo, macros
		// todo, horizontal scrolling maximum value
		// todo, ctrl + r, reload current page
		// todo, keyboard-based element dropping (press numbers before to specify count for move operations, right shift to enable mode)
		// todo, region file to_bytes
	}

	window::run();
}

pub struct NbtWorkbench {
	tabs: Vec<Tab>,
	tab: usize,
	mouse_x: usize,
	mouse_y: usize,
	window_height: usize,
	window_width: usize,
	held_mouse_keys: FxHashSet<MouseButton>,
	held_keys: FxHashSet<VirtualKeyCode>,
	held_entry: HeldEntry,
	selected_text: Option<SelectedText>,
	cache_cursor_x: Option<usize>,
	tab_scroll: usize,
	scrollbar_offset: Option<usize>,
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
			NbtElement::Compound(compound) | NbtElement::Chunk(NbtChunk { inner: compound, .. }) => {
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
			NbtElement::Region(region) => {
				if idx >= region.len() {
					unsafe { panic_unchecked("region oob") }
				} else {
					total += 1;
					for jdx in 0..idx {
						// SAFETY: n < len (is valid bounds) implies n - m (where m is a positive integer <= n) < len (is valid bounds)
						total += unsafe { region.get(jdx).panic_unchecked("index oob") }.height();
					}
					// SAFETY: asserted beforehand
					unsafe { region.get(idx).panic_unchecked("index oob") }
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

#[inline]
#[must_use]
pub const fn is_utf8_char_boundary(x: u8) -> bool {
	(x as i8) >= -0x40
}

#[inline]
#[must_use]
pub const fn is_jump_char_boundary(x: u8) -> bool {
	x.is_ascii_whitespace() || x.is_ascii_punctuation()
}

impl Default for NbtWorkbench {
	fn default() -> Self {
		let mut workbench = Self {
			tabs: vec![],
			tab: 0,
			mouse_x: 0,
			mouse_y: 0,
			window_height: WINDOW_HEIGHT,
			window_width: WINDOW_WIDTH,
			held_mouse_keys: FxHashSet::with_hasher(FxBuildHasher::default()),
			held_keys: FxHashSet::with_hasher(FxBuildHasher::default()),
			held_entry: HeldEntry::Empty,
			selected_text: None,
			cache_cursor_x: None,
			tab_scroll: 0,
			scrollbar_offset: None,
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
				window_width: WINDOW_WIDTH,
				depth: 32,
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
				if path.extension().and_then(OsStr::to_str) == Some("mca") {
					(NbtElement::from_mca(f.as_slice())?, FileFormat::Mca)
				} else if let Some(0x1F8B) = f.first_chunk::<2>().copied().map(u16::from_be_bytes) {
					(NbtElement::from_file(&DeflateDecoder::new(f.as_slice()).decode_gzip().ok()?)?, FileFormat::Gzip)
				} else if let Some(0x7801 | 0x789C | 0x78DA) = f.first_chunk::<2>().copied().map(u16::from_be_bytes) {
					(NbtElement::from_file(&DeflateDecoder::new(f.as_slice()).decode_zlib().ok()?)?, FileFormat::Zlib)
				} else if let Some(nbt) = NbtElement::from_file(f.as_slice()) {
					(nbt, FileFormat::Nbt)
				} else {
					match core::str::from_utf8(&f).ok().and_then(NbtElement::from_str)? {
						Ok((_, nbt)) | Err(nbt) => (nbt, FileFormat::Snbt),
					}
				}
			};
			match Tab::new(nbt, path, compressed, self.window_height, self.window_width) {
				None => None,
				Some(entry) => {
					if !self.close_selected_text(false) {
						self.selected_text = None
					};
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
				let shift = self.held_keys.contains(&VirtualKeyCode::LShift) | self.held_keys.contains(&VirtualKeyCode::RShift);
				if self.mouse_y < 21 {
					let scroll = if shift { -v } else { -h };
					self.tab_scroll = ((self.tab_scroll as isize + (scroll * 48.0) as isize).max(0) as usize).min(
						{
							let mut tabs_width = 3_usize;
							for tab in &self.tabs {
								tabs_width += tab.name.width() + 32 + 6 + 6;
							}
							tabs_width
						}
						.saturating_sub(self.window_width),
					);
				} else {
					let Some(tab) = self.tabs.get_mut(self.tab) else {
						return false;
					};
					if shift {
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
		let x = self.mouse_x;
		let y = self.mouse_y;
		if state == ElementState::Released {
			self.scrollbar_offset = None;
			self.held_mouse_keys.remove(&button);
			if y < 19 && x > 2 && y > 3 {
				self.click_tab(button);
			} else if y >= HEADER_SIZE {
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
						if self.toggle(button == MouseButton::Right) {
							break 'a;
						}
					}
					if button == MouseButton::Left {
						if self.try_select_text() {
							break 'a;
						}
					}
				}
			}
		} else {
			if !self.close_selected_text(false) {
				self.selected_text = None;
			}
			self.held_mouse_keys.insert(button);
			'a: {
				if self.held_entry.is_empty() && (24..46).contains(&y) {
					self.hold_entry(button);
					break 'a;
				}
				if self.held_entry.is_empty() && y >= HEADER_SIZE + 16 && x >= self.left_margin() + 16 {
					if self.steal() {
						break 'a;
					}
				}
				if let Some(tab) = self.tab() && ((self.window_width - 7)..self.window_width).contains(&x) {
					let height = tab.value.height() * 16 + 48;
					let total = self.window_height - HEADER_SIZE;
					if height > total {
						let start = total * self.scroll() / height + HEADER_SIZE;
						let end = start + total * total / height;
						if (start..=end).contains(&y) {
							self.scrollbar_offset = Some(y - start);
						}
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
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		if y < tab.value.height() && y > 0 {
			let (depth, height, true_height) = {
				let (depth, (_, _, element)) = Traverse::new(y, &mut tab.value).enumerate().last();
				(depth, element.height(), element.true_height())
			};
			if depth != target_depth {
				return false;
			}

			let mut indices = vec![];
			let mut iter = TraverseParents::new(y, &mut tab.value);
			let value =
				'w: {
					while let Some((position, idx, _, element)) = iter.next() {
						indices.push(idx);
						element.decrement(height, true_height);
						match position {
							Position::Only | Position::Last => break 'w (unsafe { element.remove(idx).panic_unchecked("we asserted above that this indeed does capture the final element") }),
							Position::First | Position::Middle => {}
						}
					}
					unsafe {
						panic_unchecked("so you're telling me, we had a node, that was **not** root, as confirmed by the let chain for `NbtWorkbench::tab_mut()` yet somehow no parent's even existed, seriously wtf")
					}
				};

			self.held_entry = HeldEntry::FromKnown(value, indices.into_boxed_slice());
			tab.recache_depth();
			true
		} else {
			false
		}
	}

	#[inline]
	fn rename(&mut self, offset: usize) -> bool {
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		let name = tab.path.as_ref().and_then(|x| x.to_str()).map_or_else(|| tab.name.clone(), Into::into);
		self.selected_text = SelectedText::new(36 + self.left_margin(), offset, HEADER_SIZE, Some((name, true)), None, false, vec![]);
		self.selected_text.is_some()
	}

	#[inline]
	fn duplicate(&mut self) -> bool {
		if self.mouse_y < HEADER_SIZE {
			return false;
		}

		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		if y < tab.value.height() && y > 0 {
			let (height, true_height) = unsafe {
				Traverse::new(y, &mut tab.value)
					.last()
					.map(|(_, _, element)| (element.height(), element.true_height()))
					.panic_unchecked("we've asserted that y > 0")
			};

			let mut indices = vec![];
			let mut iter = TraverseParents::new(y, &mut tab.value);
			while let Some((position, idx, key, element)) = iter.next() {
				match position {
					Position::First | Position::Middle => {
						element.increment(height, true_height);
						indices.push(idx);
					}
					Position::Only | Position::Last => {
						indices.push(idx + 1);
						let duplicate = unsafe { element.get(idx).panic_unchecked("it exists mate, let's stop playing around") }.clone();
						match element {
							NbtElement::Compound(compound) => {
								compound.insert_full(idx + 1, unsafe { key.panic_unchecked("it's a compound, it **has** a key for every value") }.into_string(), duplicate)
							}
							// this is the same thing, literally the same thing, i refuse to believe that it can't be added
							element => {
								let _ = element.insert(idx + 1, duplicate);
							}
						}
					}
				}
			}
			tab.undos.push(WorkbenchAction::Add { indices: indices.into_boxed_slice() });
			tab.history_changed = true;
			// not required since the max depth won't change on a duplication
			// tab.recache_depth();
			true
		} else {
			false
		}
	}

	#[inline]
	fn copy(&mut self, debug: bool) -> bool {
		if self.mouse_y < HEADER_SIZE {
			return false;
		}
		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		if y < tab.value.height() {
			let (_, key, element) = unsafe { Traverse::new(y, &mut tab.value).last().panic_unchecked("There is always at least one element - Master Oogway") };
			if debug {
				cli_clipboard::set_contents(format!("{}{}{element:#?}", key.as_ref().map(Box::as_ref).unwrap_or(""), if key.is_some() { ": " } else { "" })).is_ok()
			} else {
				cli_clipboard::set_contents(format!("{}{}{element}", key.as_ref().map(Box::as_ref).unwrap_or(""), if key.is_some() { ":" } else { "" })).is_ok()
			}
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
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		if y < tab.value.height() && y > 0 {
			let mut indices = vec![];
			let (height, true_height) = if let Some((_, key, element)) = Traverse::new(y, &mut tab.value).last() {
				if clipboard {
					let _ = cli_clipboard::set_contents(format!("{}{}{element}", key.as_ref().map(Box::as_ref).unwrap_or(""), if key.is_some() { ":" } else { "" }));
				}
				(element.height(), element.true_height())
			} else {
				unsafe { panic_unchecked("dis ain't even possible i never f**k'd with the traverse") }
			};
			let mut iter = TraverseParents::new(y, &mut tab.value);
			let element = 'w: {
				while let Some((position, idx, _, element)) = iter.next() {
					indices.push(idx);
					element.decrement(height, true_height);
					match position {
						Position::First | Position::Middle => {}
						Position::Last | Position::Only => break 'w (unsafe { element.remove(idx).panic_unchecked("da hell mate") }),
					}
				}
				unsafe { panic_unchecked("parents were dodged") }
			};
			tab.undos.push(WorkbenchAction::Remove {
				element,
				indices: indices.into_boxed_slice(),
			});
			tab.recache_depth();
			true
		} else {
			false
		}
	}

	#[inline]
	fn drop(&mut self, element: Result<(Box<str>, NbtElement), NbtElement>, from_indices: Option<Box<[usize]>>) -> bool {
		let left_margin = self.left_margin();
		let horizontal_scroll = self.horizontal_scroll();
		if self.mouse_y <= HEADER_SIZE {
			return false;
		}
		if self.mouse_x + horizontal_scroll < left_margin {
			return false;
		}
		let y = self.mouse_y - HEADER_SIZE + self.scroll();
		let x = (self.mouse_x + horizontal_scroll - left_margin) / 16;
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};

		let mut indices = vec![];
		let (key, element) = element.map_or_else(|element| (None, element), |(x, y)| (Some(x), y));

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
					tab.recache_depth();
					tab.history_changed = true;
				}

				self.selected_text = None;
			}
			DropFn::Dropped(_, _, _) => {
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
				tab.recache_depth();
				tab.history_changed = true;
			}
		}
		true
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
		let mouse_x = self.mouse_x + self.tab_scroll;
		// todo, probably bad somewhere, check with rendering to see if im stupid
		if mouse_x < 2 {
			return;
		}

		let mut x = mouse_x - 2;
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
		self.tab().map_or(8, Tab::left_margin)
	}

	#[inline]
	fn toggle(&mut self, ignore_depth: bool) -> bool {
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
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		if y >= tab.value.height() {
			return false;
		}

		let (depth, (_, _, element)) = Traverse::new(y, &mut tab.value).enumerate().last();

		if depth != x && !ignore_depth {
			return false;
		}
		let before = element.height();
		let _ = element.toggle();
		let increment = element.height().wrapping_sub(before);
		if increment == 0 {
			return true;
		}

		let mut iter = TraverseParents::new(y, &mut tab.value);
		while let Some((_, _, _, element)) = iter.next() {
			element.increment(increment, 0);
		}

		tab.scroll = tab.scroll();
		tab.recache_depth();
		true
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
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		if y >= tab.value.height() {
			return false;
		}

		let mut indices = vec![];
		let mut iter = Traverse::new(y, &mut tab.value);
		while let Some((position, idx, key, value)) = iter.next() {
			match position {
				Position::First | Position::Only => {} // do nothing
				Position::Middle => {
					indices.push(idx);
				}
				Position::Last => {
					indices.push(idx);
					self.selected_text = SelectedText::new(
						indices.len() * 16 + 32 + 4 + left_margin,
						self.mouse_x + horizontal_scroll,
						y * 16 + HEADER_SIZE,
						key.map(|x| (x, true)),
						Some(value.value()),
						matches!(value, NbtElement::Chunk(_)),
						indices,
					);
					return self.selected_text.is_some();
				}
			}
		}
		false
	}

	#[inline]
	pub fn keyfix(&mut self) {
		if let Some(SelectedText { y, indices, cursor, value, selection, keyfix, prefix, suffix, valuefix, editable: true, last_interaction: _, undos: _, redos: _ }) = self.selected_text.clone() && let Some(keyfix) = keyfix && valuefix.is_none() && suffix.is_empty() && cursor == 0 {
			if !self.close_selected_text(true) { return; }
			self.selected_text = Some(SelectedText {
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
			if !self.close_selected_text(true) { return; }
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
			if child_idx == 0 || parent.len().is_none_or(|x| x == 0) {
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
			// well it could be empty
			if indices.is_empty() {
				return;
			}
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
	pub unsafe fn selected_text_up(&mut self, ctrl: bool) {
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
			let Some(&last_index) = indices.last() else {
				return;
			};
			if !self.close_selected_text(false) {
				self.selected_text = None;
			}
			let cache_cursor_x = self.cache_cursor_x;
			let Some(tab) = self.tabs.get_mut(self.tab) else {
				return;
			};
			let original_indices_len = indices.len();
			let mouse_x = cache_cursor_x.unwrap_or(original_indices_len * 16 + 32 + 4 + left_margin + keyfix.as_ref().map_or(0, StrExt::width) + prefix.width() + str_value.split_at(cursor).0.width());

			if y == HEADER_SIZE + 16 {
				let width = tab.path.as_ref().and_then(|x| x.to_str()).map_or_else(|| tab.name.width(), StrExt::width);
				self.rename(mouse_x.min(width + 32 + 4 + self.left_margin()));
				return;
			}

			let (k, v, indices, new_y) = if ctrl && last_index > 0 {
				let mut indices = indices.into_vec();
				let tail = Navigate::new(indices.iter().copied().take(original_indices_len - 1), &mut tab.value).last().2;
				*indices.last_mut().panic_unchecked("indices cannot be empty, we're in it") = 0;
				let (k, v) = tail
					.children()
					.panic_unchecked("we are a child, some must exist")
					.map_or_else(|mut iter| {
						let (k, v) = iter.next().panic_unchecked("we are a child, some must exist");
						(Some((k.clone(), true)), Some(v.value()))
					}, |mut iter| match iter {
						iter @ ValueIterator::Generic(_) => {
							(None, Some(iter.enumerate().next().panic_unchecked("we're the child, it can't be empty").1.value()))
						},
						iter @ ValueIterator::Region(_, _) => {
							let chunk = unsafe { iter.enumerate().next().panic_unchecked("we're the child, it can't be empty").1.as_chunk_unchecked() };
							(Some((chunk.x.to_string().into_boxed_str(), true)), Some((chunk.z.to_string().into_boxed_str(), true)))
						}
					});
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
							break 'w (key.map(|k| (k, true)), Some(value.value()));
						}
					}
					panic_unchecked("Iterator was empty, somehow")
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
			self.selected_text = SelectedText::new(low, mouse_x.clamp(low, high), new_y + HEADER_SIZE, k, v, indices.len() == 1 && matches!(tab.value, box NbtElement::Region(_)), indices);
		}
	}

	#[inline]
	pub unsafe fn selected_text_down(&mut self, ctrl: bool) {
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
			// down needs a check that it doesn't surpass the end
				if total >= tab.value.height() {
					return;
				}
				total
			} else {
				return;
			};
			if !self.close_selected_text(false) {
				self.selected_text = None;
			}
			let cache_cursor_x = self.cache_cursor_x;
			let Some(tab) = self.tabs.get_mut(self.tab) else {
				return;
			};
			let original_indices_len = indices.len();
			let mouse_x = cache_cursor_x.unwrap_or(original_indices_len * 16 + 32 + 4 + left_margin + keyfix.as_ref().map_or(0, StrExt::width) + prefix.width() + str_value.split_at(cursor).0.width());
			let (k, v, end_idx) = if ctrl && indices.len() > 0 {
				Navigate::new(indices.iter().copied().take(indices.len() - 1), &mut tab.value)
					.last()
					.2
					.children()
					.panic_unchecked("we're the child")
					.map_or_else(
						|iter| {
							let tuple = iter.enumerate().last().panic_unchecked("we're the child, it can't be empty");
							(Some((tuple.1 .0.clone(), true)), Some(tuple.1 .1.value()), tuple.0)
						},
						|iter| match iter {
							iter @ ValueIterator::Generic(_) => {
								let tuple = iter.enumerate().last().panic_unchecked("we're the child, it can't be empty");
								(None, Some(tuple.1.value()), tuple.0)
							}
							iter @ ValueIterator::Region(_, _) => {
								let (idx, chunk) = iter.enumerate().last().panic_unchecked("we're the child, it can't be empty");
								let chunk = unsafe { chunk.as_chunk_unchecked() };
								(Some((chunk.x.to_string().into_boxed_str(), true)), Some((chunk.z.to_string().into_boxed_str(), true)), idx)
							}
						},
					)
			} else {
				(None, None, 0)
			};
			let (k, v, indices, new_y) = if ctrl && indices.len() > 0 && !indices.last().is_some_and(|x| *x == end_idx) {
				let mut indices = indices.into_vec();
				*indices.last_mut().panic_unchecked("it literally just can't be empty") = end_idx;
				let new_y = sum_indices(indices.iter().copied(), &tab.value) * 16;
				(k, v, indices, new_y)
			} else {
				'w: {
					let mut indices = vec![];
					let mut iter = TraverseParents::new(total, &mut tab.value);
					while let Some((position, idx, key, value)) = iter.next() {
						indices.push(idx);
						if let Position::Last | Position::Only = position {
							break 'w (
								key.map(|k| (k, true)),
								Some(value.get(idx).map(NbtElement::value).panic_unchecked("We are literally given an `idx`")),
								indices,
								y + 16 - HEADER_SIZE,
							);
						}
					}
					panic_unchecked("Iterator was empty, somehow")
				}
			};
			let low = indices.len() * 16 + 32 + 4 + left_margin;
			let high = low
				+ k.as_ref().map_or(0, |(x, display)| x.width() * (*display as usize))
				+ (k.as_ref().is_some_and(|(_, display)| *display) && v.as_ref().is_some_and(|(_, display)| *display)) as usize * ": ".width()
				+ v.as_ref().map_or(0, |(x, display)| (*display as usize) * x.width());
			if new_y + 48 > tab.scroll + tab.window_height - HEADER_SIZE {
				tab.scroll = new_y + 48 - (tab.window_height - HEADER_SIZE);
				tab.scroll = tab.scroll();
			}
			self.cache_cursor_x = self.cache_cursor_x.or(Some(mouse_x));
			self.selected_text = SelectedText::new(low, mouse_x.clamp(low, high), new_y + HEADER_SIZE, k, v, indices.len() == 1 && matches!(tab.value, box NbtElement::Region(_)), indices);
		}
	}

	#[inline]
	pub fn force_close(&mut self) {
		if let Some(SelectedText { indices, .. }) = self.selected_text.as_ref() {
			let indices = indices.clone();
			let Some(tab) = self.tabs.get_mut(self.tab) else {
				return;
			};
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
			tab.recache_depth();
		}
	}

	#[inline]
	pub fn force_open(&mut self) {
		if let Some(SelectedText { indices, .. }) = self.selected_text.as_ref() {
			let indices = indices.clone();
			let Some(tab) = self.tabs.get_mut(self.tab) else {
				return;
			};
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
			tab.recache_depth();
		}
	}

	#[inline]
	#[must_use]
	pub fn close_selected_text(&mut self, ignore_already_present_key: bool) -> bool {
		if let Some(SelectedText {
			indices,
			value,
			prefix,
			suffix,
			keyfix,
			valuefix,
			editable: true,
			..
		}) = self.selected_text.clone()
		{
			let Some(tab) = self.tabs.get_mut(self.tab) else {
				return false;
			};
			if indices.is_empty() {
				let buf = PathBuf::from(value);
				if let Some(name) = buf.file_name().and_then(OsStr::to_str) {
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
						if let NbtElement::Compound(compound) | NbtElement::Chunk(NbtChunk { inner: compound, .. }) = element {
							if compound.entries.contains_key(value.as_ref()) {
								return ignore_already_present_key;
							}
							compound.update_key(last, value.clone()).unwrap_or(value)
						} else if let NbtElement::Region(region) = element {
							let (x, z) = match (value.parse::<u8>(), unsafe { valuefix.panic_unchecked("A chunk has a key and value") }.parse::<u8>()) {
								(Ok(x @ 0..=31), Ok(z @ 0..=31)) => (x, z),
								_ => return ignore_already_present_key,
							};
							let (old_x, old_z) = {
								let chunk = unsafe { region.get_mut(last).panic_unchecked("Index was invalid").as_chunk_unchecked_mut() };
								(core::mem::replace(&mut chunk.x, x), core::mem::replace(&mut chunk.z, z))
							};
							let new_idx = ((x as usize) << 5) | (z as usize);
							if region.chunks[new_idx].is_some() {
								unsafe {
									let chunk = region.get_mut(last).panic_unchecked("Index was invalid").as_chunk_unchecked_mut();
									chunk.x = old_x;
									chunk.z = old_z;
									return ignore_already_present_key;
								}
							}
							let from = core::mem::replace(&mut region.map[last], new_idx as u16) as usize;
							region.chunks.swap(from, new_idx);
							old_x.to_string().into_boxed_str()
						} else {
							unsafe { panic_unchecked("Expected key-value indices chain tail to be of type compound") }
						}
					} else {
						if let NbtElement::Region(region) = element {
							let (x, z) = match (unsafe { keyfix.panic_unchecked("A chunk always has a key and value") }.parse::<u8>(), value.parse::<u8>()) {
								(Ok(x @ 0..=31), Ok(z @ 0..=31)) => (x, z),
								_ => return ignore_already_present_key,
							};
							let (old_x, old_z) = {
								let chunk = unsafe { region.get_mut(last).panic_unchecked("Last index was valid").as_chunk_unchecked_mut() };
								(core::mem::replace(&mut chunk.x, x), core::mem::replace(&mut chunk.z, z))
							};
							let new_idx = ((x as usize) << 5) | (z as usize);
							if region.chunks[new_idx].is_some() {
								unsafe {
									let chunk = region.get_mut(last).panic_unchecked("Last index was valid").as_chunk_unchecked_mut();
									chunk.x = old_x;
									chunk.z = old_z;
									return ignore_already_present_key;
								}
							};
							let from = core::mem::replace(&mut region.map[last], new_idx as u16) as usize;
							region.chunks.swap(from, new_idx);
							old_z.to_string().into_boxed_str()
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
					}
				};

				tab.undos.push(WorkbenchAction::Rename { indices, key, previous });
				tab.history_changed = true;
				self.selected_text = None;
			}
		}
		true
	}

	#[cfg_attr(windows_subsystem = "console", inline)]
	#[allow(clippy::collapsible_if, clippy::too_many_lines)]
	pub fn on_key_input(&mut self, key: KeyboardInput) -> bool {
		if key.state == ElementState::Pressed {
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
							let _ = self.close_selected_text(false);
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
							unsafe {
								self.selected_text_up(ctrl);
							}
							return true;
						}
						KeyResult::Down(ctrl) => {
							unsafe {
								self.selected_text_down(ctrl);
							}
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
						window_height: self.window_height,
						window_width: self.window_width,
						depth: 32,
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
							tab.recache_depth();
							self.selected_text = None;
							return true;
						}
					}
				}
				if key == VirtualKeyCode::Y && flags == flags!(Ctrl) {
					if let Some(tab) = self.tab_mut() {
						if let Some(action) = tab.redos.pop() {
							tab.undos.push(action.undo(&mut tab.value));
							tab.recache_depth();
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
					if self.copy(false) {
						self.selected_text = None;
						return true;
					}
				}
				if key == VirtualKeyCode::C && flags == flags!(Ctrl + Shift) {
					if self.copy(true) {
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
		} else if key.state == ElementState::Released {
			if let Some(x) = key.virtual_keycode {
				self.held_keys.remove(&x);
			}
		}

		false
	}

	#[inline]
	pub fn on_mouse_move(&mut self, pos: PhysicalPosition<f64>) -> bool {
		self.mouse_x = pos.x as usize;
		self.mouse_y = pos.y as usize;
		let mouse_y = self.mouse_y;
		if let Some(scrollbar_offset) = self.scrollbar_offset && let Some(tab) = self.tab_mut() && mouse_y >= HEADER_SIZE {
			let mouse_y = mouse_y - HEADER_SIZE;
			let height = tab.value.height() * 16 + 48;
			let total = tab.window_height - HEADER_SIZE;
			let start = total * tab.scroll() / height;
			let scrollbar_point = start + scrollbar_offset;
			let dy = mouse_y.wrapping_sub(scrollbar_point);
			let pixel_delta = height.wrapping_mul(dy) as isize / total as isize;
			tab.scroll = (tab.scroll as isize + pixel_delta).max(0) as usize;
			tab.scroll = tab.scroll();
		}
		true
	}

	#[inline]
	pub fn window_height(&mut self, window_height: usize) {
		self.window_height = window_height;
		for tab in &mut self.tabs {
			tab.window_height = window_height;
		}
	}

	#[inline]
	pub fn window_width(&mut self, window_width: usize) {
		self.window_width = window_width;
		for tab in &mut self.tabs {
			tab.window_width = window_width;
		}
	}

	#[inline]
	#[must_use]
	pub fn scroll(&self) -> usize {
		self.tab().map_or(0, Tab::scroll)
	}

	#[inline]
	#[must_use]
	pub fn horizontal_scroll(&self) -> usize {
		self.tab().map_or(0, Tab::horizontal_scroll)
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
		let Some(tab) = self.tabs.get(self.tab) else {
			return;
		};
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
				if text.keyfix.is_none() && (text.valuefix.is_some() || !text.suffix.is_empty()) {
					Some((text.value.clone().into_boxed_str(), text.valuefix.clone().unwrap_or_else(|| text.suffix.clone()).into_boxed_str()))
				} else if let Some(keyfix) = text.keyfix.as_ref() && text.valuefix.is_none() {
					Some((keyfix.clone().into_boxed_str(), text.value.clone().into_boxed_str()))
				} else {
					None
				}
			}),
		);
		let mut ctx = RenderContext::new(
			forbidden,
			highlight,
			ghost,
			left_margin,
			(tab.value.height() - 1) * 16 + HEADER_SIZE - tab.scroll,
			(self.mouse_x, self.mouse_y),
		);
		tab.render(builder, &mut ctx, self.scrollbar_offset.is_some());
		if let Some(selected_text) = &self.selected_text {
			builder.horizontal_scroll = horizontal_scroll;
			selected_text.render(builder, left_margin);
			builder.horizontal_scroll = 0;
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
		let mx = if (24..46).contains(&self.mouse_y) { self.mouse_x & !0b1111 } else { usize::MAX };
		builder.draw_texture((0, 26), if mx == 0 { BYTE_UV } else { BYTE_GHOST_UV }, (16, 16));
		builder.draw_texture((16, 26), if mx == 16 { SHORT_UV } else { SHORT_GHOST_UV }, (16, 16));
		builder.draw_texture((32, 26), if mx == 32 { INT_UV } else { INT_GHOST_UV }, (16, 16));
		builder.draw_texture((48, 26), if mx == 48 { LONG_UV } else { LONG_GHOST_UV }, (16, 16));
		builder.draw_texture((64, 26), if mx == 64 { FLOAT_UV } else { FLOAT_GHOST_UV }, (16, 16));
		builder.draw_texture((80, 26), if mx == 80 { DOUBLE_UV } else { DOUBLE_GHOST_UV }, (16, 16));
		builder.draw_texture((96, 26), if mx == 96 { BYTE_ARRAY_UV } else { BYTE_ARRAY_GHOST_UV }, (16, 16));
		builder.draw_texture((112, 26), if mx == 112 { INT_ARRAY_UV } else { INT_ARRAY_GHOST_UV }, (16, 16));
		builder.draw_texture((128, 26), if mx == 128 { LONG_ARRAY_UV } else { LONG_ARRAY_GHOST_UV }, (16, 16));
		builder.draw_texture((144, 26), if mx == 144 { STRING_UV } else { STRING_GHOST_UV }, (16, 16));
		builder.draw_texture((160, 26), if mx == 160 { LIST_UV } else { LIST_GHOST_UV }, (16, 16));
		builder.draw_texture((176, 26), if mx == 176 { COMPOUND_UV } else { COMPOUND_GHOST_UV }, (16, 16));
		builder.draw_texture((192, 26), if mx == 192 { UNKNOWN_NBT_UV } else { UNKNOWN_NBT_GHOST_UV }, (16, 16));
	}

	#[inline]
	fn render_tabs(&self, builder: &mut VertexBufferBuilder) {
		let mut offset = 3;
		builder.horizontal_scroll = self.tab_scroll;
		for (idx, tab) in self.tabs.iter().enumerate() {
			let uv = if idx == self.tab { SELECTED_WIDGET_UV } else { UNSELECTED_WIDGET_UV };
			builder.draw_texture((offset, 3), uv, (3, 16));
			offset += 3;
			builder.settings(offset, 3, false);
			let _ = write!(builder, "{}", tab.name);
			let remaining_width = tab.name.width() + 32 + 3;
			builder.draw_texture_region_z((offset, 3), 0.0, uv + Vec2u::new(3, 0), (remaining_width, 16), (10, 16));
			offset += remaining_width;
			builder.draw_texture((offset, 3), uv + Vec2u::new(13, 0), (3, 16));
			builder.draw_texture((offset - 32, 3), if tab.history_changed { EDITED_UV } else { UNEDITED_UV }, (16, 16));
			builder.draw_texture((offset - 16, 3), tab.compression.uv(), (16, 16));
			offset += 6;
		}
		builder.horizontal_scroll = 0;
		builder.draw_texture_region_z((0, 21), 0.0, HORIZONTAL_SEPARATOR_UV, (builder.window_width(), 2), (16, 2));
		builder.draw_texture_region_z((0, 45), 0.0, HORIZONTAL_SEPARATOR_UV, (builder.window_width(), 2), (16, 2));
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
	forbidden_key: Option<(Box<str>, Box<str>)>,
	extend_forbid_for_value: bool,
	key_invalid: bool,
	highlight: (usize, usize),
	ghost: Option<(u8, usize, usize)>,
	left_margin: usize,
	end_y: usize,
	mouse_x: usize,
	mouse_y: usize,
	pub line_number: usize,
	red_line_numbers: [usize; 2],
}

impl RenderContext {
	#[must_use]
	pub fn new(forbidden: (usize, Option<(Box<str>, Box<str>)>), highlight: (usize, usize), ghost: Option<(u8, usize, usize)>, left_margin: usize, end_y: usize, mouse: (usize, usize)) -> Self {
		Self {
			forbidden_y: forbidden.0,
			forbidden_key: forbidden.1,
			extend_forbid_for_value: false,
			key_invalid: false,
			highlight,
			ghost,
			left_margin,
			end_y,
			mouse_x: mouse.0,
			mouse_y: mouse.1,
			line_number: 1,
			red_line_numbers: [0, 0],
		}
	}

	pub fn highlight(&self, pos: (usize, usize), text_width: usize, builder: &mut VertexBufferBuilder) {
		if pos.1 == self.highlight.1 && self.highlight.0 <= pos.0 + if text_width > 0 { text_width + 4 } else { text_width } + 16 && self.highlight.0 >= pos.0 {
			builder.draw_texture(pos, SELECTION_UV, (16, 16));
		}
	}

	pub fn check_key<F: FnOnce(&str, &str) -> bool>(&mut self, f: F, extend: bool) {
		if let Some((forbidden_key, forbidden_value)) = self.forbidden_key.as_ref() {
			self.key_invalid = f(forbidden_key, forbidden_value);
			self.extend_forbid_for_value = extend;
		}
	}

	pub fn draw_toggle(&self, pos: (usize, usize), open: bool, builder: &mut VertexBufferBuilder) {
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
		builder.draw_texture(Vec2u::from(pos) + Vec2u::new(3, 5), uv, (8, 8));
	}

	#[must_use]
	pub fn forbid(&self, x: usize, y: usize, builder: &mut VertexBufferBuilder) -> bool {
		if y == self.forbidden_y {
			if self.key_invalid {
				self.draw_forbid_underline(x, y, builder);
			}
			false
		} else {
			true
		}
	}

	pub fn draw_forbid_underline_width(&self, x: usize, y: usize, overridden_width: usize, builder: &mut VertexBufferBuilder) {
		if self.forbidden_key.is_some() {
			// buck hates this and sees no ideal solution
			builder.draw_texture_region_z((x + 20, y + 14), 0.0, TEXT_UNDERLINE_UV, (overridden_width, 2), (16, 2));
		}
	}

	pub fn draw_forbid_underline(&self, x: usize, y: usize, builder: &mut VertexBufferBuilder) {
		if let Some((forbidden_key, forbidden_value)) = self.forbidden_key.as_ref() {
			// buck hates this and sees no ideal solution
			builder.draw_texture_region_z((x + 20, y + 14), 0.0, TEXT_UNDERLINE_UV, (forbidden_key.width() + (": ".width() + forbidden_value.width()) * self.extend_forbid_for_value as usize, 2), (16, 2));
		}
	}

	pub fn line_number(&mut self, y: usize, builder: &mut VertexBufferBuilder) {
		let line_number = self.line_number;
		let color = if (self.red_line_numbers[0] == y) | (self.red_line_numbers[1] == y) { 0xFF5555 } else { 0xAAAAAA };
		let color = core::mem::replace(&mut builder.color, color);
		builder.settings(self.left_margin - line_number.ilog10() as usize * 8 - 16, y, false);
		let _ = write!(builder, "{line_number}");
		builder.color = color;
		let uv = if y >= self.end_y { END_LINE_NUMBER_SEPARATOR_UV } else { LINE_NUMBER_SEPARATOR_UV };
		builder.draw_texture_z((builder.text_coords.0 + 4, y), 0.5, uv, (2, 16));
		self.line_number += 1;
	}

	pub fn ghost<F: FnOnce(usize, usize) -> bool>(&self, x_offset: usize, y_offset: usize, builder: &mut VertexBufferBuilder, f: F) -> bool {
		if let Some((id, x, y)) = self.ghost && f(x, y) {
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
		self.map_or_else(|| panic_unchecked(msg), identity)
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
	pub mod chunk;
	pub mod compound;
	pub mod element_type;
	pub mod list;
	pub mod primitive;
	pub mod string;
}
