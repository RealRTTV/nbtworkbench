use anyhow::{anyhow, Context, Result};

use std::convert::identity;
use std::ffi::OsStr;
use std::fmt::Write;
use std::fs::read;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::string::String;
use std::sync::mpsc::TryRecvError;
use std::time::SystemTime;

use compact_str::{format_compact, CompactString, ToCompactString};
use fxhash::{FxBuildHasher, FxHashSet};
use uuid::Uuid;
use winit::dpi::PhysicalPosition;
use winit::event::{ElementState, KeyboardInput, MouseButton, MouseScrollDelta, VirtualKeyCode};
use zune_inflate::DeflateDecoder;

use crate::alert::Alert;
use crate::assets::{ACTION_WHEEL_Z, BASE_TEXT_Z, BASE_Z, BOOKMARK_UV, DARK_STRIPE_UV, EDITED_UV, HEADER_SIZE, HELD_ENTRY_Z, HIDDEN_BOOKMARK_UV, HORIZONTAL_SEPARATOR_UV, HOVERED_STRIPE_UV, HOVERED_WIDGET_UV, JUST_OVERLAPPING_BASE_TEXT_Z, LIGHT_STRIPE_UV, SELECTED_ACTION_WHEEL, SELECTED_WIDGET_UV, TRAY_UV, UNEDITED_UV, UNSELECTED_ACTION_WHEEL, UNSELECTED_WIDGET_UV};
use crate::color::TextColor;
use crate::elements::chunk::{NbtChunk, NbtRegion};
use crate::elements::compound::NbtCompound;
use crate::elements::element::NbtElement;
use crate::elements::element::{NbtByte, NbtByteArray, NbtDouble, NbtFloat, NbtInt, NbtIntArray, NbtLong, NbtLongArray, NbtShort};
use crate::elements::list::{NbtList, ValueIterator};
use crate::elements::string::NbtString;
use crate::selected_text::{KeyResult, SelectedText};
use crate::tab::{FileFormat, Tab};
use crate::tree_travel::{Navigate, Traverse, TraverseParents};
use crate::vertex_buffer_builder::Vec2u;
use crate::vertex_buffer_builder::VertexBufferBuilder;
use crate::window::{MIN_WINDOW_HEIGHT, MIN_WINDOW_WIDTH, WINDOW_HEIGHT, WINDOW_WIDTH};
use crate::workbench_action::WorkbenchAction;
use crate::{encompasses, encompasses_or_equal, flags, panic_unchecked, recache_along_indices, sum_indices, Bookmark, DropFn, FileUpdateSubscription, FileUpdateSubscriptionType, HeldEntry, LinkedQueue, OptionExt, Position, RenderContext, StrExt};

pub struct Workbench {
	pub tabs: Vec<Tab>,
	pub tab: usize,
	raw_mouse_x: usize,
	mouse_x: usize,
	raw_mouse_y: usize,
	mouse_y: usize,
	pub window_height: usize,
	raw_window_height: usize,
	pub window_width: usize,
	raw_window_width: usize,
	held_mouse_keys: FxHashSet<MouseButton>,
	held_keys: FxHashSet<VirtualKeyCode>,
	pub held_entry: HeldEntry,
	// selected_text: Option<SelectedText>,
	cache_cursor_x: Option<usize>,
	tab_scroll: usize,
	scrollbar_offset: Option<usize>,
	action_wheel: Option<(usize, usize)>,
	subscription: Option<FileUpdateSubscription>,
	pub cursor_visible: bool,
	alerts: Vec<Alert>,
	pub scale: usize,
}

impl Workbench {
	#[inline]
	#[must_use]
	pub fn new(f: impl Fn(&str) + Copy) -> Self {
		let mut workbench = Self {
			tabs: vec![],
			tab: 0,
			raw_mouse_x: 0,
			mouse_x: 0,
			raw_mouse_y: 0,
			mouse_y: 0,
			window_height: WINDOW_HEIGHT,
			raw_window_height: WINDOW_HEIGHT,
			window_width: WINDOW_WIDTH,
			raw_window_width: WINDOW_WIDTH,
			held_mouse_keys: FxHashSet::with_hasher(FxBuildHasher::default()),
			held_keys: FxHashSet::with_hasher(FxBuildHasher::default()),
			held_entry: HeldEntry::Empty,
			cache_cursor_x: None,
			tab_scroll: 0,
			scrollbar_offset: None,
			action_wheel: None,
			subscription: None,
			cursor_visible: true,
			alerts: vec![],
			scale: 1,
		};
		if let Some(x) = &std::env::args()
			.nth(1)
			.and_then(|x| PathBuf::from_str(&x).ok())
		{
			if let Err(e) = workbench.on_open_file(x, f) {
				workbench.alert(Alert::new("Error!", TextColor::Red, e.to_string()))
			}
		} else {
			workbench.tabs.push(Tab {
				#[cfg(debug_assertions)]
				value: Box::new(NbtElement::from_file(include_bytes!("assets/test.nbt")).expect("Included debug nbt contains valid data")),
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
				bookmarks: vec![],
				uuid: Uuid::new_v4(),
				freehand_mode: false,
				selected_text: None,
			});
			f(if cfg!(debug_assertions) {
				"test.nbt - NBT Workbench"
			} else {
				"new.nbt - NBT Workbench"
			});
		}
		workbench
	}

	#[inline]
	pub fn alert(&mut self, alert: Alert) { self.alerts.insert(0, alert); }

	#[inline]
	#[allow(clippy::equatable_if_let)]
	pub fn on_open_file(&mut self, path: &Path, f: impl Fn(&str)) -> Result<()> {
		let buf = read(path)?;
		let (nbt, compressed) = {
			if path.extension().and_then(OsStr::to_str) == Some("mca") {
				(
					NbtElement::from_mca(buf.as_slice()).context("Failed to parse MCA file")?,
					FileFormat::Mca,
				)
			} else if let Some(0x1F8B) = buf.first_chunk::<2>().copied().map(u16::from_be_bytes) {
				(
					NbtElement::from_file(
						&DeflateDecoder::new(buf.as_slice())
							.decode_gzip()
							.context("Failed to decode gzip compressed NBT")?,
					)
					.context("Failed to parse NBT")?,
					FileFormat::Gzip,
				)
			} else if let Some(0x7801 | 0x789C | 0x78DA) = buf.first_chunk::<2>().copied().map(u16::from_be_bytes) {
				(
					NbtElement::from_file(
						&DeflateDecoder::new(buf.as_slice())
							.decode_zlib()
							.context("Failed to decode zlib compressed NBT")?,
					)
					.context("Failed to parse NBT")?,
					FileFormat::Zlib,
				)
			} else if let Some(nbt) = NbtElement::from_file(buf.as_slice()) {
				(nbt, FileFormat::Nbt)
			} else {
				(
					core::str::from_utf8(&buf)
						.ok()
						.and_then(NbtElement::from_str)
						.context(anyhow!(
							"Failed to find file type for file {}",
							path.file_name()
								.unwrap_or(&OsStr::new(""))
								.to_string_lossy()
						))?
						.1,
					FileFormat::Snbt,
				)
			}
		};
		let mut tab = Tab::new(nbt, path, compressed, self.window_height, self.window_width)?;
		if !tab.close_selected_text(false) {
			tab.selected_text = None;
		};
		self.tabs.push(tab);
		self.set_tab(self.tabs.len() - 1, f);
		Ok(())
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
					let held = self.held_entry.element();
					if shift {
						tab.set_horizontal_scroll(-v, held);
						tab.set_scroll(-h);
					} else {
						tab.set_horizontal_scroll(-h, held);
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
	pub fn on_mouse_input(&mut self, state: ElementState, button: MouseButton, f: impl Fn(&str)) -> bool {
		let horizontal_scroll = self.horizontal_scroll();
		let shift = self.held_keys.contains(&VirtualKeyCode::LShift) | self.held_keys.contains(&VirtualKeyCode::RShift);
		let x = self.mouse_x;
		let y = self.mouse_y;
		if state == ElementState::Released {
			if self.process_action_wheel() { return true }
			self.scrollbar_offset = None;
			self.held_mouse_keys.remove(&button);
			if y < 19 && x > 2 && y > 3 {
				self.click_tab(button, f);
			} else if y >= HEADER_SIZE {
				let left_margin = self.left_margin();
				'a: {
					if MouseButton::Left == button {
						if self.toggle(shift, self.tab().is_some_and(|tab| tab.freehand_mode)) {
							break 'a;
						}
					}

					match core::mem::replace(&mut self.held_entry, HeldEntry::Empty) {
						HeldEntry::Empty => {}
						HeldEntry::FromAether(x) => {
							self.drop(x, None, left_margin);
							break 'a;
						}
						HeldEntry::FromKnown(x, indices) => {
							self.drop(x, Some(indices), left_margin);
							break 'a;
						}
					}

					if (y - HEADER_SIZE) < 16 && x > 32 + left_margin {
						if self.rename(x + horizontal_scroll) {
							break 'a;
						}
					}

					if button == MouseButton::Middle {
						if self.delete(shift) {
							break 'a;
						}
					}
					if button == MouseButton::Left {
						if self.try_select_text() {
							break 'a;
						}
					}
					if button == MouseButton::Left {
						if self.bookmark_line() {
							break 'a;
						}
					}
				}
			}
		} else {
			match self.action_wheel.take() {
				Some(_) => return true,
				None => {
					if button == MouseButton::Right {
						self.action_wheel = Some((self.mouse_x, self.mouse_y));
					}
				}
			}

			if let Some(tab) = self.tab_mut() {
				if !tab.close_selected_text(false) {
					tab.selected_text = None;
				}
			}
			self.held_mouse_keys.insert(button);
			'a: {
				if self.tab().is_some_and(|tab| !tab.freehand_mode) && self.held_entry.is_empty() && (24..46).contains(&y) && button == MouseButton::Left {
					match self.hold_entry(button) {
						Ok(true) => break 'a,
						Err(e) => self.alert(Alert::new("Error!", TextColor::Red, e.to_string())),
						_ => {}
					}
				}
				if self.tab().is_some_and(|tab| !tab.freehand_mode) && self.held_entry.is_empty() && y >= HEADER_SIZE + 16 && x >= self.left_margin() + 16 && button == MouseButton::Left {
					if self.steal() {
						break 'a;
					}
				}
				if (self.window_width - 16..self.window_width).contains(&x)
					&& (26..42).contains(&y)
					&& let Some(tab) = self.tab_mut()
				{
					tab.freehand_mode = !tab.freehand_mode;
					break 'a;
				}
				if let Some(tab) = self.tab()
					&& ((self.window_width - 7)..self.window_width).contains(&x)
				{
					let height = tab.value.height() * 16 + 48;
					let total = self.window_height - HEADER_SIZE;
					if height > total {
						let start = total * self.scroll() / height + HEADER_SIZE;
						let end = start + total * total / height;
						if (start..=end).contains(&y) {
							self.scrollbar_offset = Some(y - start);
							break 'a;
						}
					}
				}
			}
		}
		true
	}

	#[inline]
	fn process_action_wheel(&mut self) -> bool {
		if let Some((cx, cy)) = self.action_wheel.take() {
			let squared_distance_from_origin = (cy as isize - self.mouse_y as isize).pow(2) + (cx as isize - self.mouse_x as isize).pow(2);
			if squared_distance_from_origin <= 8_isize.pow(2) { return true }
			let left_margin = self.left_margin();
			'a: {
				if cy >= HEADER_SIZE {
					let Some(tab) = self.tabs.get_mut(self.tab) else {
						break 'a;
					};
					let scroll = tab.scroll();
					if cy + scroll > HEADER_SIZE + tab.value.height() * 16 {
						break 'a;
					};
					let highlight_idx = (((cy as f64 - self.mouse_y as f64).atan2(cx as f64 - self.mouse_x as f64) + core::f64::consts::FRAC_PI_8 - core::f64::consts::FRAC_PI_2).rem_euclid(core::f64::consts::TAU) * core::f64::consts::FRAC_2_PI * 2.0) as usize;
					let mut indices = Vec::new();
					let mut depth = 0;
					if (cy & !0b1111) + scroll == HEADER_SIZE {
						self.subscription = tab
							.value
							.actions()
							.get(highlight_idx)
							.copied()
							.and_then(|action| action.apply(None, indices.into_boxed_slice(), tab.uuid, &mut tab.value));
						return true;
					}
					let mut iter = TraverseParents::new((cy - HEADER_SIZE) / 16 + scroll / 16, &mut tab.value);
					while let Some((position, idx, key, element, _)) = iter.next() {
						let (key, element) = (key, unsafe {
							element
								.get_mut(idx)
								.panic_unchecked("Index is always valid")
						});
						depth += 1;
						indices.push(idx);
						if let Position::Only | Position::Last = position {
							let Some(action) = element.actions().get(highlight_idx).copied() else {
								break 'a;
							};
							let min_x = depth * 16 + left_margin;
							let max_x = min_x + 32;
							if !(min_x..max_x).contains(&cx) {
								break 'a;
							};
							self.subscription = action.apply(
								if element.id() == NbtRegion::ID {
									None
								} else {
									key
								},
								indices.into_boxed_slice(),
								tab.uuid,
								element,
							);
							return true;
						}
					}
				} else {
					// features, idk
				}
			}
			true
		} else {
			false
		}
	}

	#[inline]
	#[allow(clippy::too_many_lines)]
	pub fn try_subscription(&mut self) -> Result<()> {
		fn write_snbt(subscription: &FileUpdateSubscription, data: &[u8], tab: &mut Tab) -> Result<()> {
			let Some((key, value)) = core::str::from_utf8(data)
				.ok()
				.and_then(NbtElement::from_str)
			else {
				return Err(anyhow!("SNBT failed to parse."));
			};
			if value.id() == NbtChunk::ID && tab.value.id() != NbtRegion::ID { return Err(anyhow!("Chunk SNBT is only supported for Region Tabs")) }
			if let Some((&last, rest)) = subscription.indices.split_last() {
				let (_, _, parent, mut line_number) = Navigate::new(rest.iter().copied(), &mut tab.value).last();
				let mut old_key = None;
				if let Some(key) = key {
					if let Some(compound) = parent.as_compound_mut() {
						old_key = compound.update_key(last, key);
					} else if let Some(chunk) = parent.as_chunk_mut() {
						old_key = chunk.update_key(last, key);
					} else if let Some(region) = parent.as_region_mut() {
						old_key = Some({
							let Some((x, z)) = key.split_once('|') else {
								return Err(anyhow!("Could not find chunk delimiter"));
							};
							let Ok(x @ 0..=31) = x.trim_end().parse() else {
								return Err(anyhow!("Invalid X coordinate for chunk"));
							};
							let Ok(z @ 0..=31) = z.trim_start().parse() else {
								return Err(anyhow!("Invalid Y coordinate for chunk"));
							};
							let pos = ((x as u16) << 5) | z as u16;
							let (_, chunks) = &*region.chunks;
							if !chunks[pos as usize].is_null() { return Err(anyhow!("Replacement chunk is already filled")) }
							let mut element = region.remove(last);
							let old_key = format_compact!("{}|{}", unsafe { element.as_chunk_unchecked().x }, unsafe {
								element.as_chunk_unchecked().z
							});
							// all values inside a region are chunks
							unsafe {
								element.as_chunk_unchecked_mut().x = x;
							}
							unsafe {
								element.as_chunk_unchecked_mut().z = z;
							}
							// chunk is asserted above
							unsafe {
								region.insert_unchecked(pos as usize, last, element);
							}
							old_key
						});
					}
				}
				let (diff, true_diff) = (value.height(), value.true_height());
				let old_value = core::mem::replace(
					unsafe { parent.get_mut(last).panic_unchecked("Valid index") },
					value,
				);
				let old_true_height = old_value.true_height();
				let (diff, true_diff) = (
					diff.wrapping_sub(old_value.height()),
					true_diff.wrapping_sub(old_true_height),
				);
				for idx in 0..last {
					line_number += unsafe { parent.get_mut(idx).panic_unchecked("always valid idx") }.true_height();
				}
				line_number += 1;
				let idx = tab
					.bookmarks
					.binary_search(&Bookmark::new(line_number, 0))
					.unwrap_or_else(identity);
				for bookmark in tab
					.bookmarks
					.iter_mut()
					.skip(idx)
					.take_while(|bookmark| bookmark.true_line_number < line_number + old_true_height)
				{
					bookmark.true_line_number = bookmark.true_line_number.wrapping_add(true_diff);
					bookmark.line_number = bookmark.line_number.wrapping_add(diff);
				}
				let mut iter = Navigate::new(rest.iter().copied(), &mut tab.value);
				while let Some((_, _, _, parent, _)) = iter.next() {
					parent.increment(diff, true_diff);
				}
				recache_along_indices(rest, &mut tab.value);
				tab.append_to_history(WorkbenchAction::Replace {
					indices: subscription.indices.clone(),
					value: (old_key.or(Some(CompactString::new_inline("_"))), old_value),
				});
			} else {
				if tab.value.id() == value.id() {
					tab.bookmarks.clear();
					tab.horizontal_scroll = 0;
					tab.scroll = 0;

					tab.undos.push(WorkbenchAction::Replace {
						indices: Box::new([]),
						value: (None, core::mem::replace(tab.value.as_mut(), value)),
					});
					tab.redos.clear();
					tab.history_changed = true;
				} else {
					return Err(anyhow!("Root element type cannot be changed"));
				}
			}
			Ok(())
		}

		fn write_array(subscription: &FileUpdateSubscription, tab: &mut Tab, mut new_value: NbtElement) -> Result<()> {
			let (&last, rest) = unsafe {
				subscription
					.indices
					.split_last()
					.panic_unchecked("always has at least one element")
			};
			let (_, _, parent, mut line_number) = Navigate::new(rest.iter().copied(), &mut tab.value).last();
			let old_key;
			if let Some(compound) = parent.as_compound_mut() {
				old_key = unsafe {
					Some(
						compound
							.get_mut(last)
							.panic_unchecked("valid index")
							.0
							.to_compact_string(),
					)
				};
			} else if let Some(chunk) = parent.as_chunk_mut() {
				old_key = unsafe {
					Some(
						chunk
							.get_mut(last)
							.panic_unchecked("valid index")
							.0
							.to_compact_string(),
					)
				};
			} else {
				old_key = None;
			}
			let before = unsafe { parent.get_mut(last).panic_unchecked("valid index") };
			let old_true_height = before.true_height();
			if new_value.open() != before.open() {
				let _ = new_value.toggle();
			}
			let (diff, true_diff) = (new_value.height(), new_value.true_height());
			let (diff, true_diff) = (
				diff.wrapping_sub(before.height()),
				true_diff.wrapping_sub(old_true_height),
			);
			let old_value = core::mem::replace(before, new_value);
			for idx in 0..last {
				line_number += unsafe { parent.get_mut(idx).panic_unchecked("valid idx") }.true_height();
			}
			line_number += 1;
			let idx = tab
				.bookmarks
				.binary_search(&Bookmark::new(line_number, 0))
				.unwrap_or_else(identity);
			for bookmark in tab
				.bookmarks
				.iter_mut()
				.skip(idx)
				.take_while(|bookmark| bookmark.true_line_number < line_number + old_true_height)
			{
				bookmark.true_line_number = bookmark.true_line_number.wrapping_add(true_diff);
				bookmark.line_number = bookmark.line_number.wrapping_add(diff);
			}
			let mut iter = Navigate::new(rest.iter().copied(), &mut tab.value);
			while let Some((_, _, _, parent, _)) = iter.next() {
				parent.increment(diff, true_diff);
			}
			recache_along_indices(&subscription.indices, &mut tab.value);
			tab.append_to_history(WorkbenchAction::Replace {
				indices: subscription.indices.clone(),
				value: (old_key, old_value),
			});
			Ok(())
		}

		if let Some(subscription) = &mut self.subscription {
			if let Some(tab) = self
				.tabs
				.iter_mut()
				.find(|tab| tab.uuid == subscription.tab_uuid)
			{
				if let Err(e) = subscription.watcher.poll().map_err(|x| anyhow!("{x}")) {
					self.subscription = None;
					return Err(e);
				};
				match subscription.rx.try_recv() {
					Ok(data) => match subscription.subscription_type {
						FileUpdateSubscriptionType::Snbt => write_snbt(subscription, &data, tab)?,
						FileUpdateSubscriptionType::ByteArray => write_array(subscription, tab, {
							let mut array = NbtByteArray::new();
							for (idx, byte) in data.into_iter().enumerate() {
								let _ = array.insert(idx, NbtElement::Byte(NbtByte { value: byte as i8 }));
							}
							NbtElement::ByteArray(array)
						})?,
						FileUpdateSubscriptionType::IntArray => write_array(subscription, tab, {
							let mut array = NbtIntArray::new();
							let iter = data.array_chunks::<4>();
							if !iter.remainder().is_empty() { return Err(anyhow!("Expected a multiple of 4 bytes for int array")) }
							for (idx, &chunk) in iter.enumerate() {
								let _ = array.insert(
									idx,
									NbtElement::Int(NbtInt {
										value: i32::from_be_bytes(chunk),
									}),
								);
							}
							NbtElement::IntArray(array)
						})?,
						FileUpdateSubscriptionType::LongArray => write_array(subscription, tab, {
							let mut array = NbtLongArray::new();
							let iter = data.array_chunks::<8>();
							if !iter.remainder().is_empty() { return Err(anyhow!("Expected a multiple of 8 bytes for long array")) }
							for (idx, &chunk) in iter.enumerate() {
								let _ = array.insert(
									idx,
									NbtElement::Long(NbtLong {
										value: i64::from_be_bytes(chunk),
									}),
								);
							}
							NbtElement::LongArray(array)
						})?,
					},
					Err(TryRecvError::Disconnected) => {
						self.subscription = None;
						return Err(anyhow!("Could not update; file subscription disconnected."));
					}
					Err(TryRecvError::Empty) => {
						// do nothing ig
					}
				}
			} else {
				self.subscription = None;
				return Err(anyhow!("Could not update; file subscription tab closed."));
			}
		}

		Ok(())
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
			let (depth, height, true_height, line_number) = {
				let (depth, (_, _, element, line_number)) = Traverse::new(y, &mut tab.value).enumerate().last();
				(depth, element.height(), element.true_height(), line_number)
			};
			if depth != target_depth { return false }

			let mut indices = vec![];
			let mut iter = TraverseParents::new(y, &mut tab.value);
			let mut value = 'w: {
				while let Some((position, idx, _, element, _)) = iter.next() {
					indices.push(idx);
					element.decrement(height, true_height);
					match position {
						Position::Only | Position::Last => {
							break 'w (unsafe {
								element
									.remove(idx)
									.panic_unchecked("we asserted above that this indeed does capture the final element")
							})
						}
						Position::First | Position::Middle => {}
					}
				}
				unsafe { panic_unchecked("so you're telling me, we had a node, that was **not** root, as confirmed by the let chain for `NbtWorkbench::tab_mut()` yet somehow no parent's even existed, seriously wtf") }
			};

			recache_along_indices(&indices[..indices.len() - 1], &mut tab.value);
			value.1.shut();
			let mut idx = tab
				.bookmarks
				.binary_search(&Bookmark::new(line_number, 0))
				.unwrap_or_else(identity);
			while let Some(bookmark) = tab.bookmarks.get_mut(idx) {
				if bookmark.true_line_number - line_number < true_height {
					let _ = tab.bookmarks.remove(idx);
				} else {
					bookmark.true_line_number -= true_height;
					bookmark.line_number -= height;
					idx += 1;
				}
			}
			// no need for encompass or equal since that's handled by `drop`
			self.held_entry = HeldEntry::FromKnown(value, indices.into_boxed_slice());
			true
		} else {
			false
		}
	}

	#[inline]
	fn rename(&mut self, offset: usize) -> bool {
		let left_margin = self.left_margin();
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		let name = tab
			.path
			.as_ref()
			.and_then(|x| x.to_str())
			.map_or_else(|| tab.name.clone(), Into::into);
		tab.selected_text = SelectedText::new(
			36 + left_margin,
			offset,
			HEADER_SIZE,
			Some((name, true)),
			None,
			false,
			vec![],
		);
		tab.selected_text.is_some()
	}

	#[inline]
	fn duplicate(&mut self) -> bool {
		if self.mouse_y < HEADER_SIZE { return false }

		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		if y < tab.value.height() && y > 0 {
			let (height, true_height, line_number) = unsafe {
				Traverse::new(y, &mut tab.value)
					.last()
					.map(|(_, _, element, line_number)| (element.height(), element.true_height(), line_number))
					.panic_unchecked("we've asserted that y > 0")
			};

			let mut indices = vec![];
			let mut iter = TraverseParents::new(y, &mut tab.value);
			while let Some((position, idx, key, element, _)) = iter.next() {
				match position {
					Position::First | Position::Middle => {
						element.increment(height, true_height);
						indices.push(idx);
					}
					Position::Only | Position::Last => {
						indices.push(idx + 1);
						let duplicate = unsafe {
							element
								.get(idx)
								.panic_unchecked("it exists mate, let's stop playing around")
						}
						.clone();
						if let Some(compound) = element.as_compound_mut() {
							compound.insert(
								idx + 1,
								unsafe { key.panic_unchecked("it's a compound, it **has** a key for every value") },
								duplicate,
							);
						} else {
							if element.insert(idx + 1, duplicate).is_err() { return false }
						}
					}
				}
			}
			recache_along_indices(&indices[..indices.len() - 1], &mut tab.value);
			let start = tab
				.bookmarks
				.binary_search(&Bookmark::new(line_number, 0))
				.unwrap_or_else(identity);
			for bookmark in tab.bookmarks.iter_mut().skip(start) {
				if bookmark.true_line_number - line_number < true_height {
					// do nothing, since the bookmark is within the tail node
				} else {
					bookmark.true_line_number += true_height;
					bookmark.line_number += height;
				}
			}
			if let Some(subscription) = &mut self.subscription
				&& encompasses(&indices[..indices.len() - 1], &subscription.indices)
			{
				if subscription.indices[indices.len() - 1] <= indices[indices.len() - 1] {
					subscription.indices[indices.len() - 1] += 1;
				}
			}
			tab.append_to_history(WorkbenchAction::Add {
				indices: indices.into_boxed_slice(),
			});
			true
		} else {
			false
		}
	}

	#[inline]
	fn copy(&mut self, debug: bool) -> bool {
		if self.mouse_y < HEADER_SIZE { return false }
		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		if y < tab.value.height() {
			let (_, mut key, element, _) = unsafe {
				Traverse::new(y, &mut tab.value)
					.last()
					.panic_unchecked("There is always at least one element - Master Oogway")
			};
			if element.id() == NbtChunk::ID {
				key = None;
			}
			let mut buf = String::new();
			let key = key.map(|key| {
				if key.needs_escape() {
					format_compact!("{key:?}")
				} else {
					key
				}
			});
			let key_exists = key.is_some();
			if debug {
				if write!(
					&mut buf,
					"{}{}{element:#?}",
					key.unwrap_or(CompactString::new_inline("")),
					if key_exists { ": " } else { "" }
				)
				.is_err()
				{
					return false;
				}
			} else {
				if write!(
					&mut buf,
					"{}{}{element}",
					key.unwrap_or(CompactString::new_inline("")),
					if key_exists { ":" } else { "" }
				)
				.is_err()
				{
					return false;
				}
			}
			cli_clipboard::set_contents(buf).is_ok()
		} else {
			false
		}
	}

	#[inline]
	fn delete(&mut self, clipboard: bool) -> bool {
		if self.mouse_y < HEADER_SIZE { return false };

		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		if y < tab.value.height() && y > 0 {
			let mut indices = vec![];
			let (height, true_height, line_number) = if let Some((_, key, element, line_number)) = Traverse::new(y, &mut tab.value).last() {
				if clipboard {
					let key = key.map(|key| {
						if key.needs_escape() {
							format_compact!("{key:?}")
						} else {
							key
						}
					});
					let mut buf = String::new();
					if write!(
						&mut buf,
						"{}{}{element}",
						key.as_ref().map_or("", CompactString::as_str),
						if key.is_some() { ":" } else { "" }
					)
					.is_ok()
					{
						let _ = cli_clipboard::set_contents(buf);
					}
				}
				(element.height(), element.true_height(), line_number)
			} else {
				unsafe { panic_unchecked("dis ain't even possible i never f**k'd with the traverse") }
			};
			let mut iter = TraverseParents::new(y, &mut tab.value);
			let (key, value) = 'w: {
				while let Some((position, idx, _, element, _)) = iter.next() {
					indices.push(idx);
					element.decrement(height, true_height);
					match position {
						Position::First | Position::Middle => {}
						Position::Last | Position::Only => break 'w (unsafe { element.remove(idx).panic_unchecked("da hell mate") }),
					}
				}
				unsafe { panic_unchecked("parents were dodged") }
			};
			recache_along_indices(&indices[..indices.len() - 1], &mut tab.value);
			let mut idx = tab
				.bookmarks
				.binary_search(&Bookmark::new(line_number, 0))
				.unwrap_or_else(identity);
			while let Some(bookmark) = tab.bookmarks.get_mut(idx) {
				if bookmark.true_line_number - line_number < true_height {
					let _ = tab.bookmarks.remove(idx);
				} else {
					bookmark.true_line_number -= true_height;
					bookmark.line_number -= height;
					idx += 1;
				}
			}
			if let Some(subscription) = &mut self.subscription {
				if *indices == *subscription.indices {
					self.subscription = None;
				} else if encompasses(&indices[..indices.len() - 1], &subscription.indices) {
					if subscription.indices[indices.len() - 1] > indices[indices.len() - 1] {
						subscription.indices[indices.len() - 1] -= 1;
					}
				}
			}
			tab.append_to_history(WorkbenchAction::Remove {
				element: (key, value),
				indices: indices.into_boxed_slice(),
			});
			true
		} else {
			false
		}
	}

	#[inline]
	fn drop(&mut self, pair: (Option<CompactString>, NbtElement), from_indices: Option<Box<[usize]>>, left_margin: usize) -> bool {
		let (key, element) = pair;
		let horizontal_scroll = self.horizontal_scroll();

		if self.mouse_y <= HEADER_SIZE { return false }
		if self.mouse_x + horizontal_scroll < left_margin { return false }
		let y = self.mouse_y - HEADER_SIZE + self.scroll();
		let x = (self.mouse_x + horizontal_scroll - left_margin) / 16;
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};

		if element.id() == NbtChunk::ID && tab.value.id() != NbtRegion::ID { return false }
		let mut indices = vec![];
		match NbtElement::drop(
			tab.value.as_mut(),
			key.clone(),
			element,
			&mut y.clone(),
			2,
			x,
			1,
			&mut indices,
		) {
			DropFn::InvalidType(key, element) | DropFn::Missed(key, element) => {
				if let Some(from_indices) = from_indices {
					tab.append_to_history(WorkbenchAction::Remove {
						indices: from_indices,
						element: (key, element),
					});
				}

				tab.selected_text = None;
			}
			DropFn::Dropped(height, true_height, _, line_number) => {
				if let Some(from_indices) = from_indices {
					if let Some(subscription) = &mut self.subscription
						&& encompasses_or_equal(&from_indices, &subscription.indices)
					{
						let (_, rest) = subscription.indices.split_at(from_indices.len());
						let mut slice = Box::new_uninit_slice(indices.len() + rest.len());
						unsafe {
							slice
								.as_mut_ptr()
								.cast::<usize>()
								.copy_from_nonoverlapping(indices.as_ptr(), indices.len());
						}
						unsafe {
							slice
								.as_mut_ptr()
								.cast::<usize>()
								.copy_from_nonoverlapping(rest.as_ptr(), rest.len());
						}
						subscription.indices = unsafe { slice.assume_init() };
					}

					if from_indices.as_ref() != indices.as_slice() {
						tab.append_to_history(WorkbenchAction::Move {
							from: from_indices,
							to: indices.clone().into_boxed_slice(),
							original_key: key,
						});
					}
				} else {
					tab.append_to_history(WorkbenchAction::Add {
						indices: indices.clone().into_boxed_slice(),
					});
				}
				recache_along_indices(&indices[..indices.len() - 1], &mut tab.value);
				let start = tab
					.bookmarks
					.binary_search(&Bookmark::new(line_number, 0))
					.unwrap_or_else(identity);
				for bookmark in tab.bookmarks.iter_mut().skip(start) {
					bookmark.true_line_number += true_height;
					bookmark.line_number += height;
				}
				self.subscription = None;
			}
		}
		true
	}

	#[inline]
	fn hold_entry(&mut self, button: MouseButton) -> Result<bool> {
		if let Some(tab) = self.tab()
			&& button == MouseButton::Left
		{
			if self.mouse_x / 16 == 13 {
				match NbtElement::from_str(&cli_clipboard::get_contents().map_err(|e| anyhow!("{}", e.to_string()))?) {
					Some((key, element)) => {
						if element.id() == NbtChunk::ID && tab.value.id() != NbtRegion::ID {
							return Err(anyhow!("Chunks are not supported for non-region tabs"));
						} else {
							self.held_entry = HeldEntry::FromAether((key, element))
						}
					}
					None => return Err(anyhow!("Could not parse clipboard as SNBT")),
				}
			} else {
				self.held_entry = unsafe {
					HeldEntry::FromAether((
						None,
						NbtElement::from_id(match self.mouse_x / 16 {
							0 => NbtByte::ID,
							1 => NbtShort::ID,
							2 => NbtInt::ID,
							3 => NbtLong::ID,
							4 => NbtFloat::ID,
							5 => NbtDouble::ID,
							6 => NbtByteArray::ID,
							7 => NbtIntArray::ID,
							8 => NbtLongArray::ID,
							9 => NbtString::ID,
							10 => NbtList::ID,
							11 => NbtCompound::ID,
							12 if tab.value.id() == NbtRegion::ID => NbtChunk::ID,
							_ => return Ok(false),
						})
						.panic_unchecked("Type was invalid somehow, even though we map each one"),
					))
				};
			}
		}
		Ok(true)
	}

	#[inline]
	fn click_tab(&mut self, button: MouseButton, f: impl Fn(&str)) {
		let mouse_x = self.mouse_x + self.tab_scroll;
		if mouse_x < 2 { return }

		let mut x = mouse_x - 2;
		'a: {
			for (idx, tab) in self.tabs.iter_mut().enumerate() {
				let width = tab.name.width() + 48 + 5;

				if x <= width {
					if button == MouseButton::Middle {
						let tab = self.tabs.remove(idx);
						std::thread::spawn(move || drop(tab));
						self.set_tab(idx.saturating_sub(1), f);
					} else if idx == self.tab && x > width - 16 && x < width {
						if button == MouseButton::Left {
							tab.compression = tab.compression.cycle();
						} else if button == MouseButton::Right {
							tab.compression = tab.compression.rev_cycle();
						}
					} else if idx == self.tab && x + 1 >= width - 32 && x < width - 16 {
						tab.save();
					} else if button == MouseButton::Left {
						self.set_tab(idx, f);
					}

					return;
				}

				x -= width;

				if x < 6 {
					break 'a;
				} else {
					x -= 6;
				}
			}

			if button == MouseButton::Middle {
				self.new_tab(f);
			}
		}
	}

	#[inline]
	pub fn new_tab(&mut self, f: impl Fn(&str)) {
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
			bookmarks: vec![],
			uuid: Uuid::new_v4(),
			freehand_mode: false,
			selected_text: None,
		});
		self.set_tab(self.tabs.len() - 1, f);
	}

	#[inline]
	fn left_margin(&self) -> usize {
		self.tab()
			.map_or(8, |tab| tab.left_margin(self.held_entry.element()))
	}

	#[inline]
	fn toggle(&mut self, expand: bool, ignore_depth: bool) -> bool {
		let left_margin = self.left_margin();
		let horizontal_scroll = self.horizontal_scroll();
		if self.mouse_x + horizontal_scroll < left_margin { return false }
		if self.mouse_y < HEADER_SIZE { return false }
		let x = (self.mouse_x + horizontal_scroll - left_margin) / 16;
		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		if y >= tab.value.height() { return false }

		let (depth, (_, _, element, line_number)) = Traverse::new(y, &mut tab.value).enumerate().last();
		let true_height = element.true_height();

		if x > depth && !ignore_depth { return false }
		let before = element.height();
		if expand {
			std::thread::scope(|scope| element.expand(scope));
		} else {
			let _ = element.toggle();
		}
		let increment = element.height().wrapping_sub(before);
		if increment == 0 { return true }
		let open = element.open();

		let mut iter = TraverseParents::new(y, &mut tab.value);
		let mut indices = Vec::with_capacity(depth);
		while let Some((_, idx, _, element, _)) = iter.next() {
			indices.push(idx);
			element.increment(increment, 0);
		}

		tab.scroll = tab.scroll();
		// toggle has no effect on true height
		recache_along_indices(&indices, &mut tab.value);
		let element = unsafe {
			Traverse::new(y, &mut tab.value)
				.last()
				.panic_unchecked("nothing is wrong i didn't do .next")
		}
		.2;
		let start = tab
			.bookmarks
			.binary_search(&Bookmark::new(line_number, 0))
			.unwrap_or_else(identity);
		let mut next_line_number = line_number;
		let mut next_line_number_idx = 0;
		for bookmark in tab.bookmarks.iter_mut().skip(start) {
			if bookmark.true_line_number - line_number < true_height {
				if open {
					while next_line_number < bookmark.true_line_number {
						next_line_number += unsafe {
							element
								.get(next_line_number_idx)
								.panic_unchecked("expected index to be valid for parent index")
						}
						.true_height();
						next_line_number_idx += 1;
					}
					let new_line_number = y + next_line_number_idx + 1;
					bookmark.line_number = new_line_number;
					if bookmark.true_line_number == next_line_number {
						bookmark.uv = BOOKMARK_UV;
					} else {
						bookmark.uv = HIDDEN_BOOKMARK_UV;
					}
				} else {
					bookmark.line_number = y;
					bookmark.uv = HIDDEN_BOOKMARK_UV;
				}
			} else {
				bookmark.line_number = bookmark.line_number.wrapping_add(increment);
			}
		}
		true
	}

	#[inline]
	fn try_select_text(&mut self) -> bool {
		let left_margin = self.left_margin();
		let horizontal_scroll = self.horizontal_scroll();
		if self.mouse_x + horizontal_scroll < left_margin { return false }
		if self.mouse_y < HEADER_SIZE + 16 { return false }
		let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		if y >= tab.value.height() { return false }

		let mut indices = vec![];
		let mut iter = TraverseParents::new(y, &mut tab.value);
		while let Some((position, idx, key, value, _)) = iter.next() {
			indices.push(idx);
			if let Position::Last | Position::Only = position {
				let child = unsafe { value.get(idx).panic_unchecked("Child didn't exist somehow") };
				tab.selected_text = SelectedText::new(
					indices.len() * 16 + 32 + 4 + left_margin,
					self.mouse_x + horizontal_scroll,
					y * 16 + HEADER_SIZE,
					key.or_else(|| child.as_chunk().map(|chunk| chunk.x.to_compact_string()))
						.map(|x| (x.into_string().into_boxed_str(), true)),
					Some(child.value()).map(|(a, b)| (a.into_string().into_boxed_str(), b)),
					child.id() == NbtChunk::ID,
					indices,
				);
				return tab.selected_text.is_some();
			}
		}
		false
	}

	#[inline]
	fn bookmark_line(&mut self) -> bool {
		let left_margin = self.left_margin();
		let horizontal_scroll = self.horizontal_scroll();
		let scroll = self.scroll();
		if self.mouse_x + horizontal_scroll > left_margin { return false }
		if self.mouse_y < HEADER_SIZE { return false }
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return false;
		};
		if self.mouse_y + scroll > HEADER_SIZE + tab.value.height() * 16 { return false }
		let true_height = unsafe {
			Traverse::new((self.mouse_y + scroll - HEADER_SIZE) / 16, &mut tab.value)
				.last()
				.panic_unchecked("Traverse always has something")
		}
		.3;
		let line_number = Bookmark::new(true_height, (self.mouse_y + scroll - HEADER_SIZE) / 16);
		match tab.bookmarks.binary_search(&line_number) {
			Ok(idx) => {
				let _ = tab.bookmarks.remove(idx);
			}
			Err(idx) => tab.bookmarks.insert(idx, line_number),
		}
		true
	}

	#[inline]
	pub fn keyfix(&mut self) {
		if let Some(tab) = self.tab_mut() {
			if let Some(SelectedText {
				y,
				indices,
				cursor,
				value,
				selection,
				keyfix,
				prefix,
				suffix,
				valuefix,
				editable: true,
				last_interaction: _,
				undos: _,
				redos: _,
			}) = tab.selected_text.clone()
				&& let Some(keyfix) = keyfix
				&& valuefix.is_none()
				&& suffix.is_empty()
				&& cursor == 0
			{
				if !tab.close_selected_text(false) { return }
				tab.selected_text = Some(
					SelectedText {
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
					}
					.post_process(),
				);
			}
		}
	}

	#[inline]
	pub fn valuefix(&mut self) {
		if let Some(tab) = self.tab_mut() {
			if let Some(SelectedText {
				y,
				indices,
				cursor,
				value,
				selection,
				keyfix,
				prefix,
				suffix,
				valuefix,
				editable: true,
				last_interaction: _,
				undos: _,
				redos: _,
			}) = tab.selected_text.clone()
				&& let Some(valuefix) = valuefix
				&& keyfix.is_none()
				&& prefix.is_empty()
				&& cursor == value.len()
			{
				// normally won't occur, but im future proofing
				if !tab.close_selected_text(false) { return }
				tab.selected_text = Some(
					SelectedText {
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
					}
					.post_process(),
				);
			}
		}
	}

	#[inline]
	pub fn shift_selected_text_up(&mut self) {
		let tab = unsafe { self.tabs.get_unchecked_mut(self.tab) };
		if let Some(SelectedText { y, indices, .. }) = &mut tab.selected_text {
			if indices.is_empty() { return } // well it could be empty
			let child_idx = unsafe {
				indices
					.last()
					.copied()
					.panic_unchecked("Indices list is never empty")
			};
			let parent = Navigate::new(
				indices.iter().copied().take(indices.len() - 1),
				&mut tab.value,
			)
			.last()
			.2;
			if child_idx == 0 || parent.len().is_none_or(|x| x == 0) { return }
			let original_key = parent.as_compound_mut().map(|compound| unsafe {
				compound
					.get(child_idx)
					.panic_unchecked("Index obviously exists")
					.0
					.to_compact_string()
			});
			*y -= unsafe {
				parent
					.get(child_idx - 1)
					.panic_unchecked("if i exist, the one before me exists")
			}
			.height() * 16;
			parent.swap(child_idx, child_idx - 1);
			let from = indices.clone();
			unsafe {
				*indices
					.last_mut()
					.panic_unchecked("Indices list is never empty") -= 1;
			}
			let to = indices.clone();

			if let Some(subscription) = &mut self.subscription
				&& encompasses(&indices[..indices.len() - 1], &subscription.indices)
			{
				if subscription.indices[indices.len() - 1] >= child_idx {
					subscription.indices[indices.len() - 1] -= 1;
				}
			}
			tab.scroll = tab.scroll.min((*y - HEADER_SIZE).saturating_sub(16));
			tab.scroll = tab.scroll();
			tab.append_to_history(WorkbenchAction::Move {
				from,
				to,
				original_key,
			});
		}
	}

	#[inline]
	pub fn shift_selected_text_down(&mut self) {
		let tab = unsafe { self.tabs.get_unchecked_mut(self.tab) };
		if let Some(SelectedText { y, indices, .. }) = &mut tab.selected_text {
			// well it could be empty
			if indices.is_empty() { return }
			let child_idx = unsafe {
				indices
					.last()
					.copied()
					.panic_unchecked("Indices list is never empty")
			};
			let parent = Navigate::new(
				indices.iter().copied().take(indices.len() - 1),
				&mut tab.value,
			)
			.last()
			.2;
			if parent.len().is_none_or(|x| x == child_idx + 1) { return }
			let original_key = parent.as_compound_mut().map(|compound| unsafe {
				compound
					.get(child_idx)
					.panic_unchecked("Index obviously exists")
					.0
					.to_compact_string()
			});
			*y += unsafe { parent.get(child_idx + 1).panic_unchecked("checked above") }.height() * 16;
			parent.swap(child_idx, child_idx + 1);
			let from = indices.clone();
			unsafe {
				*indices
					.last_mut()
					.panic_unchecked("Indices list is never empty") += 1;
			}
			let to = indices.clone();

			if let Some(subscription) = &mut self.subscription
				&& encompasses(&indices[..indices.len() - 1], &subscription.indices)
			{
				if subscription.indices[indices.len() - 1] <= child_idx {
					subscription.indices[indices.len() - 1] += 1;
				}
			}

			if *y + 48 > tab.scroll + tab.window_height - HEADER_SIZE {
				tab.scroll = *y + 48 - (tab.window_height - HEADER_SIZE);
				tab.scroll = tab.scroll();
			}

			tab.append_to_history(WorkbenchAction::Move {
				from,
				to,
				original_key,
			});
		}
	}

	/// # Safety
	///
	/// * Indices contains valid data
	#[inline]
	pub unsafe fn selected_text_up(&mut self, ctrl: bool) {
		let left_margin = self.left_margin();
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return;
		};
		if let Some(SelectedText {
			y,
			indices,
			cursor,
			keyfix,
			prefix,
			value: str_value,
			..
		}) = tab.selected_text.clone()
		{
			let Some(&last_index) = indices.last() else {
				return;
			};
			if !tab.close_selected_text(false) { return }
			let cache_cursor_x = self.cache_cursor_x;
			let original_indices_len = indices.len();
			let mouse_x = cache_cursor_x.unwrap_or(original_indices_len * 16 + 32 + 4 + left_margin + keyfix.as_deref().map_or(0, StrExt::width) + prefix.width() + str_value.split_at(cursor).0.width());

			if y == HEADER_SIZE + 16 {
				let width = tab
					.path
					.as_ref()
					.and_then(|x| x.to_str())
					.map_or_else(|| tab.name.width(), StrExt::width);
				self.rename(mouse_x.min(width + 32 + 4 + self.left_margin()));
				return;
			}

			let (k, v, indices, new_y) = if ctrl && last_index > 0 {
				let mut indices = indices.into_vec();
				*indices
					.last_mut()
					.panic_unchecked("indices cannot be empty, we're in it") = 0;
				let (k, v) = Navigate::new(
					indices.iter().copied().take(original_indices_len - 1),
					&mut tab.value,
				)
				.last()
				.2
				.children()
				.panic_unchecked("we are a child, some must exist")
				.map_or_else(
					|mut iter| {
						let (k, v) = iter
							.next()
							.panic_unchecked("we are a child, some must exist");
						(Some((k.to_compact_string(), true)), Some(v.value()))
					},
					|iter| match iter {
						iter @ ValueIterator::Generic(_) => (
							None,
							Some(
								iter.enumerate()
									.next()
									.panic_unchecked("we're the child, it can't be empty")
									.1
									.value(),
							),
						),
						iter @ ValueIterator::Region(_, _) => {
							let chunk = unsafe {
								iter.enumerate()
									.next()
									.panic_unchecked("we're the child, it can't be empty")
									.1
									.as_chunk_unchecked()
							};
							(
								Some((chunk.x.to_compact_string(), true)),
								Some((chunk.z.to_compact_string(), true)),
							)
						}
					},
				);
				let new_y = sum_indices(indices.iter().copied(), &tab.value) * 16;
				(k, v, indices, new_y)
			} else {
				let total = sum_indices(indices.iter().copied(), &tab.value) - 1;
				let mut indices = vec![];
				// SAFETY: total is -1'd meaning that it's original range of 1..=root.height() is now ..root.height(), which is in range
				let (k, v) = 'w: {
					let mut iter = TraverseParents::new(total, &mut tab.value);
					while let Some((position, idx, key, value, _)) = iter.next() {
						indices.push(idx);
						if let Position::Last | Position::Only = position {
							break 'w (
								key.or_else(|| {
									value
										.as_region()
										.and_then(|region| region.get(idx))
										.and_then(NbtElement::as_chunk)
										.map(|chunk| chunk.x.to_compact_string())
								})
								.map(|k| (k, true)),
								value.get(idx).map(NbtElement::value),
							);
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
				+ k.as_ref()
					.map_or(0, |(x, display)| x.width() * (*display as usize))
				+ (k.as_ref().is_some_and(|(_, display)| *display) && v.as_ref().is_some_and(|(_, display)| *display)) as usize * (": ".width())
				+ v.as_ref()
					.map_or(0, |(x, display)| (*display as usize) * x.width());
			self.cache_cursor_x = self.cache_cursor_x.or(Some(mouse_x));
			tab.selected_text = SelectedText::new(
				low,
				mouse_x.clamp(low, high),
				new_y + HEADER_SIZE,
				k.map(|(a, b)| (a.into_string().into_boxed_str(), b)),
				v.map(|(a, b)| (a.into_string().into_boxed_str(), b)),
				indices.len() == 1 && tab.value.id() == NbtRegion::ID,
				indices,
			);
		}
	}

	/// # Safety
	///
	/// * Indices contains valid data
	#[inline]
	pub unsafe fn selected_text_down(&mut self, ctrl: bool) {
		let left_margin = self.left_margin();

		let total = if let Some(tab) = self.tab()
			&& let Some(SelectedText { indices, .. }) = tab.selected_text.as_ref()
		{
			let mut total = sum_indices(indices.iter().copied(), &tab.value);
			total += 1; // move down
			// down needs a check that it doesn't surpass the end
			if total >= tab.value.height() { return }
			total
		} else {
			return;
		};

		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return;
		};
		if let Some(SelectedText {
			y,
			indices,
			cursor,
			keyfix,
			prefix,
			value: str_value,
			..
		}) = tab.selected_text.clone()
		{
			if !tab.close_selected_text(false) { return }
			let cache_cursor_x = self.cache_cursor_x;
			let original_indices_len = indices.len();
			let mouse_x = cache_cursor_x.unwrap_or(original_indices_len * 16 + 32 + 4 + left_margin + keyfix.as_deref().map_or(0, StrExt::width) + prefix.width() + str_value.split_at(cursor).0.width());
			let (k, v, end_idx) = if ctrl && indices.len() > 0 {
				Navigate::new(
					indices.iter().copied().take(indices.len() - 1),
					&mut tab.value,
				)
				.last()
				.2
				.children()
				.panic_unchecked("we're the child")
				.map_or_else(
					|iter| {
						let tuple = iter
							.enumerate()
							.last()
							.panic_unchecked("we're the child, it can't be empty");
						(
							Some((tuple.1 .0.to_compact_string(), true)),
							Some(tuple.1 .1.value()),
							tuple.0,
						)
					},
					|iter| match iter {
						iter @ ValueIterator::Generic(_) => {
							let tuple = iter
								.enumerate()
								.last()
								.panic_unchecked("we're the child, it can't be empty");
							(None, Some(tuple.1.value()), tuple.0)
						}
						iter @ ValueIterator::Region(_, _) => {
							let (idx, chunk) = iter
								.enumerate()
								.last()
								.panic_unchecked("we're the child, it can't be empty");
							let chunk = unsafe { chunk.as_chunk_unchecked() };
							(
								Some((chunk.x.to_compact_string(), true)),
								Some((chunk.z.to_compact_string(), true)),
								idx,
							)
						}
					},
				)
			} else {
				(None, None, 0)
			};
			let (k, v, indices, new_y) = if ctrl && indices.len() > 0 && !indices.last().is_some_and(|x| *x == end_idx) {
				let mut indices = indices.into_vec();
				*indices
					.last_mut()
					.panic_unchecked("it literally just can't be empty") = end_idx;
				let new_y = sum_indices(indices.iter().copied(), &tab.value) * 16;
				(k, v, indices, new_y)
			} else {
				'w: {
					let mut indices = vec![];
					let mut iter = TraverseParents::new(total, &mut tab.value);
					while let Some((position, idx, key, value, _)) = iter.next() {
						indices.push(idx);
						if let Position::Last | Position::Only = position {
							break 'w (
								key.or_else(|| {
									value
										.as_region()
										.and_then(|region| region.get(idx))
										.and_then(NbtElement::as_chunk)
										.map(|chunk| chunk.x.to_compact_string())
								})
								.map(|k| (k, true)),
								Some(
									value
										.get(idx)
										.map(NbtElement::value)
										.panic_unchecked("We are literally given an `idx`"),
								),
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
				+ k.as_ref()
					.map_or(0, |(x, display)| x.width() * (*display as usize))
				+ (k.as_ref().is_some_and(|(_, display)| *display) && v.as_ref().is_some_and(|(_, display)| *display)) as usize * ": ".width()
				+ v.as_ref()
					.map_or(0, |(x, display)| (*display as usize) * x.width());
			if new_y + 48 > tab.scroll + tab.window_height - HEADER_SIZE {
				tab.scroll = new_y + 48 - (tab.window_height - HEADER_SIZE);
				tab.scroll = tab.scroll();
			}
			self.cache_cursor_x = self.cache_cursor_x.or(Some(mouse_x));
			tab.selected_text = SelectedText::new(
				low,
				mouse_x.clamp(low, high),
				new_y + HEADER_SIZE,
				k.map(|(a, b)| (a.into_string().into_boxed_str(), b)),
				v.map(|(a, b)| (a.into_string().into_boxed_str(), b)),
				indices.len() == 1 && tab.value.id() == NbtRegion::ID,
				indices,
			);
		}
	}

	#[inline]
	pub fn force_close(&mut self) {
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return;
		};
		if let Some(SelectedText { indices, y, .. }) = tab.selected_text.as_ref() {
			let indices = indices.clone();
			let (_, _, element, line_number) = Navigate::new(indices.iter().copied(), &mut tab.value).last();
			let decrement = element.height() - 1;
			if element.open() && element.toggle().is_some() {
				let mut iter = Navigate::new(indices.iter().copied(), &mut tab.value);
				while let Some((position, _, _, value, _)) = iter.next() {
					if let Position::First | Position::Middle = position {
						value.decrement(decrement, 0);
					}
				}
				recache_along_indices(&indices, &mut tab.value);
				let start = tab
					.bookmarks
					.binary_search(&Bookmark::new(line_number, 0))
					.unwrap_or_else(identity);
				for bookmark in tab.bookmarks.iter_mut().skip(start) {
					bookmark.line_number = (*y - HEADER_SIZE) / 16;
					bookmark.uv = HIDDEN_BOOKMARK_UV;
				}
			}
		}
	}

	#[inline]
	pub fn force_open(&mut self) {
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return;
		};
		if let Some(SelectedText { indices, y, .. }) = tab.selected_text.as_ref() {
			let indices = indices.clone();
			let shift = self.held_keys.contains(&VirtualKeyCode::LShift) | self.held_keys.contains(&VirtualKeyCode::RShift);
			let (_, _, element, line_number) = Navigate::new(indices.iter().copied(), &mut tab.value).last();
			let true_height = element.true_height();
			let pred = if shift {
				std::thread::scope(|scope| element.expand(scope));
				true
			} else {
				!element.open() && element.toggle().is_some()
			};
			if pred {
				let increment = element.height() - 1;
				let mut iter = Navigate::new(indices.iter().copied(), &mut tab.value);
				while let Some((position, _, _, value, _)) = iter.next() {
					if let Position::First | Position::Middle = position {
						value.increment(increment, 0);
					}
				}
				recache_along_indices(&indices, &mut tab.value);
				let start = tab
					.bookmarks
					.binary_search(&Bookmark::new(line_number, 0))
					.unwrap_or_else(identity);
				if shift {
					let parent_line_number = (*y - HEADER_SIZE) / 16;
					for bookmark in tab.bookmarks.iter_mut().skip(start) {
						if bookmark.true_line_number - line_number < true_height {
							bookmark.line_number = parent_line_number + bookmark.true_line_number - line_number;
							bookmark.uv = BOOKMARK_UV;
						} else {
							bookmark.line_number += increment;
						}
					}
				} else {
					let element = Navigate::new(indices.iter().copied(), &mut tab.value)
						.last()
						.2;
					let mut next_line_number = line_number;
					let mut next_line_number_idx = 0;
					for bookmark in tab.bookmarks.iter_mut().skip(start) {
						if bookmark.true_line_number - line_number < true_height {
							while next_line_number < bookmark.true_line_number {
								next_line_number += unsafe {
									element
										.get(next_line_number_idx)
										.panic_unchecked("expected index to be valid for parent index")
								}
								.true_height();
								next_line_number_idx += 1;
							}
							let new_line_number = y + next_line_number_idx + 1;
							bookmark.line_number = new_line_number;
							if bookmark.true_line_number == next_line_number {
								bookmark.uv = BOOKMARK_UV;
							} else {
								bookmark.uv = HIDDEN_BOOKMARK_UV;
							}
						} else {
							bookmark.line_number += increment;
						}
					}
				}
			}
		}
	}

	#[inline]
	pub fn refresh_selected_text_horizontal_scroll(&mut self) {
		let held_element = self.held_entry.element();
		if let Some(tab) = self.tabs.get_mut(self.tab) {
			let free_space = 48 + tab.left_margin(held_element);
			if let Some(selected_text) = tab.selected_text.as_ref() {
				let left_margin = tab.left_margin(held_element);
				let horizontal_scroll = tab.horizontal_scroll(held_element);
				let pos = left_margin + selected_text.indices.len() * 16 + 32 + 4 + selected_text.prefix.width() + selected_text.keyfix.as_deref().map_or(0, StrExt::width) + selected_text.value.split_at(selected_text.cursor).0.width();
				if pos + free_space >= self.window_width + horizontal_scroll {
					tab.horizontal_scroll = pos + free_space - self.window_width;
					tab.horizontal_scroll = tab.horizontal_scroll(held_element);
				} else if pos < horizontal_scroll + free_space {
					tab.horizontal_scroll = pos.saturating_sub(free_space);
					tab.horizontal_scroll = tab.horizontal_scroll(held_element);
				}
			}
		}
	}

	#[cfg_attr(not(debug_assertions), inline)]
	#[allow(
		clippy::collapsible_if,
		clippy::too_many_lines,
		clippy::cognitive_complexity
	)]
	pub fn on_key_input(&mut self, key: KeyboardInput, f: impl Fn(&str)) -> bool {
		if key.state == ElementState::Pressed {
			if let Some(key) = key.virtual_keycode {
				self.held_keys.insert(key);
				let char = self.char_from_key(key);
				let flags = (self.held_keys.contains(&VirtualKeyCode::LControl) as u8 | self.held_keys.contains(&VirtualKeyCode::RControl) as u8) | ((self.held_keys.contains(&VirtualKeyCode::LShift) as u8 | self.held_keys.contains(&VirtualKeyCode::RShift) as u8) << 1) | ((self.held_keys.contains(&VirtualKeyCode::LAlt) as u8 | self.held_keys.contains(&VirtualKeyCode::RAlt) as u8) << 2);
				if let Some(tab) = self.tab_mut()
					&& let Some(selected_text) = &mut tab.selected_text
				{
					match selected_text.on_key_press(key, char, flags) {
						KeyResult::NothingSpecial => {
							selected_text.handle_history();
							self.cache_cursor_x = None;
							self.refresh_selected_text_horizontal_scroll();
							return true;
						}
						KeyResult::Revert => {
							tab.selected_text = None;
							self.cache_cursor_x = None;
							return true;
						}
						KeyResult::Finish => {
							// we just won't let you leave if you didn't fix it ;)
							let _ = tab.close_selected_text(true);
							self.cache_cursor_x = None;
							return true;
						}
						KeyResult::Keyfix => {
							self.keyfix();
							self.refresh_selected_text_horizontal_scroll();
							self.cache_cursor_x = None;
							return true;
						}
						KeyResult::Valuefix => {
							self.valuefix();
							self.refresh_selected_text_horizontal_scroll();
							self.cache_cursor_x = None;
							return true;
						}
						KeyResult::Up(ctrl) => {
							unsafe {
								self.selected_text_up(ctrl);
							}
							self.refresh_selected_text_horizontal_scroll();
							return true;
						}
						KeyResult::Down(ctrl) => {
							unsafe {
								self.selected_text_down(ctrl);
							}
							self.refresh_selected_text_horizontal_scroll();
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
				if key == VirtualKeyCode::Equals && flags == flags!(Ctrl) {
					self.set_scale(unsafe {
						[
							self.scale + 1,
							6,
							self.raw_window_width / MIN_WINDOW_WIDTH,
							self.raw_window_height / MIN_WINDOW_HEIGHT,
						]
						.into_iter()
						.min()
						.panic_unchecked("at least one element exists")
					});
					return true;
				}
				if key == VirtualKeyCode::Minus && flags == flags!(Ctrl) {
					self.set_scale(usize::max(1, self.scale - 1));
					return true;
				}
				if self.action_wheel.is_some() && key == VirtualKeyCode::Escape && flags == flags!() {
					self.action_wheel = None;
					return true;
				}
				if !self.held_entry.is_empty() && key == VirtualKeyCode::Escape && flags == flags!() {
					self.held_entry = HeldEntry::Empty;
					return true;
				}
				{
					if key == VirtualKeyCode::Key1 {
						self.set_tab(0, f);
						return true;
					}
					if key == VirtualKeyCode::Key2 {
						self.set_tab(1, f);
						return true;
					}
					if key == VirtualKeyCode::Key3 {
						self.set_tab(2, f);
						return true;
					}
					if key == VirtualKeyCode::Key4 {
						self.set_tab(3, f);
						return true;
					}
					if key == VirtualKeyCode::Key5 {
						self.set_tab(4, f);
						return true;
					}
					if key == VirtualKeyCode::Key6 {
						self.set_tab(5, f);
						return true;
					}
					if key == VirtualKeyCode::Key7 {
						self.set_tab(6, f);
						return true;
					}
					if key == VirtualKeyCode::Key8 {
						self.set_tab(7, f);
						return true;
					}
					if key == VirtualKeyCode::Key9 {
						self.set_tab(self.tabs.len().saturating_sub(1), f);
						return true;
					}
				}
				'a: {
					if key == VirtualKeyCode::R && flags == flags!(Ctrl) {
						let Some(tab) = self.tabs.get_mut(self.tab) else {
							break 'a;
						};
						if tab.history_changed {
							break 'a;
						};
						let Some(path) = &tab.path else { break 'a };
						let Ok(buf) = read(path) else { break 'a };
						let (nbt, compression) = {
							if path.extension().and_then(OsStr::to_str) == Some("mca") {
								(NbtElement::from_mca(buf.as_slice()), FileFormat::Mca)
							} else if buf.first_chunk::<2>().copied().map(u16::from_be_bytes) == Some(0x1F8B) {
								(
									DeflateDecoder::new(buf.as_slice())
										.decode_gzip()
										.ok()
										.and_then(|x| NbtElement::from_file(&x)),
									FileFormat::Zlib,
								)
							} else if let Some(0x7801 | 0x789C | 0x78DA) = buf.first_chunk::<2>().copied().map(u16::from_be_bytes) {
								(
									DeflateDecoder::new(buf.as_slice())
										.decode_zlib()
										.ok()
										.and_then(|x| NbtElement::from_file(&x)),
									FileFormat::Zlib,
								)
							} else if let Some(nbt) = NbtElement::from_file(&buf) {
								(Some(nbt), FileFormat::Nbt)
							} else {
								let nbt = core::str::from_utf8(&buf)
									.ok()
									.and_then(NbtElement::from_str)
									.map(|x| x.1);
								if let Some(NbtCompound::ID | NbtRegion::ID) = nbt.as_ref().map(NbtElement::id) {
									(nbt, FileFormat::Snbt)
								} else {
									break 'a;
								}
							}
						};
						let Some(nbt) = nbt else { break 'a };
						let old = core::mem::replace(&mut tab.value, Box::new(nbt));
						std::thread::spawn(move || drop(old));
						tab.compression = compression;
						tab.bookmarks = vec![];
						tab.history_changed = false;
						tab.undos = LinkedQueue::new();
						tab.redos = LinkedQueue::new();
						tab.scroll = tab.scroll();
						tab.horizontal_scroll = tab.horizontal_scroll(self.held_entry.element());
					}
				}
				if key == VirtualKeyCode::F && flags == flags!(Alt) {
					if let Some(tab) = self.tab_mut() {
						tab.freehand_mode = !tab.freehand_mode;
						return true;
					}
				}
				if key == VirtualKeyCode::N && flags == flags!(Ctrl) {
					if let Some(tab) = self.tab_mut() {
						tab.selected_text = None;
					}
					self.new_tab(f);
					return true;
				}
				if key == VirtualKeyCode::S && flags == flags!(Ctrl) {
					if let Some(tab) = self.tab_mut() {
						if tab.save() {
							tab.selected_text = None;
							return true;
						}
					}
				}
				if key == VirtualKeyCode::W && flags == flags!(Ctrl) {
					if let Some(tab) = self.tab()
						&& !tab.history_changed
					{
						let t = self.tabs.remove(self.tab);
						std::thread::spawn(move || drop(t));
						self.set_tab(self.tab.saturating_sub(1), f);
						return true;
					}
				}
				if key == VirtualKeyCode::Z && flags == flags!(Ctrl) {
					if let Some(tab) = self.tabs.get_mut(self.tab) {
						if let Some(action) = tab.undos.pop() {
							tab.redos.push(action.undo(
								&mut tab.value,
								&mut tab.bookmarks,
								&mut self.subscription,
								&mut tab.path,
								&mut tab.name,
							));
							tab.selected_text = None;
							return true;
						}
					}
				}
				if key == VirtualKeyCode::Y && flags == flags!(Ctrl) {
					if let Some(tab) = self.tabs.get_mut(self.tab) {
						if let Some(action) = tab.redos.pop() {
							tab.undos.push(action.undo(
								&mut tab.value,
								&mut tab.bookmarks,
								&mut self.subscription,
								&mut tab.path,
								&mut tab.name,
							));
							tab.selected_text = None;
							return true;
						}
					}
				}
				if ((key == VirtualKeyCode::Back || key == VirtualKeyCode::Delete) && flags == flags!()) || (key == VirtualKeyCode::X && flags == flags!(Ctrl)) {
					if self.delete(flags & flags!(Ctrl) > 0) {
						if let Some(tab) = self.tab_mut() {
							tab.selected_text = None;
						}
						return true;
					}
				}
				if key == VirtualKeyCode::D && flags == flags!(Ctrl) {
					if self.duplicate() {
						if let Some(tab) = self.tab_mut() {
							tab.selected_text = None;
						}
						return true;
					}
				}
				if key == VirtualKeyCode::C && flags == flags!(Ctrl) {
					if self.copy(false) {
						if let Some(tab) = self.tab_mut() {
							tab.selected_text = None;
						}
						return true;
					}
				}
				if key == VirtualKeyCode::C && flags == flags!(Ctrl + Shift) {
					if self.copy(true) {
						if let Some(tab) = self.tab_mut() {
							tab.selected_text = None;
						}
						return true;
					}
				}
				if let Some(tab) = self.tab()
					&& key == VirtualKeyCode::V
					&& flags == flags!(Ctrl)
					&& let Some(element) = cli_clipboard::get_contents()
						.ok()
						.and_then(|x| NbtElement::from_str(&x))
					&& (element.1.id() != NbtChunk::ID || tab.value.id() == NbtRegion::ID)
				{
					let old_held_entry = core::mem::replace(&mut self.held_entry, HeldEntry::FromAether(element));
					let left_margin = self.left_margin();
					let HeldEntry::FromAether(pair) = core::mem::replace(&mut self.held_entry, old_held_entry) else {
						unsafe { panic_unchecked("we just set it you") }
					};
					if self.drop(pair, None, left_margin) { return true }
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
		self.raw_mouse_x = pos.x as usize;
		self.raw_mouse_y = pos.y as usize;
		self.mouse_x = self.raw_mouse_x / self.scale;
		self.mouse_y = self.raw_mouse_y / self.scale;
		let mouse_y = self.mouse_y;
		if let Some(scrollbar_offset) = self.scrollbar_offset
			&& let Some(tab) = self.tab_mut()
			&& mouse_y >= HEADER_SIZE
		{
			let mouse_y = mouse_y - HEADER_SIZE;
			let height = tab.value.height() * 16 + 32 + 15;
			let total = tab.window_height - HEADER_SIZE;
			let start = total * tab.scroll() / height;
			let scrollbar_point = start + scrollbar_offset;
			let dy = mouse_y as isize - scrollbar_point as isize;
			let pixel_delta = height as isize * dy / total as isize;
			tab.scroll = (tab.scroll as isize + pixel_delta).max(0) as usize;
			tab.scroll = tab.scroll();
		}
		true
	}

	#[inline]
	pub fn window_dims(&mut self, window_width: usize, window_height: usize) {
		self.raw_window_width = window_width;
		self.raw_window_height = window_height;
		self.set_scale(usize::min(self.scale, usize::min(self.raw_window_width / MIN_WINDOW_WIDTH, self.raw_window_height / MIN_WINDOW_HEIGHT)));
	}

	#[inline]
	fn set_scale(&mut self, scale: usize) {
		self.scale = scale;
		self.mouse_x = self.raw_mouse_x / self.scale;
		self.mouse_y = self.raw_mouse_y / self.scale;
		self.window_width = self.raw_window_width / self.scale;
		self.window_height = self.raw_window_height / self.scale;
		for tab in &mut self.tabs {
			tab.window_width = self.window_width;
			tab.window_height = self.window_height;
		}
	}

	#[inline]
	#[must_use]
	pub fn scroll(&self) -> usize { self.tab().map_or(0, Tab::scroll) }

	#[inline]
	#[must_use]
	pub fn horizontal_scroll(&self) -> usize {
		self.tab()
			.map_or(0, |tab| tab.horizontal_scroll(self.held_entry.element()))
	}

	#[inline]
	fn tab(&self) -> Option<&Tab> { self.tabs.get(self.tab) }

	#[inline]
	fn tab_mut(&mut self) -> Option<&mut Tab> { self.tabs.get_mut(self.tab) }

	#[inline]
	fn set_tab(&mut self, idx: usize, f: impl Fn(&str)) {
		self.tab = idx.min(self.tabs.len().saturating_sub(1));
		if let Some(tab) = self.tab() {
			f(format!("{} - NBT Workbench", tab.name).as_str());
		}
	}

	#[inline]
	pub fn render(&mut self, builder: &mut VertexBufferBuilder) {
		for n in 0..(builder.window_height() - HEADER_SIZE + 15) / 16 {
			let uv = if n % 2 == 0 {
				DARK_STRIPE_UV + (1, 1)
			} else {
				LIGHT_STRIPE_UV + (1, 1)
			};
			builder.draw_texture_region_z(
				(0, n * 16 + HEADER_SIZE + (n > 0) as usize),
				BASE_Z,
				uv,
				(builder.window_width(), 16 + (n == 0) as usize),
				(14, 14),
			);
		}
		self.render_tabs(builder);
		let Some(tab) = self.tabs.get(self.tab) else {
			return;
		};
		let left_margin = self.left_margin();
		let horizontal_scroll = tab.horizontal_scroll;
		let ghost = if self.mouse_x + horizontal_scroll >= left_margin && self.mouse_y >= HEADER_SIZE {
			self.held_entry.element().map(|x| {
				(
					x.id(),
					((self.mouse_x + horizontal_scroll - left_margin) & !0b1111) + left_margin,
					((self.mouse_y - HEADER_SIZE) & !0b0111) + HEADER_SIZE,
					x.true_height(),
				)
			})
		} else {
			None
		};
		let selected_y = tab.selected_text
			.as_ref()
			.map(|x| x.y)
			.and_then(|x| x.checked_sub(builder.scroll()))
			.unwrap_or(0);
		let (selected_key, selected_value, selecting_key) = if let Some(selected) = tab.selected_text.as_ref() && selected.editable {
			if selected.keyfix.is_some() { // Health: __20.0__
				(selected.keyfix.clone().map(String::into_boxed_str), Some(selected.value.clone().into_boxed_str()), false)
			} else if selected.valuefix.is_some() { // __Health__: 20.0
				(Some(selected.value.clone().into_boxed_str()), selected.valuefix.clone().map(String::into_boxed_str), true)
			} else if !selected.suffix.is_empty() { // __Banana__: 4 entries
				(Some(selected.value.clone().into_boxed_str()), None, false)
			} else { // __20.0__
				(None, Some(selected.value.clone().into_boxed_str()), false)
			}
		} else {
			(None, None, false)
		};
		let mut ctx = RenderContext::new(selected_y, selected_key, selected_value, selecting_key, ghost, left_margin, (self.mouse_x, self.mouse_y));
		tab.render(
			builder,
			&mut ctx,
			self.scrollbar_offset.is_some(),
			self.held_entry.element(),
		);
		if let Some(selected_text) = &tab.selected_text {
			builder.horizontal_scroll = horizontal_scroll;
			selected_text.render(builder, left_margin);
			builder.horizontal_scroll = 0;
		}
		if self.mouse_y >= HEADER_SIZE && self.action_wheel.is_none() {
			builder.draw_texture_region_z(
				(0, self.mouse_y & !0b1111),
				BASE_Z,
				HOVERED_STRIPE_UV,
				(builder.window_width(), 16),
				(14, 14),
			);
		}
		self.render_action_wheel(builder);
		self.render_held_entry(builder);
		self.render_alerts(builder);
	}

	#[inline]
	pub fn tick(&mut self) {
		if (!self.held_entry.is_empty() || self.tab().is_some_and(|tab| tab.freehand_mode)) && self.action_wheel.is_none() && self.scrollbar_offset.is_none() {
			self.try_mouse_scroll();
		}
	}

	#[inline]
	fn render_held_entry(&self, builder: &mut VertexBufferBuilder) {
		if let Some(element) = self.held_entry.element() {
			NbtElement::render_icon(
				element.id(),
				(
					self.mouse_x.saturating_sub(8),
					self.mouse_y.saturating_sub(8),
				),
				HELD_ENTRY_Z,
				builder,
			);
		}
	}

	#[inline]
	fn render_alerts(&mut self, builder: &mut VertexBufferBuilder) {
		let mut idx = 0;
		while let Some(alert) = self.alerts.get_mut(idx) {
			if alert.is_invisible() {
				self.alerts.remove(idx);
			} else {
				alert.render(builder, idx);
				idx += 1;
			}
		}
	}

	#[inline]
	fn render_tabs(&self, builder: &mut VertexBufferBuilder) {
		let mut offset = 3;
		builder.horizontal_scroll = self.tab_scroll;
		for (idx, tab) in self.tabs.iter().enumerate() {
			let remaining_width = tab.name.width() + 48 + 3;
			let uv = if idx == self.tab {
				SELECTED_WIDGET_UV
			} else if (offset..offset + 3 + remaining_width).contains(&self.mouse_x) && (3..=19).contains(&self.mouse_y) {
				HOVERED_WIDGET_UV
			} else {
				UNSELECTED_WIDGET_UV
			};
			builder.draw_texture((offset, 3), uv, (3, 16));
			if (offset..offset + 16).contains(&self.mouse_x) && (3..19).contains(&self.mouse_y) {
				builder.draw_tooltip(&[tab.value.display_name()], (self.mouse_x, self.mouse_y));
			}
			offset += 2;
			tab.draw_icon(builder, (offset, 3), JUST_OVERLAPPING_BASE_TEXT_Z);
			offset += 1;
			builder.draw_texture_region_z(
				(offset, 3),
				BASE_Z,
				uv + (3, 0),
				(remaining_width, 16),
				(10, 16),
			);
			builder.settings((offset + 16, 3), false, BASE_TEXT_Z);
			let _ = write!(builder, "{}", tab.name);
			offset += remaining_width;
			builder.draw_texture((offset, 3), uv + (13, 0), (3, 16));
			builder.draw_texture(
				(offset - 32, 3),
				if tab.history_changed {
					EDITED_UV
				} else {
					UNEDITED_UV
				},
				(16, 16),
			);
			builder.draw_texture((offset - 16, 3), tab.compression.uv(), (16, 16));
			if (offset - 16..offset).contains(&self.mouse_x) && (3..19).contains(&self.mouse_y) {
				builder.draw_tooltip(&[tab.compression.into_str()], (self.mouse_x, self.mouse_y));
			}
			offset += 6;
		}
		builder.horizontal_scroll = 0;
		builder.draw_texture_region_z(
			(0, 21),
			BASE_Z,
			HORIZONTAL_SEPARATOR_UV,
			(builder.window_width(), 2),
			(14, 2),
		);
		builder.draw_texture_region_z(
			(0, 45),
			BASE_Z,
			HORIZONTAL_SEPARATOR_UV,
			(builder.window_width(), 2),
			(14, 2),
		);
	}

	#[inline]
	fn render_action_wheel(&mut self, builder: &mut VertexBufferBuilder) {
		let Some((cx, cy)) = self.action_wheel else {
			return;
		};
		let cx = cx.saturating_sub(31) + 31;
		let cy = cy.saturating_sub(31) + 31;
		let left_margin = self.left_margin();
		let Some(tab) = self.tabs.get_mut(self.tab) else {
			return;
		};
		let highlight_idx = (((cy as f64 - self.mouse_y as f64).atan2(cx as f64 - self.mouse_x as f64) + core::f64::consts::FRAC_PI_8 - core::f64::consts::FRAC_PI_2).rem_euclid(core::f64::consts::TAU) * core::f64::consts::FRAC_2_PI * 2.0) as usize;
		let squared_distance_from_origin = (cy as isize - self.mouse_y as isize).pow(2) + (cx as isize - self.mouse_x as isize).pow(2);
		if cy >= HEADER_SIZE {
			if cy > tab.value.height() * 16 + HEADER_SIZE { return };
			let scroll = tab.scroll();
			let (depth, (_, _, element, _)) = Traverse::new((cy - HEADER_SIZE) / 16 + scroll / 16, &mut tab.value)
				.enumerate()
				.last();
			let min_x = depth * 16 + left_margin;
			let max_x = min_x + 32;
			if !(min_x..max_x).contains(&cx) { return };
			builder.draw_texture_z((cx - 31, cy - 31), ACTION_WHEEL_Z, TRAY_UV, (64, 64));
			for (n, &action) in element.actions().iter().enumerate() {
				let (x, y) = [
					Vec2u::new(-9_isize as usize, -29_isize as usize),
					Vec2u::new(9, -26_isize as usize),
					Vec2u::new(11, -9_isize as usize),
					Vec2u::new(9, 9),
					Vec2u::new(-9_isize as usize, 11),
					Vec2u::new(-26_isize as usize, 9),
					Vec2u::new(-29_isize as usize, -9_isize as usize),
					Vec2u::new(-26_isize as usize, -26_isize as usize),
				][n]
					.into();
				let mut hovered = false;
				let uv = if squared_distance_from_origin > 8_isize.pow(2) && highlight_idx == n {
					hovered = true;
					SELECTED_ACTION_WHEEL[n]
				} else {
					UNSELECTED_ACTION_WHEEL[n]
				};
				let offset = [
					Vec2u::new(5, 3),
					Vec2u::new(5, 4),
					Vec2u::new(6, 5),
					Vec2u::new(5, 5),
					Vec2u::new(4, 6),
					Vec2u::new(4, 5),
					Vec2u::new(3, 5),
					Vec2u::new(4, 4),
				][n];
				let dims = if n % 2 == 0 {
					Vec2u::new(20, 20)
				} else {
					Vec2u::new(19, 19)
				};
				builder.draw_texture_z(
					(cx.wrapping_add(x), cy.wrapping_add(y)),
					ACTION_WHEEL_Z,
					uv,
					dims,
				);
				action.render(
					builder,
					(
						cx.wrapping_add(x).wrapping_add(offset.x),
						cy.wrapping_add(y).wrapping_add(offset.y),
					),
					hovered,
				);
			}
		} else {
			// features, idk
		}
	}

	pub fn try_mouse_scroll(&mut self) {
		if let Some(tab) = self.tabs.get_mut(self.tab) {
			if self.mouse_x >= self.window_width - 16 {
				tab.horizontal_scroll += 16;
				tab.horizontal_scroll = tab.horizontal_scroll(self.held_entry.element());
			} else if self.mouse_x < 16 {
				tab.horizontal_scroll = tab.horizontal_scroll.saturating_sub(16);
				tab.horizontal_scroll = tab.horizontal_scroll(self.held_entry.element());
			}

			if self.mouse_y < HEADER_SIZE + 16 {
				tab.scroll = tab.scroll.saturating_sub(16);
				tab.scroll = tab.scroll();
			} else if self.mouse_y
				>= usize::min(
					self.window_height - 16,
					tab.value.height() * 16 + HEADER_SIZE,
				) {
				tab.scroll += 16;
				tab.scroll = tab.scroll();
			}
		}
	}

	#[inline]
	#[allow(
		clippy::cognitive_complexity,
		clippy::match_same_arms,
		clippy::too_many_lines
	)]
	fn char_from_key(&self, key: VirtualKeyCode) -> Option<char> {
		if self.held_keys.contains(&VirtualKeyCode::LControl) || self.held_keys.contains(&VirtualKeyCode::RControl) { return None }
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
