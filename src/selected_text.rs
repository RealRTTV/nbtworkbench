use std::convert::identity;
use std::intrinsics::{likely, unlikely};
use std::time::SystemTime;
use winit::event::VirtualKeyCode;
use crate::{flags, is_utf8_char_boundary, LinkedQueue, OptionExt, StrExt};
use crate::assets::HEADER_SIZE;
use crate::selected_text::KeyResult::*;
use crate::vertex_buffer_builder::VertexBufferBuilder;
use std::fmt::Write;

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
	pub y: usize,
	pub indices: Box<[usize]>,
	pub cursor: usize,
	pub value: String,
	pub selection: Option<usize>,
	pub keyfix: Option<String>,
	pub prefix: String,
	pub suffix: String,
	pub valuefix: Option<String>,
	pub editable: bool,
	pub last_interaction: SystemTime,
	pub undos: LinkedQueue<SelectedTextCache>,
	pub redos: LinkedQueue<SelectedTextCache>,
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

				if target_x + key_width < mouse_x + key.chars().last().and_then(|char| VertexBufferBuilder::CHAR_WIDTH.get(char as usize)).copied().unwrap_or(0) as usize && mouse_x < target_x + key_width + 7 {
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
						}.post_process(),
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

				if value_x + value_width < mouse_x + key.as_ref().and_then(|(x, _)| x.chars().last()).and_then(|char| VertexBufferBuilder::CHAR_WIDTH.get(char as usize)).copied().unwrap_or(0) as usize && mouse_x < value_x + value_width + 5 {
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
						}.post_process(),
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
		self.save_state_in_history();
		self
	}

	pub fn save_state_in_history(&mut self) {
		self.undos.push(SelectedTextCache {
			keyfix: self.keyfix.clone().map(String::into_boxed_str),
			value: self.value.clone().into_boxed_str(),
			valuefix: self.valuefix.clone().map(String::into_boxed_str),
			cursor: self.cursor,
			selection: self.selection,
		});
	}

	pub fn handle_history(&mut self) {
		let should_cache = core::mem::replace(&mut self.last_interaction, SystemTime::now())
			.elapsed()
			.map_err(|e| e.duration())
			.map_or_else(identity, identity)
			.as_millis() >= 1_500;
		if should_cache && self.editable && self.undos.get().is_none_or(|x| x.ne(self)) {
			if self.redos.pop().is_none_or(|x| x.ne(self)) {
				self.redos = LinkedQueue::new();
			}

			self.save_state_in_history();
		}
	}

	#[cfg_attr(not(debug_assertions), inline)]
	#[allow(clippy::cognitive_complexity, clippy::too_many_lines)] // i handled this fn well
	#[must_use]
	pub fn on_key_press(&mut self, key: VirtualKeyCode, mut char: Option<char>, flags: u8) -> KeyResult {
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