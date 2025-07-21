use std::hint::{likely, unlikely};
use std::ops::{Deref, DerefMut};
use std::time::Duration;

use winit::keyboard::KeyCode;

use crate::flags;
use crate::history::WorkbenchAction;
use crate::render::assets::{SELECTION_UV, ZOffset};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::text::KeyResult::{Escape, Finish, GenericAction, NoAction};
use crate::util::{CharExt, LinkedQueue, StrExt, Timestamp, Vec2u, get_clipboard, is_jump_char_boundary, is_utf8_char_boundary, set_clipboard};

pub const TEXT_DOUBLE_CLICK_INTERVAL: Duration = Duration::from_millis(250);
pub const CURSOR_BLINK_RATE: Duration = Duration::from_millis(500);

pub enum SelectedTextKeyResult {
	NoAction,
	GenericAction,
	Escape,
	Finish,
	Action(Option<WorkbenchAction>),
}

#[derive(PartialEq, Eq)]
pub enum SearchBoxKeyResult {
	NoAction,
	GenericAction,
	Escape,
	MoveToReplaceBox,
	Search,
	SearchCountOnly,
}

#[derive(PartialEq, Eq)]
pub enum ReplaceBoxKeyResult {
	NoAction,
	GenericAction,
	Escape,
	MoveToSearchBox,
	ReplaceAll,
}

#[derive(PartialEq, Eq)]
pub enum KeyResult {
	NoAction,
	GenericAction,
	Escape,
	Finish,
}

impl From<KeyResult> for SelectedTextKeyResult {
	fn from(value: KeyResult) -> Self {
		match value {
			NoAction => Self::NoAction,
			GenericAction => Self::GenericAction,
			Escape => Self::Escape,
			Finish => Self::Finish,
		}
	}
}

impl From<KeyResult> for SearchBoxKeyResult {
	fn from(value: KeyResult) -> Self {
		match value {
			NoAction => Self::NoAction,
			GenericAction => Self::GenericAction,
			Escape => Self::Escape,
			Finish => Self::Search,
		}
	}
}

impl From<KeyResult> for ReplaceBoxKeyResult {
	fn from(value: KeyResult) -> Self {
		match value {
			NoAction => Self::NoAction,
			GenericAction => Self::GenericAction,
			Escape => Self::Escape,
			Finish => Self::ReplaceAll,
		}
	}
}

pub trait Cachelike<Additional: Clone>: PartialEq + Clone {
	fn new(text: &Text<Additional, Self>) -> Self
	where Self: Sized;

	fn revert(self, text: &mut Text<Additional, Self>)
	where Self: Sized;
}

pub fn get_cursor_left_jump_idx(mut cursor: usize, bytes: &[u8]) -> usize {
	if cursor > 0 {
		cursor -= 1;
		if bytes[cursor].is_ascii_whitespace() {
			while cursor > 0 {
				let byte = bytes[cursor];
				if byte.is_ascii_whitespace() {
					cursor -= 1;
				} else {
					break;
				}
			}
		}

		let last_byte = bytes[cursor];
		while cursor > 0 {
			let byte = bytes[cursor];
			if is_jump_char_boundary(byte) != is_jump_char_boundary(last_byte) {
				cursor += 1;
				while cursor < bytes.len() && !is_utf8_char_boundary(bytes[cursor]) {
					cursor += 1;
				}
				break;
			}
			cursor -= 1;
			while !is_utf8_char_boundary(bytes[cursor]) {
				cursor -= 1;
			}
		}
	}
	cursor
}

pub fn get_cursor_right_jump_idx(mut cursor: usize, bytes: &[u8]) -> usize {
	if cursor < bytes.len() {
		if bytes[cursor].is_ascii_whitespace() {
			while cursor < bytes.len() {
				let byte = bytes[cursor];
				if byte.is_ascii_whitespace() {
					cursor += 1;
				} else {
					break;
				}
			}
		}

		let last_byte = bytes[cursor];
		while cursor < bytes.len() {
			let byte = bytes[cursor];
			if is_jump_char_boundary(byte) != is_jump_char_boundary(last_byte) {
				break;
			}
			cursor += 1;
			while cursor < bytes.len() && !is_utf8_char_boundary(bytes[cursor]) {
				cursor += 1;
			}
		}
	}
	cursor
}

#[derive(Clone)]
pub struct Text<Additional: Clone, Cache: Cachelike<Additional>> {
	pub value: String,
	pub cursor: usize,
	pub selection: Option<usize>,
	pub editable: bool,
	pub additional: Additional,
	drag_selectable: bool,
	last_interaction: Timestamp,
	undos: LinkedQueue<Cache>,
	redos: LinkedQueue<Cache>,
}

impl<Additional: Clone, Cache: Cachelike<Additional>> Deref for Text<Additional, Cache> {
	type Target = Additional;

	fn deref(&self) -> &Self::Target { &self.additional }
}

impl<Additional: Clone, Cache: Cachelike<Additional>> DerefMut for Text<Additional, Cache> {
	fn deref_mut(&mut self) -> &mut Self::Target { &mut self.additional }
}

impl<Additional: Clone, Cache: Cachelike<Additional>> Text<Additional, Cache> {
	pub fn new(value: String, cursor: usize, editable: bool, additional: Additional) -> Self {
		let mut this = Self {
			value,
			cursor,
			selection: None,
			last_interaction: Timestamp::now(),
			editable,
			undos: LinkedQueue::new(),
			redos: LinkedQueue::new(),
			additional,
			drag_selectable: true,
		};
		this.save_state_in_history();
		this
	}

	pub const fn uninit() -> Self {
		Self {
			value: String::new(),
			cursor: 0,
			selection: None,
			last_interaction: Timestamp::UNIX_EPOCH,
			editable: true,
			undos: LinkedQueue::new(),
			redos: LinkedQueue::new(),
			additional: unsafe { core::mem::MaybeUninit::zeroed().assume_init() },
			drag_selectable: true,
		}
	}

	#[must_use]
	pub fn is_drag_selectable(&self) -> bool { self.drag_selectable }

	pub fn set_drag_selectable(&mut self, drag_selectable: bool) { self.drag_selectable = drag_selectable; }

	pub fn interact(&mut self) { self.last_interaction = Timestamp::now(); }

	#[must_use]
	pub fn on_key_press(&mut self, key: KeyCode, mut char: Option<char>, flags: u8) -> KeyResult {
		if key == KeyCode::Escape && flags == flags!() {
			return Escape
		}

		if let KeyCode::Enter | KeyCode::NumpadEnter = key
			&& flags == flags!()
		{
			return Finish;
		}

		if key == KeyCode::KeyZ && flags == flags!(Ctrl) && self.editable {
			let current = Cache::new(&self);

			let cache = 'a: {
				while let Some(cache) = self.undos.pop() {
					if unlikely(cache.ne(&current)) {
						self.undos.push(cache.clone());
						break 'a cache;
					}
				}
				return NoAction;
			};

			self.redos.push(current);
			self.last_interaction = Timestamp::UNIX_EPOCH;
			cache.revert(self);
			return GenericAction;
		}

		if (key == KeyCode::KeyY && flags == flags!(Ctrl) || key == KeyCode::KeyZ && flags == flags!(Ctrl + Shift)) && self.editable {
			let current = Cache::new(&self);

			let cache = 'a: {
				while let Some(cache) = self.redos.pop() {
					if likely(cache.ne(&current)) {
						self.redos.push(cache.clone());
						break 'a cache;
					}
				}
				return NoAction;
			};

			self.undos.push(current);
			self.last_interaction = Timestamp::UNIX_EPOCH;
			cache.revert(self);
			return GenericAction;
		}

		if key == KeyCode::KeyA && flags == flags!(Ctrl) && self.editable && !self.value.is_empty() {
			self.cursor = 0;
			self.selection = Some(self.value.len());
			return GenericAction;
		}

		if let Some(selection) = self.selection
			&& flags == flags!()
		{
			if let KeyCode::Backspace | KeyCode::Delete = key
				&& self.editable
			{
				let (low_selection, high_selection) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
				let (left, right) = self.value.split_at(low_selection);
				let (_, right) = right.split_at(high_selection - low_selection);
				self.value = format!("{left}{right}");
				self.selection = None;
				self.cursor = low_selection;
				return GenericAction;
			}
		}

		if key == KeyCode::KeyX && flags == flags!(Ctrl) && self.editable {
			if let Some(selection) = self.selection {
				let (start, end) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
				let (low, right) = self.value.split_at(start);
				let (cut, high) = right.split_at(end - start);
				if set_clipboard(cut.to_owned()) {
					self.value = format!("{low}{high}");
					self.selection = None;
				}
				self.cursor = start;
				return GenericAction;
			}
		}

		if key == KeyCode::KeyC && flags == flags!(Ctrl) && self.editable {
			if let Some(selection) = self.selection {
				let (start, end) = if self.cursor < selection { (self.cursor, selection) } else { (selection, self.cursor) };
				let (_, right) = self.value.split_at(start);
				let (cut, _) = right.split_at(end - start);
				set_clipboard(cut.to_owned());
				return GenericAction;
			}
		}

		if key == KeyCode::KeyV && flags == flags!(Ctrl) && self.editable {
			if let Some(clipboard) = get_clipboard() {
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
				return GenericAction;
			}
		}

		if key == KeyCode::Home && flags & !flags!(Shift) == 0 && self.editable {
			if flags == flags!(Shift) {
				let new = self.selection.map_or(self.cursor, |x| x.min(self.cursor));
				self.selection = (new != 0).then_some(new);
			} else {
				self.selection = None;
			}
			self.cursor = 0;
			return GenericAction;
		}

		if key == KeyCode::End && flags & !flags!(Shift) == 0 && self.editable {
			if flags == flags!(Shift) {
				let new = self.selection.map_or(self.cursor, |x| x.max(self.cursor));
				self.selection = (new != self.value.len()).then_some(new);
			} else {
				self.selection = None;
			}
			self.cursor = self.value.len();
			return GenericAction;
		}

		if key == KeyCode::Backspace && flags < 2 && self.editable {
			let (left, right) = self.value.split_at(self.cursor);
			if flags & flags!(Ctrl) > 0 {
				if !left.is_empty() {
					let new = get_cursor_left_jump_idx(self.cursor, self.value.as_bytes());
					let (left, _) = left.split_at(new);
					self.value = format!("{left}{right}");
					self.cursor = new;
				}
			} else {
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

			return GenericAction;
		}

		if key == KeyCode::Delete && self.editable {
			let (left, right) = self.value.split_at(self.cursor);
			if flags & flags!(Ctrl) > 0 {
				if !right.is_empty() {
					let new = get_cursor_right_jump_idx(self.cursor, self.value.as_bytes());
					let (_, right) = right.split_at(new);
					self.value = format!("{left}{right}");
					self.cursor = new;
				}
			} else {
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
			return GenericAction;
		}

		if key == KeyCode::ArrowLeft {
			if self.editable {
				if flags & flags!(Shift) == 0
					&& let Some(selection) = self.selection.take()
				{
					self.cursor = selection.min(self.cursor);
					return GenericAction;
				}

				let mut new = self.cursor;
				if flags & flags!(Ctrl) > 0 {
					new = get_cursor_left_jump_idx(self.cursor, self.value.as_bytes());
				} else {
					if new > 0 {
						new -= 1;
						while new > 0 {
							if is_utf8_char_boundary(self.value.as_bytes()[new]) {
								break;
							}

							new -= 1;
						}
					}
				}

				if flags & flags!(Shift) > 0 {
					if self.selection.is_none() {
						self.selection = Some(self.cursor);
					}
				} else {
					self.selection = None;
				}

				self.cursor = new;

				if self.selection.is_some_and(|x| x == self.cursor) {
					self.selection = None;
				}
			}
			return GenericAction;
		}

		if key == KeyCode::ArrowRight {
			if self.editable {
				if flags & flags!(Shift) == 0
					&& let Some(selection) = self.selection.take()
				{
					self.cursor = selection.max(self.cursor);
					return GenericAction;
				}

				let mut new = self.cursor;
				if flags & flags!(Ctrl) > 0 {
					new = get_cursor_right_jump_idx(self.cursor, self.value.as_bytes());
				} else {
					if new < self.value.len() {
						new += 1;
						while new < self.value.len() {
							if is_utf8_char_boundary(self.value.as_bytes()[new]) {
								break;
							}

							new += 1;
						}
					}
				}

				if flags & flags!(Shift) > 0 {
					if self.selection.is_none() {
						self.selection = Some(self.cursor);
					}
				} else {
					self.selection = None;
				}
				self.cursor = new;

				if self.selection.is_some_and(|x| x == self.cursor) {
					self.selection = None;
				}
			}
			return GenericAction;
		}

		if let KeyCode::Enter | KeyCode::NumpadEnter = key
			&& flags == flags!(Shift)
			&& self.editable
		{
			char = Some('\n');
		}

		if let Some(char) = char
			&& self.editable
		{
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

			return GenericAction;
		}

		NoAction
	}

	pub fn render(&self, builder: &mut VertexBufferBuilder, color: TextColor, pos: Vec2u, z: ZOffset, selection_z: ZOffset) {
		use std::fmt::Write as _;

		let (x, y) = pos.into();

		builder.settings((x, y), false, z);

		builder.color = color.to_raw();
		let _ = write!(builder, "{}", self.value);

		if self.editable {
			let cursor_prefixing = self.value.split_at(self.cursor).0;
			let time_since_last_interaction = self.last_interaction.elapsed();
			if let Some(selection) = self.selection
				&& self.editable
			{
				let (start, end) = if self.cursor > selection { (selection, self.cursor) } else { (self.cursor, selection) };
				let start = self.value.split_at(start).0.width();
				let end = self.value.split_at(end).0.width();
				builder.draw_texture_region_z((start + x, y), selection_z, SELECTION_UV + (1, 1), (end - start - 1, 16), (14, 14));
				if time_since_last_interaction < CURSOR_BLINK_RATE || time_since_last_interaction.subsec_millis() < CURSOR_BLINK_RATE.subsec_micros() {
					builder.draw_texture_region_z((x + cursor_prefixing.width() - 1, y), selection_z, SELECTION_UV, (2, 16), (1, 16));
				}
			} else {
				if time_since_last_interaction < CURSOR_BLINK_RATE || time_since_last_interaction.subsec_millis() < CURSOR_BLINK_RATE.subsec_millis() {
					builder.draw_texture_region_z((x + cursor_prefixing.width(), y), selection_z, SELECTION_UV, (2, 16), (1, 16));
				}
			}
		}
	}

	pub fn save_state_in_history(&mut self) { self.undos.push(Cache::new(&self)); }

	pub fn post_input(&mut self) { self.cache(); }

	pub fn cache(&mut self) {
		let current = Cache::new(self);

		let should_cache = core::mem::replace(&mut self.last_interaction, Timestamp::now()).elapsed() >= Duration::from_millis(1_500);
		if should_cache && self.editable && self.undos.get().is_none_or(|x| x.ne(&current)) {
			if self.redos.pop().is_none_or(|x| x.ne(&current)) {
				self.redos = LinkedQueue::new();
			}

			self.save_state_in_history();
		}

		if self.undos.get().is_some_and(|x| x.eq(&current))
			&& let Some(undo) = self.undos.get_mut()
		{
			*undo = current;
		}
	}

	pub fn clear(&mut self) {
		self.value.clear();
		self.cursor = 0;
		self.redos.clear();
		self.undos.clear();
		self.selection = None;
		self.last_interaction = Timestamp::now();
	}
}

#[inline]
#[must_use]
pub fn get_cursor_idx(str: &str, mut x: isize) -> usize {
	for (i, char) in str.char_indices() {
		let width = char.width() as isize;
		if x <= width / 2 {
			return i
		}
		x -= width;
	}
	str.len()
}
