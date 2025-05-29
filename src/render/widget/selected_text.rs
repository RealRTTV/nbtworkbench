use std::fmt::Write;
use std::ops::{Deref, DerefMut};

use itertools::Itertools;
use uuid::Uuid;
use winit::keyboard::KeyCode;

use crate::assets::{BASE_TEXT_Z, HEADER_SIZE, SELECTED_TEXT_SELECTION_Z, SELECTED_TEXT_Z, SELECTION_UV};
use crate::elements::NbtElement;
use crate::render::{TextColor, VertexBufferBuilder, WindowProperties};
use crate::tree::{Indices, NavigationInformation, OwnedIndices, RenameElementResult, TraversalInformation, line_number_at, rename_element};
use crate::util::{CharExt, StrExt};
use crate::widget::SelectedTextKeyResult::{Down, ForceCloseElement, ForceOpenElement, MoveToKeyfix, MoveToValuefix, ShiftDown, ShiftUp, Up};
use crate::widget::{Cachelike, SelectedTextKeyResult, Text};
use crate::workbench::{PathWithName, WorkbenchAction};
use crate::flags;

#[derive(Clone, Debug)]
#[allow(clippy::module_name_repetitions)] // yeah no, it's better like this
pub struct SelectedTextCache {
	keyfix: Option<(Box<str>, TextColor)>,
	value: Box<str>,
	valuefix: Option<(Box<str>, TextColor)>,
	cursor: usize,
	selection: Option<usize>,
}

impl PartialEq for SelectedTextCache {
	fn eq(&self, other: &Self) -> bool { self.keyfix == other.keyfix && self.value == other.value && self.valuefix == other.valuefix }
}

impl Cachelike<SelectedTextAdditional> for SelectedTextCache {
	fn new(text: &Text<SelectedTextAdditional, Self>) -> Self
	where Self: Sized {
		Self {
			keyfix: text
				.additional
				.keyfix
				.clone()
				.map(|(a, b)| (a.into_boxed_str(), b)),
			valuefix: text
				.additional
				.valuefix
				.clone()
				.map(|(a, b)| (a.into_boxed_str(), b)),
			value: text.value.clone().into_boxed_str(),
			cursor: text.cursor,
			selection: text.selection,
		}
	}

	fn revert(self, text: &mut Text<SelectedTextAdditional, Self>)
	where Self: Sized {
		let Self { keyfix, value, valuefix, cursor, selection } = self;
		text.additional.keyfix = keyfix.map(|(a, b)| (a.into_string(), b));
		text.additional.valuefix = valuefix.map(|(a, b)| (a.into_string(), b));
		text.value = value.into_string();
		text.cursor = cursor;
		text.selection = selection;
	}
}

#[derive(Clone)]
pub struct SelectedText(pub Text<SelectedTextAdditional, SelectedTextCache>);

impl Deref for SelectedText {
	type Target = Text<SelectedTextAdditional, SelectedTextCache>;

	fn deref(&self) -> &Self::Target { &self.0 }
}

impl DerefMut for SelectedText {
	fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}

#[derive(Clone)]
pub struct SelectedTextAdditional {
	pub y: usize,
	pub indices: OwnedIndices,
	pub value_color: TextColor,
	pub keyfix: Option<(String, TextColor)>,
	pub prefix: (String, TextColor),
	pub suffix: (String, TextColor),
	pub valuefix: Option<(String, TextColor)>,
	pub cached_cursor_x: Option<usize>,
	pub uuid: Uuid,
}

impl SelectedText {
	// todo: refactor to use new text functions
	#[must_use]
	pub fn from_raw(target_x: usize, mouse_x: usize, y: usize, key: Option<(String, TextColor, bool)>, value: Option<(String, TextColor, bool)>, indices: OwnedIndices, cached_cursor_x: Option<usize>) -> Option<Self> {
		let key_width = if let Some((key, key_color, true)) = key.clone() {
			let key_width = key.width();

			if mouse_x + 4 >= target_x {
				let (suffix, valuefix) = if let Some((v, valuefix_color, b)) = value.clone() {
					if b {
						((": ".to_owned(), TextColor::TreeKey), Some((v, valuefix_color)))
					} else {
						((format!(": {v}"), TextColor::TreeKey), None)
					}
				} else {
					((String::new(), TextColor::White), None)
				};

				if mouse_x <= target_x {
					return Some(Self(Text::new(key, 0, true, SelectedTextAdditional {
						y,
						indices,
						value_color: key_color,
						keyfix: None,
						prefix: (String::new(), TextColor::White),
						suffix,
						valuefix,
						cached_cursor_x,
						uuid: Uuid::new_v4(),
					})));
				}

				if target_x + key_width
					< mouse_x
						+ key
							.chars()
							.last()
							.and_then(|char| VertexBufferBuilder::CHAR_WIDTH.get(char as usize))
							.copied()
							.unwrap_or(0) as usize
					&& mouse_x < target_x + key_width + 7
				{
					let key_len = key.len();
					return Some(Self(Text::new(key, key_len, true, SelectedTextAdditional {
						y,
						indices,
						value_color: key_color,
						keyfix: None,
						prefix: (String::new(), TextColor::White),
						suffix,
						valuefix,
						cached_cursor_x,
						uuid: Uuid::new_v4(),
					})));
				}

				let mut cursor = 0;
				let mut x = (mouse_x - target_x) as isize;
				let key_width = key_width as isize;

				for char in key.chars() {
					let width = VertexBufferBuilder::CHAR_WIDTH[char as usize] as isize;
					if x * 2 >= width {
						// algebra, to understand, divide both sides by two
						cursor += char.len_utf8();
						x -= width;
					} else if x < key_width {
						return Some(Self(Text::new(key, cursor, true, SelectedTextAdditional {
							y,
							indices,
							value_color: key_color,
							keyfix: None,
							prefix: (String::new(), TextColor::White),
							suffix,
							valuefix,
							cached_cursor_x,
							uuid: Uuid::new_v4(),
						})));
					}
				}
			}
			key_width + ": ".width()
		} else {
			0
		};

		if let Some((value, value_color, true)) = value.clone() {
			let value_x = target_x + key_width;
			if mouse_x + 4 >= value_x {
				let (keyfix, prefix) = if let Some((k, key_color, b)) = key.as_ref() {
					if *b {
						(Some((k.to_owned(), *key_color)), (": ".to_owned(), TextColor::TreeKey))
					} else {
						(None, (format!("{k}: "), TextColor::TreeKey))
					}
				} else {
					(None, (String::new(), TextColor::White))
				};

				if mouse_x <= value_x {
					return Some(Self(Text::new(value.to_owned(), 0, true, SelectedTextAdditional {
						y,
						indices,
						value_color,
						keyfix,
						prefix,
						suffix: (String::new(), TextColor::White),
						valuefix: None,
						cached_cursor_x,
						uuid: Uuid::new_v4(),
					})));
				}

				let value_width = value.width();

				if mouse_x
					+ value
						.chars()
						.last()
						.and_then(|char| VertexBufferBuilder::CHAR_WIDTH.get(char as usize))
						.copied()
						.unwrap_or(0) as usize
					> value_x + value_width
					&& mouse_x < value_x + value_width + 5
				{
					return Some(Self(Text::new(value.to_owned(), value.len(), true, SelectedTextAdditional {
						y,
						indices,
						value_color,
						keyfix,
						prefix,
						suffix: (String::new(), TextColor::White),
						valuefix: None,
						cached_cursor_x,
						uuid: Uuid::new_v4(),
					})));
				}

				let value_width = value_width as isize;
				let mut x = (mouse_x - value_x) as isize;

				for (cursor, char) in value.char_indices() {
					let width = char.width() as isize;
					if x >= width / 2 {
						x -= width;
					} else if x < value_width {
						return Some(Self(Text::new(value.to_owned(), cursor, true, SelectedTextAdditional {
							y,
							indices,
							value_color,
							keyfix,
							prefix,
							suffix: (String::new(), TextColor::White),
							valuefix: None,
							cached_cursor_x,
							uuid: Uuid::new_v4(),
						})));
					}
				}
			}
		}

		let full_width = key.as_ref().map_or(0, |(x, _, _)| x.width()) + value.as_ref().map_or(0, |(x, _, _)| x.width()) + if key.is_some() && value.is_some() { ": ".width() } else { 0 };
		if key
			.as_ref()
			.is_none_or(|&(_, _, display)| !display)
			&& value
				.as_ref()
				.is_none_or(|(_, _, display)| !*display)
			&& mouse_x <= target_x + full_width
			&& mouse_x + 16 >= target_x
		{
			Some(Self(Text::new(if key.is_some() { ": ".to_owned() } else { String::new() }, 0, false, SelectedTextAdditional {
				y,
				indices,
				value_color: TextColor::TreeKey,
				keyfix: key.map(|(x, color, _)| (x, color)),
				prefix: (String::new(), TextColor::White),
				suffix: (String::new(), TextColor::White),
				valuefix: value.map(|(x, color, _)| (x, color)),
				cached_cursor_x,
				uuid: Uuid::new_v4(),
			})))
		} else {
			None
		}
	}

	#[must_use]
	pub fn for_y(left_margin: usize, horizontal_scroll: usize, root: &NbtElement, path: &PathWithName, y: usize, mouse_x: usize, snap_to_ends: bool, cached_cursor_x: Option<usize>) -> Option<SelectedText> {
		fn header(left_margin: usize, root: &NbtElement, path: &PathWithName, offset: usize, cached_cursor_x: Option<usize>) -> Option<SelectedText> {
			let name = path.name();
			let name_width = name.width();
			let path_minus_name_width = path
				.path_str()
				.map_or(name_width, |path| path.width() - name_width);
			SelectedText::from_raw(
				36 + left_margin,
				offset + path_minus_name_width,
				HEADER_SIZE,
				Some((path.path_str().unwrap_or(name).to_owned(), TextColor::TreeKey, true)),
				Some((format!("[{}]", root.value().0), TextColor::TreeKey, false)),
				OwnedIndices::new(),
				cached_cursor_x,
			)
		}

		if y == 0 {
			let max = path.name().width() + 32 + 4 + left_margin;
			return header(left_margin, root, path, mouse_x.min(max), cached_cursor_x)
		}

		if y >= root.height() {
			return None
		}
		if root
			.as_region()
			.is_some_and(|region| region.is_grid_layout())
		{
			return None
		}

		let TraversalInformation { indices, depth, key, element, .. } = root.traverse(y, None)?;
		let target_x = depth * 16 + 32 + 4 + left_margin;
		if element.as_chunk().is_some() && mouse_x < target_x - 4 {
			return None
		}
		let k = key.map(|x| (x.to_owned(), TextColor::TreeKey, true));
		let v = Some(element.value()).map(|(a, c)| (a.into_owned(), c, c != TextColor::TreeKey));
		let mouse_x = if snap_to_ends {
			let min_x = target_x;
			let max_x = k
				.as_ref()
				.map_or(0, |(k, _, b)| (*b as usize) * (k.width() + ": ".width() * v.is_some() as usize))
				+ v.as_ref()
					.map_or(0, |(v, _, b)| (*b as usize) * v.width())
				+ target_x;
			(mouse_x + horizontal_scroll).clamp(min_x, max_x)
		} else {
			mouse_x + horizontal_scroll
		};

		SelectedText::from_raw(target_x, mouse_x, y * 16 + HEADER_SIZE, k, v, indices, cached_cursor_x)
	}

	pub fn width(&self) -> usize {
		self.prefix.0.width()
			+ self
				.keyfix
				.as_ref()
				.map(|x| x.0.width())
				.unwrap_or(0)
			+ self.value.width()
			+ self
				.valuefix
				.as_ref()
				.map(|x| x.0.width())
				.unwrap_or(0)
			+ self.suffix.0.width()
	}

	#[cfg_attr(not(debug_assertions), inline)]
	#[allow(clippy::cognitive_complexity, clippy::too_many_lines)] // I handled this fn well
	#[must_use]
	pub fn on_key_press(&mut self, key: KeyCode, char: Option<char>, flags: u8) -> SelectedTextKeyResult {
		if key == KeyCode::ArrowUp {
			if flags & !flags!(Ctrl) == 0 {
				return Up(flags == flags!(Ctrl));
			} else if flags == flags!(Ctrl + Shift) {
				return ShiftUp;
			}
		}

		if key == KeyCode::ArrowDown {
			if flags & !flags!(Ctrl) == 0 {
				return Down(flags == flags!(Ctrl));
			} else if flags == flags!(Ctrl + Shift) {
				return ShiftDown;
			}
		}

		if key == KeyCode::ArrowLeft {
			if flags & !flags!(Ctrl) == 0 && self.selection.is_none() && self.cursor == 0 && self.keyfix.is_some() {
				return MoveToKeyfix
			}
			if flags & !flags!(Shift) == flags!(Alt) {
				return ForceCloseElement
			}
		}

		if key == KeyCode::ArrowRight {
			if flags & !flags!(Ctrl) == 0 && self.selection.is_none() && self.cursor == self.value.len() && self.valuefix.is_some() {
				return MoveToValuefix
			}
			if (flags) & !flags!(Shift) == flags!(Alt) {
				return ForceOpenElement
			}
		}

		self.0.on_key_press(key, char, flags).into()
	}

	#[must_use]
	pub fn cursor_x(&self, left_margin: usize) -> usize { left_margin + self.indices.len() * 16 + 32 + 4 + self.prefix.0.width() + self.keyfix.as_ref().map_or(0, |x| x.0.width()) + self.value.split_at(self.cursor).0.width() }

	pub fn post_input(&mut self, left_margin: usize) {
		self.recache_cached_cursor_x(left_margin);
		self.0.post_input()
	}

	pub fn recache_cached_cursor_x(&mut self, left_margin: usize) { self.cached_cursor_x = Some(self.cursor_x(left_margin)); }

	pub fn recache_y(&mut self, root: &NbtElement) {
		let line_number = line_number_at(&self.indices, root);
		self.y = line_number * 16 + HEADER_SIZE;
	}

	pub fn set_indices(&mut self, indices: OwnedIndices, root: &NbtElement) {
		self.indices = indices;
		self.recache_y(root);
	}

	#[must_use]
	pub fn write(&self, window_properties: &mut WindowProperties, root: &mut NbtElement, path: &mut PathWithName) -> Option<WorkbenchAction> {
		let SelectedText(Text {
			value,
			editable: true,
			additional: SelectedTextAdditional { indices, prefix, suffix, .. },
			..
		}) = self
		else {
			return None
		};
		let key = prefix.0.is_empty() && !suffix.0.is_empty();
		let (key, value) = if key { (Some(value.into()), None) } else { (None, Some(value.into())) };
		rename_element(root, indices.clone(), key, value, path, window_properties).map(RenameElementResult::into_action)
	}

	#[must_use]
	pub fn move_to_keyfix(&mut self, window_properties: &mut WindowProperties, left_margin: usize, root: &mut NbtElement, path: &mut PathWithName) -> Option<WorkbenchAction> {
		if !self.editable {
			return None
		}
		if self.valuefix.is_some() {
			return None
		}
		if !self.suffix.0.is_empty() {
			return None
		}
		if self.cursor > 0 {
			return None
		}

		// has to be here because of side effects from `self.write`
		let (keyfix, keyfix_color) = self.keyfix.clone()?;

		let action = self.write(window_properties, root, path)?;

		let old_prefix = core::mem::take(&mut self.prefix);

		self.cursor = keyfix.len();
		let old_value = core::mem::replace(&mut self.value, keyfix);
		let old_value_color = core::mem::replace(&mut self.value_color, keyfix_color);

		self.suffix = old_prefix;
		self.valuefix = Some((old_value, old_value_color));

		self.recache_cached_cursor_x(left_margin);

		Some(action)
	}

	#[must_use]
	pub fn move_to_valuefix(&mut self, window_properties: &mut WindowProperties, left_margin: usize, root: &mut NbtElement, path: &mut PathWithName) -> Option<WorkbenchAction> {
		if !self.editable {
			return None
		}
		if self.keyfix.is_some() {
			return None
		}
		if !self.prefix.0.is_empty() {
			return None
		}
		if self.cursor < self.value.len() {
			return None
		}

		// has to be here because of side effects from `self.write`
		let (valuefix, valuefix_color) = self.valuefix.clone()?;

		let action = self.write(window_properties, root, path)?;

		let old_suffix = core::mem::take(&mut self.suffix);

		self.cursor = 0;
		let old_value = core::mem::replace(&mut self.value, valuefix);
		let old_value_color = core::mem::replace(&mut self.value_color, valuefix_color);

		self.prefix = old_suffix;
		self.keyfix = Some((old_value, old_value_color));

		self.recache_cached_cursor_x(left_margin);

		Some(action)
	}

	fn r#move(
		&mut self,
		window_properties: &mut WindowProperties,
		left_margin: usize,
		horizontal_scroll: usize,
		root: &mut NbtElement,
		path: &mut PathWithName,
		mut f: impl FnMut(usize, &NbtElement, &Indices) -> Option<usize>,
	) -> Option<WorkbenchAction> {
		let y = (self.y - HEADER_SIZE) / 16;
		let Some(new_y) = f(y, root, &self.indices) else { return None };

		let mouse_x = self
			.cached_cursor_x
			.unwrap_or_else(|| self.cursor_x(left_margin));

		let Some(new_selected_text) = SelectedText::for_y(left_margin, horizontal_scroll, root, path, new_y, mouse_x, true, Some(mouse_x)) else {
			return None;
		};

		let Some(action) = self.write(window_properties, root, path) else {
			return None;
		};

		*self = new_selected_text;

		Some(action)
	}

	pub fn move_up(&mut self, ctrl: bool, window_properties: &mut WindowProperties, left_margin: usize, horizontal_scroll: usize, root: &mut NbtElement, path: &mut PathWithName) -> Option<WorkbenchAction> {
		self.r#move(window_properties, left_margin, horizontal_scroll, root, path, |y, root, indices| {
			Some(
				if ctrl
					&& let Some(last_idx) = indices.last()
					&& last_idx > 0
				{
					let NavigationInformation { line_number, .. } = root.navigate(indices)?;
					line_number
				} else {
					y.wrapping_sub(1)
				},
			)
		})
	}

	pub fn move_down(&mut self, ctrl: bool, window_properties: &mut WindowProperties, left_margin: usize, horizontal_scroll: usize, root: &mut NbtElement, path: &mut PathWithName) -> Option<WorkbenchAction> {
		self.r#move(window_properties, left_margin, horizontal_scroll, root, path, |y, root, indices| {
			Some(if ctrl && let Some((last_idx, parent_indices)) = indices.split_last() {
				let NavigationInformation { element: parent, line_number, .. } = root.navigate(&parent_indices)?;
				let len = parent.len()?;
				if last_idx + 1 == len { y + 1 } else { line_number + 1 }
			} else {
				y.wrapping_add(1)
			})
		})
	}

	pub fn render(&self, builder: &mut VertexBufferBuilder, left_margin: usize) {
		let x = self.indices.len() * 16 + 32 + 4 + left_margin;
		let y = if builder.scroll() > self.y {
			return;
		} else {
			self.y - builder.scroll()
		};
		if y < HEADER_SIZE {
			return
		}

		let prefix_width = self.prefix.0.as_str().width() + self.keyfix.as_ref().map_or(0, |x| x.0.width());
		self.0
			.render(builder, self.value_color, (x + prefix_width, y).into(), SELECTED_TEXT_Z, SELECTED_TEXT_SELECTION_Z);

		builder.draw_texture_z((x - 4 - 16, y), SELECTED_TEXT_Z, SELECTION_UV, (16, 16));
		builder.settings((x, y), false, BASE_TEXT_Z);
		if let Some((keyfix, keyfix_color)) = self.keyfix.as_ref() {
			builder.color = keyfix_color.to_raw();
			let _ = write!(builder, "{keyfix}");
		}

		builder.color = self.prefix.1.to_raw();
		let _ = write!(builder, "{}", self.prefix.0);

		builder.settings((x + prefix_width + self.value.width(), y), false, BASE_TEXT_Z);

		builder.color = self.suffix.1.to_raw();
		let _ = write!(builder, "{}", self.suffix.0);

		if let Some((valuefix, valuefix_color)) = self.valuefix.as_ref() {
			builder.color = valuefix_color.to_raw();
			let _ = write!(builder, "{valuefix}");
		}
	}
}

#[derive(Default)]
pub struct SelectedTexts {
	inner: Vec<SelectedText>,
}

impl SelectedTexts {
	pub fn set(&mut self, idx: usize, selected_text: SelectedText) {
		if let Some((older_selected_text_idx, older_selected_text)) = self
			.iter_mut()
			.find_position(|s| s.y == selected_text.y)
		{
			if older_selected_text_idx != idx {
				let Some(previous) = self.get_mut(idx) else { return };
				*previous = selected_text;
				self.remove(older_selected_text_idx);
			} else {
				*older_selected_text = selected_text;
			}
		} else {
			let Some(previous) = self.get_mut(idx) else { return };
			*previous = selected_text;
		}
	}

	pub fn push(&mut self, selected_text: SelectedText) -> &mut SelectedText {
		if let Some((older_selected_text, _)) = self
			.iter()
			.find_position(|s| s.y == selected_text.y)
		{
			self.remove(older_selected_text);
		}
		self.inner.push(selected_text);
		// SAFETY: just pushed an element
		unsafe { self.inner.last_mut().unwrap_unchecked() }
	}
}

impl SelectedTexts {
	#[must_use]
	pub const fn new() -> Self { Self { inner: Vec::new() } }
}

impl Deref for SelectedTexts {
	type Target = Vec<SelectedText>;

	fn deref(&self) -> &Self::Target { &self.inner }
}

impl DerefMut for SelectedTexts {
	fn deref_mut(&mut self) -> &mut Self::Target { &mut self.inner }
}
