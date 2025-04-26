use std::fmt::Write;
use std::ops::{Deref, DerefMut};

use winit::keyboard::KeyCode;

use crate::assets::{BASE_TEXT_Z, HEADER_SIZE, SELECTED_TEXT_SELECTION_Z, SELECTED_TEXT_Z, SELECTION_UV};
use crate::elements::NbtElement;
use crate::flags;
use crate::render::{TextColor, VertexBufferBuilder};
use crate::util::{CharExt, StrExt};
use crate::widget::{Cachelike, SelectedTextKeyResult, SelectedTextKeyResult::{Down, ForceClose, ForceOpen, Keyfix, ShiftDown, ShiftUp, Up, Valuefix}, Text};
use crate::tree::sum_indices;

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
	fn eq(&self, other: &Self) -> bool {
		self.keyfix == other.keyfix && self.value == other.value && self.valuefix == other.valuefix
	}
}

impl Cachelike<SelectedTextAdditional> for SelectedTextCache {
	fn new(text: &Text<SelectedTextAdditional, Self>) -> Self where Self: Sized {
		Self {
			keyfix: text.additional.keyfix.clone().map(|(a, b)| (a.into_boxed_str(), b)),
			valuefix: text.additional.valuefix.clone().map(|(a, b)| (a.into_boxed_str(), b)),
			value: text.value.clone().into_boxed_str(),
			cursor: text.cursor,
			selection: text.selection,
		}
	}

	fn revert(self, text: &mut Text<SelectedTextAdditional, Self>) where Self: Sized {
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

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl DerefMut for SelectedText {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

#[derive(Clone)]
pub struct SelectedTextAdditional  {
	// todo, change this to be Option<usize> and a cache that removes itself on every write
	pub y: usize,
	pub indices: Box<[usize]>,
	pub value_color: TextColor,
	pub keyfix: Option<(String, TextColor)>,
	pub prefix: (String, TextColor),
	pub suffix: (String, TextColor),
	pub valuefix: Option<(String, TextColor)>,
}

impl SelectedText {
	#[inline]
	#[must_use]
	#[allow(clippy::too_many_lines)]
	pub fn new(target_x: usize, mouse_x: usize, y: usize, key: Option<(Box<str>, TextColor, bool)>, value: Option<(Box<str>, TextColor, bool)>, indices: Vec<usize>) -> Option<Self> {
		let key_width = if let Some((key, key_color, true)) = key.clone() {
			let key_width = key.width();

			if mouse_x + 4 >= target_x {
				let (suffix, valuefix) = if let Some((v, valuefix_color, b)) = &value {
					if *b {
						(
							(": " .to_owned(), TextColor::TreeKey),
							Some((v.clone().into_string(), *valuefix_color)),
						)
					} else {
						((format!(": {v}"), TextColor::TreeKey), None)
					}
				} else {
					((String::new(), TextColor::White), None)
				};

				if mouse_x <= target_x {
					return Some(
						Self(Text::new(key.into_string(), 0, true, SelectedTextAdditional {
							y,
							indices: indices.into_boxed_slice(),
							value_color: key_color,
							keyfix: None,
							prefix: (String::new(), TextColor::White),
							suffix,
							valuefix,
						})),
					);
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
					return Some(
						Self(Text::new(key.clone().into_string(), key.len(), true, SelectedTextAdditional {
							y,
							indices: indices.into_boxed_slice(),
							value_color: key_color,
							keyfix: None,
							prefix: (String::new(), TextColor::White),
							suffix,
							valuefix,
						})),
					);
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
						return Some(Self(Text::new(key.into_string(), cursor, true, SelectedTextAdditional {
							y,
							indices: indices.into_boxed_slice(),
							value_color: key_color,
							keyfix: None,
							prefix: (String::new(), TextColor::White),
							suffix,
							valuefix,
						})),
						);
					}
				}
			}
			key_width + ": ".width()
		} else {
			0
		};

		if let Some((value, value_color, true)) = value.as_ref() {
			let value_x = target_x + key_width;
			if mouse_x + 4 >= value_x {
				let (keyfix, prefix) = if let Some((k, key_color, b)) = key.as_ref() {
					if *b {
						(
							Some((k.as_ref().to_owned(), *key_color)),
							(": ".to_owned(), TextColor::TreeKey),
						)
					} else {
						(None, (format!("{k}: "), TextColor::TreeKey))
					}
				} else {
					(None, (String::new(), TextColor::White))
				};

				if mouse_x <= value_x {
					return Some(Self(Text::new(value.as_ref().to_owned(), 0, true, SelectedTextAdditional {
						y,
						indices: indices.into_boxed_slice(),
						value_color: *value_color,
						keyfix,
						prefix,
						suffix: (String::new(), TextColor::White),
						valuefix: None,
					})),
					);
				}

				let value_width = value.width();

				if mouse_x
					+ value
						.as_ref()
						.chars()
						.last()
						.and_then(|char| VertexBufferBuilder::CHAR_WIDTH.get(char as usize))
						.copied()
						.unwrap_or(0) as usize
					> value_x + value_width
					&& mouse_x < value_x + value_width + 5
				{
					return Some(Self(Text::new(value.as_ref().to_owned(), value.len(), true, SelectedTextAdditional {
						y,
						indices: indices.into_boxed_slice(),
						value_color: *value_color,
						keyfix,
						prefix,
						suffix: (String::new(), TextColor::White),
						valuefix: None,
					})),
					);
				}

				let value_width = value_width as isize;
				let mut x = (mouse_x - value_x) as isize;

				for (cursor, char) in value.char_indices() {
					let width = char.width() as isize;
					if x >= width / 2 {
						x -= width;
					} else if x < value_width {
						return Some(Self(Text::new(value.as_ref().to_owned(), cursor, true, SelectedTextAdditional {
							y,
							indices: indices.into_boxed_slice(),
							value_color: *value_color,
							keyfix,
							prefix,
							suffix: (String::new(), TextColor::White),
							valuefix: None,
						})));
					}
				}
			}
		}

		let full_width = key.as_ref().map_or(0, |(x, _, _)| x.width())
			+ value.as_ref().map_or(0, |(x, _, _)| x.width())
			+ if key.is_some() && value.is_some() {
				": ".width()
			} else {
				0
			};
		if key.as_ref().is_none_or(|(_, _, display)| !*display) && value.as_ref().is_none_or(|(_, _, display)| !*display) && mouse_x <= target_x + full_width && mouse_x + 16 >= target_x {
			Some(Self(Text::new(if key.is_some() { ": ".to_owned() } else { String::new() }, 0, false, SelectedTextAdditional {
				y,
				indices: indices.into_boxed_slice(),
				value_color: TextColor::TreeKey,
				keyfix: key.map(|(x, color, _)| (x.into_string(), color)),
				prefix: (String::new(), TextColor::White),
				suffix: (String::new(), TextColor::White),
				valuefix: value.map(|(x, color, _)| (x.into_string(), color)),
			})),
			)
		} else {
			None
		}
	}

	pub fn width(&self) -> usize { self.prefix.0.width() + self.keyfix.as_ref().map(|x| x.0.width()).unwrap_or(0) + self.value.width() + self.valuefix.as_ref().map(|x| x.0.width()).unwrap_or(0) + self.suffix.0.width() }

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
			if flags & !flags!(Ctrl) == 0 && self.selection.is_none() && self.cursor == 0 && self.keyfix.is_some() { return Keyfix }
			if flags & !flags!(Shift) == flags!(Alt) { return ForceClose }
		}

		if key == KeyCode::ArrowRight {
			if flags & !flags!(Ctrl) == 0 && self.selection.is_none() && self.cursor == self.value.len() && self.valuefix.is_some() { return Valuefix }
			if (flags) & !flags!(Shift) == flags!(Alt) { return ForceOpen }
		}

		self.0.on_key_press(key, char, flags).into()
	}

	#[inline]
	pub fn post_input(&mut self) {
		self.0.post_input()
	}

	pub fn recache_y(&mut self, root: &NbtElement) {
		let line_number = sum_indices(self.indices.iter().copied(), root);
		self.y = line_number * 16 + HEADER_SIZE;
	}

	#[inline]
	pub fn render(&self, builder: &mut VertexBufferBuilder, left_margin: usize) {
		let x = self.indices.len() * 16 + 32 + 4 + left_margin;
		let y = if builder.scroll() > self.y { return; } else { self.y - builder.scroll() };
		if y < HEADER_SIZE { return }

		let prefix_width = self.prefix.0.as_str().width() + self.keyfix.as_ref().map_or(0, |x| x.0.width());
		self.0.render(builder, self.value_color, (x + prefix_width, y).into(), SELECTED_TEXT_Z, SELECTED_TEXT_SELECTION_Z);

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
