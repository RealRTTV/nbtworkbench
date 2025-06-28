use std::{
	fmt::Write,
	ops::{Deref, DerefMut},
};

use compact_str::ToCompactString;
use thiserror::Error;
use uuid::Uuid;
use winit::keyboard::KeyCode;

use crate::{
	action_result::{ActionResult, IntoFailingActionResult},
	elements::element::NbtElement,
	flags,
	history::{manager::HistoryMananger, WorkbenchAction}
	,
	render::{
		assets::{BASE_TEXT_Z, HEADER_SIZE, SELECTED_TEXT_SELECTION_Z, SELECTED_TEXT_Z, SELECTION_UV},
		color::TextColor,
		vertex_buffer_builder::VertexBufferBuilder,
		widget::{
			alert::manager::{AlertManager, Alertable},
			text::{Cachelike, SelectedTextKeyResult, Text},
		},
	},
	tree::{
		actions::{
			close::{close_element, CloseElementError},
			expand::expand_element,
			open::open_element,
			rename::{rename_element, RenameElementError},
			swap::{swap_element_same_depth, SwapElementErrorSameDepth},
			AmbiguiousOpenElementError,
		},
		indices::{Indices, OwnedIndices},
		line_number_at,
		navigate::{NavigationError, NavigationInformation, ParentNavigationError, ParentNavigationInformationMut},
		traverse::{TraversalError, TraversalInformation},
		MutableIndices,
	},
	util::{CharExt, StrExt},
	workbench::{
		marked_line::MarkedLines,
		tab::{FilePath, TabConstants},
	},
};

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
			keyfix: text.additional.keyfix.clone().map(|(a, b)| (a.into_boxed_str(), b)),
			valuefix: text.additional.valuefix.clone().map(|(a, b)| (a.into_boxed_str(), b)),
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
	pub const PREFIXING_SPACE_WIDTH: usize = 4;

	// todo: refactor using new text functions
	pub fn from_raw(target_x: usize, mouse_x: usize, y: usize, key: Option<(String, TextColor, bool)>, value: Option<(String, TextColor, bool)>, indices: OwnedIndices, cached_cursor_x: Option<usize>) -> Result<Self, SelectedTextConstructionError> {
		let key_width = if let Some((key, key_color, true)) = key.clone() {
			let key_width = key.width();

			if mouse_x + Self::PREFIXING_SPACE_WIDTH >= target_x {
				let (suffix, valuefix) = if let Some((v, valuefix_color, b)) = &value {
					if *b {
						((": ".to_owned(), TextColor::TreeKey), Some((v.clone(), *valuefix_color)))
					} else {
						((format!(": {v}"), TextColor::TreeKey), None)
					}
				} else {
					((String::new(), TextColor::White), None)
				};

				if mouse_x <= target_x {
					return Ok(Self(Text::new(key, 0, true, SelectedTextAdditional {
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

				if target_x + key_width < mouse_x + key.chars().last().and_then(|char| VertexBufferBuilder::CHAR_WIDTH.get(char as usize)).copied().unwrap_or(0) as usize && mouse_x < target_x + key_width + 7 {
					return Ok(Self(Text::new(key.clone(), key.len(), true, SelectedTextAdditional {
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
						return Ok(Self(Text::new(key, cursor, true, SelectedTextAdditional {
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

		if let Some((value, value_color, true)) = value.as_ref() {
			let value_x = target_x + key_width;
			if mouse_x + Self::PREFIXING_SPACE_WIDTH >= value_x {
				let (keyfix, prefix) = if let Some((k, key_color, b)) = key.as_ref() {
					if *b {
						(Some((k.clone(), *key_color)), (": ".to_owned(), TextColor::TreeKey))
					} else {
						(None, (format!("{k}: "), TextColor::TreeKey))
					}
				} else {
					(None, (String::new(), TextColor::White))
				};

				if mouse_x <= value_x {
					return Ok(Self(Text::new(value.clone(), 0, true, SelectedTextAdditional {
						y,
						indices,
						value_color: *value_color,
						keyfix,
						prefix,
						suffix: (String::new(), TextColor::White),
						valuefix: None,
						cached_cursor_x,
						uuid: Uuid::new_v4(),
					})));
				}

				let value_width = value.width();

				if mouse_x + value.chars().last().and_then(|char| VertexBufferBuilder::CHAR_WIDTH.get(char as usize)).copied().unwrap_or(0) as usize > value_x + value_width && mouse_x < value_x + value_width + 5 {
					return Ok(Self(Text::new(value.clone(), value.len(), true, SelectedTextAdditional {
						y,
						indices,
						value_color: *value_color,
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
						return Ok(Self(Text::new(value.clone(), cursor, true, SelectedTextAdditional {
							y,
							indices,
							value_color: *value_color,
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
		if key.as_ref().is_none_or(|(_, _, display)| !*display) && value.as_ref().is_none_or(|(_, _, display)| !*display) && mouse_x <= target_x + full_width && mouse_x + 16 >= target_x {
			Ok(Self(Text::new(if key.is_some() { ": ".to_owned() } else { String::new() }, 0, false, SelectedTextAdditional {
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
			Err(SelectedTextConstructionError::OutOfBounds {
				min_x: target_x,
				max_x: target_x + full_width,
				mouse_x,
			})
		}
	}

	#[must_use]
	pub fn width(&self) -> usize { self.prefix.0.width() + self.keyfix.as_ref().map(|x| x.0.width()).unwrap_or(0) + self.value.width() + self.valuefix.as_ref().map(|x| x.0.width()).unwrap_or(0) + self.suffix.0.width() }

	#[must_use]
	pub fn end_x(&self, left_margin: usize) -> usize {
		self.indices.end_x(left_margin) + Self::PREFIXING_SPACE_WIDTH + self.width()
	}

	pub fn on_key_press<'m1, 'm2: 'm1>(
		&mut self,
		key: KeyCode,
		ch: Option<char>,
		flags: u8,
		consts: TabConstants,
		root: &mut NbtElement,
		path: &mut FilePath,
		mi: &'m1 mut MutableIndices<'m2>,
		alerts: &mut AlertManager,
		history: &mut HistoryMananger,
	) -> ActionResult<bool> {
		fn on_key_press0<'m1, 'm2: 'm1>(
			this: &mut SelectedText,
			key: KeyCode,
			ch: Option<char>,
			flags: u8,
			consts: TabConstants,
			root: &mut NbtElement,
			path: &mut FilePath,
			mi: &'m1 mut MutableIndices<'m2>,
		) -> Result<SelectedTextKeyResult, SelectedTextInputError> {
			if key == KeyCode::ArrowUp {
				if flags & !flags!(Ctrl) == 0 {
					return Ok(SelectedTextKeyResult::Action(Some(this.move_up(consts, flags == flags!(Ctrl), root, path)?)))
				} else if flags == flags!(Ctrl + Shift) {
					return Ok(SelectedTextKeyResult::Action(Some(this.shift_up(consts, root, mi)?)))
				}
			}

			if key == KeyCode::ArrowDown {
				if flags & !flags!(Ctrl) == 0 {
					return Ok(SelectedTextKeyResult::Action(Some(this.move_down(consts, flags == flags!(Ctrl), root, path)?)))
				} else if flags == flags!(Ctrl + Shift) {
					return Ok(SelectedTextKeyResult::Action(Some(this.shift_down(consts, root, mi)?)))
				}
			}

			if key == KeyCode::ArrowLeft {
				if flags & !flags!(Ctrl) == 0 && this.selection.is_none() && this.cursor == 0 && this.keyfix.is_some() {
					return Ok(SelectedTextKeyResult::Action(Some(this.move_to_keyfix(consts, root, path)?)))
				}
				if flags & !flags!(Shift) == flags!(Alt) {
					this.force_close(root, mi.bookmarks)?;
					return Ok(SelectedTextKeyResult::Action(None))
				}
			}

			if key == KeyCode::ArrowRight {
				if flags & !flags!(Ctrl) == 0 && this.selection.is_none() && this.cursor == this.value.len() && this.valuefix.is_some() {
					return Ok(SelectedTextKeyResult::Action(Some(this.move_to_valuefix(consts, root, path)?)))
				}
				if (flags) & !flags!(Shift) == flags!(Alt) {
					this.force_open((flags & !flags!(Alt)) == flags!(Shift), root, mi.bookmarks)?;
					return Ok(SelectedTextKeyResult::Action(None))
				}
			}

			Ok(this.0.on_key_press(key, ch, flags).into())
		}
		let result = on_key_press0(self, key, ch, flags, consts, root, path, mi);
		match result.alert_err(alerts).failure_on_err()? {
			SelectedTextKeyResult::NoAction => ActionResult::Pass,
			SelectedTextKeyResult::Action(None) => {
				self.post_input();
				ActionResult::Success(false)
			}
			SelectedTextKeyResult::Action(Some(action)) => {
				self.post_input();
				history.append(action);
				ActionResult::Success(false)
			}
			SelectedTextKeyResult::Escape => ActionResult::Success(true),
			SelectedTextKeyResult::Finish => {
				history.append(self.save(root, path).alert_err(alerts).failure_on_err()?);
				ActionResult::Success(true)
			}
			SelectedTextKeyResult::GenericAction => {
				self.post_input();
				ActionResult::Success(false)
			}
		}
	}

	#[must_use]
	pub fn cursor_x(&self, left_margin: usize) -> usize { self.indices.end_x(left_margin) + Self::PREFIXING_SPACE_WIDTH + self.prefix.0.width() + self.keyfix.as_ref().map_or(0, |x| x.0.width()) + self.value.split_at(self.cursor).0.width() }

	pub fn post_input(&mut self) { self.0.post_input() }

	pub fn recache_cached_cursor_x(&mut self, consts: TabConstants) {
		let TabConstants { left_margin, .. } = consts;

		self.cached_cursor_x = Some(self.cursor_x(left_margin));
	}

	pub fn recache_y(&mut self, root: &NbtElement) {
		let line_number = line_number_at(&self.indices, root);
		self.y = line_number * 16 + HEADER_SIZE;
	}

	pub fn set_indices(&mut self, indices: OwnedIndices, root: &NbtElement) {
		self.indices = indices;
		self.recache_y(root);
	}

	pub fn render(&self, builder: &mut VertexBufferBuilder, left_margin: usize) {
		let x = self.indices.end_x(left_margin);
		let y = if builder.scroll() > self.y {
			return;
		} else {
			self.y - builder.scroll()
		};
		if y < HEADER_SIZE {
			return
		}

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

	pub fn for_y(consts: TabConstants, root: &NbtElement, path: &FilePath, y: usize, mouse_x: usize, snap_to_ends: bool, cached_cursor_x: Option<usize>) -> Result<SelectedText, SelectedTextConstructionError> {
		fn header(consts: TabConstants, root: &NbtElement, path: &FilePath, offset: usize, cached_cursor_x: Option<usize>) -> Result<SelectedText, SelectedTextConstructionError> {
			let TabConstants { left_margin, .. } = consts;
			let name = path.name();
			let path_minus_name_width = path.path_str().width() - name.width();
			SelectedText::from_raw(
				36 + left_margin,
				offset + path_minus_name_width,
				HEADER_SIZE,
				Some((path.path_str().to_string(), TextColor::TreeKey, true)),
				Some((format!("[{}]", root.value().0), TextColor::TreeKey, false)),
				OwnedIndices::new(),
				cached_cursor_x,
			)
		}

		let TabConstants { left_margin, horizontal_scroll, .. } = consts;

		if y == 0 {
			let max = Indices::end_x_from_depth(0, left_margin) + Self::PREFIXING_SPACE_WIDTH + path.name().width();
			return header(consts, root, path, mouse_x.min(max), cached_cursor_x)
		}

		if root.as_region().is_some_and(|region| region.is_grid_layout()) {
			return Err(SelectedTextConstructionError::Region)
		}

		let TraversalInformation { indices, depth, key, element, .. } = root.traverse(y, None)?;
		let target_x = Indices::end_x_from_depth(depth, left_margin);
		if element.as_chunk().is_some() && mouse_x < target_x - 4 {
			return Err(SelectedTextConstructionError::OutOfBounds {
				min_x: target_x,
				max_x: target_x + element.value_width(),
				mouse_x,
			})
		}
		let k = key.map(|x| (x.to_owned(), TextColor::TreeKey, true));
		let v = Some(element.value()).map(|(a, c)| (a.into_owned(), c, c != TextColor::TreeKey));
		let mouse_x = if snap_to_ends {
			let min_x = target_x;
			let max_x = k.as_ref().map_or(0, |(k, _, b)| (*b as usize) * (k.width() + ": ".width() * v.is_some() as usize)) + v.as_ref().map_or(0, |(v, _, b)| (*b as usize) * v.width()) + target_x;
			(mouse_x + horizontal_scroll).clamp(min_x, max_x)
		} else {
			mouse_x + horizontal_scroll
		};

		SelectedText::from_raw(target_x, mouse_x, y * 16 + HEADER_SIZE, k, v, indices, cached_cursor_x)
	}

	pub fn save(&self, root: &mut NbtElement, path: &mut FilePath) -> Result<WorkbenchAction, SaveSelectedTextError> {
		if !self.editable {
			return Err(SaveSelectedTextError::NonEditable)
		}

		let key = self.prefix.0.is_empty() && !self.suffix.0.is_empty();
		let (key, value) = if key { (Some(self.value.to_compact_string()), None) } else { (None, Some(self.value.clone())) };
		Ok(rename_element(root, self.indices.clone(), key, value, path)?.into_action())
	}
	pub fn move_to_keyfix(&mut self, consts: TabConstants, root: &mut NbtElement, path: &mut FilePath) -> Result<WorkbenchAction, MoveToKeyfixError> {
		if !self.editable {
			return Err(MoveToKeyfixError::Save(SaveSelectedTextError::NonEditable))
		}
		if self.valuefix.is_some() || !self.suffix.0.is_empty() {
			return Err(MoveToKeyfixError::AlreadyAtKey)
		}

		// has to be here because of side effects from `self.write`
		let (keyfix, keyfix_color) = self.keyfix.clone().ok_or(MoveToKeyfixError::NoKey)?;

		let action = self.save(root, path)?;

		let old_prefix = core::mem::take(&mut self.prefix);

		self.cursor = keyfix.len();
		let old_value = core::mem::replace(&mut self.value, keyfix);
		let old_value_color = core::mem::replace(&mut self.value_color, keyfix_color);

		self.suffix = old_prefix;
		self.valuefix = Some((old_value, old_value_color));

		self.recache_cached_cursor_x(consts);

		Ok(action)
	}

	pub fn move_to_valuefix(&mut self, consts: TabConstants, root: &mut NbtElement, path: &mut FilePath) -> Result<WorkbenchAction, MoveToValuefixError> {
		if !self.editable {
			return Err(MoveToValuefixError::Save(SaveSelectedTextError::NonEditable))
		}
		if self.keyfix.is_some() || !self.prefix.0.is_empty() {
			return Err(MoveToValuefixError::AlreadyAtValue)
		}

		// has to be here because of side effects from `self.write`
		let (valuefix, valuefix_color) = self.valuefix.clone().ok_or(MoveToValuefixError::NoValue)?;

		let action = self.save(root, path)?;

		let old_suffix = core::mem::take(&mut self.suffix);

		self.cursor = 0;
		let old_value = core::mem::replace(&mut self.value, valuefix);
		let old_value_color = core::mem::replace(&mut self.value_color, valuefix_color);

		self.prefix = old_suffix;
		self.keyfix = Some((old_value, old_value_color));

		self.recache_cached_cursor_x(consts);

		Ok(action)
	}

	fn move_text(&mut self, consts: TabConstants, root: &mut NbtElement, path: &mut FilePath, mut f: impl FnMut(usize, &NbtElement, &Indices) -> Result<usize, MoveSelectedTextError>) -> Result<WorkbenchAction, MoveSelectedTextError> {
		let TabConstants { left_margin, .. } = consts;

		let y = (self.y - HEADER_SIZE) / 16;
		let new_y = f(y, root, &self.indices)?;

		let mouse_x = self.cached_cursor_x.unwrap_or_else(|| self.cursor_x(left_margin));

		let new_selected_text = SelectedText::for_y(consts, root, path, new_y, mouse_x, true, Some(mouse_x))?;

		let action = self.save(root, path)?;

		*self = new_selected_text;

		Ok(action)
	}

	pub fn move_up(&mut self, consts: TabConstants, ctrl: bool, root: &mut NbtElement, path: &mut FilePath) -> Result<WorkbenchAction, MoveSelectedTextError> {
		self.move_text(consts, root, path, |y, root, indices| {
			Ok(
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

	pub fn move_down(&mut self, consts: TabConstants, ctrl: bool, root: &mut NbtElement, path: &mut FilePath) -> Result<WorkbenchAction, MoveSelectedTextError> {
		self.move_text(consts, root, path, |y, root, indices| {
			Ok(if ctrl && let Some((last_idx, parent_indices)) = indices.split_last() {
				let NavigationInformation { element: parent, line_number, .. } = root.navigate(&parent_indices).map_err(|e| MoveSelectedTextError::Navigation(e))?;
				let len = parent.len().ok_or_else(|| {
					MoveSelectedTextError::Save(SaveSelectedTextError::Rename(RenameElementError::Navigation(ParentNavigationError::NavigationError(NavigationError::ParentWasPrimitive {
						indices: parent_indices.to_owned(),
					}))))
				})?;
				if last_idx + 1 == len { y + 1 } else { line_number + 1 }
			} else {
				y.wrapping_add(1)
			})
		})
	}

	pub fn shift<'m1, 'm2: 'm1>(&mut self, _consts: TabConstants, root: &mut NbtElement, mi: &'m1 mut MutableIndices<'m2>, sibling_idx: impl FnOnce(usize) -> Option<usize>) -> Result<WorkbenchAction, ShiftSelectedTextError> {
		let ParentNavigationInformationMut { idx: a_idx, parent_indices, .. } = root.navigate_parent_mut(&self.indices)?;
		let b_idx = sibling_idx(a_idx).ok_or(ShiftSelectedTextError::InvalidSiblingIndex { original_index: a_idx })?;
		Ok(swap_element_same_depth(root, parent_indices.to_owned(), a_idx, b_idx, mi)?.into_action())
	}

	pub fn shift_up<'m1, 'm2: 'm1>(&mut self, consts: TabConstants, root: &mut NbtElement, mi: &'m1 mut MutableIndices<'m2>) -> Result<WorkbenchAction, ShiftSelectedTextError> { self.shift(consts, root, mi, |idx| idx.checked_sub(1)) }

	pub fn shift_down<'m1, 'm2: 'm1>(&mut self, consts: TabConstants, root: &mut NbtElement, mi: &'m1 mut MutableIndices<'m2>) -> Result<WorkbenchAction, ShiftSelectedTextError> { self.shift(consts, root, mi, |idx| idx.checked_add(1)) }

	pub fn force_close(&self, root: &mut NbtElement, bookmarks: &mut MarkedLines) -> Result<(), CloseElementError> { close_element(root, &self.indices, bookmarks) }

	pub fn force_open(&self, expand: bool, root: &mut NbtElement, bookmarks: &mut MarkedLines) -> Result<(), AmbiguiousOpenElementError> {
		if expand { Ok(expand_element(root, &self.indices, bookmarks)?) } else { Ok(open_element(root, &self.indices, bookmarks)?) }
	}
}

#[derive(Error, Debug)]
pub enum SelectedTextConstructionError {
	#[error(transparent)]
	Traversal(#[from] TraversalError),
	#[error("Out of text bounds (min = {min_x}, max = {max_x}); mouse was at {mouse_x}")]
	OutOfBounds { min_x: usize, max_x: usize, mouse_x: usize },
	#[error("Cannot select chunk from grid view as selected text")]
	Region,
}

#[derive(Error, Debug)]
pub enum SaveSelectedTextError {
	#[error(transparent)]
	Rename(#[from] RenameElementError),
	#[error("Non-editable selected text")]
	NonEditable,
}

#[derive(Error, Debug)]
pub enum MoveSelectedTextError {
	#[error(transparent)]
	Save(#[from] SaveSelectedTextError),
	#[error(transparent)]
	Navigation(#[from] NavigationError),
	#[error("Could not create new selected text: {0}")]
	NoNewSelectedText(#[from] SelectedTextConstructionError),
}

#[derive(Error, Debug)]
pub enum MoveToKeyfixError {
	#[error(transparent)]
	Save(#[from] SaveSelectedTextError),
	#[error("Tried to move to key but was indicated to be already at key")]
	AlreadyAtKey,
	#[error("No key found to move to")]
	NoKey,
}

#[derive(Error, Debug)]
pub enum MoveToValuefixError {
	#[error(transparent)]
	Save(#[from] SaveSelectedTextError),
	#[error("Tried to move to value but was indicated to be already at value")]
	AlreadyAtValue,
	#[error("No value found to move to")]
	NoValue,
}

#[derive(Error, Debug)]
pub enum ShiftSelectedTextError {
	#[error(transparent)]
	Navigation(#[from] ParentNavigationError),
	#[error(transparent)]
	SwapElementSameDepth(#[from] SwapElementErrorSameDepth),
	#[error("Invalid sibling index; original index: {original_index}")]
	InvalidSiblingIndex { original_index: usize },
}

#[derive(Error, Debug)]
pub enum SelectedTextInputError {
	#[error(transparent)]
	CloseElement(#[from] CloseElementError),
	#[error(transparent)]
	OpenElement(#[from] AmbiguiousOpenElementError),
	#[error(transparent)]
	ShiftSelectedText(#[from] ShiftSelectedTextError),
	#[error(transparent)]
	MoveToValuefix(#[from] MoveToValuefixError),
	#[error(transparent)]
	MoveToKeyfix(#[from] MoveToKeyfixError),
	#[error(transparent)]
	MoveSelectedText(#[from] MoveSelectedTextError),
}
