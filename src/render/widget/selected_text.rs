use std::fmt::Write;
use std::ops::{Deref, DerefMut, Range};

use compact_str::ToCompactString;
use thiserror::Error;
use uuid::Uuid;
use winit::keyboard::KeyCode;

use crate::action_result::{ActionResult, IntoFailingActionResult, PassOrFail};
use crate::elements::element::NbtElement;
use crate::flags;
use crate::history::WorkbenchAction;
use crate::history::manager::HistoryMananger;
use crate::render::assets::{BASE_TEXT_Z, HEADER_SIZE, SELECTED_TEXT_SELECTION_Z, SELECTED_TEXT_Z, SELECTION_UV};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::alert::manager::{AlertManager, Alertable};
use crate::render::widget::text::{Cachelike, SelectedTextKeyResult, Text, get_cursor_idx};
use crate::tree::actions::AmbiguiousOpenElementError;
use crate::tree::actions::close::{CloseElementError, close_element};
use crate::tree::actions::expand::expand_element;
use crate::tree::actions::open::open_element;
use crate::tree::actions::rename::{RenameElementError, RenameElementResult, rename_element};
use crate::tree::actions::swap::{SwapElementErrorSameDepth, swap_element_same_depth};
use crate::tree::indices::{Indices, OwnedIndices};
use crate::tree::navigate::{NavigationError, NavigationInformation, ParentNavigationError, ParentNavigationInformationMut};
use crate::tree::traverse::{TraversalError, TraversalInformation};
use crate::tree::{MutableIndices, line_number_at};
use crate::util::StrExt;
use crate::workbench::tab::{FilePath, TabConstants};

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

// required so chunk coordinates function with the hardcoded spacing offset
static_assertions::const_assert_eq!(VertexBufferBuilder::CHAR_WIDTH[b':' as usize], VertexBufferBuilder::CHAR_WIDTH[b',' as usize]);

impl SelectedText {
	pub const PREFIXING_SPACE_WIDTH: usize = 4;
	pub const POSTFIXING_SPACE_WIDTH: usize = 4;

	pub fn from_raw(
		base_x: usize,
		mouse_x: usize,
		y: usize,
		key: Option<(String, TextColor)>,
		value: Option<(String, TextColor)>,
		seperator_color: TextColor,
		indices: OwnedIndices,
		cached_cursor_x: Option<usize>,
		snap_to_ends: bool,
	) -> Result<Self, SelectedTextConstructionError> {
		let key_width = key.as_ref().map_or(0, |k| k.0.width());
		let value_width = value.as_ref().map_or(0, |v| v.0.width());
		let has_key_and_value = key.as_ref().is_some_and(|k| k.1.is_editable()) && value.as_ref().is_some_and(|v| v.1.is_editable());
		let intersection_width = has_key_and_value as usize * ": ".width();
		let full_width = key_width + intersection_width + value_width;

		let (keyfix, prefix) = if let Some((key, key_color)) = key.clone() {
			(Some((key, key_color)), (": ".to_owned(), seperator_color))
		} else {
			(None, (String::new(), TextColor::White))
		};

		let (suffix, valuefix) = if let Some((value, value_color)) = value.clone() {
			((": ".to_owned(), seperator_color), Some((value, value_color)))
		} else {
			((String::new(), TextColor::White), None)
		};

		if let Some((key, key_color)) = key
			&& key_color.is_editable()
		{
			let x = mouse_x as isize - base_x as isize;
			if (-(Self::PREFIXING_SPACE_WIDTH as isize) <= x || snap_to_ends)
				&& (x.try_into().unwrap_or(0_usize) < key_width + if has_key_and_value { intersection_width / 2 } else { Self::POSTFIXING_SPACE_WIDTH } || !has_key_and_value && snap_to_ends)
			{
				let idx = get_cursor_idx(&key, x);
				return Ok(Self(Text::new(key, idx, true, SelectedTextAdditional {
					y,
					indices,
					value_color: key_color,
					keyfix: None,
					prefix: (String::new(), TextColor::White),
					suffix,
					valuefix,
					cached_cursor_x,
					uuid: Uuid::new_v4(),
				})))
			}
		}

		if let Some((value, value_color)) = value
			&& value_color.is_editable()
		{
			let x = mouse_x as isize - base_x as isize - key_width as isize - intersection_width as isize;
			if (-(if has_key_and_value { (intersection_width + 1) / 2 } else { Self::PREFIXING_SPACE_WIDTH } as isize) <= x || !has_key_and_value && snap_to_ends)
				&& (x.try_into().unwrap_or(0_usize) < value_width + Self::POSTFIXING_SPACE_WIDTH || snap_to_ends)
			{
				let idx = get_cursor_idx(&value, x);
				return Ok(Self(Text::new(value, idx, true, SelectedTextAdditional {
					y,
					indices,
					value_color,
					keyfix,
					prefix,
					suffix: (String::new(), TextColor::White),
					valuefix: None,
					cached_cursor_x,
					uuid: Uuid::new_v4(),
				})))
			}
		}

		Err(SelectedTextConstructionError::OutOfBounds {
			min_x: base_x,
			max_x: base_x + full_width,
			mouse_x,
		})
	}

	#[must_use]
	pub fn width(&self) -> usize { self.prefix.0.width() + self.keyfix.as_ref().map(|x| x.0.width()).unwrap_or(0) + self.value.width() + self.valuefix.as_ref().map(|x| x.0.width()).unwrap_or(0) + self.suffix.0.width() }

	#[must_use]
	pub fn end_x(&self, left_margin: usize) -> usize { self.indices.end_x(left_margin) + Self::PREFIXING_SPACE_WIDTH + self.width() }

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
		) -> ActionResult<SelectedTextKeyResult, SelectedTextInputError> {
			if key == KeyCode::ArrowUp {
				if flags & !flags!(Ctrl) == 0 {
					return this
						.move_up(consts, flags == flags!(Ctrl), root, path)
						.map(SelectedTextKeyResult::Action)
						.map_err(SelectedTextInputError::from)
						.pass_or_fail(SelectedTextInputError::is_generally_ignored);
				} else if flags == flags!(Ctrl + Shift) {
					return this
						.shift_up(consts, root, mi)
						.map(Some)
						.map(SelectedTextKeyResult::Action)
						.map_err(SelectedTextInputError::from)
						.pass_or_fail(SelectedTextInputError::is_generally_ignored);
				}
			}

			if key == KeyCode::ArrowDown {
				if flags & !flags!(Ctrl) == 0 {
					return this
						.move_down(consts, flags == flags!(Ctrl), root, path)
						.map(SelectedTextKeyResult::Action)
						.map_err(SelectedTextInputError::from)
						.pass_or_fail(SelectedTextInputError::is_generally_ignored)
				} else if flags == flags!(Ctrl + Shift) {
					return this
						.shift_down(consts, root, mi)
						.map(Some)
						.map(SelectedTextKeyResult::Action)
						.map_err(SelectedTextInputError::from)
						.pass_or_fail(SelectedTextInputError::is_generally_ignored);
				}
			}

			if key == KeyCode::ArrowLeft {
				if flags & !flags!(Ctrl) == 0 && this.selection.is_none() && this.cursor == 0 && this.keyfix.as_ref().is_some_and(|keyfix| keyfix.1.is_editable()) {
					return this
						.move_to_keyfix(consts, root, path)
						.map(SelectedTextKeyResult::Action)
						.map_err(SelectedTextInputError::from)
						.pass_or_fail(SelectedTextInputError::is_generally_ignored);
				}
				if flags & !flags!(Shift) == flags!(Alt) {
					return this
						.force_close(root, mi)
						.map(|_| None)
						.map(SelectedTextKeyResult::Action)
						.map_err(SelectedTextInputError::from)
						.pass_or_fail(SelectedTextInputError::is_generally_ignored);
				}
			}

			if key == KeyCode::ArrowRight {
				if flags & !flags!(Ctrl) == 0 && this.selection.is_none() && this.cursor == this.value.len() && this.valuefix.as_ref().is_some_and(|valuefix| valuefix.1.is_editable()) {
					return this
						.move_to_valuefix(consts, root, path)
						.map(SelectedTextKeyResult::Action)
						.map_err(SelectedTextInputError::from)
						.pass_or_fail(SelectedTextInputError::is_generally_ignored);
				}
				if (flags) & !flags!(Shift) == flags!(Alt) {
					return this
						.force_open((flags & !flags!(Alt)) == flags!(Shift), root, mi)
						.map(|_| None)
						.map(SelectedTextKeyResult::Action)
						.map_err(SelectedTextInputError::from)
						.pass_or_fail(SelectedTextInputError::is_generally_ignored);
				}
			}

			let cursor_before = this.cursor;
			let result = this.0.on_key_press(key, ch, flags).into();
			if this.cursor != cursor_before {
				this.recache_cached_cursor_x(consts);
			}
			ActionResult::Success(result)
		}
		match on_key_press0(self, key, ch, flags, consts, root, path, mi).alert_err(alerts) {
			ActionResult::Pass => ActionResult::Pass,
			ActionResult::Failure(e) => ActionResult::Failure(e),
			ActionResult::Success(SelectedTextKeyResult::NoAction) => ActionResult::Pass,
			ActionResult::Success(SelectedTextKeyResult::Action(action)) => {
				self.post_input();
				history.append_all(action);
				ActionResult::Success(false)
			}
			ActionResult::Success(SelectedTextKeyResult::Escape) => ActionResult::Success(true),
			ActionResult::Success(SelectedTextKeyResult::Finish) => {
				history.append_all(self.save(root, path).map_success(Some).flatten_pass(Ok(None)).alert_err(alerts).failure_on_err()?);
				ActionResult::Success(true)
			}
			ActionResult::Success(SelectedTextKeyResult::GenericAction) => {
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

	#[must_use]
	pub fn is_editing_key(&self) -> bool { self.keyfix.is_none() && self.prefix.0.is_empty() && !self.suffix.0.is_empty() && self.valuefix.is_some() }

	#[must_use]
	pub fn key_span(&self, left_margin: usize) -> Option<Range<usize>> {
		self.keyfix.as_ref().map(|keyfix| &*keyfix.0).or(Some(&*self.value).filter(|_| self.is_editing_key())).map(|key| {
			let start = self.indices.end_x(left_margin) + Self::PREFIXING_SPACE_WIDTH;
			let width = key.width();
			start..start + width
		})
	}

	#[must_use]
	pub fn is_editing_value(&self) -> bool { self.keyfix.is_some() && !self.prefix.0.is_empty() && self.suffix.0.is_empty() && self.valuefix.is_none() }

	#[must_use]
	pub fn value_span(&self, left_margin: usize) -> Option<Range<usize>> {
		self.valuefix
			.as_ref()
			.map(|valuefix| &*valuefix.0)
			.map(|valuefix| (self.indices.end_x(left_margin) + Self::PREFIXING_SPACE_WIDTH, valuefix))
			.or_else(|| {
				Some((
					self.indices.end_x(left_margin) + Self::PREFIXING_SPACE_WIDTH + self.keyfix.as_ref().map_or(0, |keyfix| keyfix.0.width()) + self.prefix.0.width(),
					&*self.value,
				))
			})
			.map(|(start, value)| start..start + value.width())
	}

	pub fn render(&self, builder: &mut VertexBufferBuilder, left_margin: usize) {
		let x = self.indices.end_x(left_margin) + Self::PREFIXING_SPACE_WIDTH;
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

		builder.draw_texture_z((x - Self::PREFIXING_SPACE_WIDTH - NbtElement::DEPTH_INCREMENT_WIDTH, y), SELECTED_TEXT_Z, SELECTION_UV, (16, 16));
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
		fn header(consts: TabConstants, root: &NbtElement, path: &FilePath, offset: usize, cached_cursor_x: Option<usize>, snap_to_ends: bool) -> Result<SelectedText, SelectedTextConstructionError> {
			let TabConstants { left_margin, .. } = consts;
			let name = path.name();
			let path_minus_name_width = path.path_str().width() - name.width();
			SelectedText::from_raw(
				left_margin + NbtElement::INITIAL_DEPTH_WIDTH + SelectedText::PREFIXING_SPACE_WIDTH,
				offset + path_minus_name_width,
				HEADER_SIZE,
				Some((path.path_str().to_string(), TextColor::TreeKey)),
				Some((root.value().0.into_owned(), TextColor::TreeValueDesc)),
				TextColor::TreeValueDesc,
				OwnedIndices::new(),
				cached_cursor_x,
				snap_to_ends,
			)
		}

		let TabConstants { left_margin, horizontal_scroll, .. } = consts;

		if y == 0 {
			return header(consts, root, path, mouse_x, cached_cursor_x, snap_to_ends)
		}

		if root.as_region().is_some_and(|region| region.is_grid_layout()) {
			return Err(SelectedTextConstructionError::Region)
		}

		let TraversalInformation { indices, depth, key, element, .. } = root.traverse(y, None)?;
		let target_x = Indices::end_x_from_depth(depth, left_margin) + Self::PREFIXING_SPACE_WIDTH;
		if element.as_chunk().is_some() && mouse_x < target_x - 4 {
			return Err(SelectedTextConstructionError::OutOfBounds {
				min_x: target_x,
				max_x: target_x + element.value_width(),
				mouse_x,
			})
		}
		let k = key.map(|x| (x.to_owned(), TextColor::TreeKey));
		let v = Some(element.value()).map(|(a, c)| (a.into_owned(), c));
		let seperator_color = element.seperator_color();

		SelectedText::from_raw(target_x, mouse_x + horizontal_scroll, y * 16 + HEADER_SIZE, k, v, seperator_color, indices, cached_cursor_x, snap_to_ends)
	}

	pub fn save(&self, root: &mut NbtElement, path: &mut FilePath) -> ActionResult<WorkbenchAction, SaveSelectedTextError> {
		if !self.editable {
			return ActionResult::Failure(SaveSelectedTextError::NonEditable)
		}

		let key = self.prefix.0.is_empty() && !self.suffix.0.is_empty();
		let (key, value) = if key { (Some(self.value.to_compact_string()), None) } else { (None, Some(self.value.clone())) };
		rename_element(root, self.indices.clone(), key, value, path).map_success(RenameElementResult::into_action).map_failure(|e| e.into())
	}
	pub fn move_to_keyfix(&mut self, consts: TabConstants, root: &mut NbtElement, path: &mut FilePath) -> Result<Option<WorkbenchAction>, MoveToKeyfixError> {
		if !self.editable {
			return Err(MoveToKeyfixError::Save(SaveSelectedTextError::NonEditable))
		}
		if self.valuefix.as_ref().is_some_and(|valuefix| valuefix.1.is_editable()) || !self.suffix.0.is_empty() {
			return Err(MoveToKeyfixError::AlreadyAtKey)
		}

		let action = self.save(root, path).map_success(Some).flatten_pass(Ok(None))?;

		let (keyfix, keyfix_color) = self.keyfix.take().ok_or(MoveToKeyfixError::NoKey)?;
		let old_prefix = core::mem::take(&mut self.prefix);

		self.cursor = keyfix.len();
		let old_value = core::mem::replace(&mut self.value, keyfix);
		let old_value_color = core::mem::replace(&mut self.value_color, keyfix_color);

		self.suffix = old_prefix;
		self.valuefix = Some((old_value, old_value_color));

		self.recache_cached_cursor_x(consts);

		Ok(action)
	}

	pub fn move_to_valuefix(&mut self, consts: TabConstants, root: &mut NbtElement, path: &mut FilePath) -> Result<Option<WorkbenchAction>, MoveToValuefixError> {
		if !self.editable {
			return Err(MoveToValuefixError::Save(SaveSelectedTextError::NonEditable))
		}
		if self.keyfix.as_ref().is_some_and(|keyfix| keyfix.1.is_editable()) || !self.prefix.0.is_empty() {
			return Err(MoveToValuefixError::AlreadyAtValue)
		}

		let action = self.save(root, path).map_success(Some).flatten_pass(Ok(None))?;

		let (valuefix, valuefix_color) = self.valuefix.take().ok_or(MoveToValuefixError::NoValue)?;
		let old_suffix = core::mem::take(&mut self.suffix);

		self.cursor = 0;
		let old_value = core::mem::replace(&mut self.value, valuefix);
		let old_value_color = core::mem::replace(&mut self.value_color, valuefix_color);

		self.prefix = old_suffix;
		self.keyfix = Some((old_value, old_value_color));

		self.recache_cached_cursor_x(consts);

		Ok(action)
	}

	fn move_text(&mut self, consts: TabConstants, root: &mut NbtElement, path: &mut FilePath, mut f: impl FnMut(usize, &NbtElement, &Indices) -> Result<usize, MoveSelectedTextError>) -> Result<Option<WorkbenchAction>, MoveSelectedTextError> {
		let TabConstants { left_margin, .. } = consts;

		let y = (self.y - HEADER_SIZE) / 16;
		let new_y = f(y, root, &self.indices)?;

		let mouse_x = self.cached_cursor_x.unwrap_or_else(|| self.cursor_x(left_margin));

		let new_selected_text = SelectedText::for_y(consts, root, path, new_y, mouse_x, true, Some(mouse_x))?;

		let action = self.save(root, path).map_success(Some).flatten_pass(Ok(None))?;

		*self = new_selected_text;

		Ok(action)
	}

	pub fn move_up(&mut self, consts: TabConstants, ctrl: bool, root: &mut NbtElement, path: &mut FilePath) -> Result<Option<WorkbenchAction>, MoveSelectedTextError> {
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

	pub fn move_down(&mut self, consts: TabConstants, ctrl: bool, root: &mut NbtElement, path: &mut FilePath) -> Result<Option<WorkbenchAction>, MoveSelectedTextError> {
		self.move_text(consts, root, path, |y, root, indices| {
			Ok(if ctrl && let Some((last_idx, parent_indices)) = indices.split_last() {
				let NavigationInformation { element: parent, line_number, .. } = root.navigate(&parent_indices).map_err(|e| MoveSelectedTextError::Navigation(e))?;
				let len = parent.len().ok_or_else(|| {
					MoveSelectedTextError::Save(SaveSelectedTextError::Rename(RenameElementError::Navigation(ParentNavigationError::Navigation(NavigationError::ParentWasPrimitive {
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

	pub fn force_close<'m1, 'm2: 'm1>(&self, root: &mut NbtElement, mi: &'m1 mut MutableIndices<'m2>) -> Result<(), CloseElementError> { close_element(root, &self.indices, mi) }

	pub fn force_open<'m1, 'm2: 'm1>(&self, expand: bool, root: &mut NbtElement, mi: &'m1 mut MutableIndices<'m2>) -> Result<(), AmbiguiousOpenElementError> {
		if expand { Ok(expand_element(root, &self.indices, mi)?) } else { Ok(open_element(root, &self.indices, mi)?) }
	}
}

#[derive(Debug, Error)]
pub enum SelectedTextConstructionError {
	#[error(transparent)]
	Traversal(#[from] TraversalError),
	#[error("Out of text bounds (min = {min_x}, max = {max_x}); mouse was at {mouse_x}")]
	OutOfBounds { min_x: usize, max_x: usize, mouse_x: usize },
	#[error("Cannot select chunk from grid view as selected text")]
	Region,
}

impl SelectedTextConstructionError {
	#[must_use]
	pub const fn is_generally_ignored(&self) -> bool {
		match self {
			Self::Traversal(e) => e.is_generally_ignored(),
			Self::OutOfBounds { .. } => true,
			Self::Region => true,
		}
	}
}

#[derive(Debug, Error)]
pub enum SaveSelectedTextError {
	#[error(transparent)]
	Rename(#[from] RenameElementError),
	#[error("Non-editable selected text")]
	NonEditable,
}

#[derive(Debug, Error)]
pub enum MoveSelectedTextError {
	#[error(transparent)]
	Save(#[from] SaveSelectedTextError),
	#[error(transparent)]
	Navigation(#[from] NavigationError),
	#[error("Could not create new selected text: {0}")]
	NoNewSelectedText(#[from] SelectedTextConstructionError),
}

impl MoveSelectedTextError {
	#[must_use]
	pub const fn is_generally_ignored(&self) -> bool {
		match self {
			Self::Save(_save) => false,
			Self::Navigation(_navigation) => false,
			Self::NoNewSelectedText(construction) => construction.is_generally_ignored(),
		}
	}
}

#[derive(Debug, Error)]
pub enum MoveToKeyfixError {
	#[error(transparent)]
	Save(#[from] SaveSelectedTextError),
	#[error("Tried to move to key but was indicated to be already at key")]
	AlreadyAtKey,
	#[error("No key found to move to")]
	NoKey,
}

#[derive(Debug, Error)]
pub enum MoveToValuefixError {
	#[error(transparent)]
	Save(#[from] SaveSelectedTextError),
	#[error("Tried to move to value but was indicated to be already at value")]
	AlreadyAtValue,
	#[error("No value found to move to")]
	NoValue,
}

#[derive(Debug, Error)]
pub enum ShiftSelectedTextError {
	#[error(transparent)]
	Navigation(#[from] ParentNavigationError),
	#[error(transparent)]
	SwapElementSameDepth(#[from] SwapElementErrorSameDepth),
	#[error("Invalid sibling index; original index: {original_index}")]
	InvalidSiblingIndex { original_index: usize },
}

#[derive(Debug, Error)]
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

impl SelectedTextInputError {
	#[must_use]
	pub const fn is_generally_ignored(&self) -> bool {
		match self {
			Self::CloseElement(_e) => false,
			Self::OpenElement(_e) => false,
			Self::ShiftSelectedText(_e) => false,
			Self::MoveToValuefix(_e) => false,
			Self::MoveToKeyfix(_e) => false,
			Self::MoveSelectedText(e) => e.is_generally_ignored(),
		}
	}
}

#[derive(Debug, Error)]
pub enum SelectedTextKeyValueError {
	#[error("This value is not valid for this type.")]
	InvalidValue,
	#[error("This key is not valid for this type.")]
	InvalidKey,
	#[error("This key is duplicate of another.")]
	DuplicateKey,
}
