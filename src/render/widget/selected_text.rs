use ControlFlow::{Break, Continue};
use std::fmt::Write;
use std::ops::{ControlFlow, Deref, DerefMut, Range};

use compact_str::ToCompactString;
use thiserror::Error;
use uuid::Uuid;
use winit::keyboard::KeyCode;

use crate::elements::element::NbtElement;
use crate::flags;
use crate::history::WorkbenchAction;
use crate::history::manager::HistoryMananger;
use crate::render::assets::{BASE_TEXT_Z, HEADER_SIZE, SELECTED_TEXT_SELECTION_Z, SELECTED_TEXT_Z, SELECTION_UV};
use crate::render::color::{TextColor, TextWithColor};
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::alert::manager::{AlertManager, Alertable};
use crate::render::widget::text::{Cache, SelectedTextKeyResult, Text, get_cursor_idx, KeyResult};
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

impl Cache<SelectedTextAdditional> for SelectedTextCache {
	fn new(text: &Text<SelectedTextAdditional, Self>) -> Self
	where Self: Sized {
		Self {
			keyfix: text.additional.keyfix.clone().map(|x| (x.text.into_boxed_str(), x.color)),
			valuefix: text.additional.valuefix.clone().map(|x| (x.text.into_boxed_str(), x.color)),
			value: text.value.clone().into_boxed_str(),
			cursor: text.cursor,
			selection: text.selection,
		}
	}

	fn revert(self, text: &mut Text<SelectedTextAdditional, Self>)
	where Self: Sized {
		let Self { keyfix, value, valuefix, cursor, selection } = self;
		text.additional.keyfix = keyfix.map(|(a, b)| TextWithColor::new(a.into_string(), b));
		text.additional.valuefix = valuefix.map(|(a, b)| TextWithColor::new(a.into_string(), b));
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
	pub keyfix: Option<TextWithColor>,
	pub prefix: TextWithColor,
	pub suffix: TextWithColor,
	pub valuefix: Option<TextWithColor>,
	pub cached_cursor_x: Option<usize>,
	pub uuid: Uuid,
}

// required so chunk coordinates function with the hardcoded spacing offset
static_assertions::const_assert_eq!(VertexBufferBuilder::CHAR_WIDTH[b':' as usize], VertexBufferBuilder::CHAR_WIDTH[b',' as usize]);

type ShouldRemove = bool;

fn width(text: &Option<TextWithColor>) -> usize {
	text.as_deref().map(String::as_str).map_or(0, str::width)
}

impl SelectedText {
	pub const PREFIXING_SPACE_WIDTH: usize = 4;
	pub const POSTFIXING_SPACE_WIDTH: usize = 4;

	fn try_select_key(
		base_x: usize,
		mouse_x: usize,
		y: usize,
		key: Option<TextWithColor>,
		value: Option<TextWithColor>,
		separator: Option<TextWithColor>,
		indices: OwnedIndices,
		cached_cursor_x: Option<usize>,
		snap_to_ends: bool,
	) -> ControlFlow<Self> {
		let Some(key) = key else { return Continue(()) };
		if key.color.is_non_editable() { return Continue(()) };

		// relative to start of key
		let x = mouse_x as isize - base_x as isize;

		let hitbox_suffix_width = separator.as_ref().map_or(Self::PREFIXING_SPACE_WIDTH, |sep| sep.text.width() / 2);
		let hitbox_lhs = -(Self::PREFIXING_SPACE_WIDTH as isize);
		let hitbox_rhs = key.text.width() + hitbox_suffix_width;
		let hitbox = hitbox_lhs..hitbox_rhs as _;
		let is_in_hitbox = hitbox.contains(&x);
		let is_within_bounds = is_in_hitbox || snap_to_ends;

		if !is_within_bounds {
			return Continue(());
		}

		let idx = get_cursor_idx(&key.text, x);
		Break(Self(Text::new(key.text, idx, true, SelectedTextAdditional {
			y,
			indices,
			value_color: key.color,
			keyfix: None,
			prefix: TextWithColor::default(),
			suffix: separator.unwrap_or_default(),
			valuefix: value,
			cached_cursor_x,
			uuid: Uuid::new_v4(),
		})))
	}

	fn try_select_value(
		base_x: usize,
		mouse_x: usize,
		y: usize,
		key: Option<TextWithColor>,
		value: Option<TextWithColor>,
		separator: Option<TextWithColor>,
		indices: OwnedIndices,
		cached_cursor_x: Option<usize>,
		snap_to_ends: bool,
	) -> ControlFlow<Self> {
		let Some(value) = value else { return Continue(()) };
		if value.color.is_non_editable() { return Continue(()) }

		// relative to start of value
		let x = mouse_x as isize - base_x as isize - width(&key) as isize - width(&separator) as isize;

		let hitbox_lhs = -(separator.as_ref().map_or(Self::PREFIXING_SPACE_WIDTH, |sep| sep.text.width() / 2) as isize);
		let hitbox_rhs = value.text.width() + Self::POSTFIXING_SPACE_WIDTH;
		let hitbox = hitbox_lhs..hitbox_rhs as _;
		let is_in_hitbox = hitbox.contains(&x);
		let is_within_bounds = is_in_hitbox || snap_to_ends;

		if !is_within_bounds {
			return Continue(());
		}

		let idx = get_cursor_idx(&value.text, x);
		Break(Self(Text::new(value.text, idx, true, SelectedTextAdditional {
			y,
			indices,
			value_color: value.color,
			prefix: separator.unwrap_or_default(),
			keyfix: key,
			suffix: TextWithColor::default(),
			valuefix: None,
			cached_cursor_x,
			uuid: Uuid::new_v4(),
		})))
	}

	pub fn from_raw(
		base_x: usize,
		mouse_x: usize,
		y: usize,
		key: Option<TextWithColor>,
		value: Option<TextWithColor>,
		separator: TextWithColor,
		indices: OwnedIndices,
		cached_cursor_x: Option<usize>,
		snap_to_ends: bool,
	) -> Result<Self, SelectedTextConstructionError> {
		let has_key_and_value = key.as_ref().is_some_and(|key| key.color.is_editable()) && value.as_ref().is_some_and(|value| value.color.is_editable());
		let separator = has_key_and_value.then_some(separator);
		let full_width = [&key, &separator, &value].map(Option::as_ref).into_iter().flatten().map(|x| x.text.width()).sum::<usize>();

		if let Break(result) = Self::try_select_key(base_x, mouse_x, y, key.clone(), value.clone(), separator.clone(), indices.clone(), cached_cursor_x, snap_to_ends) {
			return Ok(result)
		}

		if let Break(result) = Self::try_select_value(base_x, mouse_x, y, key, value, separator, indices, cached_cursor_x, snap_to_ends) {
			return Ok(result)
		}

		Err(SelectedTextConstructionError::OutOfBounds {
			min_x: base_x.saturating_sub(Self::PREFIXING_SPACE_WIDTH),
			max_x: base_x + full_width + Self::POSTFIXING_SPACE_WIDTH,
			mouse_x,
		})
	}

	#[must_use]
	pub fn width(&self) -> usize { width(&self.keyfix) + self.prefix.text.width() + self.value.width() + self.suffix.width() + width(&self.valuefix) }

	#[must_use]
	pub fn end_x(&self, left_margin: usize) -> usize { self.indices.end_x(left_margin) + Self::PREFIXING_SPACE_WIDTH + self.width() }

	#[must_use]
	pub fn cursor_x(&self, left_margin: usize) -> usize { self.indices.end_x(left_margin) + Self::PREFIXING_SPACE_WIDTH + self.prefix.text.width() + self.keyfix.as_ref().map_or(0, |x| x.text.width()) + self.value.split_at(self.cursor).0.width() }

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
	pub fn is_editing_key(&self) -> bool { self.keyfix.is_none() && self.prefix.text.is_empty() && !self.suffix.text.is_empty() && self.valuefix.is_some() }

	#[must_use]
	pub fn key_span(&self, left_margin: usize) -> Option<Range<usize>> {
		self.keyfix.as_ref().map(|keyfix| &*keyfix.text).or(Some(&*self.value).filter(|_| self.is_editing_key())).map(|key| {
			let start = self.indices.end_x(left_margin) + Self::PREFIXING_SPACE_WIDTH;
			let width = key.width();
			start..start + width
		})
	}

	#[must_use]
	pub fn is_editing_value(&self) -> bool { self.keyfix.is_some() && !self.prefix.text.is_empty() && self.suffix.text.is_empty() && self.valuefix.is_none() }

	#[must_use]
	pub fn value_span(&self, left_margin: usize) -> Option<Range<usize>> {
		self.valuefix
			.as_ref()
			.map(|valuefix| &*valuefix.text)
			.map(|valuefix| (self.indices.end_x(left_margin) + Self::PREFIXING_SPACE_WIDTH, valuefix))
			.or_else(|| {
				Some((
					self.indices.end_x(left_margin) + Self::PREFIXING_SPACE_WIDTH + self.keyfix.as_ref().map_or(0, |keyfix| keyfix.text.width()) + self.prefix.text.width(),
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

		let prefix_width = self.prefix.text.as_str().width() + self.keyfix.as_ref().map_or(0, |x| x.text.width());
		self.0.render(builder, self.value_color, (x + prefix_width, y).into(), SELECTED_TEXT_Z, SELECTED_TEXT_SELECTION_Z);

		builder.draw_texture_z((x - Self::PREFIXING_SPACE_WIDTH - NbtElement::DEPTH_INCREMENT_WIDTH, y), SELECTED_TEXT_Z, SELECTION_UV, (16, 16));
		builder.text_settings((x, y), false, BASE_TEXT_Z);
		if let Some(keyfix) = self.keyfix.as_ref() {
			builder.color = keyfix.color.to_raw();
			let _ = write!(builder, "{}", keyfix.text);
		}

		builder.color = self.prefix.color.to_raw();
		let _ = write!(builder, "{}", self.prefix.text);

		builder.text_settings((x + prefix_width + self.value.width(), y), false, BASE_TEXT_Z);

		builder.color = self.suffix.color.to_raw();
		let _ = write!(builder, "{}", self.suffix.text);

		if let Some(valuefix) = self.valuefix.as_ref() {
			builder.color = valuefix.color.to_raw();
			let _ = write!(builder, "{}", valuefix.text);
		}
	}

	pub fn for_header(consts: TabConstants, root: &NbtElement, path: &FilePath, offset: usize, cached_cursor_x: Option<usize>, snap_to_ends: bool) -> Result<SelectedText, SelectedTextConstructionError> {
		let TabConstants { left_margin, .. } = consts;
		let name = path.name();
		let path_minus_name_width = path.path_str().width() - name.width();
		SelectedText::from_raw(
			left_margin + NbtElement::INITIAL_DEPTH_WIDTH + SelectedText::PREFIXING_SPACE_WIDTH,
			offset + path_minus_name_width,
			HEADER_SIZE,
			Some(TextWithColor::new(path.path_str().to_string(), TextColor::TreeKey)),
			Some(TextWithColor::new(root.value().0.into_owned(), TextColor::TreeValueDesc)),
			TextWithColor::new(": ".to_owned(), TextColor::TreeValueDesc),
			OwnedIndices::new(),
			cached_cursor_x,
			snap_to_ends,
		)
	}

	pub fn for_y(consts: TabConstants, root: &NbtElement, path: &FilePath, y: usize, mouse_x: usize, snap_to_ends: bool, cached_cursor_x: Option<usize>) -> Result<SelectedText, SelectedTextConstructionError> {
		let TabConstants { left_margin, horizontal_scroll, .. } = consts;

		if y == 0 {
			return Self::for_header(consts, root, path, mouse_x, cached_cursor_x, snap_to_ends)
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
		let k = key.map(|x| TextWithColor::new(x.to_owned(), TextColor::TreeKey));
		let v = Some(element.value()).map(|(a, c)| TextWithColor::new(a.into_owned(), c));
		let separator = TextWithColor::new(": ".to_owned(), element.separator_color());

		SelectedText::from_raw(target_x, mouse_x + horizontal_scroll, y * 16 + HEADER_SIZE, k, v, separator, indices, cached_cursor_x, snap_to_ends)
	}

	pub fn save(&self, root: &mut NbtElement, path: &mut FilePath) -> Result<Option<WorkbenchAction>, SaveSelectedTextError> {
		if !self.editable {
			return Err(SaveSelectedTextError::NonEditable)
		}

		let key = self.prefix.text.is_empty() && !self.suffix.text.is_empty();
		let (key, value) = if key { (Some(self.value.to_compact_string()), None) } else { (None, Some(self.value.clone())) };
		Ok(rename_element(root, self.indices.clone(), key, value, path)?.map(RenameElementResult::into_action))
	}
	pub fn move_to_keyfix(&mut self, consts: TabConstants, root: &mut NbtElement, path: &mut FilePath) -> Result<Option<WorkbenchAction>, MoveToKeyfixError> {
		if !self.editable {
			return Err(MoveToKeyfixError::Save(SaveSelectedTextError::NonEditable))
		}
		if self.valuefix.as_ref().is_some_and(|valuefix| valuefix.color.is_editable()) || !self.suffix.text.is_empty() {
			return Err(MoveToKeyfixError::AlreadyAtKey)
		}

		let action = self.save(root, path)?;

		let keyfix = self.keyfix.take().ok_or(MoveToKeyfixError::NoKey)?;
		let old_prefix = core::mem::take(&mut self.prefix);

		self.cursor = keyfix.len();
		let old_value = core::mem::replace(&mut self.value, keyfix.text);
		let old_value_color = core::mem::replace(&mut self.value_color, keyfix.color);

		self.suffix = old_prefix;
		self.valuefix = Some(TextWithColor::new(old_value, old_value_color));

		self.recache_cached_cursor_x(consts);

		Ok(action)
	}

	pub fn move_to_valuefix(&mut self, consts: TabConstants, root: &mut NbtElement, path: &mut FilePath) -> Result<Option<WorkbenchAction>, MoveToValuefixError> {
		if !self.editable {
			return Err(MoveToValuefixError::Save(SaveSelectedTextError::NonEditable))
		}
		if self.keyfix.as_ref().is_some_and(|keyfix| keyfix.color.is_editable()) || !self.prefix.text.is_empty() {
			return Err(MoveToValuefixError::AlreadyAtValue)
		}

		let action = self.save(root, path)?;

		let valuefix = self.valuefix.take().ok_or(MoveToValuefixError::NoValue)?;
		let old_suffix = core::mem::take(&mut self.suffix);

		self.cursor = 0;
		let old_value = core::mem::replace(&mut self.value, valuefix.text);
		let old_value_color = core::mem::replace(&mut self.value_color, valuefix.color);

		self.prefix = old_suffix;
		self.keyfix = Some(TextWithColor::new(old_value, old_value_color));

		self.recache_cached_cursor_x(consts);

		Ok(action)
	}

	fn move_text(&mut self, consts: TabConstants, root: &mut NbtElement, path: &mut FilePath, mut f: impl FnMut(usize, &NbtElement, &Indices) -> Result<usize, MoveSelectedTextError>) -> Result<Option<WorkbenchAction>, MoveSelectedTextError> {
		let TabConstants { left_margin, .. } = consts;

		let y = (self.y - HEADER_SIZE) / 16;
		let new_y = f(y, root, &self.indices)?;

		let mouse_x = self.cached_cursor_x.unwrap_or_else(|| self.cursor_x(left_margin));

		let new_selected_text = SelectedText::for_y(consts, root, path, new_y, mouse_x, true, Some(mouse_x))?;

		let action = self.save(root, path)?;

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

impl SelectedText {
	fn try_move_up(
		&mut self,
		key: KeyCode,
		_ch: Option<char>,
		flags: u8,
		consts: TabConstants,
		root: &mut NbtElement,
		path: &mut FilePath,
		mi: &mut MutableIndices<'_>
	) -> ControlFlow<Result<SelectedTextKeyResult, SelectedTextInputError>> {
		if key == KeyCode::ArrowUp {
			if flags & !flags!(Ctrl) == 0 {
				return Break(self
					.move_up(consts, flags == flags!(Ctrl), root, path)
					.map(SelectedTextKeyResult::WorkbenchAction)
					.map_err(SelectedTextInputError::from))
			} else if flags == flags!(Ctrl + Shift) {
				return Break(self
					.shift_up(consts, root, mi)
					.map(Some)
					.map(SelectedTextKeyResult::WorkbenchAction)
					.map_err(SelectedTextInputError::from));
			}
		}

		Continue(())
	}

	fn try_move_down(
		&mut self,
		key: KeyCode,
		_ch: Option<char>,
		flags: u8,
		consts: TabConstants,
		root: &mut NbtElement,
		path: &mut FilePath,
		mi: &mut MutableIndices<'_>
	) -> ControlFlow<Result<SelectedTextKeyResult, SelectedTextInputError>> {
		if key == KeyCode::ArrowDown {
			if flags & !flags!(Ctrl) == 0 {
				return Break(self
					.move_down(consts, flags == flags!(Ctrl), root, path)
					.map(SelectedTextKeyResult::WorkbenchAction)
					.map_err(SelectedTextInputError::from))
			} else if flags == flags!(Ctrl + Shift) {
				return Break(self
					.shift_down(consts, root, mi)
					.map(Some)
					.map(SelectedTextKeyResult::WorkbenchAction)
					.map_err(SelectedTextInputError::from));
			}
		}

		Continue(())
	}

	fn try_move_left(
		&mut self,
		key: KeyCode,
		_ch: Option<char>,
		flags: u8,
		consts: TabConstants,
		root: &mut NbtElement,
		path: &mut FilePath,
		mi: &mut MutableIndices<'_>
	) -> ControlFlow<Result<SelectedTextKeyResult, SelectedTextInputError>> {
		if key == KeyCode::ArrowLeft {
			if flags & !flags!(Ctrl) == 0 && self.selection.is_none() && self.cursor == 0 && self.keyfix.as_ref().is_some_and(|keyfix| keyfix.color.is_editable()) {
				return Break(self
					.move_to_keyfix(consts, root, path)
					.map(SelectedTextKeyResult::WorkbenchAction)
					.map_err(SelectedTextInputError::from));
			}
			if flags & !flags!(Shift) == flags!(Alt) {
				return Break(self
					.force_close(root, mi)
					.map(|_| None)
					.map(SelectedTextKeyResult::WorkbenchAction)
					.map_err(SelectedTextInputError::from));
			}
		}

		Continue(())
	}

	fn try_move_right(
		&mut self,
		key: KeyCode,
		_ch: Option<char>,
		flags: u8,
		consts: TabConstants,
		root: &mut NbtElement,
		path: &mut FilePath,
		mi: &mut MutableIndices<'_>
	) -> ControlFlow<Result<SelectedTextKeyResult, SelectedTextInputError>> {
		if key == KeyCode::ArrowRight {
			if flags & !flags!(Ctrl) == 0 && self.selection.is_none() && self.cursor == self.value.len() && self.valuefix.as_ref().is_some_and(|valuefix| valuefix.color.is_editable()) {
				return Break(self
					.move_to_valuefix(consts, root, path)
					.map(SelectedTextKeyResult::WorkbenchAction)
					.map_err(SelectedTextInputError::from));
			}
			if (flags) & !flags!(Shift) == flags!(Alt) {
				return Break(self
					.force_open((flags & !flags!(Alt)) == flags!(Shift), root, mi)
					.map(|_| None)
					.map(SelectedTextKeyResult::WorkbenchAction)
					.map_err(SelectedTextInputError::from));
			}
		}

		Continue(())
	}

	fn on_key_press<'m1, 'm2: 'm1>(
		&mut self,
		key: KeyCode,
		ch: Option<char>,
		flags: u8,
		consts: TabConstants,
		root: &mut NbtElement,
		path: &mut FilePath,
		mi: &'m1 mut MutableIndices<'m2>,
	) -> ControlFlow<Result<SelectedTextKeyResult, SelectedTextInputError>> {
		self.try_move_up(key, ch, flags, consts, root, path, mi)?;
		self.try_move_down(key, ch, flags, consts, root, path, mi)?;
		self.try_move_left(key, ch, flags, consts, root, path, mi)?;
		self.try_move_right(key, ch, flags, consts, root, path, mi)?;

		let cursor_before = self.cursor;
		let Break(result) = self.0.on_key_press(key, ch, flags).map_break(SelectedTextKeyResult::from) else { return Continue(()) };
		if self.cursor != cursor_before {
			self.recache_cached_cursor_x(consts);
		}
		Break(Ok(result))
	}

	pub fn handle_key_press<'m1, 'm2: 'm1>(
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
	) -> ControlFlow<ShouldRemove> {
		let Break(result) = self.on_key_press(key, ch, flags, consts, root, path, mi) else { return Continue(()) };
		match result {
			Err(e) if !e.is_generally_ignored() => {
				alerts.alert(e);
				Break(true)
			},
			Err(_) => Continue(()),
			Ok(SelectedTextKeyResult::WorkbenchAction(action)) => {
				self.post_input();
				history.append_all(action);
				Break(false)
			}
			Ok(SelectedTextKeyResult::Generic(KeyResult::Escape)) => Break(true),
			Ok(SelectedTextKeyResult::Generic(KeyResult::Finish)) => {
				history.append_all(self.save(root, path).alert_err(alerts).flatten());
				Break(true)
			}
			Ok(SelectedTextKeyResult::Generic(KeyResult::GenericAction)) => {
				self.post_input();
				Break(false)
			}
		}
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
			Self::NoNewSelectedText(construction) => construction.is_generally_ignored(),
			_ => false,
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
			Self::MoveSelectedText(e) => e.is_generally_ignored(),
			_ => false,
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
