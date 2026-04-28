use ControlFlow::{Break, Continue};
use std::fmt::{Display, Formatter};
use std::ops::{ControlFlow, Deref, DerefMut};

use compact_str::{CompactString, ToCompactString};
use itertools::Either::{Left, Right};
use regex::Regex;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use winit::dpi::PhysicalSize;
use winit::event::MouseButton;
use winit::keyboard::KeyCode;

use crate::elements::compound::CompoundEntry;
use crate::elements::element::{NbtElement, SNBTParseError};
use crate::elements::{Matches, NbtElementAndKey, NbtElementAndKeyRef, NbtElementAndKeyRefMut};
use crate::history::WorkbenchAction;
use crate::render::assets::{DARK_STRIPE_UV, REPLACE_BOX_SELECTION_Z, REPLACE_BOX_Z, REPLACE_BY_BOOKMARKED_LINES, REPLACE_BY_SEARCH_HITS};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::alert::manager::AlertManager;
use crate::render::widget::notification::manager::NotificationManager;
use crate::render::widget::notification::{Notification, NotificationKind};
use crate::render::widget::search_box::{SEARCH_BOX_END_X, SEARCH_BOX_START_X, SearchBox, SearchFlags, SearchMode};
use crate::render::widget::text::{Cache, ReplaceBoxKeyResult, Text, get_cursor_idx, KeyResult};
use crate::render::window::Theme;
use crate::tree::actions::rename::{RenameElementError, rename_element};
use crate::tree::actions::replace::{ReplaceElementError, replace_element};
use crate::tree::indices::{Indices, OwnedIndices};
use crate::tree::{MutableIndices, indices_for_true};
use crate::util::{StrExt, Timestamp, Vec2u, create_regex};
use crate::workbench::SortAlgorithm;
use crate::workbench::tab::{FilePath, Tab};
use crate::{config, error, flags, mutable_indices};

#[derive(Copy, Clone, Default, Serialize, Deserialize)]
pub enum ReplaceBy {
	#[default]
	SearchHits,
	BookmarkedLines,
}

impl Display for ReplaceBy {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			Self::SearchHits => "Search Hits",
			Self::BookmarkedLines => "Bookmarked Lines",
		})
	}
}

impl ReplaceBy {
	#[must_use]
	pub fn cycle(self) -> Self {
		match self {
			Self::SearchHits => Self::BookmarkedLines,
			Self::BookmarkedLines => Self::SearchHits,
		}
	}

	#[must_use]
	pub fn rev_cycle(self) -> Self {
		match self {
			Self::SearchHits => Self::BookmarkedLines,
			Self::BookmarkedLines => Self::SearchHits,
		}
	}

	#[must_use]
	pub fn uv(self) -> Vec2u {
		match self {
			Self::SearchHits => REPLACE_BY_SEARCH_HITS,
			Self::BookmarkedLines => REPLACE_BY_BOOKMARKED_LINES,
		}
	}

	#[must_use]
	pub fn can_use_regex(self) -> bool {
		match self {
			Self::SearchHits => true,
			Self::BookmarkedLines => false,
		}
	}
}

pub struct ReplaceBox(Text<ReplaceBoxAdditional, ReplaceBoxCache>);

impl Deref for ReplaceBox {
	type Target = Text<ReplaceBoxAdditional, ReplaceBoxCache>;

	fn deref(&self) -> &Self::Target { &self.0 }
}

impl DerefMut for ReplaceBox {
	fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}

#[derive(Clone)]
pub struct ReplaceBoxAdditional {
	selected: bool,
	pub horizontal_scroll: usize,
	pub last_interaction: (usize, Timestamp),
}

#[derive(Clone, Eq)]
pub struct ReplaceBoxCache {
	value: String,
	cursor: usize,
	selection: Option<usize>,
}

impl PartialEq for ReplaceBoxCache {
	fn eq(&self, other: &Self) -> bool { self.value == other.value }
}

impl Cache<ReplaceBoxAdditional> for ReplaceBoxCache {
	fn new(text: &Text<ReplaceBoxAdditional, Self>) -> Self
	where Self: Sized {
		Self {
			value: text.value.clone(),
			cursor: text.cursor,
			selection: text.selection,
		}
	}

	fn revert(self, text: &mut Text<ReplaceBoxAdditional, Self>)
	where Self: Sized {
		let Self { value, cursor, selection } = self;

		text.value = value;
		text.cursor = cursor;
		text.selection = selection;
	}
}

impl ReplaceBox {
	pub const fn uninit() -> Self { Self(Text::uninit()) }

	pub fn new() -> Self {
		Self(Text::new(String::new(), 0, true, ReplaceBoxAdditional {
			selected: false,
			horizontal_scroll: 0,
			last_interaction: (0, Timestamp::UNIX_EPOCH),
		}))
	}

	pub fn render(&self, builder: &mut VertexBufferBuilder) {
		use std::fmt::Write;

		let search_mode = config::get_search_mode();
		let pos = Vec2u::new(SEARCH_BOX_START_X, 47);

		builder.draw_texture_region_z(pos, REPLACE_BOX_Z, DARK_STRIPE_UV, (builder.window_width() - SEARCH_BOX_END_X - pos.x, 22), (16, 16));

		builder.horizontal_scroll = self.horizontal_scroll;

		if self.value.is_empty() {
			builder.text_settings(pos + (0, 3), false, REPLACE_BOX_Z);
			builder.color = TextColor::Gray.to_raw();
			let _ = write!(builder, "{}", match search_mode {
				SearchMode::String => "Replace...",
				SearchMode::Regex => "Rep$1ce",
				SearchMode::Snbt => r#"{value: "replace", ...}"#,
			});
		}
		let color = match config::get_theme() {
			Theme::Light => TextColor::Black,
			Theme::Dark => TextColor::White,
		};
		if self.is_selected() {
			self.0.render(builder, color, pos + (0, 3), REPLACE_BOX_Z, REPLACE_BOX_SELECTION_Z);
		} else {
			builder.text_settings(pos + (0, 3), false, REPLACE_BOX_Z);
			builder.color = color.to_raw();
			let _ = write!(builder, "{}", self.value);
		}

		builder.horizontal_scroll = 0;
	}

	#[must_use]
	pub fn is_within_bounds(mouse: Vec2u, window_dims: PhysicalSize<u32>) -> bool {
		let pos = Vec2u::new(SEARCH_BOX_START_X, 47);

		(pos.x..window_dims.width as usize - SEARCH_BOX_END_X - 1).contains(&mouse.x) && (47..71).contains(&mouse.y)
	}

	#[must_use]
	pub fn is_visible(search_box: &SearchBox, replace_box: &ReplaceBox) -> bool { search_box.is_selected() || replace_box.is_selected() }

	pub fn deselect(&mut self) {
		self.selected = false;
		self.cursor = 0;
		self.selection = None;
	}

	pub fn select(&mut self, x: usize, button: MouseButton) {
		if button == MouseButton::Right {
			self.value.clear();
			self.cursor = 0;
			self.selection = None;
			self.horizontal_scroll = 0;
			self.0.post_input();
		} else {
			self.cursor = get_cursor_idx(&self.value, (x + self.horizontal_scroll) as isize);
			self.selection = None;
		}
		self.selected = true;
		self.interact();
	}

	#[must_use]
	pub fn is_selected(&self) -> bool { self.selected }

	pub fn post_input(&mut self, window_dims: PhysicalSize<u32>) {
		self.0.post_input();
		let field_width = window_dims.width as usize - SEARCH_BOX_END_X - SEARCH_BOX_START_X - 17 - 16 - 16;
		let precursor_width = self.value.split_at(self.cursor).0.width();
		// 8px space just to look cleaner
		let horizontal_scroll = (precursor_width + 8).saturating_sub(field_width);
		self.horizontal_scroll = horizontal_scroll;
	}

	fn on_key_press(&mut self, key: KeyCode, ch: Option<char>, flags: u8) -> ControlFlow<ReplaceBoxKeyResult> {
		if !self.is_selected() {
			return Continue(())
		}

		self.try_replace_all(key, ch, flags)?;
		self.try_move_search_box(key, ch, flags)?;

		self.0.on_key_press(key, ch, flags).map_break(Into::into)
	}

	fn try_replace_all(&mut self, key: KeyCode, _ch: Option<char>, flags: u8) -> ControlFlow<ReplaceBoxKeyResult> {
		if let KeyCode::Enter | KeyCode::NumpadEnter = key
			&& flags == flags!()
		{
			return Break(ReplaceBoxKeyResult::ReplaceAll);
		}

		Continue(())
	}

	fn try_move_search_box(&mut self, key: KeyCode, _ch: Option<char>, flags: u8) -> ControlFlow<ReplaceBoxKeyResult> {
		if let KeyCode::ArrowUp | KeyCode::Tab = key
			&& flags == flags!()
		{
			return Break(ReplaceBoxKeyResult::MoveToSearchBox);
		}

		Continue(())
	}

	pub fn handle_key_press(&mut self, key: KeyCode, ch: Option<char>, flags: u8, search_box: &mut SearchBox, tab: &mut Tab, _alerts: &mut AlertManager, notifications: &mut NotificationManager, window_dims: PhysicalSize<u32>) -> ControlFlow<()> {
		let Break(result) = self.on_key_press(key, ch, flags) else { return Continue(()) };
		match result {
			ReplaceBoxKeyResult::Generic(KeyResult::GenericAction) => {
				self.post_input(window_dims);
				Break(())
			}
			ReplaceBoxKeyResult::Generic(KeyResult::Escape | KeyResult::Finish) => {
				self.post_input(window_dims);
				self.deselect();
				Break(())
			}
			ReplaceBoxKeyResult::MoveToSearchBox => {
				self.post_input(window_dims);
				search_box.select(self.value.split_at(self.cursor).0.width().saturating_sub(self.horizontal_scroll), MouseButton::Left);
				self.deselect();
				Break(())
			}
			ReplaceBoxKeyResult::ReplaceAll => {
				let (notification, bulk) = self.replace(mutable_indices!(tab), &mut tab.root, search_box);
				if let Some(bulk) = bulk {
					tab.history.append(bulk);
				}
				notifications.notify(notification);
				self.post_input(window_dims);
				Break(())
			}
		}
	}

	#[must_use]
	pub fn replace<'m1, 'm2: 'm1>(&self, mi: &'m1 mut MutableIndices<'m2>, root: &mut NbtElement, search_box: &SearchBox) -> (Notification, Option<WorkbenchAction>) {
		let replace_by = config::get_replace_by();
		match replace_by {
			ReplaceBy::SearchHits => self.replace_by_search_box(mi, root, search_box),
			ReplaceBy::BookmarkedLines => self.replace_by_bookmarked_lines(mi, root),
		}
	}

	#[must_use]
	pub fn replace_by_search_box<'m1, 'm2: 'm1>(&self, mi: &'m1 mut MutableIndices<'m2>, root: &mut NbtElement, search_box: &SearchBox) -> (Notification, Option<WorkbenchAction>) {
		if search_box.value.is_empty() {
			return (Notification::new("0 replacements for \"\" (0ms) []", TextColor::White, NotificationKind::Replace), None);
		}

		let start = Timestamp::now();
		let Some(replacement) = SearchReplacement::new(search_box.value.clone(), self.value.clone()) else {
			return (Notification::new(format!("Invalid replacement syntax ({})", self.value), TextColor::Red, NotificationKind::Replace), None)
		};
		let (bulk, errors) = Self::replace_by_search_box0(mi, root, &replacement);
		let bulk_len = if let WorkbenchAction::Bulk { actions } = &bulk { actions.len() } else { 0 };
		let ms = start.elapsed();
		let errors_len = errors.len();
		for e in errors {
			error!("Error while replacing line: {e}");
		}
		(
			Notification::new(
				format!(
					"{replacements} replacement{suffix} for \"{search}\" ({ms}ms) [{errors_len} failure{error_suffix}]",
					replacements = bulk_len,
					suffix = if bulk_len == 1 { "" } else { "s" },
					error_suffix = if errors_len == 1 { "" } else { "s" },
					search = search_box.value,
					ms = ms.as_millis()
				),
				TextColor::White,
				NotificationKind::Replace,
			),
			Some(bulk).filter(|bulk| matches!(bulk, WorkbenchAction::Bulk { actions } if !actions.is_empty())),
		)
	}

	fn try_replace<'m1, 'm2: 'm1>((key, element): NbtElementAndKeyRef, replacement: &SearchReplacement, mi: &'m1 mut MutableIndices<'m2>, current_indices: &Indices, root: &mut NbtElement, actions: &mut Vec<WorkbenchAction>, errors: &mut Vec<ReplacementError>) -> Result<(), ()> {
		let mut element_replaced = false;
		if replacement.matches((key, element)) {
			let key_str = key.filter(|_| replacement.needs_key()).map(|s| s.to_owned());
			let element_str = if replacement.needs_element_snbt() {
				Some((element.to_string(), TextColor::White))
			} else if replacement.needs_element_value() {
				Some(element.value()).map(|(a, b)| (a.into_owned(), b))
			} else {
				None
			};
			match replacement.replace(root, key_str, element_str.filter(|&(_, color)| color != TextColor::TreeValueDesc).map(|(x, _)| x), mi, current_indices) {
				Break(Ok((action, replaced))) => {
					actions.push(action);
					element_replaced = replaced;
				}
				Continue(()) => {}
				Break(Err(e)) => errors.push(e),
			}
		}
		if element_replaced { Ok(()) } else { Err(()) }
	}

	#[must_use]
	pub(crate) fn replace_by_search_box0<'root, 'root2: 'root, 'm1, 'm2: 'm1>(mi: &'m1 mut MutableIndices<'m2>, root: &'root mut NbtElement, replacement: &SearchReplacement) -> (WorkbenchAction, Vec<ReplacementError>) {
		// SAFETY: the `alternative_root` ptr is used for writes in 2 different ways within `SearchReplacement::replace`:
		// `rename_element`
		// and `replace_element`
		// (both use to `current_indices`)
		//
		// for 'rename_element', the modified fields (that are accessed via `root`) are:
		// `NbtCompound.entries.key` (will be dropped) and (local) `element` (guaranteed to not be dropped)
		//
		// for `replace_element`, the modified fields (that are accessed via `root`) are:
		// `NbtCompound` / `NbtList` / `*Array` (parent) (guaranteed to not be dropped)
		// and the element at `current_indices` (guaranteed to not be dropped; will move memory address to the resulting WorkbenchAction)
		// therefore, in the case of all writes, (as long as `element` isn't read after it is replaced)
		let alternative_root: &'root2 mut NbtElement = unsafe { (&raw const root).cast::<&'root2 mut NbtElement>().read() };

		let mut current_indices = OwnedIndices::new();
		let mut indices_max = vec![];
		let mut actions = vec![];
		let mut queue: Vec<NbtElementAndKeyRef> = vec![(None, root)];
		let mut errors = Vec::new();

		while let Some((key, element)) = queue.pop() {
			let replacement = ReplaceBox::try_replace((key, element), replacement, mi, &current_indices, alternative_root, &mut actions, &mut errors);
			match replacement {
				Ok(()) => Self::next_line_contracted(&mut current_indices, &mut indices_max),
				Err(()) => Self::next_line_expanded(&mut current_indices, &mut indices_max, &mut queue, element),
			}
		}
		(WorkbenchAction::Bulk { actions: actions.into_boxed_slice() }, errors)
	}

	/// Moves to the sibling node, if not present, then parent's sibling, if not present, grandparent's sibling, etc.
	///
	/// Will not go inside current node
	///
	/// ## Examples
	/// ```json
	/// {
	///     "people": [
	///         {
	///             "name": "Alice", // current line
	///             "initial": "A", // next line
	///         },
	///         {
	///             "name": "Bob",
	///             "initial": "B",
	///         }
	///     ]
	/// }
	/// ```
	/// ```json
	/// {
	///     "people": [
	///         {
	///             "name": "Alice",
	///             "initial": "A", // current line
	///         },
	///         {
	///             "name": "Bob", // next line
	///             "initial": "B",
	///         }
	///     ]
	/// }
	/// ```
	/// ```json
	/// {
	///     "people": [
	///         {
	///             "name": "Alice",
	///             "initial": "A",
	///         },
	///         {
	///             "bonus_data": { // current line
	///                 "crush": "Alice",
	///             },
	///             "name": "Bob", // next line
	///             "initial": "B",
	///         }
	///     ],
	/// }
	/// ```
	fn next_line_contracted(current_indices: &mut OwnedIndices, indices_max: &mut Vec<usize>) {
		while let Some(idx) = current_indices.last_mut() && let Some(len) = indices_max.last().copied() {
			if *idx + 1 == len {
				indices_max.pop();
				current_indices.pop();
			} else {
				*idx += 1;
			}
		}
	}

	/// Moves the queue and indices into the current element's children nodes, setting up iteration through those.
	///
	/// Will go inside current node
	///
	/// Two cases:
	///
	/// 1. Has children:\
	/// 1a. Adds all children to the queue -- making the first thing in the queue the next child.\
	/// 1b. Adds `indices_max` value of length of current depth
	///
	/// 2. Has no children: [`Self::next_line_contracted`]
	///
	/// ## Examples
	/// ```json
	/// {
	///     "people": [
	///         {
	///             "name": "Alice", // current line
	///             "initial": "A", // next line
	///         },
	///         {
	///             "name": "Bob",
	///             "initial": "B",
	///         }
	///     ]
	/// }
	/// ```
	/// ```json
	/// {
	///     "people": [
	///         {
	///             "name": "Alice",
	///             "initial": "A", // current line
	///         },
	///         {
	///             "name": "Bob", // next line
	///             "initial": "B",
	///         }
	///     ]
	/// }
	/// ```
	/// ```json
	/// {
	///     "people": [
	///         {
	///             "name": "Alice",
	///             "initial": "A",
	///         },
	///         {
	///             "bonus_data": { // current line
	///                 "crush": "Alice", // next line
	///             },
	///             "name": "Bob",
	///             "initial": "B",
	///         }
	///     ],
	/// }
	/// ```
	fn next_line_expanded<'root>(current_indices: &mut OwnedIndices, indices_max: &mut Vec<usize>, queue: &mut Vec<NbtElementAndKeyRef<'root>>, element: &'root NbtElement) {
		match element.children() {
			Some(Left(iter)) => {
				let mut len = 0_usize;
				for value in iter.rev() {
					queue.push((None, value));
					len += 1;
				}
				indices_max.push(len);
				current_indices.push(0_usize);
			}
			Some(Right(iter)) => {
				let mut len = 0_usize;
				for CompoundEntry { key, value } in iter.rev() {
					queue.push((Some(key), value));
					len += 1;
				}
				indices_max.push(len);
				current_indices.push(0_usize);
			}
			None => Self::next_line_contracted(current_indices, indices_max),
		}
	}

	#[must_use]
	pub fn replace_by_bookmarked_lines<'m1, 'm2: 'm1>(&self, mi: &'m1 mut MutableIndices<'m2>, root: &mut NbtElement) -> (Notification, Option<WorkbenchAction>) {
		let start = Timestamp::now();
		let replacement = match BookmarkedBasedSearchReplacement::new(&self.value) {
			Ok(replacement) => replacement,
			Err(e) => return (Notification::new(format!("Invalid replacement syntax: {e}"), TextColor::Red, NotificationKind::Replace), None),
		};
		let (bulk, errors) = Self::replace_by_bookmarked_lines0(mi, root, &replacement);
		let bulk_len = if let WorkbenchAction::Bulk { actions } = &bulk { actions.len() } else { 0 };
		let ms = start.elapsed();
		let errors_len = errors.len();
		for e in errors {
			error!("Error while replacing bookmarked line: {e}")
		}
		(
			Notification::new(
				format!(
					"{replacements} replacement{suffix} ({ms}ms) [{errors_len} error{error_suffix}]",
					replacements = bulk_len,
					suffix = if bulk_len == 1 { "" } else { "s" },
					error_suffix = if errors_len == 1 { "" } else { "s" },
					ms = ms.as_millis()
				),
				TextColor::White,
				NotificationKind::Replace,
			),
			Some(bulk).filter(|bulk| matches!(bulk, WorkbenchAction::Bulk { actions } if !actions.is_empty())),
		)
	}

	#[must_use]
	pub fn replace_by_bookmarked_lines0<'m1, 'm2: 'm1>(old_mi: &'m1 mut MutableIndices<'m2>, root: &mut NbtElement, replacement: &BookmarkedBasedSearchReplacement) -> (WorkbenchAction, Vec<ReplacementError>) {
		// the `rev` is done so that pop (O(1) time) removes the first element rather than the last
		let mut bookmark_indices = old_mi.bookmarks.iter().rev().map(|bookmark| indices_for_true(bookmark.true_line_number(), root)).collect::<Vec<_>>();
		let mut mutable_indices = MutableIndices::new(old_mi.subscription, old_mi.selected_text, old_mi.bookmarks);
		mutable_indices.temp = bookmark_indices.iter_mut().collect::<Vec<_>>();

		let mut fake_path = FilePath::new("dummy.nbt").expect("Expected dummy value to be valid");

		let mut actions = Vec::new();
		let mut errors = Vec::new();

		while let Some(indices) = mutable_indices.temp.pop() {
			let Some(indices) = indices.take() else { continue };
			match replacement.replace(root, indices, &mut fake_path, &mut mutable_indices) {
				Break(Ok(action)) => actions.push(action),
				Continue(()) => {}
				Break(Err(e)) => errors.push(e),
			}
		}

		(WorkbenchAction::Bulk { actions: actions.into_boxed_slice() }, errors)
	}
}

pub struct SearchReplacement {
	search_flags: SearchFlags,
	inner: SearchReplacementInner,
}

pub enum SearchReplacementInner {
	Substring { find: String, replacement: String, case_sensitive: bool },
	Regex { regex: Regex, replacement: String },
	Snbt { find: NbtElementAndKey, replacement: NbtElementAndKey, exact_match: bool },
}

impl SearchReplacement {
	pub fn new(find: String, replacement: String) -> Option<Self> {
		let search_mode = config::get_search_mode();
		let search_flags = config::get_search_flags();
		let exact_match = config::get_search_exact_match();

		let inner = match search_mode {
			SearchMode::String => SearchReplacementInner::Substring { find: if exact_match { find.clone() } else { find.to_lowercase() }, replacement, case_sensitive: exact_match },
			SearchMode::Regex if let Some(regex) = create_regex(find.clone(), exact_match) => SearchReplacementInner::Regex { regex, replacement },
			SearchMode::Snbt if let Ok((find, replacement)) = {
				let sort = config::set_sort_algorithm(SortAlgorithm::None);
				let find = NbtElement::from_str(&find);
				config::set_sort_algorithm(sort);
				let replacement = NbtElement::from_str(&replacement);
				find.and_then(|find| replacement.map(|replacement| (find, replacement)))
			} => SearchReplacementInner::Snbt { find, replacement, exact_match },
			_ => return None,
		};

		Some(Self {
			inner,
			search_flags
		})
	}

	pub fn matches(&self, kv: NbtElementAndKeyRef) -> bool {
		let flags = self.search_flags as u8 + 1;
		let value_flag = (flags & 0b01) > 0;
		let key_flag = (flags & 0b10) > 0;
		match &self.inner {
			SearchReplacementInner::Substring { find, case_sensitive, .. } => {
				let (value, color) = kv.1.value();
				if *case_sensitive {
					(value_flag && color.is_non_editable() && value.contains(find)) || (key_flag && kv.0.is_some_and(|k| k.contains(find)))
				} else {
					(value_flag && color.is_non_editable() && value.contains_ignore_ascii_case(find)) || (key_flag && kv.0.is_some_and(|k| k.contains_ignore_ascii_case(find)))
				}
			}
			SearchReplacementInner::Regex { regex, .. } => {
				let (value, color) = kv.1.value();
				(value_flag && color.is_non_editable() && regex.is_match(&value)) || (key_flag && kv.0.is_some_and(|k| regex.is_match(k)))
			}
			SearchReplacementInner::Snbt { find: (find_key, find_value), exact_match, .. } =>
				if *exact_match {
					(!value_flag || kv.1.eq(find_value)) && (!key_flag || kv.0 == find_key.as_ref().map(|k| k.as_str()))
				} else {
					(!value_flag || kv.1.matches(find_value)) && (!key_flag || kv.0 == find_key.as_ref().map(|k| k.as_str()))
				},
		}
	}

	pub fn needs_key(&self) -> bool {
		let flags = self.search_flags as u8 + 1;
		(flags & 0b10) > 0
	}

	pub fn needs_element_snbt(&self) -> bool {
		let flags = self.search_flags as u8 + 1;
		(flags & 0b01) > 0 && matches!(self.inner, SearchReplacementInner::Snbt { .. })
	}

	pub fn needs_element_value(&self) -> bool {
		let flags = self.search_flags as u8 + 1;
		(flags & 0b01) > 0 && !matches!(self.inner, SearchReplacementInner::Snbt { .. })
	}

	pub fn replace<'m1, 'm2: 'm1>(&self, root: &mut NbtElement, key: Option<String>, value: Option<String>, mi: &'m1 mut MutableIndices<'m2>, indices: &Indices) -> ControlFlow<Result<(WorkbenchAction, bool), ReplacementError>> {
		fn rename(root: &mut NbtElement, indices: &Indices, path: &mut FilePath, key: Option<CompactString>, value: Option<String>) -> ControlFlow<Result<(WorkbenchAction, bool), ReplacementError>> {
			match rename_element(root, indices.to_owned(), key, value, path) {
				Ok(Some(result)) => Break(Ok((result.into_action(), false))),
				Ok(None) => Continue(()),
				Err(e) => Break(Err(e.into())),
			}
		}

		#[must_use]
		fn replace_case_sensitivity(value: &str, find: &str, replacement: &str, case_sensitive: bool) -> String { if case_sensitive { value.replace(find, replacement) } else { value.replace_ignore_ascii_case(find, replacement) } }

		// we don't use the real path because aren't able to modify it
		let mut fake_path = FilePath::new("dummy.nbt").expect("Expected dummy value to be valid");
		match &self.inner {
			SearchReplacementInner::Substring { find, replacement, case_sensitive } => rename(
				root, indices, &mut fake_path,
				key.map(|key| replace_case_sensitivity(&key, find, replacement, *case_sensitive).into()),
				value.map(|value| replace_case_sensitivity(&value, find, replacement, *case_sensitive)),
			),
			SearchReplacementInner::Regex { regex, replacement } => rename(
				root, indices, &mut fake_path,
				key.map(|key| regex.replace_all(&key, replacement).into()),
				value.map(|value| regex.replace_all(&value, replacement).into()),
			),
			SearchReplacementInner::Snbt { replacement, .. } => match replace_element(root, replacement.clone(), indices.to_owned(), mi) {
				Ok(result) => Break(Ok((result.into_action(), true))),
				Err(e) => Break(Err(e.into())),
			},
		}
	}
}

pub struct BookmarkedBasedSearchReplacement {
	search_flags: SearchFlags,
	inner: BookmarkedBasedSearchReplacementInner,
}

pub enum BookmarkedBasedSearchReplacementInner {
	String(String),
	Snbt(NbtElementAndKey),
}

impl BookmarkedBasedSearchReplacement {
	pub fn new(value: &str) -> Result<Self, SNBTParseError> {
		let search_mode = config::get_search_mode();
		let search_flags = config::get_search_flags();

		let inner = match search_mode {
			SearchMode::String => BookmarkedBasedSearchReplacementInner::String(value.to_owned()),
			SearchMode::Regex => BookmarkedBasedSearchReplacementInner::String(value.to_owned()),
			SearchMode::Snbt => BookmarkedBasedSearchReplacementInner::Snbt(NbtElement::from_str(value)?),
		};

		Ok(Self { search_flags, inner })
	}

	pub fn replace<'m1, 'm2: 'm1>(&self, root: &mut NbtElement, indices: OwnedIndices, path: &mut FilePath, mi: &'m1 mut MutableIndices<'m2>) -> ControlFlow<Result<WorkbenchAction, ReplacementError>> {
		match &self.inner {
			BookmarkedBasedSearchReplacementInner::String(str) => {
				let key = self.search_flags.has_key().then(|| str.to_compact_string());
				let value = self.search_flags.has_value().then(|| str.to_owned());
				let action = match rename_element(root, indices.to_owned(), key, value, path) {
					Ok(Some(result)) => result.into_action(),
					Ok(None) => return Continue(()),
					Err(e) => return Break(Err(e.into())),
				};
				Break(Ok(action))
			}
			BookmarkedBasedSearchReplacementInner::Snbt(replacement) => {
				let action = match replace_element(root, replacement.clone(), indices, mi) {
					Ok(result) => result.into_action(),
					Err(e) => return Break(Err(e.into())),
				};
				Break(Ok(action))
			},
		}
	}
}

#[derive(Debug, Error)]
pub enum ReplacementError {
	#[error(transparent)]
	Rename(#[from] RenameElementError),
	#[error(transparent)]
	Replace(#[from] ReplaceElementError),
}
