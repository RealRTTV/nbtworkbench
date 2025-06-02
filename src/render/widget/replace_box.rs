use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};
use std::time::Duration;

use compact_str::ToCompactString;
use regex::Regex;
use serde::{Deserialize, Serialize};
use winit::event::MouseButton;
use winit::keyboard::KeyCode;

use crate::assets::{DARK_STRIPE_UV, REPLACE_BOX_SELECTION_Z, REPLACE_BOX_Z, REPLACE_BY_BOOKMARKED_LINES, REPLACE_BY_SEARCH_HITS};
use crate::elements::{CompoundEntry, Matches, NbtElement, NbtElementAndKey, NbtElementAndKeyRef};
use crate::render::widget::text::get_cursor_idx;
use crate::render::{TextColor, Theme, VertexBufferBuilder, WindowProperties};
use crate::tree::{Indices, MutableIndices, OwnedIndices, RenameElementResult, ReplaceElementResult, indices_for_true, rename_element, replace_element};
use crate::util::{StrExt, Vec2u, create_regex, now};
use crate::widget::{Cachelike, Notification, NotificationKind, ReplaceBoxKeyResult, SEARCH_BOX_END_X, SEARCH_BOX_START_X, SearchBox, SearchFlags, SearchMode, Text};
use crate::workbench::{MarkedLines, SortAlgorithm, WorkbenchAction};
use crate::{config, flags};

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
	pub last_interaction: (usize, Duration),
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

impl Cachelike<ReplaceBoxAdditional> for ReplaceBoxCache {
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
			last_interaction: (0, Duration::ZERO),
		}))
	}

	pub fn render(&self, builder: &mut VertexBufferBuilder) {
		use std::fmt::Write;

		let search_mode = config::get_search_mode();
		let pos = Vec2u::new(SEARCH_BOX_START_X, 47);

		builder.draw_texture_region_z(pos, REPLACE_BOX_Z, DARK_STRIPE_UV, (builder.window_width() - SEARCH_BOX_END_X - pos.x, 22), (16, 16));

		builder.horizontal_scroll = self.horizontal_scroll;

		if self.value.is_empty() {
			builder.settings(pos + (0, 3), false, REPLACE_BOX_Z);
			builder.color = TextColor::Gray.to_raw();
			let _ = write!(builder, "{}", match search_mode {
				SearchMode::String => r#"Replace..."#,
				SearchMode::Regex => r#"Rep$1ce"#,
				SearchMode::Snbt => r#"{value: "replace", ...}"#,
			});
		}
		let color = match config::get_theme() {
			Theme::Light => TextColor::Black,
			Theme::Dark => TextColor::White,
		};
		if self.is_selected() {
			self.0
				.render(builder, color, pos + (0, 3), REPLACE_BOX_Z, REPLACE_BOX_SELECTION_Z);
		} else {
			builder.settings(pos + (0, 3), false, REPLACE_BOX_Z);
			builder.color = color.to_raw();
			let _ = write!(builder, "{}", self.value);
		}

		builder.horizontal_scroll = 0;
	}

	#[must_use]
	pub fn is_within_bounds(mouse: (usize, usize), window_width: usize) -> bool {
		let (mouse_x, mouse_y) = mouse;
		let pos = Vec2u::new(SEARCH_BOX_START_X, 47);

		(pos.x..window_width - SEARCH_BOX_END_X - 1).contains(&mouse_x) && (47..71).contains(&mouse_y)
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

	pub fn post_input(&mut self, window_dims: (usize, usize)) {
		let (window_width, _) = window_dims;
		self.0.post_input();
		let field_width = window_width - SEARCH_BOX_END_X - SEARCH_BOX_START_X - 17 - 16 - 16;
		let precursor_width = self.value.split_at(self.cursor).0.width();
		// 8px space just to look cleaner
		let horizontal_scroll = (precursor_width + 8).saturating_sub(field_width);
		self.horizontal_scroll = horizontal_scroll;
	}

	#[must_use]
	pub fn on_key_press(&mut self, key: KeyCode, char: Option<char>, flags: u8) -> ReplaceBoxKeyResult {
		if let KeyCode::Enter | KeyCode::NumpadEnter = key
			&& flags == flags!()
		{
			return ReplaceBoxKeyResult::ReplaceAll;
		}
		if let KeyCode::ArrowUp | KeyCode::Tab = key
			&& flags == flags!()
		{
			return ReplaceBoxKeyResult::MoveToSearchBox;
		}
		self.0.on_key_press(key, char, flags).into()
	}

	#[must_use]
	pub fn replace<'m1, 'm2: 'm1>(&self, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>, root: &mut NbtElement, search_box: &SearchBox) -> (Notification, Option<WorkbenchAction>) {
		let replace_by = config::get_replace_by();
		match replace_by {
			ReplaceBy::SearchHits => self.replace_by_search_box(bookmarks, mutable_indices, root, search_box),
			ReplaceBy::BookmarkedLines => self.replace_by_bookmarked_lines(bookmarks, mutable_indices, root),
		}
	}

	#[must_use]
	pub fn replace_by_search_box<'m1, 'm2: 'm1>(&self, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>, root: &mut NbtElement, search_box: &SearchBox) -> (Notification, Option<WorkbenchAction>) {
		if search_box.value.is_empty() {
			return (Notification::new("0 replacements for \"\" (0ms) []", TextColor::White, NotificationKind::Replace), None);
		}

		let start = now();
		let Some(replacement) = SearchReplacement::new(search_box.value.clone(), self.value.clone()) else {
			return (Notification::new(format!("Invalid replacement syntax ({})", self.value), TextColor::Red, NotificationKind::Replace), None)
		};
		let (bulk, failures) = Self::replace_by_search_box0(bookmarks, mutable_indices, root, &replacement);
		let bulk_len = if let WorkbenchAction::Bulk { actions } = &bulk { actions.len() } else { 0 };
		let ms = now() - start;
		(
			Notification::new(
				format!(
					"{replacements} replacement{suffix} for \"{search}\" ({ms}ms) [{failures} failure{failure_suffix}]",
					replacements = bulk_len,
					suffix = if bulk_len == 1 { "" } else { "s" },
					failure_suffix = if failures == 1 { "" } else { "s" },
					search = search_box.value,
					ms = ms.as_millis()
				),
				TextColor::White,
				NotificationKind::Replace,
			),
			Some(bulk).filter(|bulk| matches!(bulk, WorkbenchAction::Bulk { actions } if !actions.is_empty())),
		)
	}

	#[must_use]
	pub fn replace_by_search_box0<'root, 'root2: 'root, 'm1, 'm2: 'm1>(bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>, root: &'root mut NbtElement, replacement: &SearchReplacement) -> (WorkbenchAction, usize) {
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
		let alternative_root: &'root2 mut NbtElement = unsafe {
			(&raw const root)
				.cast::<&'root2 mut NbtElement>()
				.read()
		};

		let mut current_indices = OwnedIndices::new();
		let mut indices_max = vec![];
		let mut actions = vec![];
		let mut queue: Vec<NbtElementAndKeyRef> = vec![(None, root)];
		let mut failures = 0;

		while let Some((key, element)) = queue.pop() {
			let mut element_replaced = false;
			if replacement.matches((key, element)) {
				let key_str = key
					.filter(|_| replacement.needs_key())
					.map(|s| s.to_owned());
				let element_str = if replacement.needs_element_snbt() {
					Some((element.to_string(), TextColor::White))
				} else if replacement.needs_element_value() {
					Some(element.value()).map(|(a, b)| (a.into_owned(), b))
				} else {
					None
				};
				if let (Some(action), replaced) = replacement.replace(
					alternative_root,
					key_str,
					element_str
						.filter(|&(_, color)| color != TextColor::TreeKey)
						.map(|(x, _)| x),
					bookmarks,
					mutable_indices,
					&current_indices,
				) {
					actions.push(action);
					element_replaced |= replaced;
				} else {
					failures += 1;
				}
			}

			if element_replaced {
				while let Some(idx) = current_indices.last_mut()
					&& let Some(len) = indices_max.last().copied()
				{
					if *idx + 1 == len {
						indices_max.pop();
						current_indices.pop();
					} else {
						*idx += 1;
					}
				}
			} else {
				match element.children() {
					Some(Ok(iter)) => {
						let mut len = 0_usize;
						for value in iter.rev() {
							queue.push((None, value));
							len += 1;
						}
						indices_max.push(len);
						current_indices.push(0_usize);
					}
					Some(Err(iter)) => {
						let mut len = 0_usize;
						for CompoundEntry { key, value } in iter.rev() {
							queue.push((Some(key), value));
							len += 1;
						}
						indices_max.push(len);
						current_indices.push(0_usize);
					}
					None => {
						while let Some(idx) = current_indices.last_mut()
							&& let Some(len) = indices_max.last().copied()
						{
							if *idx + 1 == len {
								indices_max.pop();
								current_indices.pop();
							} else {
								*idx += 1;
							}
						}
					}
				}
			}
		}
		(WorkbenchAction::Bulk { actions: actions.into_boxed_slice() }, failures)
	}

	#[must_use]
	pub fn replace_by_bookmarked_lines<'m1, 'm2: 'm1>(&self, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>, root: &mut NbtElement) -> (Notification, Option<WorkbenchAction>) {
		let start = now();
		let Some(replacement) = BookmarkedBasedSearchReplacement::new(&self.value) else {
			return (Notification::new(format!("Invalid replacement syntax ({})", self.value), TextColor::Red, NotificationKind::Replace), None)
		};
		let (bulk, failures) = Self::replace_by_bookmarked_lines0(bookmarks, mutable_indices, root, &replacement);
		let bulk_len = if let WorkbenchAction::Bulk { actions } = &bulk { actions.len() } else { 0 };
		let ms = now() - start;
		(
			Notification::new(
				format!(
					"{replacements} replacement{suffix} ({ms}ms) [{failures} failure{failure_suffix}]",
					replacements = bulk_len,
					suffix = if bulk_len == 1 { "" } else { "s" },
					failure_suffix = if failures == 1 { "" } else { "s" },
					ms = ms.as_millis()
				),
				TextColor::White,
				NotificationKind::Replace,
			),
			Some(bulk).filter(|bulk| matches!(bulk, WorkbenchAction::Bulk { actions } if !actions.is_empty())),
		)
	}

	#[must_use]
	pub fn replace_by_bookmarked_lines0<'m1, 'm2: 'm1>(bookmarks: &mut MarkedLines, old_mutable_indices: &'m1 mut MutableIndices<'m2>, root: &mut NbtElement, replacement: &BookmarkedBasedSearchReplacement) -> (WorkbenchAction, usize) {
		// the `rev` is done so that pop (O(1) time) removes the first element rather than the last
		let mut bookmark_indices = bookmarks
			.iter()
			.rev()
			.map(|bookmark| indices_for_true(bookmark.true_line_number(), root))
			.collect::<Vec<_>>();
		let (subscription, selected_text, _) = old_mutable_indices.as_inner_mut();
		let mut mutable_indices = MutableIndices::new(subscription, selected_text);
		mutable_indices.temp = bookmark_indices.iter_mut().collect::<Vec<_>>();

		let mut fake_name = String::new().into_boxed_str();

		let mut failures = 0;
		let mut actions = Vec::new();

		while let Some(indices) = mutable_indices.temp.pop() {
			let Some(indices) = indices.take() else { continue };
			match replacement.replace(root, indices, &mut fake_name, &mut mutable_indices, bookmarks) {
				Some(action) => actions.push(action),
				None => failures += 1,
			}
		}

		(WorkbenchAction::Bulk { actions: actions.into_boxed_slice() }, failures)
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
		Some(match search_mode {
			SearchMode::String => Self {
				inner: SearchReplacementInner::Substring {
					find: if exact_match { find } else { find.to_lowercase() },
					replacement,
					case_sensitive: exact_match,
				},
				search_flags,
			},
			SearchMode::Regex =>
				if let Some(regex) = create_regex(find, exact_match) {
					Self {
						inner: SearchReplacementInner::Regex { regex, replacement },
						search_flags,
					}
				} else {
					return None
				},
			SearchMode::Snbt =>
				if let Ok((find, replacement)) = {
					let sort = config::set_sort_algorithm(SortAlgorithm::None);
					let find = NbtElement::from_str(&find);
					config::set_sort_algorithm(sort);
					let replacement = NbtElement::from_str(&replacement);
					find.and_then(|find| replacement.map(|replacement| (find, replacement)))
				} {
					Self {
						inner: SearchReplacementInner::Snbt { find, replacement, exact_match },
						search_flags,
					}
				} else {
					return None
				},
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
					(value_flag && color != TextColor::TreeKey && value.contains(find)) || (key_flag && kv.0.is_some_and(|k| k.contains(find)))
				} else {
					(value_flag && color != TextColor::TreeKey && value.contains_ignore_ascii_case(find))
						|| (key_flag
							&& kv
								.0
								.is_some_and(|k| k.contains_ignore_ascii_case(find)))
				}
			}
			SearchReplacementInner::Regex { regex, .. } => {
				let (value, color) = kv.1.value();
				(value_flag && color != TextColor::TreeKey && regex.is_match(&value)) || (key_flag && kv.0.is_some_and(|k| regex.is_match(k)))
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

	pub fn replace<'m1, 'm2: 'm1>(&self, root: &mut NbtElement, key: Option<String>, value: Option<String>, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>, indices: &Indices) -> (Option<WorkbenchAction>, bool) {
		#[must_use]
		fn replace_case_sensitivity(value: &str, find: &str, replacement: &str, case_sensitive: bool) -> String { if case_sensitive { value.replace(find, replacement) } else { value.replace_ignore_ascii_case(find, replacement) } }

		let fake_name = &mut String::new().into_boxed_str();
		match &self.inner {
			SearchReplacementInner::Substring { find, replacement, case_sensitive } => {
				let key = key.map(|key| replace_case_sensitivity(&key, find, replacement, *case_sensitive).into());
				let value = value.map(|value| replace_case_sensitivity(&value, find, replacement, *case_sensitive).into());
				(rename_element(root, indices.to_owned(), key, value, &mut None, fake_name, &mut WindowProperties::Fake).map(RenameElementResult::into_action), false)
			}
			SearchReplacementInner::Regex { regex, replacement } => {
				let key = key.map(|key| regex.replace_all(&key, replacement).into());
				let value = value.map(|value| regex.replace_all(&value, replacement).into());
				(rename_element(root, indices.to_owned(), key, value, &mut None, fake_name, &mut WindowProperties::Fake).map(RenameElementResult::into_action), false)
			}
			SearchReplacementInner::Snbt { replacement, .. } => (replace_element(root, replacement.clone(), indices.to_owned(), bookmarks, mutable_indices).map(ReplaceElementResult::into_action), true),
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
	pub fn new(value: &str) -> Option<Self> {
		let search_mode = config::get_search_mode();
		let search_flags = config::get_search_flags();

		let inner = match search_mode {
			SearchMode::String => BookmarkedBasedSearchReplacementInner::String(value.to_owned()),
			SearchMode::Regex => BookmarkedBasedSearchReplacementInner::String(value.to_owned()),
			SearchMode::Snbt => BookmarkedBasedSearchReplacementInner::Snbt(NbtElement::from_str(value).ok()?),
		};

		Some(Self { search_flags, inner })
	}

	pub fn replace<'m1, 'm2: 'm1>(&self, root: &mut NbtElement, indices: OwnedIndices, fake_name: &mut Box<str>, mutable_indices: &'m1 mut MutableIndices<'m2>, bookmarks: &mut MarkedLines) -> Option<WorkbenchAction> {
		match &self.inner {
			BookmarkedBasedSearchReplacementInner::String(str) => {
				let key = self
					.search_flags
					.has_key()
					.then(|| str.to_compact_string());
				let value = self
					.search_flags
					.has_value()
					.then(|| str.to_owned());
				rename_element(root, indices, key, value, &mut None, fake_name, &mut WindowProperties::Fake).map(RenameElementResult::into_action)
			}
			BookmarkedBasedSearchReplacementInner::Snbt(replacement) => replace_element(root, replacement.clone(), indices, bookmarks, mutable_indices).map(ReplaceElementResult::into_action),
		}
	}
}
