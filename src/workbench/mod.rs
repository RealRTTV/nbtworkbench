pub mod element_action;
pub mod keyboard;
pub mod marked_line;
pub mod mouse;
pub mod tab;

use std::assert_matches::debug_assert_matches;
use std::fmt::{Display, Formatter, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::mpsc::TryRecvError;
#[cfg(not(target_arch = "wasm32"))] use std::thread::scope;
use std::time::Duration;

use anyhow::{Context, Result, anyhow, bail, ensure};
use compact_str::{CompactString, ToCompactString, format_compact};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use winit::dpi::{PhysicalPosition, PhysicalSize};
use winit::event::{ElementState, KeyEvent, MouseButton, MouseScrollDelta};
use winit::keyboard::{KeyCode, PhysicalKey};

use crate::action_result::{ActionResult, AnyhowActionResult, IntoFailingActionResult};
use crate::elements::array::{NbtByteArray, NbtIntArray, NbtLongArray};
use crate::elements::byte::NbtByte;
use crate::elements::chunk::NbtChunk;
use crate::elements::compound::{CompoundMap, NbtCompound};
use crate::elements::double::NbtDouble;
use crate::elements::element::{NbtElement, SNBTParseError};
use crate::elements::float::NbtFloat;
use crate::elements::int::NbtInt;
use crate::elements::list::NbtList;
use crate::elements::long::NbtLong;
use crate::elements::region::NbtRegion;
use crate::elements::short::NbtShort;
use crate::elements::string::NbtString;
use crate::elements::{NbtElementAndKey, NbtElementVariant};
use crate::history::WorkbenchAction;
use crate::render::TreeRenderContext;
use crate::render::assets::{
	ACTION_WHEEL_Z, BASE_TEXT_Z, BASE_Z, CLOSED_WIDGET_UV, DARK_STRIPE_UV, HEADER_SIZE, HELD_ENTRY_Z, HORIZONTAL_SEPARATOR_UV, HOVERED_STRIPE_UV, HOVERED_WIDGET_UV, JUST_OVERLAPPING_BASE_TEXT_Z, LIGHT_STRIPE_UV, LINE_NUMBER_SEPARATOR_UV,
	REPLACE_BOX_Z, SAVE_GRAYSCALE_UV, SAVE_UV, SELECTED_ACTION_WHEEL, SELECTED_WIDGET_UV, TRAY_UV, UNSELECTED_ACTION_WHEEL, UNSELECTED_WIDGET_UV, ZOffset,
};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::alert::Alert;
use crate::render::widget::alert::manager::{AlertManager, Alertable};
use crate::render::widget::button::exact_match::ExactMatchButton;
use crate::render::widget::button::freehand_mode::FreehandModeButton;
use crate::render::widget::button::new_tab::NewTabButton;
use crate::render::widget::button::open_file::OpenFileButton;
use crate::render::widget::button::refresh::RefreshButton;
use crate::render::widget::button::replace_by::ReplaceByButton;
use crate::render::widget::button::search_flags::SearchFlagsButton;
use crate::render::widget::button::search_mode::SearchModeButton;
use crate::render::widget::button::search_operation::SearchOperationButton;
use crate::render::widget::button::sort_algorithm::SortAlgorithmButton;
use crate::render::widget::button::theme::ThemeButton;
use crate::render::widget::notification::manager::NotificationManager;
use crate::render::widget::notification::{Notification, NotificationKind};
use crate::render::widget::replace_box::ReplaceBox;
use crate::render::widget::search_box::{SEARCH_BOX_END_X, SEARCH_BOX_START_X, SearchBox};
use crate::render::widget::selected_text::SelectedText;
use crate::render::widget::text::{TEXT_DOUBLE_CLICK_INTERVAL, get_cursor_idx, get_cursor_left_jump_idx, get_cursor_right_jump_idx};
use crate::render::widget::vertical_list::VerticalList;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::render::window::{MIN_WINDOW_HEIGHT, MIN_WINDOW_WIDTH, Theme, WINDOW_HEIGHT, WINDOW_WIDTH};
use crate::serialization::decoder::{BigEndianDecoder, Decoder};
use crate::serialization::encoder::UncheckedBufWriter;
use crate::tree::actions::add::{AddElementResult, add_element};
use crate::tree::actions::close::close_element;
use crate::tree::actions::expand::expand_element;
use crate::tree::actions::expand_to_indices::expand_element_to_indices;
use crate::tree::actions::open::open_element;
use crate::tree::actions::remove::{RemoveElementResult, remove_element};
use crate::tree::actions::replace::replace_element;
use crate::tree::indices::{Indices, OwnedIndices};
use crate::tree::navigate::NavigationInformation;
use crate::tree::traverse::{TraversalError, TraversalInformation, TraversalInformationMut};
use crate::util::{self, AABB, LinkedQueue, StrExt, Timestamp, Vec2d, Vec2u, drop_on_separate_thread, get_clipboard, nth, set_clipboard};
#[cfg(target_arch = "wasm32")] use crate::wasm::fake_scope as scope;
use crate::workbench::element_action::ElementAction;
use crate::workbench::keyboard::{KeyboardManager, Modifiers};
use crate::workbench::marked_line::MarkedLine;
use crate::workbench::mouse::MouseManager;
use crate::workbench::tab::manager::TabManager;
use crate::workbench::tab::{FilePath, NbtFileFormat, Tab, TabConstants};
use crate::{config, flags, get_interaction_information, hash, mutable_indices};

#[derive(Debug)]
pub enum InteractionInformation<'a> {
	Header,
	InvalidContent {
		x: usize,
		y: usize,
		e: TraversalError,
	},
	Content {
		is_in_left_margin: bool,
		depth: usize,
		x: usize,
		y: usize,
		line_number: usize,
		true_line_number: usize,
		key: Option<CompactString>,
		value: &'a mut NbtElement,
		indices: OwnedIndices,
	}
}

pub struct Workbench {
	pub tabs: TabManager,
	last_mouse_state: ElementState,
	raw_mouse: Vec2d,
	pub window_dims: PhysicalSize<u32>,
	raw_window_dims: PhysicalSize<u32>,
	keyboard: KeyboardManager,
	mouse: MouseManager,
	// todo: make TabManager a widget and add this
	tab_scroll: usize,
	// todo: make widget
	scrollbar_offset: Option<usize>,
	// todo: need to rework this
	action_wheel: Option<Vec2u>,
	pub cursor_visible: bool,
	pub alerts: AlertManager,
	pub notifications: NotificationManager,
	pub scale: f32,
	search_box: SearchBox,
	replace_box: ReplaceBox,
	ignore_event_end: Timestamp,
	debug_menu: bool,

	search_flags_button: SearchFlagsButton,
	search_operation_button: SearchOperationButton,
	search_mode_button: SearchModeButton,
	exact_match_button: ExactMatchButton,
	sort_algorithm_button: SortAlgorithmButton,
	theme_button: ThemeButton,
	freehand_mode_button: FreehandModeButton,
	refresh_button: RefreshButton,
	new_tab_button: NewTabButton,
	open_file_button: OpenFileButton,
	replace_by_button: ReplaceByButton,
}

impl Workbench {
	#[must_use]
	pub const unsafe fn uninit() -> Self {
		Self {
			tabs: TabManager::without_tab(),
			last_mouse_state: ElementState::Released,
			raw_mouse: Vec2d::new(0.0, 0.0),
			window_dims: PhysicalSize::new(0, 0),
			raw_window_dims: PhysicalSize::new(0, 0),
			keyboard: KeyboardManager::new(),
			mouse: MouseManager::new(),
			tab_scroll: 0,
			scrollbar_offset: None,
			action_wheel: None,
			cursor_visible: false,
			alerts: AlertManager::new(),
			notifications: unsafe { NotificationManager::uninit() },
			scale: 0.0,
			search_box: SearchBox::uninit(),
			replace_box: ReplaceBox::uninit(),
			ignore_event_end: Timestamp::UNIX_EPOCH,
			debug_menu: false,

			search_flags_button: unsafe { core::mem::zeroed() },
			search_mode_button: unsafe { core::mem::zeroed() },
			search_operation_button: unsafe { core::mem::zeroed() },
			exact_match_button: unsafe { core::mem::zeroed() },
			sort_algorithm_button: unsafe { core::mem::zeroed() },
			theme_button: unsafe { core::mem::zeroed() },
			freehand_mode_button: unsafe { core::mem::zeroed() },
			refresh_button: unsafe { core::mem::zeroed() },
			new_tab_button: unsafe { core::mem::zeroed() },
			open_file_button: unsafe { core::mem::zeroed() },
			replace_by_button: unsafe { core::mem::zeroed() },
		}
	}

	pub fn new(window_dims: Option<PhysicalSize<u32>>) -> Result<Self> {
		let mut workbench = Self {
			tabs: TabManager::without_tab(),
			last_mouse_state: ElementState::Released,
			raw_mouse: Vec2d::new(0.0, 0.0),
			window_dims: window_dims.unwrap_or(PhysicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT)),
			raw_window_dims: window_dims.unwrap_or(PhysicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT)),
			keyboard: KeyboardManager::new(),
			mouse: MouseManager::new(),
			tab_scroll: 0,
			scrollbar_offset: None,
			action_wheel: None,
			cursor_visible: true,
			alerts: AlertManager::new(),
			notifications: NotificationManager::new(),
			scale: 1.0,
			search_box: SearchBox::new(),
			replace_box: ReplaceBox::new(),
			ignore_event_end: Timestamp::UNIX_EPOCH,
			debug_menu: false,

			exact_match_button: Default::default(),
			freehand_mode_button: Default::default(),
			search_flags_button: Default::default(),
			search_operation_button: Default::default(),
			search_mode_button: Default::default(),
			sort_algorithm_button: Default::default(),
			theme_button: Default::default(),
			refresh_button: Default::default(),
			new_tab_button: Default::default(),
			open_file_button: Default::default(),
			replace_by_button: Default::default(),
		};
		if let Some(window_dims) = window_dims {
			workbench.raw_window_dims = window_dims;

			let scale = config::get_scale();
			if let Some(scale) = scale {
				workbench.set_scale(scale);
			} else {
				workbench.set_scale(1000.0);
				workbench.set_scale(workbench.scale.floor());
			}
		}
		'create_tab: {
			if let Some(path) = &std::env::args().nth(1).and_then(|x| PathBuf::from_str(&x).ok())
				&& let Ok(buf) = std::fs::read(path)
			{
				if workbench.on_open_file(path, buf).alert_err(&mut workbench.alerts).is_some() {
					break 'create_tab;
				}
			}
			workbench.tabs.add(Tab::new(
				if cfg!(debug_assertions) {
					let sort = config::set_sort_algorithm(SortAlgorithm::None);
					let result = NbtElement::from_be_file(include_bytes!("../assets/test.nbt")).context("Included debug nbt contains valid data")?;
					config::set_sort_algorithm(sort);
					result
				} else {
					NbtElement::Compound(NbtCompound::default())
				},
				if cfg!(debug_assertions) { FilePath::new("test.nbt")? } else { FilePath::new("new.nbt")? },
				NbtFileFormat::Nbt,
				window_dims.unwrap_or(PhysicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT)),
			)?);
		}
		Ok(workbench)
	}

	pub fn on_scroll(&mut self, scroll: MouseScrollDelta) -> ActionResult {
		let (h, v) = match scroll {
			MouseScrollDelta::LineDelta(h, v) => (h, v),
			MouseScrollDelta::PixelDelta(pos) => (pos.x as f32, pos.y as f32),
		};
		let Modifiers { ctrl, shift, .. } = self.keyboard.modifiers();
		if ctrl {
			self.set_scale(self.scale + v.signum() * if shift { 1.0 } else { 0.1 });
		} else {
			if AABB::new(0, usize::MAX, 0, 21).contains(self.mouse.coords) {
				let scroll = if shift { -v } else { -h };
				self.tab_scroll = ((self.tab_scroll as isize + (scroll * 48.0) as isize).max(0) as usize).min(
					{
						let mut tabs_width = 3_usize;
						for tab in &self.tabs {
							tabs_width += tab.path.name().width() + 32 + 6 + 6;
						}
						tabs_width
					}
					.saturating_sub(self.window_dims.width as usize),
				);
			} else {
				let tab = self.tabs.active_tab_mut();
				if shift {
					tab.on_horizontal_scroll(-v);
					tab.on_scroll(-h);
				} else {
					tab.on_horizontal_scroll(-h);
					tab.on_scroll(-v);
				}
			}
		}
		ActionResult::Success(())
	}

	pub fn on_mouse_input(&mut self, state: ElementState, button: MouseButton) -> ActionResult {
		self.tabs.active_tab_mut().last_interaction = Timestamp::now();
		let TabConstants { left_margin, horizontal_scroll, .. } = self.tabs.active_tab().consts();
		let Modifiers { shift, .. } = self.keyboard.modifiers();
		self.last_mouse_state = state;

		match state {
			ElementState::Pressed => {
				self.mouse.on_button_pressed(button);

				if let MouseButton::Left | MouseButton::Right = button
					&& let tab = self.tabs.active_tab_mut()
					&& tab.selected_text.is_some()
				{
					tab.save_selected_text().alert_err(&mut self.alerts);
					// do not exit early
				}

				{
					let mut new_alerts = AlertManager::new();
					let mut new_notifications = NotificationManager::new();
					let mut ctx = WidgetContextMut::new(&mut self.tabs, &mut self.search_box, &mut self.replace_box, &mut new_alerts, &mut new_notifications, shift);

					macro_rules! click_widgets {
						($($widget:expr,)* $(,)?) => {
							$(
								#[allow(unused_mut)]
								let mut widget = $widget;
								let dims = widget.dimensions(self.window_dims);
								let alignment = widget.alignment();
								let pos = alignment.coordinates(dims, self.window_dims);
								let aabb = AABB::from_pos_and_dims(pos, dims);
								if let Some(pos) = self.mouse.coords.relative_to(aabb) {
								    // hardcoded to mouse_down only for now
								    let result = widget.on_mouse_input(state, button, pos, dims, &mut ctx);
								    if result == ActionResult::Success(()) {
								    	let $crate::render::widget::WidgetAccumulatedResult { open_file_requests } = ctx.take_accumulated();

								    	for _ in 0..open_file_requests {
								    		self.open_file()?;
								    	}

								    	return ActionResult::Success(());
								    }
								    result?
								}
							)*
						};
					}

					click_widgets![
						&mut self.search_mode_button,
						&mut self.search_operation_button,
						&mut self.search_flags_button,
						&mut self.exact_match_button,
						&mut self.sort_algorithm_button,
						&mut self.theme_button,
						&mut self.freehand_mode_button,
						&mut self.refresh_button,
						&mut self.new_tab_button,
						&mut self.open_file_button,
						&mut self.replace_by_button,
						self.alerts.as_vertical_list(),
						self.notifications.as_vertical_list(),
					];
					
					self.alerts |= new_alerts;
					self.notifications |= new_notifications;
				}

				if let MouseButton::Left | MouseButton::Right = button
					&& ReplaceBox::is_visible(&self.search_box, &self.replace_box)
					&& ReplaceBox::is_within_bounds(self.mouse.coords, self.window_dims)
				{
					self.try_select_replace_box(button)?;
				} else {
					self.replace_box.deselect();
				}

				if let MouseButton::Left | MouseButton::Right = button
					&& SearchBox::is_within_bounds(self.mouse.coords, self.window_dims)
				{
					self.try_select_search_box(button)?;
				} else {
					self.search_box.deselect();
				}

				if AABB::new(4, usize::MAX, 2, 19).contains(self.mouse.coords) {
					self.click_tab(button)?;
				}
				if AABB::new(0, 16, 24, 46).contains(self.mouse.coords) {
					self.open_file()?;
				}

				if button == MouseButton::Left && AABB::new(0, usize::MAX, HEADER_SIZE, usize::MAX).contains(self.mouse.coords) && self.tabs.active_tab().held_entry.is_some() {
					self.drop_held_entry()?;
				}

				if button == MouseButton::Left {
					self.bookmark_line(true)?;
				}

				if AABB::new(left_margin, usize::MAX, HEADER_SIZE, usize::MAX).contains(self.mouse.coords + (horizontal_scroll, 0)) {
					match self.action_wheel.take() {
						Some(_) => {}
						None => {
							if button == MouseButton::Right
								&& let InteractionInformation::Content { is_in_left_margin: false, depth, y, .. } = get_interaction_information!(self)
								&& depth + 1 == (self.mouse.coords.x + horizontal_scroll - left_margin) / 16
							{
								self.action_wheel = Some(Vec2u::new(left_margin + depth * 16 + 16 + 6, y * 16 + HEADER_SIZE + 7));
								return ActionResult::Success(());
							}
						}
					}

					if button == MouseButton::Left {
						self.try_root_style_change()?;
					}

					if button == MouseButton::Left {
						self.toggle(shift, self.tabs.active_tab().freehand_mode)?;
					}

					if button == MouseButton::Left {
						self.try_double_click_interaction()?;
					}

					if button == MouseButton::Right {
						self.try_select_text(false)?
					}

					if button == MouseButton::Left {
						match self.try_steal(true) {
							ActionResult::Success(()) => return ActionResult::Success(()),
							result => {
								self.tabs.active_tab_mut().steal_animation_data = None;
								result?
							}
						}
					}

					{
						let tab = self.tabs.active_tab_mut();
						let TabConstants { scroll, .. } = tab.consts();
						let height = tab.root.height() * 16 + 48;
						let total = self.window_dims.height as usize - HEADER_SIZE;
						if height - 48 > total {
							let start = total * scroll / height + HEADER_SIZE;
							let end = start + total * total / height;
							if AABB::new(self.window_dims.width as usize - 7, self.window_dims.width as usize, start, end + 1).contains(self.mouse.coords) {
								self.scrollbar_offset = Some(self.mouse.coords.y - start);
								return ActionResult::Success(());
							}
						}
					}
				} else {
					if !self.tabs.active_tab().freehand_mode && self.tabs.active_tab().held_entry.is_none() && AABB::new(0, usize::MAX, 24, 46).contains(self.mouse.coords) && button == MouseButton::Left {
						match self.hold_entry(button) {
							ActionResult::Success(()) => return ActionResult::Success(()),
							ActionResult::Pass => {}
							ActionResult::Failure(e) => self.alerts.alert(e),
						}
					}
				}
			}
			ElementState::Released => {
				self.mouse.on_button_released(button);

				if let MouseButton::Right = button
					&& let Some(selected_text) = self.tabs.active_tab_mut().selected_text.as_mut()
				{
					selected_text.set_drag_selectable(false);
				}

				self.process_action_wheel()?;
				self.scrollbar_offset = None;
				if button == MouseButton::Left {
					self.tabs.active_tab_mut().steal_animation_data = None;
				}
			}
		}
		ActionResult::Pass
	}

	#[deprecated = "refactor to UFCS only"]
	pub fn on_open_file(&mut self, path: &Path, buf: Vec<u8>) -> Result<()> {
		let (nbt, format) = Tab::parse_raw(path, buf)?;
		let tab = Tab::new(nbt, FilePath::new(path).map_err(|path| anyhow!("Invalid file path: {path:?}"))?, format, self.window_dims)?;
		self.tabs.add(tab);
		Ok(())
	}

	#[deprecated = "refactor to UFCS only"]
	fn process_action_wheel(&mut self) -> ActionResult {
		use core::f64::consts::TAU;

		let Some(center) = self.action_wheel.take() else { return ActionResult::Pass };
		if center.y < HEADER_SIZE {
			return ActionResult::Failure(());
		}
		let tab = self.tabs.active_tab_mut();
		let TabConstants { left_margin, scroll, .. } = tab.consts();
		if (Vec2d::from(center) - Vec2d::from(self.mouse.coords)).distance_squared() <= 8_f64.powi(2) {
			return ActionResult::Failure(());
		}
		let highlight_idx = ((center - self.mouse.coords).angle() / TAU * 8.0 + 3.5).rem_euclid(8.0) as usize;
		let TraversalInformation { indices, element, .. } = tab.root.traverse((center.y - (HEADER_SIZE + 7) + scroll) / 16, Some((center.x - left_margin) / 16)).alert_err(&mut self.alerts).failure_on_err()?;
		if let Some(action) = element.actions().get(highlight_idx).copied() {
			if let Some(Some(action)) = action.apply(&mut tab.root, indices, mutable_indices!(tab)).alert_err(&mut self.alerts) {
				tab.history.append(action);
			}
		}
		ActionResult::Success(())
	}

	#[deprecated = "refactor to UFCS only"]
	pub fn try_subscription(&mut self) -> Result<()> {
		for tab in &mut self.tabs {
			let Some(subscription) = &mut tab.subscription else { continue };
			if let Err(e) = subscription.watcher.poll().map_err(|x| anyhow!("{x}")) {
				tab.subscription = None;
				return Err(e);
			};
			match subscription.rx.try_recv() {
				Ok(data) => {
					let kv = match subscription.r#type {
						FileUpdateSubscriptionType::Snbt => {
							let s = core::str::from_utf8(&data).context("File was not a valid UTF8 string")?;
							let sort = config::set_sort_algorithm(SortAlgorithm::None);
							let kv = NbtElement::from_str(s)?;
							config::set_sort_algorithm(sort);
							kv
						}
						kind => {
							let (id, prefix, width): (u8, &[u8], usize) = match kind {
								FileUpdateSubscriptionType::ByteArray => (NbtByteArray::ID, &[], 1),
								FileUpdateSubscriptionType::IntArray => (NbtIntArray::ID, &[], 4),
								FileUpdateSubscriptionType::LongArray => (NbtLongArray::ID, &[], 8),
								FileUpdateSubscriptionType::ByteList => (NbtList::ID, &[NbtByte::ID], 1),
								FileUpdateSubscriptionType::ShortList => (NbtList::ID, &[NbtShort::ID], 2),
								FileUpdateSubscriptionType::IntList => (NbtList::ID, &[NbtInt::ID], 4),
								FileUpdateSubscriptionType::LongList => (NbtList::ID, &[NbtLong::ID], 8),
								FileUpdateSubscriptionType::Snbt => bail!("Explicit SNBT parsing was skipped??"),
							};
							let mut buf = UncheckedBufWriter::new();
							buf.write(prefix);
							ensure!(data.len() % width == 0, "Hex data was of an incorrect length, length was {len} bytes, should be multiples of {width}", len = data.len());
							let buf = buf.finish();
							let mut decoder = BigEndianDecoder::new(&buf);
							let value = NbtElement::from_bytes(id, &mut decoder).context("Could not read bytes for array")?;
							let NavigationInformation { key, .. } = tab.root.navigate(&subscription.indices).context("Failed to navigate subscription indices")?;
							let key = key.map(CompactString::from);
							(key, value)
						}
					};
					let action = replace_element(&mut tab.root, kv, subscription.indices.clone(), mutable_indices!(tab)).context("Failed to replace element")?.into_action();
					tab.history.append(action);
					tab.refresh_scrolls();
				}
				Err(TryRecvError::Disconnected) => {
					tab.subscription = None;
					bail!("Could not update; file subscription disconnected.");
				}
				Err(TryRecvError::Empty) => {
					// do nothing ig
				}
			}
		}

		Ok(())
	}

	#[deprecated = "refactor to UFCS only"]
	fn try_double_click_interaction(&mut self) -> ActionResult {
		let shift = self.keyboard.shift();

		if self.tabs.active_tab().held_entry.is_some() || self.tabs.active_tab().freehand_mode || self.tabs.active_tab().last_selected_text_interaction.2.elapsed() <= LINE_DOUBLE_CLICK_INTERVAL {
			return ActionResult::Pass
		};

		if let InteractionInformation::Content { is_in_left_margin: false, y, .. } = get_interaction_information!(self) {
			let tab = self.tabs.active_tab_mut();
			if tab.last_double_click_interaction.0 == y && tab.last_double_click_interaction.1.elapsed() <= LINE_DOUBLE_CLICK_INTERVAL {
				tab.last_double_click_interaction = (y, Timestamp::now());
				if tab.root.as_region().is_some_and(|region| region.is_grid_layout()) {
					self.bookmark_line(false)
				} else {
					self.toggle(shift, true)
				}
			} else {
				tab.last_double_click_interaction = (y, Timestamp::now());
				ActionResult::Pass
			}
		} else {
			ActionResult::Pass
		}
	}

	#[deprecated = "refactor to UFCS only"]
	fn try_steal(&mut self, can_initialize: bool) -> ActionResult {
		if self.tabs.active_tab().held_entry.is_some() || self.tabs.active_tab().freehand_mode {
			return ActionResult::Pass
		};
		let is_grid_layout = self.tabs.active_tab().root.as_region().is_some_and(|region| region.is_grid_layout());

		if let InteractionInformation::Content { is_in_left_margin: false, depth, x, y, .. } = get_interaction_information!(self) {
			if can_initialize {
				self.tabs.active_tab_mut().steal_animation_data.get_or_insert((Timestamp::now(), (x, y).into()));
			}
			if let Some((_, expected)) = self.tabs.active_tab_mut().steal_animation_data.clone() {
				if expected == (depth + 1, y) || (is_grid_layout && expected == (x, y)) {
					return ActionResult::Success(())
				}
			}
		}
		ActionResult::Pass
	}

	#[deprecated = "refactor to UFCS only"]
	fn steal(&mut self) -> ActionResult {
		// todo, the fact that these indices aren't seemingly (to me) stored between like a queue might pose an issue in creating a correct workbench action history model -- correct, is still buggy :(

		if self.tabs.active_tab().steal_animation_data.as_ref().is_some_and(|x| x.0.elapsed() >= LINE_DOUBLE_CLICK_INTERVAL) {
			return ActionResult::Pass
		}

		if self.tabs.active_tab().held_entry.is_some() {
			return ActionResult::Pass
		}
		let is_grid_layout = self.tabs.active_tab().root.as_region().is_some_and(NbtRegion::is_grid_layout);

		if let InteractionInformation::Content {
			is_in_left_margin: false,
			depth,
			x,
			y,
			indices,
			..
		} = get_interaction_information!(self)
			&& (depth + 1 == x || is_grid_layout)
			&& y > 0
		{
			let tab = self.tabs.active_tab_mut();

			let RemoveElementResult { indices, kv: (key, mut value), replaces: _ } = remove_element(&mut tab.root, indices, mutable_indices!(tab)).alert_err(&mut self.alerts).failure_on_err()?;

			// SAFETY: value is detached from all caches
			scope(|scope| unsafe { value.shut(scope) });
			tab.history.append(WorkbenchAction::RemoveToHeldEntry);
			tab.held_entry = Some(HeldEntry::from_indices((key, value), indices));
			ActionResult::Success(())
		} else {
			ActionResult::Pass
		}
	}

	#[deprecated = "refactor to UFCS only"]
	fn try_duplicate(&mut self) -> ActionResult {
		if let InteractionInformation::Content {
			is_in_left_margin: false,
			y,
			key,
			value,
			mut indices,
			..
		} = get_interaction_information!(self)
			&& y > 0
		{
			*indices.last_mut().expect("y > 0") += 1;
			let duplicate = value.clone();
			let tab = self.tabs.active_tab_mut();
			let result = add_element(&mut tab.root, (key, duplicate), indices, mutable_indices!(tab)).alert_err(&mut self.alerts).failure_on_err()?;
			tab.history.append(result.into_action());
			tab.refresh_scrolls();
			ActionResult::Success(())
		} else {
			ActionResult::Pass
		}
	}

	#[deprecated = "refactor to UFCS only"]
	fn try_copy(&mut self, debug: bool) -> ActionResult {
		let InteractionInformation::Content { is_in_left_margin: false, key, value, .. } = get_interaction_information!(self) else {
			return ActionResult::Pass
		};
		let mut buf = String::new();
		let key = key.map(|key| if key.needs_escape() { format_compact!("{key:?}") } else { key });
		let key_exists = key.is_some();
		if debug {
			write!(&mut buf, "{}{}{value:#?}", key.as_deref().unwrap_or(""), if key_exists { ": " } else { "" }).alert_err(&mut self.alerts).failure_on_err()?;
		} else {
			write!(&mut buf, "{}{}{value}", key.as_deref().unwrap_or(""), if key_exists { ":" } else { "" }).alert_err(&mut self.alerts).failure_on_err()?;
		}
		if set_clipboard(buf) {
			ActionResult::Success(())
		} else {
			self.alerts.alert(Alert::error("Could not set clipboard"));
			ActionResult::Failure(())
		}
	}

	#[deprecated = "refactor to UFCS only"]
	fn delete(&mut self, clipboard: bool) -> ActionResult {
		if let InteractionInformation::Content {
			is_in_left_margin: false, indices, key, value, ..
		} = get_interaction_information!(self)
		{
			if clipboard {
				let key = key.map(|key| if key.needs_escape() { format_compact!("{key:?}") } else { key });
				let mut buf = String::new();
				if write!(&mut buf, "{}{}{value}", key.as_ref().map_or("", CompactString::as_str), if key.is_some() { ":" } else { "" }).is_ok() {
					set_clipboard(buf);
				}
			}
			let tab = self.tabs.active_tab_mut();
			let result = remove_element(&mut tab.root, indices, mutable_indices!(tab)).alert_err(&mut self.alerts).failure_on_err()?;
			tab.history.append(result.into_action());
			ActionResult::Success(())
		} else {
			ActionResult::Pass
		}
	}

	#[deprecated = "refactor to UFCS only"]
	fn drop_held_entry(&mut self) -> ActionResult {
		let tab = self.tabs.active_tab_mut();
		let TabConstants { left_margin, scroll, horizontal_scroll } = tab.consts();

		if self.mouse.coords.y <= HEADER_SIZE {
			return ActionResult::Pass
		}
		if self.mouse.coords.x + horizontal_scroll + 16 < left_margin {
			return ActionResult::Pass
		}
		let y = self.mouse.coords.y - HEADER_SIZE + scroll;
		let x = (self.mouse.coords.x + horizontal_scroll - left_margin) / 16 - 1;

		let Some(HeldEntry { kv, indices_history }) = tab.held_entry.take() else { return ActionResult::Pass };
		if let Some(indices) = tab.root.create_drop_indices((kv.0.as_deref(), &kv.1), y, x) {
			let AddElementResult { indices, old_kv } = add_element(&mut tab.root, kv, indices, mutable_indices!(tab)).alert_err(&mut self.alerts).failure_on_err()?;
			expand_element_to_indices(&mut tab.root, &indices, &mut tab.bookmarks).alert_err(&mut self.alerts);
			tab.history.append(WorkbenchAction::AddFromHeldEntry { indices, old_kv, indices_history });
			ActionResult::Success(())
		} else {
			tab.history.append(WorkbenchAction::DiscardHeldEntry { held_entry: HeldEntry { kv, indices_history } });
			ActionResult::Success(())
		}
	}

	#[deprecated = "refactor to UFCS only"]
	fn hold_entry(&mut self, button: MouseButton) -> AnyhowActionResult {
		if button == MouseButton::Left && self.mouse.coords.x >= 16 + 16 + 4 {
			let tab = self.tabs.active_tab_mut();
			let x = self.mouse.coords.x - (16 + 16 + 4);
			if x / 16 == 13 {
				let (key, element) = NbtElement::from_str(&get_clipboard().ok_or_else(|| anyhow!("Failed to get clipboard")).failure_on_err()?).failure_on_err()?;
				if element.is_chunk() && !tab.root.is_region() {
					return ActionResult::Failure(anyhow!("Chunks are not supported for non-region tabs"));
				} else {
					let old_held_entry = tab.held_entry.replace(HeldEntry::from_aether((key, element)));
					if let Some(held_entry) = old_held_entry {
						tab.history.append(WorkbenchAction::DiscardHeldEntry { held_entry });
					}
					tab.history.append(WorkbenchAction::CreateHeldEntry);
				}
			} else {
				let old_held_entry = tab.held_entry.replace(HeldEntry::from_aether((None, match x / 16 {
					0 => NbtElement::Byte(NbtByte::default()),
					1 => NbtElement::Short(NbtShort::default()),
					2 => NbtElement::Int(NbtInt::default()),
					3 => NbtElement::Long(NbtLong::default()),
					4 => NbtElement::Float(NbtFloat::default()),
					5 => NbtElement::Double(NbtDouble::default()),
					6 => NbtElement::ByteArray(NbtByteArray::default()),
					7 => NbtElement::IntArray(NbtIntArray::default()),
					8 => NbtElement::LongArray(NbtLongArray::default()),
					9 => NbtElement::String(NbtString::default()),
					10 => NbtElement::List(NbtList::default()),
					11 => NbtElement::Compound(NbtCompound::default()),
					12 if tab.root.is_region() => NbtElement::Chunk(NbtChunk::default()),
					_ => return ActionResult::Pass,
				})));
				if let Some(held_entry) = old_held_entry {
					tab.history.append(WorkbenchAction::DiscardHeldEntry { held_entry })
				}
				tab.history.append(WorkbenchAction::CreateHeldEntry);
			}
		}
		ActionResult::Success(())
	}

	fn click_tab(&mut self, button: MouseButton) -> ActionResult {
		let mouse_x = self.mouse.coords.x + self.tab_scroll;
		if mouse_x < 2 {
			return ActionResult::Pass
		}

		let shift = self.keyboard.shift();
		let active_tab_idx = self.tabs.active_tab_idx();

		let mut x = mouse_x - 2;
		for (idx, tab) in self.tabs.iter_mut().enumerate() {
			let width = tab.path.name().width() + 48 + 5;

			if x <= width {
				if button == MouseButton::Middle {
					drop_on_separate_thread(self.tabs.remove(idx));
					return ActionResult::Success(());
				} else if idx == active_tab_idx && x > width - 16 && x < width {
					if button == MouseButton::Left {
						tab.format = tab.format.cycle();
						return ActionResult::Success(());
					} else if button == MouseButton::Right {
						tab.format = tab.format.rev_cycle();
						return ActionResult::Success(());
					}
				} else if idx == active_tab_idx && x + 1 >= width - 32 && x < width - 16 {
					tab.save(shift).alert_err(&mut self.alerts);
					return ActionResult::Success(());
				} else if button == MouseButton::Left {
					self.tabs.set_active_idx(idx);
					return ActionResult::Success(());
				}
			}

			x -= width;

			if x < 6 {
				return ActionResult::Pass;
			} else {
				x -= 6;
			}
		}

		if button == MouseButton::Middle {
			self.tabs.add(Tab::new_empty_tab(shift, self.window_dims));
		}

		ActionResult::Pass
	}

	#[deprecated = "refactor to UFCS only"]
	#[cfg(any(target_os = "windows", target_os = "macos", target_os = "linux"))]
	fn open_file(&mut self) -> ActionResult {
		let dialog = native_dialog::FileDialogBuilder::default()
			.set_location("~/Downloads")
			.add_filters(Tab::FILE_TYPE_FILTERS.iter().copied().map(|(a, b)| (a.to_owned(), b.iter().map(|x| x.to_string()).collect::<Vec<_>>())))
			.open_single_file();
		let dialog_result = dialog.show();
		self.ignore_event_end = Timestamp::now() + Duration::from_millis(50);
		let path = dialog_result.alert_err(&mut self.alerts).failure_on_err()?.failure_on_err()?;
		let bytes = std::fs::read(&path).alert_err(&mut self.alerts).failure_on_err()?;
		self.on_open_file(&path, bytes).alert_err(&mut self.alerts);
		ActionResult::Success(())
	}

	#[deprecated = "refactor to UFCS only"]
	#[cfg(target_arch = "wasm32")]
	fn open_file(&mut self) -> ActionResult { crate::wasm::try_open_dialog(); }

	#[must_use]
	pub fn get_interaction_information_raw(consts: TabConstants, mouse: Vec2u, root: &mut NbtElement) -> InteractionInformation {
		let TabConstants { left_margin, scroll, horizontal_scroll } = consts;

		if mouse.y < HEADER_SIZE {
			return InteractionInformation::Header
		}
		let y = (mouse.y + scroll - HEADER_SIZE) / 16;
		let is_in_left_margin = mouse.x + horizontal_scroll < left_margin;
		let x = (mouse.x + horizontal_scroll).saturating_sub(left_margin) / 16;
		match root.traverse_mut(y, Some(x)) {
			Ok(TraversalInformationMut {
				depth,
				key,
				element: value,
				line_number,
				true_line_number,
				indices,
			}) => InteractionInformation::Content {
				is_in_left_margin,
				depth,
				key: key.map(|s| s.to_compact_string()),
				value,
				line_number,
				true_line_number,
				x,
				y,
				indices,
			},
			Err(e) => InteractionInformation::InvalidContent { x, y, e },
		}
	}

	#[deprecated = "refactor to UFCS only"]
	fn try_root_style_change(&mut self) -> ActionResult {
		let tab = self.tabs.active_tab_mut();
		let TabConstants { left_margin, horizontal_scroll, scroll, .. } = tab.consts();
		if self.mouse.coords.x + horizontal_scroll < left_margin {
			return ActionResult::Pass
		}
		if self.mouse.coords.y < HEADER_SIZE {
			return ActionResult::Pass
		}
		let x = (self.mouse.coords.x + horizontal_scroll - left_margin) / 16;
		let y = (self.mouse.coords.y - HEADER_SIZE) / 16 + scroll / 16;
		if !(x == 1 && y == 0) {
			return ActionResult::Pass
		}
		tab.root.on_style_change(&mut tab.bookmarks);
		tab.root.recache_along_indices(Indices::EMPTY);
		tab.refresh_scrolls();
		ActionResult::Success(())
	}

	#[deprecated = "refactor to UFCS only"]
	fn toggle(&mut self, expand: bool, ignore_depth: bool) -> ActionResult {
		if let InteractionInformation::Content {
			is_in_left_margin: false,
			x,
			depth,
			value,
			indices,
			..
		} = get_interaction_information!(self)
			&& (x <= depth || ignore_depth)
			&& value.is_complex()
			&& value.true_height() > 1
		{
			let is_open = value.is_open();
			let tab = self.tabs.active_tab_mut();
			if expand {
				expand_element(&mut tab.root, &indices, &mut tab.bookmarks).alert_err(&mut self.alerts);
			} else {
				if is_open {
					close_element(&mut tab.root, &indices, &mut tab.bookmarks).alert_err(&mut self.alerts);
				} else {
					open_element(&mut tab.root, &indices, &mut tab.bookmarks).alert_err(&mut self.alerts);
				}
			};
			ActionResult::Success(())
		} else {
			ActionResult::Pass
		}
	}

	fn try_select_search_box(&mut self, button: MouseButton) -> ActionResult {
		if !SearchBox::is_within_bounds(self.mouse.coords, self.window_dims) {
			return ActionResult::Pass
		}
		self.search_box.select(self.mouse.coords.x - SEARCH_BOX_START_X, button);
		self.replace_box.deselect();

		let (times_clicked, timestamp) = self.search_box.last_interaction;
		if timestamp.elapsed() < TEXT_DOUBLE_CLICK_INTERVAL && !self.search_box.value.is_empty() {
			self.search_box.last_interaction = (times_clicked + 1, Timestamp::now());
			// the previous click count was divisible by 1
			let (left, right) = if times_clicked % 2 == 1 {
				(0, self.search_box.value.len())
			} else {
				(
					get_cursor_left_jump_idx(self.search_box.cursor, self.search_box.value.as_bytes()),
					get_cursor_right_jump_idx(self.search_box.cursor, self.search_box.value.as_bytes()),
				)
			};
			// if they're == it's also false, just being careful here
			if right > left {
				self.search_box.selection = Some(left);
			}
			self.search_box.cursor = right;
		} else {
			self.search_box.last_interaction = (0, Timestamp::now());
		}
		ActionResult::Success(())
	}

	fn try_select_replace_box(&mut self, button: MouseButton) -> ActionResult {
		if !ReplaceBox::is_within_bounds(self.mouse.coords, self.window_dims) {
			return ActionResult::Pass
		}
		self.replace_box.select(self.mouse.coords.x - SEARCH_BOX_START_X, button);
		self.search_box.deselect();

		let (times_clicked, timestamp) = self.replace_box.last_interaction;
		if timestamp.elapsed() < TEXT_DOUBLE_CLICK_INTERVAL && !self.replace_box.value.is_empty() {
			self.replace_box.last_interaction = (times_clicked + 1, Timestamp::now());
			// the previous click count was divisible by 1
			let (left, right) = if times_clicked % 2 == 1 {
				(0, self.replace_box.value.len())
			} else {
				(
					get_cursor_left_jump_idx(self.replace_box.cursor, self.replace_box.value.as_bytes()),
					get_cursor_right_jump_idx(self.replace_box.cursor, self.replace_box.value.as_bytes()),
				)
			};
			// if they're == it's also false, just being careful here
			if right > left {
				self.replace_box.selection = Some(left);
			}
			self.replace_box.cursor = right;
		} else {
			self.replace_box.last_interaction = (0, Timestamp::now());
		}
		ActionResult::Success(())
	}

	fn try_select_text(&mut self, snap_to_ends: bool) -> ActionResult {
		let tab = self.tabs.active_tab_mut();
		let consts @ TabConstants { left_margin, scroll, horizontal_scroll, .. } = tab.consts();

		if self.mouse.coords.x + horizontal_scroll < left_margin {
			return ActionResult::Pass
		}
		if self.mouse.coords.y < HEADER_SIZE {
			return ActionResult::Pass
		}

		let y = (self.mouse.coords.y - HEADER_SIZE) / 16 + scroll / 16;
		tab.set_selected_text_with_doubleclick(SelectedText::for_y(consts, &tab.root, &tab.path, y, self.mouse.coords.x, snap_to_ends, None))
			.alert_err(&mut self.alerts)
			.failure_on_err()?;
		ActionResult::Success(())
	}

	fn bookmark_line(&mut self, require_left_margin_cursor: bool) -> ActionResult {
		if let InteractionInformation::Content { is_in_left_margin, true_line_number, y, .. } = get_interaction_information!(self)
			&& (is_in_left_margin || !require_left_margin_cursor)
		{
			let bookmark = MarkedLine::new(true_line_number, y);
			let _ = self.tabs.active_tab_mut().bookmarks.toggle(bookmark);
			ActionResult::Success(())
		} else {
			ActionResult::Pass
		}
	}

	#[allow(clippy::collapsible_if, clippy::too_many_lines, clippy::cognitive_complexity)]
	pub fn on_key_input(&mut self, key: KeyEvent) -> ActionResult {
		use ActionResult::{Failure, Pass, Success};

		self.tabs.active_tab_mut().last_interaction = Timestamp::now();
		let consts = self.tabs.active_tab().consts();
		if key.state == ElementState::Pressed {
			if let PhysicalKey::Code(key) = key.physical_key {
				self.keyboard.on_press(key);
				let char = self.char_from_key(key);
				let flags = self.keyboard.modifiers().into_bitflags();
				self.search_box
					.on_key_press(key, char, flags, &mut self.replace_box, self.tabs.active_tab_mut(), &mut self.alerts, &mut self.notifications, self.window_dims)?;
				self.replace_box
					.on_key_press(key, char, flags, &mut self.search_box, self.tabs.active_tab_mut(), &mut self.alerts, &mut self.notifications, self.window_dims)?;
				#[allow(irrefutable_let_patterns)]
				if let tab = self.tabs.active_tab_mut()
					&& let Some(mut selected_text) = tab.selected_text.take()
				{
					let result = selected_text.on_key_press(key, char, flags, consts, &mut tab.root, &mut tab.path, mutable_indices!(tab), &mut self.alerts, &mut tab.history);
					tab.selected_text = Some(selected_text);
					match result {
						Success(remove) => {
							if remove {
								tab.selected_text = None;
							}
							tab.refresh_scrolls();
							tab.refresh_selected_text_horizontal_scroll();
						}
						Pass => {}
						Failure(()) => return Failure(()),
					}
				}
				if key == KeyCode::KeyF && flags == flags!(Ctrl) {
					self.search_box.select(0, MouseButton::Left);
					self.replace_box.deselect();
					return Success(());
				}
				if key == KeyCode::KeyR && flags == flags!(Ctrl) {
					self.replace_box.select(0, MouseButton::Left);
					self.search_box.deselect();
					return Success(());
				}
				if key == KeyCode::Equal && flags & !flags!(Shift) == flags!(Ctrl) {
					self.set_scale(self.scale + if flags == flags!(Ctrl + Shift) { 1.0 } else { 0.1 });
					return Success(());
				}
				if key == KeyCode::Minus && flags & !flags!(Shift) == flags!(Ctrl) {
					self.set_scale(self.scale - if flags == flags!(Ctrl + Shift) { 1.0 } else { 0.1 });
					return Success(());
				}
				if self.action_wheel.is_some() && key == KeyCode::Escape && flags == flags!() {
					self.action_wheel = None;
					return Success(());
				}
				if key == KeyCode::Escape
					&& flags == flags!()
					&& let Some(held_entry) = self.tabs.active_tab_mut().held_entry.take()
				{
					self.tabs.active_tab_mut().history.append(WorkbenchAction::DiscardHeldEntry { held_entry });
					return Success(());
				}
				if (key == KeyCode::Enter || key == KeyCode::NumpadEnter)
					&& let tab = self.tabs.active_tab_mut()
					&& tab.selected_text.is_none()
					&& flags == flags!()
				{
					if tab.held_entry.is_some() {
						self.drop_held_entry()?;
					} else {
						self.try_select_text(true)?;
					}
					return Failure(());
				}
				if key == KeyCode::F3 && flags == flags!() {
					self.debug_menu = !self.debug_menu;
				}
				if flags == flags!(Ctrl) {
					let idx = match key {
						KeyCode::Digit1 => Some(0),
						KeyCode::Digit2 => Some(1),
						KeyCode::Digit3 => Some(2),
						KeyCode::Digit4 => Some(3),
						KeyCode::Digit5 => Some(4),
						KeyCode::Digit6 => Some(5),
						KeyCode::Digit7 => Some(6),
						KeyCode::Digit8 => Some(7),
						KeyCode::Digit9 => Some(usize::MAX),
						_ => None,
					};
					if let Some(idx) = idx {
						self.tabs.set_active_idx(idx);
						return Success(());
					}
				}
				if key == KeyCode::KeyR && flags == flags!(Ctrl) {
					let tab = self.tabs.active_tab_mut();
					tab.refresh().alert_err(&mut self.alerts);
					return Success(());
				}
				if key == KeyCode::KeyF && flags == flags!(Ctrl + Shift) {
					let tab = self.tabs.active_tab_mut();
					tab.freehand_mode = !tab.freehand_mode;
					return Success(());
				}
				if key == KeyCode::KeyT && flags == flags!(Ctrl + Alt) {
					config::set_theme(match config::get_theme() {
						Theme::Light => Theme::Dark,
						Theme::Dark => Theme::Light,
					});
					return Success(());
				}
				if key == KeyCode::KeyN && flags & (!flags!(Shift)) == flags!(Ctrl) {
					self.tabs.add(Tab::new_empty_tab((flags & flags!(Shift)) > 0, self.window_dims));
					return Success(());
				}
				if key == KeyCode::KeyO && flags == flags!(Ctrl) {
					self.open_file()?;
					return Success(());
				}
				if key == KeyCode::KeyS && flags & (!flags!(Shift)) == flags!(Ctrl) {
					let tab = self.tabs.active_tab_mut();
					tab.save((flags & flags!(Shift)) > 0).alert_err(&mut self.alerts).failure_on_err()?;
				}
				if key == KeyCode::KeyW && flags == flags!(Ctrl) {
					let active_tab_idx = self.tabs.active_tab_idx();
					drop_on_separate_thread(self.tabs.remove(active_tab_idx));
					return Success(());
				}
				if key == KeyCode::KeyZ && flags == flags!(Ctrl) {
					let tab = self.tabs.active_tab_mut();
					tab.history.undo(&mut tab.root, mutable_indices!(tab), &mut tab.path, &mut tab.held_entry).alert_err(&mut self.alerts).failure_on_err()?;
				}
				if key == KeyCode::KeyY && flags == flags!(Ctrl) || key == KeyCode::KeyZ && flags == flags!(Ctrl + Shift) {
					let tab = self.tabs.active_tab_mut();
					tab.history.redo(&mut tab.root, mutable_indices!(tab), &mut tab.path, &mut tab.held_entry).alert_err(&mut self.alerts).failure_on_err()?;
				}
				if ((key == KeyCode::Backspace || key == KeyCode::Delete) && flags == flags!()) || (key == KeyCode::KeyX && flags == flags!(Ctrl)) {
					self.delete(flags & flags!(Ctrl) > 0)?
				}
				if key == KeyCode::KeyD && flags == flags!(Ctrl) {
					self.try_duplicate()?;
				}
				if key == KeyCode::KeyC && (flags & !flags!(Shift)) == flags!(Ctrl) {
					self.try_copy((flags & !flags!(Ctrl)) == flags!(Shift))?;
				}
				if flags == flags!() {
					let tab = self.tabs.active_tab_mut();
					let kv = match key {
						KeyCode::Digit1 => (None, NbtElement::Byte(NbtByte::default())),
						KeyCode::Digit2 => (None, NbtElement::Short(NbtShort::default())),
						KeyCode::Digit3 => (None, NbtElement::Int(NbtInt::default())),
						KeyCode::Digit4 => (None, NbtElement::Long(NbtLong::default())),
						KeyCode::Digit5 => (None, NbtElement::Float(NbtFloat::default())),
						KeyCode::Digit6 => (None, NbtElement::Double(NbtDouble::default())),
						KeyCode::Digit7 => (None, NbtElement::ByteArray(NbtByteArray::default())),
						KeyCode::Digit8 => (None, NbtElement::IntArray(NbtIntArray::default())),
						KeyCode::Digit9 => (None, NbtElement::LongArray(NbtLongArray::default())),
						KeyCode::Digit0 => (None, NbtElement::String(NbtString::default())),
						KeyCode::Minus => (None, NbtElement::List(NbtList::default())),
						KeyCode::Equal => (None, NbtElement::Compound(NbtCompound::default())),
						KeyCode::Backquote =>
							if tab.root.is_region() {
								(None, NbtElement::Chunk(NbtChunk::default()))
							} else {
								return Failure(())
							},
						KeyCode::KeyV => {
							#[derive(Debug, Error)]
							enum ClipboardParseError {
								#[error("Failed to get clipboard")]
								ClipboardFailed,
								#[error(transparent)]
								SNBT(#[from] SNBTParseError),
							}

							fn element_from_clipboard() -> Result<NbtElementAndKey, ClipboardParseError> {
								let clipboard = get_clipboard().ok_or(ClipboardParseError::ClipboardFailed)?;
								Ok(NbtElement::from_str(&clipboard)?)
							}

							element_from_clipboard().alert_err(&mut self.alerts).failure_on_err()?
						}
						_ => return Failure(()),
					};
					let old_held_entry = tab.held_entry.replace(HeldEntry::from_aether(kv));
					if let Some(held_entry) = old_held_entry {
						tab.history.append(WorkbenchAction::DiscardHeldEntry { held_entry });
					}
					tab.history.append(WorkbenchAction::CreateHeldEntry);
					return Success(());
				}
			}
		} else if key.state == ElementState::Released {
			if let PhysicalKey::Code(x) = key.physical_key {
				self.keyboard.on_release(x);
			}
		}

		Pass
	}

	pub fn on_mouse_move(&mut self, pos: PhysicalPosition<f64>) -> ActionResult {
		self.raw_mouse = pos.into();
		self.mouse.coords = (self.raw_mouse / self.scale as f64).into();
		let tab = self.tabs.active_tab_mut();
		let TabConstants { scroll, .. } = tab.consts();
		if let Some(scrollbar_offset) = self.scrollbar_offset
			&& self.mouse.coords.y >= HEADER_SIZE
		{
			let mouse_y = self.mouse.coords.y - HEADER_SIZE;
			let height = tab.root.height() * 16 + 32 + 15;
			let total = tab.window_dims.height as usize - HEADER_SIZE;
			let start = total * scroll / height;
			let scrollbar_point = start + scrollbar_offset;
			let dy = mouse_y as isize - scrollbar_point as isize;
			let pixel_delta = height as isize * dy / total as isize;
			tab.modify_scroll(|scroll| (scroll as isize + pixel_delta).max(0) as usize);
		}
		{
			let mut new_alerts = AlertManager::new();
			let mut new_notifications = NotificationManager::new();
			let shift = self.keyboard.shift();
			let mut ctx = WidgetContextMut::new(&mut self.tabs, &mut self.search_box, &mut self.replace_box, &mut new_alerts, &mut new_notifications, shift);
			
			macro_rules! hover_widgets {
				($($widget:expr),+ $(,)?) => {
					$({
						#[allow(unused_mut)]
						let mut widget = $widget;
						let alignment = widget.alignment();
						let dims = widget.dimensions(self.window_dims);
						let pos = alignment.coordinates(dims, self.window_dims);
						let aabb = AABB::from_pos_and_dims(pos, dims);
						let relative_pos = self.mouse.coords.relative_to(aabb);
						let widget_is_currently_hovering = widget.is_currently_hovering();
						match (relative_pos, widget_is_currently_hovering) {
							(None, false) => {},
							(Some(_), true) => {},
							(None, true) => widget.on_stop_hovering(&mut ctx),
							(Some(pos), false) => widget.on_start_hovering(pos, dims, &mut ctx),
						}
					})+
				};
			}

			hover_widgets![
				self.alerts.as_vertical_list(),
			    self.notifications.as_vertical_list(),
		    ];
			
			self.alerts |= new_alerts;
			self.notifications |= new_notifications;
		}

		self.try_extend_drag_selection();
		ActionResult::Success(())
	}

	pub fn try_extend_drag_selection(&mut self) {
		let tab = self.tabs.active_tab_mut();
		let TabConstants { left_margin, horizontal_scroll, .. } = tab.consts();
		if self.last_mouse_state == ElementState::Pressed {
			if let Some(selected_text) = tab.selected_text.as_mut()
				&& tab.last_selected_text_interaction.1 == 0
				&& selected_text.is_drag_selectable()
			{
				let cursor = selected_text.selection.unwrap_or(selected_text.cursor);
				let selection = get_cursor_idx(
					&selected_text.value,
					(self.mouse.coords.x + horizontal_scroll) as isize
						- (selected_text.indices.end_x(left_margin) + SelectedText::PREFIXING_SPACE_WIDTH) as isize
						- selected_text.keyfix.as_ref().map_or(0, |k| k.0.width()) as isize
						- selected_text.prefix.0.width() as isize,
				);
				selected_text.cursor = selection;
				selected_text.selection = Some(cursor).filter(|cursor| *cursor != selected_text.cursor);
				selected_text.interact();
			}

			if self.search_box.is_selected() && self.search_box.last_interaction.0 == 0 {
				let cursor = self.search_box.selection.unwrap_or(self.search_box.cursor);
				let selection = get_cursor_idx(&self.search_box.value, (self.mouse.coords.x.saturating_sub(SEARCH_BOX_START_X) + self.search_box.horizontal_scroll) as isize);
				self.search_box.cursor = selection;
				self.search_box.selection = Some(cursor).filter(|cursor| *cursor != self.search_box.cursor);
				self.search_box.interact();
			}
		}
	}

	pub fn on_window_dims(&mut self, window_dims: PhysicalSize<u32>) {
		let new_dims: Vec2d = window_dims.cast::<f64>().into();
		let old_dims: Vec2d = self.raw_window_dims.cast::<f64>().into();
		self.raw_window_dims = window_dims;
		let scaling = new_dims / old_dims;
		self.raw_mouse *= scaling;
		self.set_scale(self.scale);
	}

	pub fn set_scale(&mut self, scale: f32) {
		let old_scale = self.scale;
		let scale = (scale * 10.0).round() / 10.0;
		let max_scales = Vec2u::from(self.raw_window_dims) / Vec2u::new(MIN_WINDOW_WIDTH as usize, MIN_WINDOW_HEIGHT as usize);
		let max_scale = usize::min(max_scales.x, max_scales.y) as f32;
		let scale = scale.clamp(1.0, max_scale);

		self.scale = scale;
		config::set_scale(Some(scale));
		self.mouse.coords = (self.raw_mouse / self.scale as f64).into();
		let dims = Vec2u::from((Vec2d::from(self.raw_window_dims.cast::<f64>()) / self.scale as f64).round());
		self.window_dims = PhysicalSize::new(dims.x as u32, dims.y as u32);
		for tab in &mut self.tabs {
			tab.set_window_dims(self.window_dims);
		}

		if old_scale != scale {
			self.notifications.notify(Notification::new(format!("Scale: {scale:.1}x (Max {max_scale}.0)"), TextColor::White, NotificationKind::Scale))
		}
	}

	// todo: replace commented std::time::Instant::now() with debug pie for ms to complete and pct
	pub fn render(&mut self, builder: &mut VertexBufferBuilder) {
		if self.raw_window_dims.width < MIN_WINDOW_WIDTH || self.raw_window_dims.height < MIN_WINDOW_HEIGHT {
			return;
		}

		let shift = self.keyboard.shift();

		builder.draw_texture_region_z((SEARCH_BOX_START_X - 3, 22), BASE_Z, LINE_NUMBER_SEPARATOR_UV, (2, 23), (2, 16));
		builder.draw_texture_region_z((builder.window_width() - SEARCH_BOX_END_X, 22), BASE_Z, LINE_NUMBER_SEPARATOR_UV, (2, 23), (2, 16));

		if ReplaceBox::is_visible(&self.search_box, &self.replace_box) {
			builder.draw_texture_region_z((SEARCH_BOX_START_X - 3, 45), REPLACE_BOX_Z, LINE_NUMBER_SEPARATOR_UV, (2, 25), (2, 16));
			builder.draw_texture_region_z((builder.window_width() - SEARCH_BOX_END_X, 45), REPLACE_BOX_Z, LINE_NUMBER_SEPARATOR_UV, (2, 25), (2, 16));
			builder.draw_texture_region_z((SEARCH_BOX_START_X - 1, 68), REPLACE_BOX_Z, HORIZONTAL_SEPARATOR_UV, (builder.window_width() - SEARCH_BOX_END_X - SEARCH_BOX_START_X + 1, 2), (14, 2));
		}

		for n in 0..(builder.window_height() - HEADER_SIZE + 15) / 16 {
			let uv = if (n % 2 == 0) ^ ((builder.scroll() / 16) % 2 == 0) { DARK_STRIPE_UV + (1, 1) } else { LIGHT_STRIPE_UV + (1, 1) };
			builder.draw_texture_region_z((0, n * 16 + HEADER_SIZE - (n == 0) as usize), BASE_Z, uv, (builder.window_width(), 16 + (n == 0) as usize), (14, 14));
		}
		// let start = std::time::Instant::now();
		self.render_tabs(builder);
		// println!("Tabs Bar: {}ms", start.elapsed().as_millis_f64());
		let tab = self.tabs.active_tab();
		let TabConstants { left_margin, horizontal_scroll, .. } = tab.consts();
		let ghost = if self.mouse.coords.x + horizontal_scroll >= left_margin && self.mouse.coords.y >= HEADER_SIZE {
			tab.held_entry.as_ref().map(|entry| {
				(
					&entry.kv.1,
					Vec2u::new(((self.mouse.coords.x + horizontal_scroll - left_margin) & !15) + left_margin, ((self.mouse.coords.y - HEADER_SIZE) & !0b0111) + HEADER_SIZE),
				)
			})
		} else {
			None
		};
		let selected_text_y = tab.selected_text.as_ref().map(|x| x.y).and_then(|x| x.checked_sub(builder.scroll()));
		let (selected_key, selected_value, selecting_key) = if let Some(SelectedText(selected)) = tab.selected_text.as_ref()
			&& selected.editable
		{
			if selected.keyfix.is_some() {
				// Health: __20.0__
				(selected.keyfix.clone().map(|x| x.0.clone().into_boxed_str()), Some(selected.value.clone().into_boxed_str()), false)
			} else if selected.valuefix.is_some() {
				// __Health__: 20.0
				(Some(selected.value.clone().into_boxed_str()), selected.valuefix.clone().map(|x| x.0.clone().into_boxed_str()), true)
			} else if !selected.suffix.0.is_empty() {
				// __Banana__: 4 entries
				(Some(selected.value.clone().into_boxed_str()), None, false)
			} else {
				// __20.0__
				(None, Some(selected.value.clone().into_boxed_str()), false)
			}
		} else {
			(None, None, false)
		};
		let mut ctx = TreeRenderContext::new(selected_text_y, selected_key, selected_value, selecting_key, ghost, left_margin, self.mouse.coords, tab.freehand_mode);
		if self.mouse.coords.y >= HEADER_SIZE && self.action_wheel.is_none() && !ReplaceBox::is_within_bounds(self.mouse.coords, builder.window_dims()) {
			builder.draw_texture_region_z((0, self.mouse.coords.y & !15), BASE_Z, HOVERED_STRIPE_UV, (builder.window_width(), 16), (14, 14));
		}
		{
			builder.draw_texture_region_z((33, 22), BASE_Z, LINE_NUMBER_SEPARATOR_UV, (2, 23), (2, 16));
		}
		{
			// let start = std::time::Instant::now();
			tab.render(
				builder,
				&mut ctx,
				self.scrollbar_offset.is_some(),
				self.action_wheel.is_some(),
				tab.steal_animation_data
					.as_ref()
					.map(|x| x.0.elapsed().min(LINE_DOUBLE_CLICK_INTERVAL).as_millis() as f32 / LINE_DOUBLE_CLICK_INTERVAL.as_millis_f32())
					.unwrap_or(0.0),
			);
			// println!("Active Tab: {}ms", start.elapsed().as_millis_f64());
		}
		{
			// let start = std::time::Instant::now();
			if let Some(selected_text) = &tab.selected_text {
				builder.horizontal_scroll = horizontal_scroll;
				selected_text.render(builder, left_margin);
				builder.horizontal_scroll = 0;
			}
			// println!("Selected Text: {}ms", start.elapsed().as_millis_f64());
		}
		{
			// let start = std::time::Instant::now();
			let ctx = WidgetContext::new(&self.tabs, &self.search_box, &self.replace_box, shift);

			macro_rules! render_widgets {
                ($($widget:expr),* $(,)?) => {{
	                $({
	                let widget = $widget;
	                let alignment = widget.alignment();
	                let dims = widget.dimensions(self.window_dims);
	                let pos = alignment.coordinates(dims, self.window_dims);
	                if widget.is_visible(&ctx) {
	                    widget.render_at(pos, dims, builder, &self.mouse, &ctx);
	                }
	                })*
                }};
			}

			let (mut list_0, mut list_1) = (self.notifications.as_vertical_list(), self.alerts.as_vertical_list());

			render_widgets![
				self.search_flags_button,
				self.search_operation_button,
				self.search_mode_button,
				self.exact_match_button,
				self.sort_algorithm_button,
				self.theme_button,
				self.freehand_mode_button,
				self.refresh_button,
				self.new_tab_button,
				self.open_file_button,
				self.replace_by_button,
				VerticalList::new(
					[&mut list_0 as &mut dyn Widget, &mut list_1 as &mut dyn Widget],
					WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Right, VerticalWidgetAlignmentPreference::Static(HEADER_SIZE as _))
				),
			];
			// println!("Widgets: {}ms", start.elapsed().as_millis_f64());
		}

		{
			// let start = std::time::Instant::now();
			self.render_action_wheel(builder);
			self.render_held_entry(builder);
			self.render_debug_menu(builder);
			// println!("Misc: {}ms", start.elapsed().as_millis_f64());
		}
		builder.draw_tooltips();
	}

	pub fn render_search_boxes(&self, builder: &mut VertexBufferBuilder) {
		self.search_box.render(builder);
		if ReplaceBox::is_visible(&self.search_box, &self.replace_box) {
			self.replace_box.render(builder);
		}
		builder.draw_tooltips();
	}

	pub fn tick(&mut self) {
		#[cfg(not(target_arch = "wasm32"))]
		for (idx, tab) in self.tabs.iter_mut().enumerate() {
			if (tab.last_interaction.elapsed() >= Tab::AUTOSAVE_INTERVAL) && tab.history.has_unsaved_changes() && tab.root.true_height() <= Tab::AUTOSAVE_MAXIMUM_LINES {
				if let Err(e) = tab.save(false) {
					self.alerts.alert(e.context(format!("Failed to autosave {nth} tab", nth = nth(idx + 1))));
				}
			}
		}
		if (self.tabs.active_tab().held_entry.is_some() || self.tabs.active_tab().freehand_mode || ((self.tabs.active_tab().selected_text.is_some() || self.search_box.is_selected()) && self.last_mouse_state == ElementState::Pressed))
			&& self.action_wheel.is_none()
			&& self.scrollbar_offset.is_none()
		{
			self.try_mouse_scroll();
			self.try_search_box_scroll();
			self.try_replace_box_scroll();
			self.try_extend_drag_selection();
		}
		self.alerts.cleanup();
		self.notifications.cleanup();
		if self.tabs.active_tab().steal_animation_data.is_some()
			&& let ActionResult::Success(()) = self.try_steal(false)
		{
			if !self.steal().passed() {
				return;
			}
		} else {
			self.tabs.active_tab_mut().steal_animation_data = None;
		}
	}

	#[must_use]
	pub fn close(&mut self) -> usize {
		let mut failed_tabs = 0_usize;

		for tab in &mut self.tabs {
			if tab.history.has_unsaved_changes() && core::mem::replace(&mut tab.last_close_attempt, Timestamp::now()).elapsed() >= Tab::TAB_CLOSE_DOUBLE_CLICK_INTERVAL {
				failed_tabs += 1;
			}
		}

		if failed_tabs > 0 {
			self.alerts.alert(Alert::new(
				"Are you sure you want to exit?",
				TextColor::Yellow,
				format!("You have {failed_tabs} unsaved tab{tab_suffix}.", tab_suffix = if failed_tabs == 1 { "" } else { "s" }),
			));
		}
		failed_tabs
	}

	fn render_held_entry(&self, builder: &mut VertexBufferBuilder) {
		let shift = self.keyboard.shift();

		if let Some(held_entry) = &self.tabs.active_tab().held_entry {
			let element = &held_entry.kv.1;
			builder.draw_texture_z(self.mouse.coords.saturating_sub((8, 8).into()), HELD_ENTRY_Z, element.uv(), (16, 16));

			if (!element.is_primitive() || !element.is_default_state()) && element.should_render_description() || shift {
				let (text, color) = element.value();
				builder.color = color.to_raw();
				builder.draw_tooltip(&[&text], self.mouse.coords, false);
			}
		}
	}

	fn render_debug_menu(&mut self, builder: &mut VertexBufferBuilder) {
		if !self.debug_menu {
			return
		}

		let tab = self.tabs.active_tab();
		let lines = [
			format!("dims: {}x{}", self.window_dims.width, self.window_dims.height),
			format!("mouse state: {:?}", self.last_mouse_state),
			format!("mouse px coords: {:?}", self.mouse.coords),
			format!("action wheel coords: {:?}", self.action_wheel),
			format!("sub indices: {:?}", tab.subscription.as_ref().map(|subscription| &subscription.indices)),
			format!("scale: {}", self.scale),
			format!("last SB input: y={}, since={}ms", self.search_box.last_interaction.0, self.search_box.last_interaction.1.elapsed().as_millis()),
			format!("file format: {:?}", tab.format),
			format!("history: {:?}", tab.history),
			format!("scroll: {}", tab.scroll),
			format!("hscroll: {}", tab.horizontal_scroll),
			format!("bookmark count: {}", tab.bookmarks.len()),
			format!(
				"select txt: {data}",
				data = if let Some(txt) = tab.selected_text.as_ref() {
					format!(
						"y={}, cursor={}, pre={}, key={:?}, txt={}, val={:?}, suf={}, ccx={:?}",
						txt.y,
						txt.cursor,
						txt.prefix.0,
						txt.keyfix.as_ref().map(|x| &x.0),
						txt.value,
						txt.valuefix.as_ref().map(|x| &x.0),
						txt.suffix.0,
						txt.cached_cursor_x
					)
				} else {
					"null".to_owned()
				}
			),
			format!(
				"select txt val: {data}",
				data = if let Some(txt) = tab.selected_text.as_ref()
					&& let Some((key, value)) = tab.root.get_kv_under_indices(&txt.indices)
				{
					format!("key={key:?}, value={}, h={}, th={}, end_x={}", value.value().0, value.height(), value.true_height(), value.end_x())
				} else {
					"null".to_owned()
				}
			),
			format!(
				"held entry: {data}",
				data = if let Some(held) = &tab.held_entry {
					format!("h={}, th={}", held.kv.1.height(), held.kv.1.true_height())
				} else {
					"null".to_owned()
				}
			),
			format!("value: h={}, th={}, depth={}", tab.root.height(), tab.root.true_height(), tab.root.end_x()),
		];
		for (idx, line) in lines.iter().enumerate() {
			if builder.window_height() < (idx + 1) * VertexBufferBuilder::CHAR_HEIGHT {
				continue
			}
			builder.settings(
				(builder.window_width().saturating_sub(line.width()), builder.window_height() - (idx + 1) * VertexBufferBuilder::CHAR_HEIGHT),
				false,
				ZOffset::DEBUG_TEXT_Z,
			);
			builder.color = TextColor::White.to_raw();
			let _ = write!(builder, "{line}");
		}
	}

	fn render_tabs(&self, builder: &mut VertexBufferBuilder) {
		let mut offset = 3;
		builder.horizontal_scroll = self.tab_scroll;
		for (idx, tab) in self.tabs.iter().enumerate() {
			let remaining_width = tab.path.name().width() + 48 + 3;
			let uv = if tab.last_close_attempt.elapsed() <= Tab::TAB_CLOSE_DOUBLE_CLICK_INTERVAL {
				CLOSED_WIDGET_UV
			} else if idx == self.tabs.active_tab_idx() {
				SELECTED_WIDGET_UV
			} else if (offset..offset + 3 + remaining_width).contains(&self.mouse.coords.x) && (3..=19).contains(&self.mouse.coords.y) {
				HOVERED_WIDGET_UV
			} else {
				UNSELECTED_WIDGET_UV
			};
			builder.draw_texture((offset, 3), uv, (3, 16));
			if (offset..offset + 16).contains(&self.mouse.coords.x) && (3..19).contains(&self.mouse.coords.y) {
				builder.draw_tooltip(&[tab.root.display_name()], self.mouse.coords, false);
			}
			offset += 2;
			tab.draw_icon(builder, (offset, 2), JUST_OVERLAPPING_BASE_TEXT_Z);
			offset += 1;
			builder.draw_texture_region_z((offset, 3), BASE_Z, uv + (3, 0), (remaining_width, 16), (10, 16));
			builder.settings((offset + 16, 3), false, BASE_TEXT_Z);
			builder.color = match config::get_theme() {
				Theme::Light => TextColor::DarkGray,
				Theme::Dark => TextColor::White,
			}
			.to_raw();
			let _ = write!(builder, "{}", tab.path.name());
			offset += remaining_width;
			builder.draw_texture((offset, 3), uv + (13, 0), (3, 16));
			builder.draw_texture((offset - 32, 3), if tab.history.has_unsaved_changes() { SAVE_UV } else { SAVE_GRAYSCALE_UV }, (16, 16));
			builder.draw_texture((offset - 16, 3), tab.format.uv(), (16, 16));
			if AABB::new(offset - 32, offset - 16, 3, 19).contains(self.mouse.coords) {
				builder.draw_tooltip(&["Save"], self.mouse.coords, false);
			}
			if (offset - 16..offset).contains(&self.mouse.coords.x) && (3..19).contains(&self.mouse.coords.y) {
				builder.draw_tooltip(&[tab.format.into_str()], self.mouse.coords, false);
			}
			offset += 6;
		}
		builder.horizontal_scroll = 0;
		builder.draw_texture_region_z((0, 21), BASE_Z, HORIZONTAL_SEPARATOR_UV, (builder.window_width(), 2), (14, 2));
		builder.draw_texture_region_z((0, 45), BASE_Z, HORIZONTAL_SEPARATOR_UV, (builder.window_width(), 2), (14, 2));
	}

	fn render_action_wheel(&mut self, builder: &mut VertexBufferBuilder) {
		use std::f64::consts::TAU;

		let Some(mut center) = self.action_wheel else { return };
		center.x = center.x.saturating_sub(31) + 31;
		center.y = center.y.saturating_sub(31) + 31;
		let tab = self.tabs.active_tab_mut();
		let consts @ TabConstants { left_margin, .. } = tab.consts();
		let highlight_idx = ((center - self.mouse.coords).angle() / TAU * 8.0 + 3.5).rem_euclid(8.0) as usize;
		let squared_distance_from_origin = (center.y as isize - self.mouse.coords.y as isize).pow(2) + (center.x as isize - self.mouse.coords.x as isize).pow(2);
		if center.y >= HEADER_SIZE {
			if center.y > tab.root.height() * 16 + HEADER_SIZE {
				return
			};
			let InteractionInformation::Content {
				is_in_left_margin: false, depth, key, value, ..
			} = Self::get_interaction_information_raw(consts, center, &mut tab.root)
			else {
				return
			};
			let min_x = depth * 16 + left_margin;
			let max_x = min_x + 32 + value.value_width() + key.map(|key| key.width() + ": ".width()).unwrap_or(0);
			if !(min_x..max_x).contains(&center.x) {
				return
			};
			builder.draw_texture_z((center.x - 31, center.y - 31), ACTION_WHEEL_Z, TRAY_UV, (64, 64));
			for (n, &action) in value.actions().iter().enumerate().take(8) {
				let (x, y) = [
					Vec2u::new(9, 9),
					Vec2u::new(-9_isize as usize, 11),
					Vec2u::new(-26_isize as usize, 9),
					Vec2u::new(-29_isize as usize, -9_isize as usize),
					Vec2u::new(-26_isize as usize, -26_isize as usize),
					Vec2u::new(-9_isize as usize, -29_isize as usize),
					Vec2u::new(9, -26_isize as usize),
					Vec2u::new(11, -9_isize as usize),
				][n]
					.into();
				let mut hovered = false;
				let uv = if squared_distance_from_origin > 8_isize.pow(2) && highlight_idx == n {
					hovered = true;
					SELECTED_ACTION_WHEEL[n]
				} else {
					UNSELECTED_ACTION_WHEEL[n]
				};
				let offset = [Vec2u::new(5, 5), Vec2u::new(5, 6), Vec2u::new(4, 5), Vec2u::new(3, 5), Vec2u::new(4, 4), Vec2u::new(5, 3), Vec2u::new(5, 4), Vec2u::new(6, 5)][n];
				let dims = if n % 2 == 0 { Vec2u::new(19, 19) } else { Vec2u::new(20, 20) };
				builder.draw_texture_z(center + (x, y), ACTION_WHEEL_Z, uv, dims);
				action.render(builder, center + (x, y) + offset, hovered);
			}
		}
	}

	pub fn try_mouse_scroll(&mut self) {
		let tab = self.tabs.active_tab_mut();
		if self.mouse.coords.x >= self.window_dims.width as usize - 16 && self.mouse.coords.y >= HEADER_SIZE {
			tab.modify_horizontal_scroll(|scroll| scroll + 16);
		} else if self.mouse.coords.x < 16 {
			tab.modify_horizontal_scroll(|scroll| scroll.saturating_sub(16));
		}

		if self.mouse.coords.y < HEADER_SIZE + 16 {
			tab.modify_scroll(|scroll| scroll.saturating_sub(16));
		} else if self.mouse.coords.y >= usize::min(self.window_dims.width as usize - 16, tab.root.height() * 16 + HEADER_SIZE) {
			tab.modify_scroll(|scroll| scroll + 16);
		}
	}

	pub fn try_search_box_scroll(&mut self) {
		let search_box_x = (SEARCH_BOX_START_X + 16)..(self.window_dims.width as usize - (SEARCH_BOX_END_X + 64));
		if self.last_mouse_state == ElementState::Pressed && self.search_box.is_selected() && !search_box_x.contains(&self.mouse.coords.x) {
			if self.mouse.coords.x < search_box_x.start {
				self.search_box.horizontal_scroll = self.search_box.horizontal_scroll.saturating_sub(4);
			} else {
				self.search_box.horizontal_scroll = (self.search_box.horizontal_scroll + 4).min(self.search_box.value.width().saturating_sub(search_box_x.end - search_box_x.start));
			}
		}
	}

	pub fn try_replace_box_scroll(&mut self) {
		let replace_box_x = (SEARCH_BOX_START_X + 16)..(self.window_dims.width as usize - SEARCH_BOX_END_X);
		if self.last_mouse_state == ElementState::Pressed && self.replace_box.is_selected() && !replace_box_x.contains(&self.mouse.coords.x) {
			if self.mouse.coords.x < replace_box_x.start {
				self.replace_box.horizontal_scroll = self.replace_box.horizontal_scroll.saturating_sub(4);
			} else {
				self.replace_box.horizontal_scroll = (self.replace_box.horizontal_scroll + 4).min(self.replace_box.value.width().saturating_sub(replace_box_x.end - replace_box_x.start));
			}
		}
	}

	pub fn should_ignore_event(&self) -> bool { Timestamp::now() < self.ignore_event_end }

	#[allow(clippy::cognitive_complexity, clippy::match_same_arms, clippy::too_many_lines)]
	#[must_use]
	fn char_from_key(&self, key: KeyCode) -> Option<char> {
		let Modifiers { ctrl, shift, .. } = self.keyboard.modifiers();
		if ctrl {
			return None
		}
		Some(match key {
			KeyCode::Digit1 =>
				if shift {
					'!'
				} else {
					'1'
				},
			KeyCode::Digit2 =>
				if shift {
					'@'
				} else {
					'2'
				},
			KeyCode::Digit3 =>
				if shift {
					'#'
				} else {
					'3'
				},
			KeyCode::Digit4 =>
				if shift {
					'$'
				} else {
					'4'
				},
			KeyCode::Digit5 =>
				if shift {
					'%'
				} else {
					'5'
				},
			KeyCode::Digit6 =>
				if shift {
					'^'
				} else {
					'6'
				},
			KeyCode::Digit7 =>
				if shift {
					'&'
				} else {
					'7'
				},
			KeyCode::Digit8 =>
				if shift {
					'*'
				} else {
					'8'
				},
			KeyCode::Digit9 =>
				if shift {
					'('
				} else {
					'9'
				},
			KeyCode::Digit0 =>
				if shift {
					')'
				} else {
					'0'
				},
			KeyCode::KeyA =>
				if shift {
					'A'
				} else {
					'a'
				},
			KeyCode::KeyB =>
				if shift {
					'B'
				} else {
					'b'
				},
			KeyCode::KeyC =>
				if shift {
					'C'
				} else {
					'c'
				},
			KeyCode::KeyD =>
				if shift {
					'D'
				} else {
					'd'
				},
			KeyCode::KeyE =>
				if shift {
					'E'
				} else {
					'e'
				},
			KeyCode::KeyF =>
				if shift {
					'F'
				} else {
					'f'
				},
			KeyCode::KeyG =>
				if shift {
					'G'
				} else {
					'g'
				},
			KeyCode::KeyH =>
				if shift {
					'H'
				} else {
					'h'
				},
			KeyCode::KeyI =>
				if shift {
					'I'
				} else {
					'i'
				},
			KeyCode::KeyJ =>
				if shift {
					'J'
				} else {
					'j'
				},
			KeyCode::KeyK =>
				if shift {
					'K'
				} else {
					'k'
				},
			KeyCode::KeyL =>
				if shift {
					'L'
				} else {
					'l'
				},
			KeyCode::KeyM =>
				if shift {
					'M'
				} else {
					'm'
				},
			KeyCode::KeyN =>
				if shift {
					'N'
				} else {
					'n'
				},
			KeyCode::KeyO =>
				if shift {
					'O'
				} else {
					'o'
				},
			KeyCode::KeyP =>
				if shift {
					'P'
				} else {
					'p'
				},
			KeyCode::KeyQ =>
				if shift {
					'Q'
				} else {
					'q'
				},
			KeyCode::KeyR =>
				if shift {
					'R'
				} else {
					'r'
				},
			KeyCode::KeyS =>
				if shift {
					'S'
				} else {
					's'
				},
			KeyCode::KeyT =>
				if shift {
					'T'
				} else {
					't'
				},
			KeyCode::KeyU =>
				if shift {
					'U'
				} else {
					'u'
				},
			KeyCode::KeyV =>
				if shift {
					'V'
				} else {
					'v'
				},
			KeyCode::KeyW =>
				if shift {
					'W'
				} else {
					'w'
				},
			KeyCode::KeyX =>
				if shift {
					'X'
				} else {
					'x'
				},
			KeyCode::KeyY =>
				if shift {
					'Y'
				} else {
					'y'
				},
			KeyCode::KeyZ =>
				if shift {
					'Z'
				} else {
					'z'
				},
			KeyCode::Space => ' ',
			KeyCode::Numpad0 => '0',
			KeyCode::Numpad1 => '1',
			KeyCode::Numpad2 => '2',
			KeyCode::Numpad3 => '3',
			KeyCode::Numpad4 => '4',
			KeyCode::Numpad5 => '5',
			KeyCode::Numpad6 => '6',
			KeyCode::Numpad7 => '7',
			KeyCode::Numpad8 => '8',
			KeyCode::Numpad9 => '9',
			KeyCode::NumpadAdd => '+',
			KeyCode::NumpadDivide => '/',
			KeyCode::NumpadDecimal => '.',
			KeyCode::NumpadComma => ',',
			KeyCode::NumpadEqual => '=',
			KeyCode::NumpadMultiply => '*',
			KeyCode::NumpadSubtract => '-',
			KeyCode::Quote =>
				if shift {
					'"'
				} else {
					'\''
				},
			KeyCode::Backslash =>
				if shift {
					'|'
				} else {
					'\\'
				},
			KeyCode::Semicolon =>
				if shift {
					':'
				} else {
					';'
				},
			KeyCode::Comma =>
				if shift {
					'<'
				} else {
					','
				},
			KeyCode::Equal =>
				if shift {
					'+'
				} else {
					'='
				},
			KeyCode::Backquote =>
				if shift {
					'~'
				} else {
					'`'
				},
			KeyCode::BracketLeft =>
				if shift {
					'{'
				} else {
					'['
				},
			KeyCode::Minus =>
				if shift {
					'_'
				} else {
					'-'
				},
			KeyCode::Period =>
				if shift {
					'>'
				} else {
					'.'
				},
			KeyCode::BracketRight =>
				if shift {
					'}'
				} else {
					']'
				},
			KeyCode::Slash =>
				if shift {
					'?'
				} else {
					'/'
				},
			KeyCode::Tab => '\t',
			_ => return None,
		})
	}
}

pub const LINE_DOUBLE_CLICK_INTERVAL: Duration = Duration::from_millis(250);

#[derive(Debug)]
pub struct HeldEntry {
	pub(super) kv: NbtElementAndKey,
	pub(super) indices_history: LinkedQueue<OwnedIndices>,
}

impl HeldEntry {
	#[must_use]
	pub fn from_aether(kv: NbtElementAndKey) -> Self { Self { kv, indices_history: LinkedQueue::new() } }

	#[must_use]
	pub fn from_indices(kv: NbtElementAndKey, indices: OwnedIndices) -> Self {
		Self {
			kv,
			indices_history: {
				let mut queue = LinkedQueue::new();
				queue.push(indices);
				queue
			},
		}
	}
}

#[derive(Copy, Clone, Default, Serialize, Deserialize)]
pub enum SortAlgorithm {
	None,
	Name,
	#[default]
	Type,
}

impl SortAlgorithm {
	#[must_use]
	pub fn cycle(self) -> Self {
		match self {
			Self::None => Self::Name,
			Self::Name => Self::Type,
			Self::Type => Self::None,
		}
	}

	#[must_use]
	pub fn rev_cycle(self) -> Self {
		match self {
			Self::None => Self::Type,
			Self::Name => Self::None,
			Self::Type => Self::Name,
		}
	}

	/// # Safety
	///
	/// * Data must be created before any modifications as to eliminate the possibility of bookmarks, history, etc.
	pub unsafe fn sort(self, map: &mut CompoundMap) {
		let mapping = match self {
			Self::None => return,
			Self::Name => map.create_sort_mapping(ElementAction::by_name),
			Self::Type => map.create_sort_mapping(ElementAction::by_type),
		};

		for (idx, &new_idx) in mapping.iter().enumerate() {
			*unsafe { map.indices.find_mut(hash!(map.entries.get_unchecked(idx).key), |&x| x == idx).unwrap_unchecked() } = new_idx;
		}

		let result = util::reorder(&mut map.entries, mapping);
		debug_assert_matches!(result, Ok(()));
	}
}

impl Display for SortAlgorithm {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			Self::None => "None",
			Self::Name => "Name-Based",
			Self::Type => "Type-Based",
		})
	}
}

pub struct FileUpdateSubscription {
	r#type: FileUpdateSubscriptionType,
	pub indices: OwnedIndices,
	rx: std::sync::mpsc::Receiver<Vec<u8>>,
	watcher: notify::PollWatcher,
}

impl FileUpdateSubscription {
	#[must_use]
	pub fn new(r#type: FileUpdateSubscriptionType, indices: OwnedIndices, rx: std::sync::mpsc::Receiver<Vec<u8>>, watcher: notify::PollWatcher) -> Self { Self { r#type, indices, rx, watcher } }
}

#[derive(Copy, Clone)]
pub enum FileUpdateSubscriptionType {
	Snbt,
	ByteArray,
	IntArray,
	LongArray,
	ByteList,
	ShortList,
	IntList,
	LongList,
}

pub enum DropResult {
	Dropped,
	Missed,
	Failed,
}
