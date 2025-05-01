mod tab;
mod workbench_action;
mod marked_line;
mod element_action;

pub use element_action::*;
pub use marked_line::*;
pub use tab::*;
pub use workbench_action::*;

use std::cell::SyncUnsafeCell;
use std::fmt::{Display, Formatter, Write};
use std::mem::MaybeUninit;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::mpsc::TryRecvError;
use std::sync::Arc;
use std::time::Duration;

use crate::assets::{ZOffset, ACTION_WHEEL_Z, BASE_TEXT_Z, BASE_Z, CLOSED_WIDGET_UV, DARK_STRIPE_UV, HEADER_SIZE, HELD_ENTRY_Z, HIDDEN_BOOKMARK_UV, HORIZONTAL_SEPARATOR_UV, HOVERED_STRIPE_UV, HOVERED_WIDGET_UV, JUST_OVERLAPPING_BASE_TEXT_Z, LIGHT_STRIPE_UV, LINE_NUMBER_SEPARATOR_UV, NEW_FILE_UV, OPEN_FOLDER_UV, REPLACE_BOX_Z, SAVE_GRAYSCALE_UV, SAVE_UV, SELECTED_ACTION_WHEEL, SELECTED_WIDGET_UV, SELECTION_UV, SORT_COMPOUND_BY_NAME_UV, SORT_COMPOUND_BY_NOTHING_UV, SORT_COMPOUND_BY_TYPE_UV, TRAY_UV, UNSELECTED_ACTION_WHEEL, UNSELECTED_WIDGET_UV};
use crate::config;
use crate::elements::{CompoundMap, NbtByte, NbtByteArray, NbtChunk, NbtCompound, NbtDouble, NbtElement, NbtElementAndKey, NbtFloat, NbtInt, NbtIntArray, NbtList, NbtLong, NbtLongArray, NbtRegion, NbtShort, NbtString};
use crate::render::{RenderContext, TextColor, Vec2u, VertexBufferBuilder, WindowProperties, MIN_WINDOW_HEIGHT, MIN_WINDOW_WIDTH, WINDOW_HEIGHT, WINDOW_WIDTH};
use crate::serialization::{BigEndianDecoder, Decoder, UncheckedBufWriter};
use crate::util::{drop_on_separate_thread, encompasses_or_equal, get_clipboard, now, nth, set_clipboard, LinkedQueue, StrExt};
use crate::widget::{get_cursor_idx, get_cursor_left_jump_idx, get_cursor_right_jump_idx, Alert, Notification, NotificationKind, ReplaceBox, ReplaceBoxKeyResult, SearchBox, SearchBoxKeyResult, SelectedText, SelectedTextAdditional, SelectedTextKeyResult, Text, SEARCH_BOX_END_X, SEARCH_BOX_START_X, TEXT_DOUBLE_CLICK_INTERVAL};
use crate::{flags, get_interaction_information, hash, tab, tab_mut};
use crate::tree::{replace_element, remove_element, add_element, swap_element_same_depth, MutableIndices, Traverse, TraverseParents, Navigate, recache_along_indices, line_number_at, OwnedIndices, NavigationInformation, ParentNavigationInformationMut, TraversalInformation, TraversalInformationMut, ParentNavigationInformation};

use anyhow::{anyhow, Context, Result};
use compact_str::{format_compact, CompactString, ToCompactString};
use enum_map::EnumMap;
use fxhash::{FxBuildHasher, FxHashSet};
use uuid::Uuid;
use winit::dpi::{PhysicalPosition, PhysicalSize};
use winit::event::{ElementState, KeyEvent, MouseButton, MouseScrollDelta};
use winit::keyboard::{KeyCode, PhysicalKey};
use winit::window::Theme;
use itertools::Position;

#[derive(Debug)]
pub enum InteractionInformation<'a> {
    Header,
    InvalidContent { x: usize, y: usize },
    Content { is_in_left_margin: bool, depth: usize, x: usize, y: usize, line_number: usize, true_line_number: usize, key: Option<CompactString>, value: &'a mut NbtElement, indices: OwnedIndices },
}

pub struct Workbench {
    pub tabs: Vec<Tab>,
    pub tab: usize,
    last_mouse_state: ElementState,
    raw_mouse_x: f64,
    mouse_x: usize,
    raw_mouse_y: f64,
    mouse_y: usize,
    pub window_height: usize,
    raw_window_height: usize,
    pub window_width: usize,
    raw_window_width: usize,
    held_mouse_keys: FxHashSet<MouseButton>,
    held_keys: FxHashSet<KeyCode>,
    tab_scroll: usize,
    scrollbar_offset: Option<usize>,
    action_wheel: Option<(usize, usize)>,
    subscription: Option<FileUpdateSubscription>,
    pub cursor_visible: bool,
    alerts: Vec<Alert>,
    notifications: EnumMap<NotificationKind, Option<Notification>>,
    pub scale: f32,
    steal_animation_data: Option<(Duration, Vec2u)>,
    search_box: SearchBox,
    replace_box: ReplaceBox,
    ignore_event_end: Duration,
    debug_menu: bool,
}

impl Workbench {
    #[must_use]
    pub const unsafe fn uninit() -> Self {
        Self {
            tabs: vec![],
            tab: 0,
            last_mouse_state: ElementState::Released,
            raw_mouse_x: 0.0,
            mouse_x: 0,
            raw_mouse_y: 0.0,
            mouse_y: 0,
            window_height: 0,
            raw_window_height: 0,
            window_width: 0,
            raw_window_width: 0,
            held_mouse_keys: FxHashSet::with_hasher(unsafe { core::mem::zeroed() }),
            held_keys: FxHashSet::with_hasher(unsafe { core::mem::zeroed() }),
            tab_scroll: 0,
            scrollbar_offset: None,
            action_wheel: None,
            subscription: None,
            cursor_visible: false,
            alerts: vec![],
            notifications: EnumMap::from_array(const {
                let mut array = [const { MaybeUninit::<Option<Notification>>::uninit() }; core::mem::variant_count::<NotificationKind>()];
                let mut i = 0;
                while i < core::mem::variant_count::<NotificationKind>() {
                    array[i].write(None);
                    i += 1;
                }
                unsafe { MaybeUninit::array_assume_init(array) }
            }),
            scale: 0.0,
            steal_animation_data: None,
            search_box: SearchBox::uninit(),
            replace_box: ReplaceBox::uninit(),
            ignore_event_end: Duration::ZERO,
            debug_menu: false,
        }
    }

    #[must_use]
    pub fn new(window_properties: &mut WindowProperties, window_dims: Option<PhysicalSize<u32>>) -> Self {
        let mut workbench = Self {
            tabs: vec![],
            tab: 0,
            last_mouse_state: ElementState::Released,
            raw_mouse_x: 0.0,
            mouse_x: 0,
            raw_mouse_y: 0.0,
            mouse_y: 0,
            window_height: WINDOW_HEIGHT,
            raw_window_height: WINDOW_HEIGHT,
            window_width: WINDOW_WIDTH,
            raw_window_width: WINDOW_WIDTH,
            held_mouse_keys: FxHashSet::with_hasher(FxBuildHasher::default()),
            held_keys: FxHashSet::with_hasher(FxBuildHasher::default()),
            tab_scroll: 0,
            scrollbar_offset: None,
            action_wheel: None,
            subscription: None,
            cursor_visible: true,
            alerts: vec![],
            notifications: EnumMap::from_array(const {
                let mut array = [const { MaybeUninit::<Option<Notification>>::uninit() }; core::mem::variant_count::<NotificationKind>()];
                let mut i = 0;
                while i < core::mem::variant_count::<NotificationKind>() {
                    array[i].write(None);
                    i += 1;
                }
                unsafe { MaybeUninit::array_assume_init(array) }
            }),
            scale: 1.0,
            steal_animation_data: None,
            search_box: SearchBox::new(),
            replace_box: ReplaceBox::new(),
            ignore_event_end: Duration::ZERO,
            debug_menu: false,
        };
        if let Some(window_dims) = window_dims {
            workbench.raw_window_width = window_dims.width as usize;
            workbench.raw_window_height = window_dims.height as usize;

            let scale = config::get_scale();
            if let Some(scale) = scale {
                workbench.set_scale(scale);
            } else {
                workbench.set_scale(1000.0);
                workbench.set_scale(workbench.scale.floor());
            }
        }
        'create_tab: {
            if let Some(path) = &std::env::args()
                .nth(1)
                .and_then(|x| PathBuf::from_str(&x).ok())
            {
                if let Err(e) = workbench.on_open_file(path, std::fs::read(path).unwrap_or(vec![]), window_properties) {
                    workbench.alert(Alert::new("Error!", TextColor::Red, e.to_string()))
                } else {
                    break 'create_tab;
                }
            }
            workbench.new_custom_tab(window_properties, Tab {
                #[cfg(debug_assertions)]
                value: Box::new({
                    let sort = config::set_sort_algorithm(SortAlgorithm::None);
                    let result = NbtElement::from_be_file(include_bytes!("../assets/test.nbt")).expect("Included debug nbt contains valid data");
                    config::set_sort_algorithm(sort);
                    result
                }),
                #[cfg(debug_assertions)]
                name: "test.nbt".into(),
                #[cfg(not(debug_assertions))]
                value: Box::new(NbtElement::Compound(NbtCompound::new())),
                #[cfg(not(debug_assertions))]
                name: "new.nbt".into(),
                path: None,
                format: FileFormat::Nbt,
                undos: LinkedQueue::new(),
                redos: LinkedQueue::new(),
                unsaved_changes: false,
                scroll: 0,
                horizontal_scroll: 0,
                window_height: WINDOW_HEIGHT,
                window_width: WINDOW_WIDTH,
                bookmarks: MarkedLines::new(),
                uuid: Uuid::new_v4(),
                freehand_mode: false,
                selected_text: None,
                last_close_attempt: Duration::ZERO,
                last_selected_text_interaction: (0, 0, Duration::ZERO),
                last_interaction: now(),
                last_double_click_interaction: (0, Duration::ZERO),
                held_entry: HeldEntry::Empty,
                from_indices_arc: None,
            });
        }
        workbench
    }

    pub fn alert(&mut self, alert: Alert) { self.alerts.insert(0, alert); }

    pub fn notify(&mut self, notification: Notification) {
        let kind = notification.kind();
        match self.notifications[kind].as_mut() {
            Some(old_notification) => old_notification.update(notification),
            None => self.notifications[kind] = Some(notification),
        }
    }

    #[allow(clippy::equatable_if_let)]
    pub fn on_open_file(&mut self, path: &Path, buf: Vec<u8>, window_properties: &mut WindowProperties) -> Result<()> {
        let (nbt, format) = Tab::parse_raw(path, buf)?;
        let tab = Tab::new(nbt, path, format, self.window_height, self.window_width)?;
        self.new_custom_tab(window_properties, tab);
        Ok(())
    }

    pub fn on_scroll(&mut self, scroll: MouseScrollDelta) -> bool {
        let (h, v) = match scroll {
            MouseScrollDelta::LineDelta(h, v) => {
                (h, v)
            },
            MouseScrollDelta::PixelDelta(pos) => {
                (pos.x as f32, pos.y as f32)
            }
        };
        let ctrl = self.ctrl();
        let shift = self.shift();
        if ctrl {
            self.set_scale(self.scale + v.signum() * if shift { 1.0 } else { 0.1 });
            return true;
        }
        let shift = self.shift();
        if self.mouse_y < 21 {
            let scroll = if shift { -v } else { -h };
            self.tab_scroll = ((self.tab_scroll as isize + (scroll * 48.0) as isize).max(0) as usize).min(
                {
                    let mut tabs_width = 3_usize;
                    for tab in &self.tabs {
                        tabs_width += tab.name.width() + 32 + 6 + 6;
                    }
                    tabs_width
                }.saturating_sub(self.window_width),
            );
        } else {
            let tab = tab_mut!(self);
            if shift {
                tab.on_horizontal_scroll(-v);
                tab.on_scroll(-h);
            } else {
                tab.on_horizontal_scroll(-h);
                tab.on_scroll(-v);
            }
        }
        true
    }

    #[allow(clippy::collapsible_if)]
    pub fn on_mouse_input(&mut self, state: ElementState, button: MouseButton, window_properties: &mut WindowProperties) -> bool {
        tab_mut!(self).last_interaction = now();
        let left_margin = self.left_margin();
        let horizontal_scroll = self.horizontal_scroll();
        let shift = self.shift();
        let x = self.mouse_x;
        let y = self.mouse_y;
        self.last_mouse_state = state;
        match state {
            ElementState::Pressed => {
                {
                    self.held_mouse_keys.insert(button);

                    if let MouseButton::Left | MouseButton::Right = button && ReplaceBox::is_visible(&self.search_box, &self.replace_box) && self.try_select_replace_box(button) {
                        return true;
                    } else {
                        self.replace_box.deselect();
                    }

                    if let MouseButton::Left | MouseButton::Right = button && self.try_select_search_box(button) {
                        return true;
                    } else {
                        self.search_box.deselect();
                    }
                }

                if let MouseButton::Left | MouseButton::Right = button {
                    let shift = (self.shift()) ^ (button == MouseButton::Right);

                    if (self.window_width - SEARCH_BOX_END_X - 17 - 16 - 16 - 16..self.window_width - SEARCH_BOX_END_X - 1 - 16 - 16 - 16).contains(&self.mouse_x) & &(26..42).contains(&self.mouse_y) {
                        let tab = tab_mut!( self );
                        let notification = self.search_box.on_bookmark_widget(shift, &mut tab.bookmarks, &mut tab.value);
                        self.notify(notification);
                        return true;
                    }

                    if (self.window_width - SEARCH_BOX_END_X - 17 - 16 - 16..self.window_width - SEARCH_BOX_END_X - 1 - 16 - 16).contains(&self.mouse_x) & &(26..42).contains(&self.mouse_y) {
                        self.search_box.on_search_widget(shift);
                        return true;
                    }

                    if (self.window_width - SEARCH_BOX_END_X - 17 - 16..self.window_width - SEARCH_BOX_END_X - 1 - 16).contains(&self.mouse_x) & &(26..42).contains(&self.mouse_y) {
                        self.search_box.on_mode_widget(shift);
                        return true;
                    }

                    if (self.window_width - SEARCH_BOX_END_X - 17..self.window_width - SEARCH_BOX_END_X - 1).contains(&self.mouse_x) & &(26..42).contains(&self.mouse_y) {
                        self.search_box.on_exact_match_widget(shift);
                        return true;
                    }
                }

                if y < 19 && x > 2 && y > 3 {
                    self.click_tab(button, window_properties);
                } else if y < 42 && y > 26 && x < 16 {
                    self.open_file(window_properties);
                } else if y < 42 && y > 26 && x < 32 {
                    self.new_tab(window_properties, shift);
                } else if y < 42 && y > 26 && x >= 296 && x < 312 {
                    if let Err(e) = tab_mut!(self).refresh() {
                        self.alert(Alert::new("Error!", TextColor::Red, e.to_string()))
                    }
                }

                'a: {
                    if button == MouseButton::Left && y >= HEADER_SIZE {
                        let tab = tab_mut!(self);
                        match tab.held_entry.take() {
                            HeldEntry::Empty => {}
                            HeldEntry::FromAether(x) => {
                                self.drop(x, None, None, left_margin, false);
                                break 'a;
                            }
                            HeldEntry::FromKnown(x, indices, is_swap) => {
                                let from_indices_arc = tab.from_indices_arc.take();
                                self.drop(x, Some(indices), from_indices_arc, left_margin, is_swap);
                                break 'a;
                            }
                        }
                    }

                    if button == MouseButton::Left {
                        if self.bookmark_line(true) {
                            break 'a;
                        }
                    }

                    if x + horizontal_scroll >= left_margin && y >= HEADER_SIZE {
                        match self.action_wheel.take() {
                            Some(_) => {}
                            None => {
                                if button == MouseButton::Right && let InteractionInformation::Content { is_in_left_margin: false, depth, y, .. } = get_interaction_information!(self) && depth + 1 == (x + horizontal_scroll - left_margin) / 16 {
                                    self.action_wheel = Some((left_margin + depth * 16 + 16 + 6, y * 16 + HEADER_SIZE + 7));
                                    break 'a;
                                }
                            }
                        }

                        if MouseButton::Left == button {
                            if self.try_root_style_change() {
                                break 'a;
                            }
                        }

                        if MouseButton::Left == button {
                            if self.toggle(shift, tab!(self).freehand_mode) {
                                break 'a;
                            }
                        }

                        if button == MouseButton::Left {
                            if self.try_double_click_interaction() {
                                break 'a;
                            }
                        }

                        if button == MouseButton::Right {
                            if self.try_select_text(false) {
                                break 'a;
                            }
                        }

                        if button == MouseButton::Left {
                            if !self.try_steal(true) {
                                self.steal_animation_data = None;
                            }
                        }

                        if ((self.window_width - 7)..self.window_width).contains(&x) {
                            let tab = tab_mut!(self);
                            let height = tab.value.height() * 16 + 48;
                            let total = self.window_height - HEADER_SIZE;
                            if height - 48 > total {
                                let start = total * self.scroll() / height + HEADER_SIZE;
                                let end = start + total * total / height;
                                if (start..=end).contains(&y) {
                                    self.scrollbar_offset = Some(y - start);
                                    break 'a;
                                }
                            }
                        }
                    } else {
                        if !tab!(self).freehand_mode && tab!(self).held_entry.is_empty() && (24..46).contains(&y) && button == MouseButton::Left {
                            match self.hold_entry(button) {
                                Ok(true) => break 'a,
                                Err(e) => self.alert(Alert::new("Error!", TextColor::Red, e.to_string())),
                                _ => {}
                            }
                        }

                        if let MouseButton::Left | MouseButton::Right = button {
                            if (264..280).contains(&x) && (26..42).contains(&y) {
                                let tab = tab_mut!(self);
                                tab.freehand_mode = !tab.freehand_mode;
                                break 'a;
                            }
                            if (280..296).contains(&x) && (26..42).contains(&y) {
                                let sort_algorithm = config::get_sort_algorithm();
                                config::set_sort_algorithm(if (button == MouseButton::Right) ^ shift { sort_algorithm.rev_cycle() } else { sort_algorithm.cycle() });
                                break 'a;
                            }
                            if (312..328).contains(&x) && (26..42).contains(&y) {
                                config::set_theme(match config::get_theme() { Theme::Light => Theme::Dark, Theme::Dark => Theme::Light });
                                break 'a;
                            }
                        }
                    }
                }
            }
            ElementState::Released => {
                if let MouseButton::Right = button && let Some(selected_text) = tab_mut!(self).selected_text.as_mut() {
                    selected_text.set_drag_selectable(false);
                }

                if self.process_action_wheel() { return true; }
                self.scrollbar_offset = None;
                if button == MouseButton::Left {
                    self.steal_animation_data = None;
                }

                self.held_mouse_keys.remove(&button);
            }
        }
        true
    }

    fn process_action_wheel(&mut self) -> bool {
        if let Some((cx, cy)) = self.action_wheel.take() {
            let squared_distance_from_origin = (cy as isize - self.mouse_y as isize).pow(2) as usize + (cx as isize - self.mouse_x as isize).pow(2) as usize;
            if squared_distance_from_origin <= 8_usize.pow(2) { return true }
            let left_margin = self.left_margin();
            'a: {
                if cy >= HEADER_SIZE {
                    let tab = tab_mut!(self);
                    let scroll = tab.scroll();
                    if cy + scroll > HEADER_SIZE + tab.value.height() * 16 {
                        break 'a;
                    };
                    let highlight_idx = (((cy as f64 - self.mouse_y as f64).atan2(cx as f64 - self.mouse_x as f64) + core::f64::consts::FRAC_PI_8 + core::f64::consts::FRAC_PI_2 + core::f64::consts::FRAC_PI_4).rem_euclid(core::f64::consts::TAU) * core::f64::consts::FRAC_2_PI * 2.0) as usize;
                    let mut indices = OwnedIndices::new();
                    let mut depth = 0;
                    if (cy & !15) + scroll == HEADER_SIZE {
                        if let Some(action) = tab.value.actions().get(highlight_idx) {
                            if let Some(action) = action.apply(None, indices.clone(), tab.uuid, 1, 0, &mut tab.value, &mut tab.bookmarks, &mut MutableIndices::new(&mut self.subscription, &mut tab.selected_text), &mut self.alerts) {
                                recache_along_indices(&indices, &mut tab.value);
                                tab.append_to_history(action);
                            }
                        }
                        return true;
                    }
                    let x = (cx - left_margin) / 16;
                    let y = (cy - (HEADER_SIZE + 7) + scroll) / 16;
                    let mut iter = TraverseParents::new(x, y, &mut tab.value);
                    while let Some((position, idx, key, element, true_line_number)) = iter.next() {
                        let (key, element) = (key, element
                            .get_mut(idx)
                            .expect("Index is always valid"));
                        depth += 1;
                        indices.push(idx);
                        if let Position::Only | Position::Last = position {
                            let Some(action) = element.actions().get(highlight_idx).copied() else {
                                break 'a;
                            };
                            let min_x = depth * 16 + left_margin;
                            let max_x = min_x + 32;
                            if !(min_x..max_x).contains(&cx) {
                                break 'a;
                            };
                            if let Some(action) = action.apply(key, indices.clone(), tab.uuid, true_line_number, y, element, &mut tab.bookmarks, &mut MutableIndices::new(&mut self.subscription, &mut tab.selected_text), &mut self.alerts) {
                                recache_along_indices(&indices, &mut tab.value);
                                tab.append_to_history(action);
                            }
                            return true;
                        }
                    }
                }
            }
            true
        } else {
            false
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn try_subscription(&mut self) -> Result<()> {
        fn read_snbt(subscription: &FileUpdateSubscription, data: &[u8], tab: &mut Tab) -> Result<()> {
            let Ok(s) = core::str::from_utf8(data) else { return Err(anyhow!("File was not a valid UTF8 string")) };
            let sort = config::set_sort_algorithm(SortAlgorithm::None);
            let result = NbtElement::from_str(s);
            config::set_sort_algorithm(sort);
            let kv = match result {
                Ok((key, value)) => (key, value),
                Err(idx) => return Err(anyhow!("SNBT failed to parse at index {idx}"))
            };
            let action = replace_element(&mut tab.value, kv, subscription.indices.clone(), &mut tab.bookmarks, MutableIndices::empty()).context("Failed to replace element")?.into_action();
            tab.append_to_history(action);
            tab.refresh_scrolls();
            Ok(())
        }

        fn write_array(subscription: &FileUpdateSubscription, tab: &mut Tab, new_value: NbtElement) -> Result<()> {
            let NavigationInformation { key, .. } = tab.value.navigate(&subscription.indices).context("Failed to navigate subscription indices")?;
            let key = key.map(CompactString::from);
            let action = replace_element(&mut tab.value, (key, new_value), subscription.indices.clone(), &mut tab.bookmarks, MutableIndices::empty()).context("Failed to replace element")?.into_action();
            tab.append_to_history(action);
            tab.refresh_scrolls();
            Ok(())
        }

        if let Some(subscription) = &mut self.subscription {
            if let Some(tab) = self
                .tabs
                .iter_mut()
                .find(|tab| tab.uuid == subscription.tab_uuid)
            {
                if let Err(e) = subscription.watcher.poll().map_err(|x| anyhow!("{x}")) {
                    self.subscription = None;
                    return Err(e);
                };
                match subscription.rx.try_recv() {
                    Ok(data) => match subscription.subscription_type {
                        FileUpdateSubscriptionType::Snbt => read_snbt(subscription, &data, tab)?,
                        kind @ (FileUpdateSubscriptionType::ByteArray | FileUpdateSubscriptionType::IntArray | FileUpdateSubscriptionType::LongArray | FileUpdateSubscriptionType::ByteList | FileUpdateSubscriptionType::ShortList | FileUpdateSubscriptionType::IntList | FileUpdateSubscriptionType::LongList) => write_array(subscription, tab, {
                            let mut buf = UncheckedBufWriter::new();
                            let id = match kind {
                                FileUpdateSubscriptionType::ByteArray if data.len() % 1 == 0 => {
                                    buf.write(&(data.len() as u32).to_be_bytes());
                                    NbtByteArray::ID
                                },
                                FileUpdateSubscriptionType::IntArray if data.len() % 4 == 0 => {
                                    buf.write(&(data.len() as u32 / 4).to_be_bytes());
                                    NbtIntArray::ID
                                },
                                FileUpdateSubscriptionType::LongArray if data.len() % 8 == 0 => {
                                    buf.write(&(data.len() as u32 / 8).to_be_bytes());
                                    NbtLongArray::ID
                                },
                                FileUpdateSubscriptionType::ByteList if data.len() % 1 == 0 => {
                                    buf.write(&[NbtByte::ID]);
                                    buf.write(&(data.len() as u32).to_be_bytes());
                                    NbtList::ID
                                },
                                FileUpdateSubscriptionType::ShortList if data.len() % 2 == 0 => {
                                    buf.write(&[NbtShort::ID]);
                                    buf.write(&(data.len() as u32 / 2).to_be_bytes());
                                    NbtList::ID
                                },
                                FileUpdateSubscriptionType::IntList if data.len() % 4 == 0 => {
                                    buf.write(&[NbtInt::ID]);
                                    buf.write(&(data.len() as u32 / 4).to_be_bytes());
                                    NbtList::ID
                                },
                                FileUpdateSubscriptionType::LongList if data.len() % 8 == 0 => {
                                    buf.write(&[NbtLong::ID]);
                                    buf.write(&(data.len() as u32 / 8).to_be_bytes());
                                    NbtList::ID
                                },
                                _ => return Err(anyhow!("Invalid width for designated type of array")),
                            };
                            buf.write(&data);
                            let buf = buf.finish();
                            let mut decoder = BigEndianDecoder::new(&buf);
                            NbtElement::from_bytes(id, &mut decoder).context("Could not read bytes for array")?
                        })?,
                    },
                    Err(TryRecvError::Disconnected) => {
                        self.subscription = None;
                        return Err(anyhow!("Could not update; file subscription disconnected."));
                    }
                    Err(TryRecvError::Empty) => {
                        // do nothing ig
                    }
                }
            } else {
                self.subscription = None;
                return Err(anyhow!("Could not update; file subscription tab closed."));
            }
        }

        Ok(())
    }

    fn try_double_click_interaction(&mut self) -> bool {
        let now = now();
        let shift = self.shift();

        if !tab!(self).held_entry.is_empty() || tab!(self).freehand_mode || now - tab!(self).last_selected_text_interaction.2 <= LINE_DOUBLE_CLICK_INTERVAL { return false };

        if let InteractionInformation::Content { is_in_left_margin: false, y, .. } = get_interaction_information!(self) {
            let tab = tab_mut!(self);
            if tab.last_double_click_interaction.0 == y && now - tab.last_double_click_interaction.1 <= LINE_DOUBLE_CLICK_INTERVAL {
                tab.last_double_click_interaction = (y, now);
                if tab.value.as_region().is_some_and(|region| region.is_grid_layout()) {
                    self.bookmark_line(false);
                } else {
                    self.toggle(shift, true);
                }
                return true;
            } else {
                tab.last_double_click_interaction = (y, now);
            }
        }

        false
    }

    fn try_steal(&mut self, can_initialize: bool) -> bool {
        let now = now();

        if !tab!(self).held_entry.is_empty() || tab!(self).freehand_mode { return false };
        let is_grid_layout = tab!(self).value.as_region().is_some_and(|region| region.is_grid_layout());

        if let InteractionInformation::Content { is_in_left_margin: false, depth, x, y, .. } = get_interaction_information!(self) {
            if can_initialize {
                self.steal_animation_data.get_or_insert((now, (x, y).into()));
            }
            if let Some((_, expected)) = self.steal_animation_data.clone() {
                return expected == (depth + 1, y) || (is_grid_layout && expected == (x, y))
            }
        }
        false
    }

    fn steal(&mut self) -> bool {
        if !tab!(self).held_entry.is_empty() { return false }
        let is_grid_layout = tab!(self).value.as_region().is_some_and(NbtRegion::is_grid_layout);

        if let InteractionInformation::Content { is_in_left_margin: false, depth, x, y, indices, .. } = get_interaction_information!(self) && (depth + 1 == x || is_grid_layout) && y > 0 {
            let tab = tab_mut!(self);

            let result = match remove_element(&mut tab.value, indices, &mut tab.bookmarks, &mut MutableIndices::new(&mut self.subscription, &mut tab.selected_text)) {
                Some(result) => result,
                None => {
                    self.alert(Alert::new("Error!", TextColor::Red, "Failed to remove element"));
                    return false
                }
            };

            let (indices, (key, mut value), _replaced) = result.into_raw();
            value.shut();
            let from_indices_arc = Arc::new(SyncUnsafeCell::new(indices.clone()));
            tab.from_indices_arc = Some(Arc::clone(&from_indices_arc));
            tab.append_to_history(WorkbenchAction::HeldEntrySteal {
                from_indices: Some(from_indices_arc),
                original_key: key.clone(),
            });
            tab.held_entry = HeldEntry::FromKnown((key, value), indices, false);
            true
        } else {
            false
        }
    }

    fn rename(&mut self, offset: usize) -> bool {
        let left_margin = self.left_margin();
        let tab = tab_mut!(self);
        let path_minus_name_width = if let Some(path) = tab.path.as_ref().and_then(|path| path.as_os_str().to_str()) {
            path.width() - tab.name.width()
        } else {
            0
        };
        let name = tab
            .path
            .as_ref()
            .and_then(|x| x.to_str())
            .map_or_else(|| tab.name.clone(), Into::into);
        let suffix = format!("[{}]", tab.value.value().0).into_boxed_str();
        tab.set_selected_text(Some(0), SelectedText::new(
            36 + left_margin,
            offset + path_minus_name_width,
            HEADER_SIZE,
            Some((name, TextColor::TreeKey, true)),
            Some((suffix, TextColor::TreeKey, false)),
            OwnedIndices::new(),
        ));
        tab.selected_text.is_some()
    }

    fn duplicate(&mut self) -> bool {
        if let InteractionInformation::Content { is_in_left_margin: false, y, key, value, mut indices, .. } = get_interaction_information!(self) && y > 0 {
            *indices.last_mut().expect("y > 0") += 1;
            let duplicate = value.clone();
            let tab = tab_mut!(self);
            let action = match add_element(&mut tab.value, key, duplicate, indices, &mut tab.bookmarks, &mut MutableIndices::new(&mut self.subscription, &mut tab.selected_text)) {
                Some(action) => action,
                None => {
                    self.alert(Alert::new("Error!", TextColor::Red, "Failed to duplicate element"));
                    return false
                }
            };
            tab.append_to_history(action);
            tab.refresh_scrolls();
            true
        } else {
            false
        }
    }

    fn copy(&mut self, debug: bool) -> bool {
        if let InteractionInformation::Content { is_in_left_margin: false, key, value, .. } = get_interaction_information!(self) {
            let mut buf = String::new();
            let key = key.map(|key| {
                if key.needs_escape() {
                    format_compact!("{key:?}")
                } else {
                    key
                }
            });
            let key_exists = key.is_some();
            if debug {
                if write!(
                    &mut buf,
                    "{}{}{value:#?}",
                    key.unwrap_or(CompactString::const_new("")),
                    if key_exists { ": " } else { "" }
                )
                    .is_err()
                {
                    return false;
                }
            } else {
                if write!(
                    &mut buf,
                    "{}{}{value}",
                    key.unwrap_or(CompactString::const_new("")),
                    if key_exists { ":" } else { "" }
                )
                    .is_err()
                {
                    return false;
                }
            }
            set_clipboard(buf)
        } else {
            false
        }
    }

    fn delete(&mut self, clipboard: bool) -> bool {
        if let InteractionInformation::Content { is_in_left_margin: false, indices, key, value, .. } = get_interaction_information!(self) {
            if clipboard {
                let key = key.map(|key| {
                    if key.needs_escape() {
                        format_compact!("{key:?}")
                    } else {
                        key
                    }
                });
                let mut buf = String::new();
                if write!(&mut buf, "{}{}{value}", key.as_ref().map_or("", CompactString::as_str), if key.is_some() { ":" } else { "" }).is_ok() {
                    set_clipboard(buf);
                }
            }
            let tab = tab_mut!(self);
            let result = match remove_element(&mut tab.value, indices, &mut tab.bookmarks, &mut MutableIndices::new(&mut self.subscription, &mut tab.selected_text)) {
                Some(result) => result,
                None => {
                    self.alert(Alert::new("Error!", TextColor::Red, "Failed to remove element"));
                    return false
                }
            };
            tab.append_to_history(result.into_action());
            true
        } else {
            false
        }
    }

    fn drop(&mut self, pair: NbtElementAndKey, from_known_data: Option<OwnedIndices>, from_indices_arc: Option<Arc<SyncUnsafeCell<OwnedIndices>>>, left_margin: usize, is_swap: bool) -> bool {
        let (key, element) = pair;
        let horizontal_scroll = self.horizontal_scroll();

        if self.mouse_y <= HEADER_SIZE { return false }
        if self.mouse_x + horizontal_scroll < left_margin { return false }
        let y = self.mouse_y - HEADER_SIZE + self.scroll();
        let x = (self.mouse_x + horizontal_scroll - left_margin) / 16;
        let tab = tab_mut!(self);

        if element.id() == NbtChunk::ID && tab.value.id() != NbtRegion::ID { return false }
        let mut indices = OwnedIndices::new();
        match NbtElement::drop(
            &mut tab.value,
            key.clone(),
            element,
            &mut y.clone(),
            2,
            x,
            1,
            &mut indices,
        ) {
            DropFn::InvalidType((key, element)) | DropFn::Missed((key, element)) => {
                if let Some(from_indices) = from_known_data.clone() {
                    tab.append_to_history(WorkbenchAction::DeleteHeldEntry {
                        held_entry: HeldEntry::FromKnown((key, element), from_indices, is_swap),
                    });
                }
                false
            }
            DropFn::Dropped(height, true_height, _, line_number, value) => {
                if let Some(from_indices) = from_known_data.clone() {
                    if let Some(subscription) = &mut self.subscription
                        && encompasses_or_equal(&from_indices, &subscription.indices)
                    {
                        let (_, rest) = subscription.indices.split_at(from_indices.len());
                        let mut slice = Vec::with_capacity(indices.len() + rest.len());
                        slice.extend_from_slice(&indices);
                        slice.extend_from_slice(&rest);
                        subscription.indices = slice.into_boxed_slice();
                    }
                }
                recache_along_indices(&indices[..indices.len() - 1], &mut tab.value);
                tab.bookmarks[line_number..].increment(height, true_height);
                if let Some(value) = value {
                    tab.append_to_history(WorkbenchAction::HeldEntrySwap {
                        indices: indices.clone().into_boxed_slice(),
                        original_key: key,
                    });
                    tab.held_entry = HeldEntry::FromKnown(value, indices.into_boxed_slice(), true)
                } else {
                    if let Some(from_indices_arc) = from_indices_arc {
                        let _ = unsafe { core::ptr::replace(from_indices_arc.get(), indices.clone().into_boxed_slice()) };
                    }
                    tab.append_to_history(WorkbenchAction::HeldEntryDrop {
                        from_indices: from_known_data.map(|from_known_data| Arc::new(SyncUnsafeCell::new(from_known_data))),
                        indices: indices.into_boxed_slice(),
                        original_key: key,
                    });
                    tab.held_entry = HeldEntry::Empty;
                }
                self.subscription = None;
                true
            }
        }
    }

    fn hold_entry(&mut self, button: MouseButton) -> Result<bool> {
        if button == MouseButton::Left && self.mouse_x >= 16 + 16 + 4 {
            let tab = tab_mut!(self);
            let x = self.mouse_x - (16 + 16 + 4);
            if x / 16 == 13 {
                match NbtElement::from_str(&get_clipboard().ok_or_else(|| anyhow!("Failed to get clipboard"))?) {
                    Ok((key, element)) => {
                        if element.id() == NbtChunk::ID && tab.value.id() != NbtRegion::ID {
                            return Err(anyhow!("Chunks are not supported for non-region tabs"));
                        } else {
                            let old_held_entry = core::mem::replace(&mut tab.held_entry, HeldEntry::FromAether((key, element)));
                            if !old_held_entry.is_empty() {
                                tab.append_to_history(WorkbenchAction::DeleteHeldEntry { held_entry: old_held_entry })
                            }
                            tab.append_to_history(WorkbenchAction::CreateHeldEntry);
                        }
                    }
                    Err(idx) => return Err(anyhow!("Could not parse clipboard as SNBT (failed at index {idx})")),
                }
            } else {
                let old_held_entry = core::mem::replace(&mut tab.held_entry, HeldEntry::FromAether((None, NbtElement::from_id(match x / 16 {
                    0 => NbtByte::ID,
                    1 => NbtShort::ID,
                    2 => NbtInt::ID,
                    3 => NbtLong::ID,
                    4 => NbtFloat::ID,
                    5 => NbtDouble::ID,
                    6 => NbtByteArray::ID,
                    7 => NbtIntArray::ID,
                    8 => NbtLongArray::ID,
                    9 => NbtString::ID,
                    10 => NbtList::ID,
                    11 => NbtCompound::ID,
                    12 if tab.value.id() == NbtRegion::ID => NbtChunk::ID,
                    _ => return Ok(false),
                }))));
                if !old_held_entry.is_empty() {
                    tab.append_to_history(WorkbenchAction::DeleteHeldEntry { held_entry: old_held_entry })
                }
                tab.append_to_history(WorkbenchAction::CreateHeldEntry);
            }
        }
        Ok(true)
    }

    fn click_tab(&mut self, button: MouseButton, window_properties: &mut WindowProperties) {
        let mouse_x = self.mouse_x + self.tab_scroll;
        if mouse_x < 2 { return }

        let shift = self.shift();

        let mut x = mouse_x - 2;
        'a: {
            for (idx, tab) in self.tabs.iter_mut().enumerate() {
                let width = tab.name.width() + 48 + 5;

                if x <= width {
                    if button == MouseButton::Middle {
                        self.remove_tab(idx, window_properties);
                    } else if idx == self.tab && x > width - 16 && x < width {
                        if button == MouseButton::Left {
                            tab.format = tab.format.cycle();
                        } else if button == MouseButton::Right {
                            tab.format = tab.format.rev_cycle();
                        }
                    } else if idx == self.tab && x + 1 >= width - 32 && x < width - 16 {
                        if let Err(e) = tab.save(shift, window_properties) {
                            self.alert(Alert::new("Error!", TextColor::Red, e.to_string()));
                        }
                    } else if button == MouseButton::Left {
                        self.set_tab(idx, window_properties);
                    }

                    return;
                }

                x -= width;

                if x < 6 {
                    break 'a;
                } else {
                    x -= 6;
                }
            }

            if button == MouseButton::Middle {
                self.new_tab(window_properties, self.shift());
            }
        }
    }

    pub fn new_tab(&mut self, window_properties: &mut WindowProperties, region: bool) {
        self.new_custom_tab(window_properties, Tab {
            value: Box::new(if region { NbtElement::Region(NbtRegion::new()) } else { NbtElement::Compound(NbtCompound::new()) }),
            name: "new.nbt".into(),
            path: None,
            format: FileFormat::Nbt,
            undos: LinkedQueue::new(),
            redos: LinkedQueue::new(),
            unsaved_changes: false,
            scroll: 0,
            horizontal_scroll: 0,
            window_height: self.window_height,
            window_width: self.window_width,
            bookmarks: MarkedLines::new(),
            uuid: Uuid::new_v4(),
            freehand_mode: false,
            selected_text: None,
            last_close_attempt: Duration::ZERO,
            last_selected_text_interaction: (0, 0, Duration::ZERO),
            last_interaction: now(),
            last_double_click_interaction: (0, Duration::ZERO),
            held_entry: HeldEntry::Empty,
            from_indices_arc: None,
        });
    }

    pub fn new_custom_tab(&mut self, window_properties: &mut WindowProperties, tab: Tab) {
        self.tabs.push(tab);
        self.set_tab(self.tabs.len() - 1, window_properties);
    }

    pub fn remove_tab(&mut self, idx: usize, window_properties: &mut WindowProperties) -> bool {
        let tab = unsafe { self.tabs.get_unchecked_mut(idx) };
        if tab.unsaved_changes && (now() - core::mem::replace(&mut tab.last_close_attempt, now())) > Tab::TAB_CLOSE_DOUBLE_CLICK_INTERVAL {
            return false;
        }

        let tab = self.tabs.remove(idx);
        if self.tabs.is_empty() {
            #[cfg(target_arch = "wasm32")]
            if let Some(window) = web_sys::window() {
                let _ = window.close();
            }
            std::process::exit(0);
        }
        if idx <= self.tab {
            self.set_tab(self.tab.saturating_sub(1), window_properties);
        }
        drop_on_separate_thread(tab);
        true
    }

    #[cfg(any(target_os = "windows", target_os = "macos", target_os = "linux"))]
    fn open_file(&mut self, window_properties: &mut WindowProperties) {
        let dialog_result = Tab::FILE_TYPE_FILTERS.iter().fold(native_dialog::FileDialog::new().set_location("~/Downloads"), |builder, filter| builder.add_filter(filter.0, filter.1)).show_open_single_file();
        self.ignore_event_end = now() + Duration::from_millis(50);
        match dialog_result {
            Err(e) => self.alert(Alert::new("Error!", TextColor::Red, e.to_string())),
            Ok(None) => {},
            Ok(Some(path)) => match std::fs::read(&path) {
                Ok(bytes) => if let Err(e) = self.on_open_file(&path, bytes, window_properties) {
                    self.alert(Alert::new("Error!", TextColor::Red, e.to_string()))
                },
                Err(e) => self.alert(Alert::new("Error!", TextColor::Red, e.to_string())),
            }
        }
    }

    #[cfg(target_arch = "wasm32")]
    fn open_file(&mut self, _: &mut WindowProperties) {
        crate::wasm::try_open_dialog();
    }

    #[must_use]
    fn left_margin(&self) -> usize {
        tab!(self).left_margin()
    }

    #[must_use]
    #[cfg(target_os = "macos")]
    pub fn ctrl(&self) -> bool {
        self.held_keys.contains(&KeyCode::ControlLeft) | self.held_keys.contains(&KeyCode::ControlRight) | self.held_keys.contains(&KeyCode::SuperLeft) | self.held_keys.contains(&KeyCode::SuperRight)
    }

    #[must_use]
    #[cfg(not(target_os = "macos"))]
    pub fn ctrl(&self) -> bool {
        self.held_keys.contains(&KeyCode::ControlLeft) | self.held_keys.contains(&KeyCode::ControlRight)
    }

    #[must_use]
    pub fn shift(&self) -> bool {
        self.held_keys.contains(&KeyCode::ShiftLeft) | self.held_keys.contains(&KeyCode::ShiftRight)
    }

    #[must_use]
    pub fn alt(&self) -> bool {
        self.held_keys.contains(&KeyCode::AltLeft) | self.held_keys.contains(&KeyCode::AltRight)
    }

    #[must_use]
    pub fn get_interaction_information_raw(left_margin: usize, horizontal_scroll: usize, scroll: usize, mouse_x: usize, mouse_y: usize, value: &mut NbtElement) -> InteractionInformation {
        if mouse_y < HEADER_SIZE { return InteractionInformation::Header }
        let y = (mouse_y + scroll - HEADER_SIZE) / 16;
        let is_in_left_margin = mouse_x + horizontal_scroll < left_margin;
        let x = (mouse_x + horizontal_scroll).saturating_sub(left_margin) / 16;
        if y >= value.height() { return InteractionInformation::InvalidContent { x, y } };

        match value.traverse_mut(y, Some(x)) {
            Some(TraversalInformationMut { depth, key, element: value, line_number, true_line_number, indices }) => InteractionInformation::Content { is_in_left_margin, depth, key, value, line_number, true_line_number, x, y, indices },
            None => InteractionInformation::InvalidContent { x, y },
        }
    }

    fn try_root_style_change(&mut self) -> bool {
        let left_margin = self.left_margin();
        let horizontal_scroll = self.horizontal_scroll();
        if self.mouse_x + horizontal_scroll < left_margin { return false }
        if self.mouse_y < HEADER_SIZE { return false }
        let x = (self.mouse_x + horizontal_scroll - left_margin) / 16;
        let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;
        let tab = tab_mut!(self);
        if !(x == 1 && y == 0) {
            return false
        }
        tab.value.on_root_style_change(&mut tab.bookmarks);
        recache_along_indices(&[], &mut tab.value);
        tab.refresh_scrolls();
        true
    }

    fn toggle(&mut self, expand: bool, ignore_depth: bool) -> bool {
        let left_margin = self.left_margin();
        let horizontal_scroll = self.horizontal_scroll();
        if let InteractionInformation::Content { is_in_left_margin: false, x, y, depth, value, true_line_number, indices, .. } = get_interaction_information!(self) && (x <= depth || ignore_depth) && !value.is_primitive() && value.true_height() > 1 {
            let (height, true_height) = (value.height(), value.true_height());

            if expand {
                #[cfg(not(target_arch = "wasm32"))]
                std::thread::scope(|scope| value.expand(scope));
                #[cfg(target_arch = "wasm32")]
                value.expand();
            } else {
                // let start = std::time::Instant::now();
                let _ = value.toggle();
                // println!("Toggle: {}ns", start.elapsed().as_nanos());
            }
            let increment = value.height().wrapping_sub(height);
            let open = value.open();
            let tab = tab_mut!(self);

            // toggle has no effect on true height
            // let start = std::time::Instant::now();
            recache_along_indices(&indices, &mut tab.value);
            // println!("Recache along indices: {}ns", start.elapsed().as_nanos());
            tab.refresh_scrolls();
            let InteractionInformation::Content { is_in_left_margin: false, value, .. } = Self::get_interaction_information_raw(left_margin, horizontal_scroll, tab.scroll(), self.mouse_x, self.mouse_y, &mut tab.value) else { panic!("wut") };
            if expand {
                tab.bookmarks[true_line_number + 1..true_line_number + true_height].iter_mut().for_each(|bookmark| *bookmark = bookmark.open(y + bookmark.true_line_number() - true_line_number));
                tab.bookmarks[true_line_number + true_height..].increment(increment, 0);
            } else if open {
                let mut current_true_line_number = true_line_number + 1;
                let mut current_line_number = y + 1;
                for idx in 0..value.len().expect("Value is complex") {
                    let child_true_height = value[idx].true_height();
                    for bookmark in tab.bookmarks[current_true_line_number..current_true_line_number + child_true_height].iter_mut() {
                        if bookmark.true_line_number() == current_true_line_number {
                            *bookmark = bookmark.open(current_line_number);
                            continue
                        }

                        *bookmark = bookmark.hidden(current_line_number);
                    }
                    current_true_line_number += child_true_height;
                    current_line_number += 1;
                }
                tab.bookmarks[true_line_number + true_height..].increment(increment, 0);
            } else {
                tab.bookmarks[true_line_number + 1..true_line_number + true_height].iter_mut().for_each(|bookmark| *bookmark = bookmark.hidden(y));
                tab.bookmarks[true_line_number + true_height..].increment(increment, 0);
            }
            true
        } else {
            false
        }
    }

    fn try_select_search_box(&mut self, button: MouseButton) -> bool {
        if SearchBox::is_within_bounds((self.mouse_x, self.mouse_y), self.window_width) {
            self.search_box.select(self.mouse_x - SEARCH_BOX_START_X, button);
            self.replace_box.deselect();

            let now = now();
            let (times_clicked, timestamp) = self.search_box.last_interaction;
            if now - timestamp < TEXT_DOUBLE_CLICK_INTERVAL && !self.search_box.value.is_empty() {
                self.search_box.last_interaction = (times_clicked + 1, now);
                // the previous click count was divisible by 1
                let (left, right) = if times_clicked % 2 == 1 {
                    (0, self.search_box.value.len())
                } else {
                    (get_cursor_left_jump_idx(self.search_box.cursor, self.search_box.value.as_bytes()), get_cursor_right_jump_idx(self.search_box.cursor, self.search_box.value.as_bytes()))
                };
                // if they're == it's also false, just being careful here
                if right > left {
                    self.search_box.selection = Some(left);
                }
                self.search_box.cursor = right;
            } else {
                self.search_box.last_interaction = (0, now);
            }
            true
        } else {
            false
        }
    }

    fn try_select_replace_box(&mut self, button: MouseButton) -> bool {
        if ReplaceBox::is_within_bounds((self.mouse_x, self.mouse_y), self.window_width) {
            self.replace_box.select(self.mouse_x - SEARCH_BOX_START_X, button);
            self.search_box.deselect();

            let now = now();
            let (times_clicked, timestamp) = self.replace_box.last_interaction;
            if now - timestamp < TEXT_DOUBLE_CLICK_INTERVAL && !self.replace_box.value.is_empty() {
                self.replace_box.last_interaction = (times_clicked + 1, now);
                // the previous click count was divisible by 1
                let (left, right) = if times_clicked % 2 == 1 {
                    (0, self.replace_box.value.len())
                } else {
                    (get_cursor_left_jump_idx(self.replace_box.cursor, self.replace_box.value.as_bytes()), get_cursor_right_jump_idx(self.replace_box.cursor, self.replace_box.value.as_bytes()))
                };
                // if they're == it's also false, just being careful here
                if right > left {
                    self.replace_box.selection = Some(left);
                }
                self.replace_box.cursor = right;
            } else {
                self.replace_box.last_interaction = (0, now);
            }
            true
        } else {
            false
        }
    }

    fn try_select_text(&mut self, snap_to_ends: bool) -> bool {
        let left_margin = self.left_margin();
        let horizontal_scroll = self.horizontal_scroll();

        if self.mouse_x + horizontal_scroll < left_margin { return false }
        if self.mouse_y < HEADER_SIZE { return false }

        let y = (self.mouse_y - HEADER_SIZE) / 16 + self.scroll() / 16;

        self.set_selected_text_at_y(y, self.mouse_x, snap_to_ends)
    }

    pub fn set_selected_text_at_y(&mut self, y: usize, mouse_x: usize, snap_to_ends: bool) -> bool {
        let left_margin = self.left_margin();
        let horizontal_scroll = self.horizontal_scroll();

        let tab = tab_mut!(self);

        if y == 0 {
            let max = tab.name.width() + 32 + 4 + self.left_margin();
            return self.rename(mouse_x.min(max))
        }

        if y >= tab.value.height() { return false }
        if tab.value.as_region().is_some_and(|region| region.is_grid_layout()) { return false }

        let TraversalInformation { indices, depth, key, element, .. } = tab.value.traverse(y, None);
        let target_x = depth * 16 + 32 + 4 + left_margin;
        if element.as_chunk().is_some() && self.mouse_x < target_x - 4 { return false }
        let k = key.map(|x| (x.to_owned().into_boxed_str(), TextColor::TreeKey, true));
        let v = Some(element.value()).map(|(a, c)| (a.into_string().into_boxed_str(), c, c != TextColor::TreeKey));
        let mouse_x = if snap_to_ends {
            let min_x = target_x;
            let max_x = k.as_ref().map_or(0, |(k, _, b)| (*b as usize) * (k.width() + ": ".width() * v.is_some() as usize)) + v.as_ref().map_or(0, |(v, _, b)| (*b as usize) * v.width()) + target_x;
            (mouse_x + horizontal_scroll).clamp(min_x, max_x)
        } else {
            mouse_x + horizontal_scroll
        };

        tab.set_selected_text(Some(y), SelectedText::new(target_x, mouse_x, y * 16 + HEADER_SIZE, k, v, indices));
        tab.selected_text.is_some()
    }

    fn bookmark_line(&mut self, require_left_margin_cursor: bool) -> bool {
        if let InteractionInformation::Content { is_in_left_margin, true_line_number, y, .. } = get_interaction_information!(self) && (is_in_left_margin || !require_left_margin_cursor) {
            let bookmark = MarkedLine::new(true_line_number, y);
            let _ = tab_mut!(self).bookmarks.toggle(bookmark);
            true
        } else {
            false
        }
    }

    pub fn keyfix(&mut self, window_properties: &mut WindowProperties) {
        let tab = tab_mut!(self);
        if let Some(SelectedText(Text { value, cursor, editable: true, additional: SelectedTextAdditional { y, indices, value_color, keyfix, prefix, suffix, valuefix }, .. })) = tab.selected_text.clone()
            && let Some((keyfix, keyfix_color)) = keyfix
            && valuefix.is_none()
            && suffix.0.is_empty()
            && cursor == 0
        {
            if !tab.write_selected_text(false, window_properties, true) { return }
            tab.set_selected_text(None, Some(SelectedText(Text::new(keyfix.clone(), keyfix.len(), true, SelectedTextAdditional {
                y,
                indices,
                value_color: keyfix_color,
                keyfix: None,
                prefix: (String::new(), TextColor::White),
                suffix: prefix,
                valuefix: Some((value, value_color)),
            }))));
        }
    }

    pub fn valuefix(&mut self, window_properties: &mut WindowProperties) {
        let tab = tab_mut!(self);
        if let Some(SelectedText(Text { value, cursor, editable: true, additional: SelectedTextAdditional { y, indices, value_color, keyfix, prefix, suffix, valuefix }, .. })) = tab.selected_text.clone()
            && let Some((valuefix, valuefix_color)) = valuefix
            && keyfix.is_none()
            && prefix.0.is_empty()
            && cursor == value.len()
        {
            // normally won't occur, but im future proofing
            if !tab.write_selected_text(false, window_properties, true) { return }
            tab.set_selected_text(None, Some(SelectedText(Text::new(valuefix, 0, true, SelectedTextAdditional {
                y,
                indices,
                value_color: valuefix_color,
                keyfix: Some((value, value_color)),
                prefix: suffix,
                suffix: (String::new(), TextColor::White),
                valuefix: None,
            }))));
        }
    }

    pub fn shift_selected_text_same_depth(&mut self, sibling_idx: impl FnOnce(usize) -> Option<usize>) -> Option<()> {
        let tab = tab_mut!(self);
        let Some(SelectedText(Text { additional: SelectedTextAdditional { indices, .. }, .. })) = &tab.selected_text else { return None };
        let ParentNavigationInformationMut { parent, idx: a_idx, parent_indices, .. } = tab.value.navigate_parent_mut(indices)?;
        let b_idx = sibling_idx(a_idx)?;
        if parent.get(a_idx).is_none() || parent.get(b_idx).is_none() { return None };
        let result = match swap_element_same_depth(&mut tab.value, parent_indices.to_owned(), a_idx, b_idx, &mut tab.bookmarks, &mut MutableIndices::new(&mut self.subscription, &mut tab.selected_text)) {
            Some(action) => action,
            None => {
                self.alert(Alert::new("Error!", TextColor::Red, "Failed to swap elements"));
                return None
            }
        };

        let mut result_indices = result.as_raw().0.to_owned();
        result_indices.push(b_idx);
        tab.refresh_scrolls();
        tab.append_to_history(result.into_action());

        if let Some(selected_text) = &mut tab.selected_text {
            selected_text.set_indices(result_indices, &tab.value);
        }

        Some(())
    }

    pub fn shift_selected_text_up(&mut self) -> Option<()> {
        self.shift_selected_text_same_depth(|idx| idx.checked_sub(1))
    }

    pub fn shift_selected_text_down(&mut self) -> Option<()> {
        self.shift_selected_text_same_depth(|idx| idx.checked_add(1))
    }

    pub fn move_selected_text(&mut self, window_properties: &mut WindowProperties, f: impl FnOnce(usize, &NbtElement, OwnedIndices) -> Option<usize>) -> bool {
        let left_margin = self.left_margin();
        let tab = tab_mut!(self);

        let Some(SelectedText(Text { additional: SelectedTextAdditional { y, mut indices, keyfix, prefix, .. }, value: str_value, cursor, .. })) = tab.selected_text.clone() else { return false };
        if !tab.write_selected_text(false, window_properties, true) { return false }

        let cache_cursor_x = tab.cache_cursor_x;
        let depth = indices.len();
        let y = (*y - HEADER_SIZE) / 16;
        let mouse_x = cache_cursor_x.unwrap_or(depth * 16 + 32 + 4 + left_margin + keyfix.as_ref().map_or(0, |x| x.0.width()) + prefix.0.width() + str_value.split_at(*cursor).0.width());

        let Some(new_y) = f(y, &tab.value, indices) else { return false };

        self.set_selected_text_at_y(new_y, mouse_x, true);
        tab.cache_cursor_x.get_or_insert(mouse_x);
        tab.modify_scroll(|scroll| scroll.min(new_y * 16));
        tab.refresh_scrolls();
        true
    }

    pub fn selected_text_up(&mut self, ctrl: bool, window_properties: &mut WindowProperties) -> bool {
        self.move_selected_text(window_properties, |y, root, mut indices| Some(if ctrl && let Some(last_idx) = indices.last_mut() && *last_idx > 0 {
            *last_idx -= 1;
            root.navigate(&indices)?.line_number
        } else {
            y - 1
        }))
    }

    pub fn selected_text_down(&mut self, ctrl: bool, window_properties: &mut WindowProperties) -> bool {
        self.move_selected_text(window_properties, |y, root, mut indices| Some(if ctrl && let Some((last_idx, parent_indices)) = indices.split_last() {
            let NavigationInformation { element: parent, line_number, .. } = root.navigate(&parent_indices)?;
            let len = parent.len()?;
            if last_idx + 1 == len {
                y + 1
            } else {
                line_number + 1
            }
        } else {
            y + 1
        }))
    }

    pub fn force_close(&mut self) {
        let tab = tab_mut!(self);
        if let Some(SelectedText(Text { additional: SelectedTextAdditional { y, indices, .. }, .. })) = tab.selected_text.as_ref() {
            let indices = indices.clone();
            let (_, _, element, line_number) = Navigate::new(indices.iter().copied(), &mut tab.value).last();
            if element.open() && element.toggle().is_some() {
                recache_along_indices(&indices, &mut tab.value);
                for bookmark in tab.bookmarks[line_number..].iter_mut() {
                    *bookmark = MarkedLine::with_uv(bookmark.true_line_number(), (*y - HEADER_SIZE) / 16, HIDDEN_BOOKMARK_UV);
                }
            }
        }
    }

    pub fn force_open(&mut self) {
        let shift = self.shift();
        let tab = tab_mut!(self);
        if let Some(SelectedText(Text { additional: SelectedTextAdditional { y, indices, .. }, .. })) = tab.selected_text.as_ref() {
            let indices = indices.clone();
            let (_, _, element, line_number) = Navigate::new(indices.iter().copied(), &mut tab.value).last();
            let true_height = element.true_height();
            let predicate = if shift {
                #[cfg(not(target_arch = "wasm32"))]
                std::thread::scope(|scope| element.expand(scope));
                #[cfg(target_arch = "wasm32")]
                element.expand();
                true
            } else {
                !element.open() && element.toggle().is_some()
            };
            if predicate {
                let increment = element.height() - 1;
                recache_along_indices(&indices, &mut tab.value);
                if shift {
                    let parent_line_number = (*y - HEADER_SIZE) / 16;
                    for bookmark in tab.bookmarks[line_number..].iter_mut() {
                        *bookmark = if bookmark.true_line_number() - line_number < true_height {
                            bookmark.open(parent_line_number + bookmark.true_line_number() - line_number)
                        } else {
                            MarkedLine::with_uv(bookmark.true_line_number(), bookmark.line_number() + increment, bookmark.uv())
                        };
                    }
                } else {
                    let element = Navigate::new(indices.iter().copied(), &mut tab.value)
                        .last()
                        .2;
                    let mut next_line_number = line_number;
                    let mut next_line_number_idx = 0;
                    for bookmark in tab.bookmarks[line_number..].iter_mut() {
                        *bookmark = if bookmark.true_line_number() - line_number < true_height {
                            while next_line_number < bookmark.true_line_number() {
                                next_line_number += element[next_line_number_idx].true_height();
                                next_line_number_idx += 1;
                            }
                            let new_line_number = y + next_line_number_idx + 1;
                            if bookmark.true_line_number() == next_line_number {
                                bookmark.open(new_line_number)
                            } else {
                                bookmark.hidden(new_line_number)
                            }
                        } else {
                            MarkedLine::with_uv(bookmark.true_line_number(), bookmark.line_number() + increment, bookmark.uv())
                        };
                    }
                }
            }
        }
    }

    pub fn refresh_selected_text_horizontal_scroll(&mut self) {
        let tab = tab_mut!(self);
        let free_space = 48 + tab.left_margin();
        if let Some(selected_text) = tab.selected_text.as_ref() {
            let left_margin = tab.left_margin();
            let horizontal_scroll = tab.horizontal_scroll();
            let pos = left_margin + selected_text.indices.len() * 16 + 32 + 4 + selected_text.prefix.0.width() + selected_text.keyfix.as_ref().map_or(0, |x| x.0.width()) + selected_text.value.split_at(selected_text.cursor).0.width();
            if pos + free_space < self.window_width {
                tab.horizontal_scroll = 0;
                tab.horizontal_scroll = tab.horizontal_scroll();
            } else if pos + free_space >= self.window_width + horizontal_scroll {
                tab.horizontal_scroll = pos + free_space - self.window_width;
                tab.horizontal_scroll = tab.horizontal_scroll();
            } else if pos < horizontal_scroll + free_space {
                tab.horizontal_scroll = pos.saturating_sub(free_space);
                tab.horizontal_scroll = tab.horizontal_scroll();
            }
        }
    }

    #[allow(
        clippy::collapsible_if,
        clippy::too_many_lines,
        clippy::cognitive_complexity
    )]
    pub fn on_key_input(&mut self, key: &KeyEvent, window_properties: &mut WindowProperties) -> bool {
        tab_mut!(self).last_interaction = now();
        if key.state == ElementState::Pressed {
            if let PhysicalKey::Code(key) = key.physical_key {
                self.held_keys.insert(key);
                let char = self.char_from_key(key);
                let flags = self.ctrl() as u8 | ((self.shift() as u8) << 1) | ((self.alt() as u8) << 2);
                let left_margin = self.left_margin();
                let tab = tab_mut!(self);
                if self.search_box.is_selected() {
                    match self.search_box.on_key_press(key, char, flags) {
                        SearchBoxKeyResult::Failed => {} // next thing, please
                        SearchBoxKeyResult::NothingSpecial => {
                            self.search_box.post_input((self.window_width, self.window_height));
                            return true;
                        }
                        SearchBoxKeyResult::Escape => {
                            self.search_box.post_input((self.window_width, self.window_height));
                            self.search_box.deselect();
                            return true;
                        }
                        SearchBoxKeyResult::MoveToReplaceBox => {
                            self.search_box.post_input((self.window_width, self.window_height));
                            self.replace_box.select(self.search_box.value.split_at(self.search_box.cursor).0.width().saturating_sub(self.search_box.horizontal_scroll), MouseButton::Left);
                            self.search_box.deselect();
                            return true;
                        }
                        result @ (SearchBoxKeyResult::ClearAndSearch | SearchBoxKeyResult::Search | SearchBoxKeyResult::SearchCountOnly) => {
                            if result == SearchBoxKeyResult::ClearAndSearch {
                                tab.bookmarks.clear();
                                self.search_box.post_input((self.window_width, self.window_height));
                            }
                            let notification = self.search_box.search(&mut tab.bookmarks, &tab.value, result == SearchBoxKeyResult::SearchCountOnly);
                            self.notify(notification);
                            self.search_box.post_input((self.window_width, self.window_height));
                            return true;
                        }
                    }
                } else if self.replace_box.is_selected() {
                    match self.replace_box.on_key_press(key, char, flags) {
                        ReplaceBoxKeyResult::Failed => {} // next thing, please
                        ReplaceBoxKeyResult::NothingSpecial => {
                            self.replace_box.post_input((self.window_width, self.window_height));
                            return true;
                        }
                        ReplaceBoxKeyResult::Escape => {
                            self.replace_box.post_input((self.window_width, self.window_height));
                            self.replace_box.deselect();
                            return true;
                        }
                        ReplaceBoxKeyResult::MoveToSearchBox => {
                            self.replace_box.post_input((self.window_width, self.window_height));
                            self.search_box.select(self.replace_box.value.split_at(self.replace_box.cursor).0.width().saturating_sub(self.replace_box.horizontal_scroll), MouseButton::Left);
                            self.replace_box.deselect();
                            return true;
                        }
                        ReplaceBoxKeyResult::ReplaceAll => {
                            let (notification, bulk) = self.replace_box.replace(&mut tab.bookmarks, &mut MutableIndices::new(&mut self.subscription, &mut tab.selected_text), &mut tab.value, &self.search_box);
                            if let Some(bulk) = bulk {
                                tab.append_to_history(bulk);
                            }
                            self.notify(notification);
                            self.replace_box.post_input((self.window_width, self.window_height));
                            return true;
                        }
                    }
                }
                if let Some(selected_text) = &mut tab.selected_text {
                    match selected_text.on_key_press(key, char, flags) {
                        SelectedTextKeyResult::NothingSpecial => {
                            selected_text.post_input();
                            self.cache_cursor_x = None;
                            self.refresh_selected_text_horizontal_scroll();
                            return true;
                        }
                        SelectedTextKeyResult::Escape => {
                            tab.selected_text = None;
                            self.cache_cursor_x = None;
                            return true;
                        }
                        SelectedTextKeyResult::Finish => {
                            // we just won't let you leave if you didn't fix it ;)
                            let _ = tab.write_selected_text(true, window_properties, true);
                            self.cache_cursor_x = None;
                            return true;
                        }
                        SelectedTextKeyResult::Keyfix => {
                            self.keyfix(window_properties);
                            self.refresh_selected_text_horizontal_scroll();
                            self.cache_cursor_x = None;
                            return true;
                        }
                        SelectedTextKeyResult::Valuefix => {
                            self.valuefix(window_properties);
                            self.refresh_selected_text_horizontal_scroll();
                            self.cache_cursor_x = None;
                            return true;
                        }
                        SelectedTextKeyResult::Up(ctrl) => {
                            self.selected_text_up(ctrl, window_properties);
                            self.refresh_selected_text_horizontal_scroll();
                            return true;
                        }
                        SelectedTextKeyResult::Down(ctrl) => {
                            unsafe {
                                self.selected_text_down(ctrl, window_properties);
                            }
                            self.refresh_selected_text_horizontal_scroll();
                            return true;
                        }
                        SelectedTextKeyResult::ForceClose => {
                            selected_text.post_input();
                            self.force_close();
                            return true;
                        }
                        SelectedTextKeyResult::ForceOpen => {
                            selected_text.post_input();
                            self.force_open();
                            return true;
                        }
                        SelectedTextKeyResult::ShiftUp => {
                            selected_text.post_input();
                            self.shift_selected_text_up();
                            return true;
                        }
                        SelectedTextKeyResult::ShiftDown => {
                            selected_text.post_input();
                            self.shift_selected_text_down();
                            return true;
                        }
                        SelectedTextKeyResult::Failed => {} // next thing pls
                    }
                }
                if key == KeyCode::KeyF && flags == flags!(Ctrl) {
                    self.search_box.select(0, MouseButton::Left);
                    self.replace_box.deselect();
                    return true;
                }
                if key == KeyCode::KeyR && flags == flags!(Ctrl) {
                    self.replace_box.select(0, MouseButton::Left);
                    self.search_box.deselect();
                    return true;
                }
                if key == KeyCode::Equal && flags & !flags!(Shift) == flags!(Ctrl) {
                    self.set_scale(self.scale + if flags == flags!(Ctrl + Shift) { 1.0 } else { 0.1 });
                    return true;
                }
                if key == KeyCode::Minus && flags & !flags!(Shift) == flags!(Ctrl) {
                    self.set_scale(self.scale - if flags == flags!(Ctrl + Shift) { 1.0 } else { 0.1 });
                    return true;
                }
                if self.action_wheel.is_some() && key == KeyCode::Escape && flags == flags!() {
                    self.action_wheel = None;
                    return true;
                }
                if !tab.held_entry.is_empty() && key == KeyCode::Escape && flags == flags!() {
                    let action = WorkbenchAction::DeleteHeldEntry { held_entry: tab.held_entry.take() };
                    tab.append_to_history(action);
                    return true;
                }
                if (key == KeyCode::Enter || key == KeyCode::NumpadEnter) && tab.selected_text.is_none() && flags == flags!() {
                    return match tab.held_entry.take() {
                        HeldEntry::Empty => { self.try_select_text(true); true },
                        HeldEntry::FromAether(pair) => self.drop(pair, None, None, left_margin, false),
                        HeldEntry::FromKnown(pair, indices, is_swap) => {
                            let from_indices_arc = tab.from_indices_arc.take();
                            return self.drop(pair, Some(indices), from_indices_arc, left_margin, is_swap)
                        }
                    }
                }
                if key == KeyCode::F3 && flags == flags!() {
                    self.debug_menu = !self.debug_menu;
                }
                if flags == flags!(Ctrl) {
                    if key == KeyCode::Digit1 {
                        self.set_tab(0, window_properties);
                        return true;
                    }
                    if key == KeyCode::Digit2 {
                        self.set_tab(1, window_properties);
                        return true;
                    }
                    if key == KeyCode::Digit3 {
                        self.set_tab(2, window_properties);
                        return true;
                    }
                    if key == KeyCode::Digit4 {
                        self.set_tab(3, window_properties);
                        return true;
                    }
                    if key == KeyCode::Digit5 {
                        self.set_tab(4, window_properties);
                        return true;
                    }
                    if key == KeyCode::Digit6 {
                        self.set_tab(5, window_properties);
                        return true;
                    }
                    if key == KeyCode::Digit7 {
                        self.set_tab(6, window_properties);
                        return true;
                    }
                    if key == KeyCode::Digit8 {
                        self.set_tab(7, window_properties);
                        return true;
                    }
                    if key == KeyCode::Digit9 {
                        self.set_tab(usize::MAX, window_properties);
                        return true;
                    }
                }
                if key == KeyCode::KeyR && flags == flags!(Ctrl) {
                    if let Err(e) = tab.refresh() {
                        self.alert(Alert::new("Error!", TextColor::Red, e.to_string()))
                    }
                    return true;
                }
                if key == KeyCode::KeyF && flags == flags!(Ctrl + Shift) {
                    tab.freehand_mode = !tab.freehand_mode;
                    return true;
                }
                if key == KeyCode::KeyT && flags == flags!(Ctrl + Alt) {
                    config::set_theme(match config::get_theme() { Theme::Light => Theme::Dark, Theme::Dark => Theme::Light });
                    return true;
                }
                if key == KeyCode::KeyN && flags & (!flags!(Shift)) == flags!(Ctrl) {
                    self.new_tab(window_properties, (flags & flags!(Shift)) > 0);
                    return true;
                }
                if key == KeyCode::KeyO && flags == flags!(Ctrl) {
                    self.open_file(window_properties);
                    return true;
                }
                if key == KeyCode::KeyS && flags & (!flags!(Shift)) == flags!(Ctrl) {
                    return if let Err(e) = tab.save((flags & flags!(Shift)) > 0, window_properties) {
                        self.alert(Alert::new("Error!", TextColor::Red, e.to_string()));
                        false
                    } else {
                        true
                    }
                }
                if key == KeyCode::KeyW && flags == flags!(Ctrl) {
                    self.remove_tab(self.tab, window_properties);
                    return true;
                }
                if key == KeyCode::KeyZ && flags == flags!(Ctrl) {
                    if let Some(action) = tab.undos.pop() {
                        tab.redos.push(action.undo(
                            &mut tab.value,
                            &mut tab.bookmarks,
                            &mut MutableIndices::new(&mut self.subscription, &mut tab.selected_text),
                            &mut tab.path,
                            &mut tab.name,
                            &mut tab.held_entry,
                            window_properties,
                        ));
                        return true;
                    }
                }
                if key == KeyCode::KeyY && flags == flags!(Ctrl) || key == KeyCode::KeyZ && flags == flags!(Ctrl + Shift) {
                    if let Some(action) = tab.redos.pop() {
                        tab.undos.push(action.undo(
                            &mut tab.value,
                            &mut tab.bookmarks,
                            &mut MutableIndices::new(&mut self.subscription, &mut tab.selected_text),
                            &mut tab.path,
                            &mut tab.name,
                            &mut tab.held_entry,
                            window_properties,
                        ));
                        return true;
                    }
                }
                if ((key == KeyCode::Backspace || key == KeyCode::Delete) && flags == flags!()) || (key == KeyCode::KeyX && flags == flags!(Ctrl)) {
                    if self.delete(flags & flags!(Ctrl) > 0) {
                        return true;
                    }
                }
                if key == KeyCode::KeyD && flags == flags!(Ctrl) {
                    if self.duplicate() {
                        return true;
                    }
                }
                if key == KeyCode::KeyC && flags == flags!(Ctrl) {
                    if self.copy(false) {
                        return true;
                    }
                }
                if key == KeyCode::KeyC && flags == flags!(Ctrl + Shift) {
                    if self.copy(true) {
                        return true;
                    }
                }
                if flags == flags!() {
                    let tab = tab_mut!(self);
                    let x = if key == KeyCode::Digit1 {
                        (None, NbtElement::from_id(NbtByte::ID))
                    } else if key == KeyCode::Digit2 {
                        (None, NbtElement::from_id(NbtShort::ID))
                    } else if key == KeyCode::Digit3 {
                        (None, NbtElement::from_id(NbtInt::ID))
                    } else if key == KeyCode::Digit4 {
                        (None, NbtElement::from_id(NbtLong::ID))
                    } else if key == KeyCode::Digit5 {
                        (None, NbtElement::from_id(NbtFloat::ID))
                    } else if key == KeyCode::Digit6 {
                        (None, NbtElement::from_id(NbtDouble::ID))
                    } else if key == KeyCode::Digit7 {
                        (None, NbtElement::from_id(NbtByteArray::ID))
                    } else if key == KeyCode::Digit8 {
                        (None, NbtElement::from_id(NbtIntArray::ID))
                    } else if key == KeyCode::Digit9 {
                        (None, NbtElement::from_id(NbtLongArray::ID))
                    } else if key == KeyCode::Digit0 {
                        (None, NbtElement::from_id(NbtString::ID))
                    } else if key == KeyCode::Minus {
                        (None, NbtElement::from_id(NbtList::ID))
                    } else if key == KeyCode::Equal {
                        (None, NbtElement::from_id(NbtCompound::ID))
                    } else if key == KeyCode::Backquote && tab.value.id() == NbtRegion::ID {
                        (None, NbtElement::from_id(NbtChunk::ID))
                    } else if key == KeyCode::KeyV {
                        let Some(clipboard) = get_clipboard() else {
                            self.alert(Alert::new("Error!", TextColor::Red, "Failed to get clipboard"));
                            return true;
                        };
                        match NbtElement::from_str(&clipboard) {
                            Ok((key, value)) => (key, value),
                            Err(idx) => {
                                self.alert(Alert::new("Error!", TextColor::Red, format!("Could not parse clipboard as SNBT (failed at index {idx})")));
                                return true;
                            }
                        }
                    } else {
                        return true;
                    };
                    let old_held_entry = core::mem::replace(&mut tab.held_entry, HeldEntry::FromAether(x));
                    if !old_held_entry.is_empty() {
                        tab.append_to_history(WorkbenchAction::DeleteHeldEntry {
                            held_entry: old_held_entry,
                        });
                    }
                    tab.append_to_history(WorkbenchAction::CreateHeldEntry);
                    return true;
                }
            }
        } else if key.state == ElementState::Released {
            if let PhysicalKey::Code(x) = key.physical_key {
                self.held_keys.remove(&x);
            }
        }

        false
    }

    pub fn on_mouse_move(&mut self, pos: PhysicalPosition<f64>) -> bool {
        self.raw_mouse_x = pos.x;
        self.raw_mouse_y = pos.y;
        self.mouse_x = (self.raw_mouse_x / self.scale as f64) as usize;
        self.mouse_y = (self.raw_mouse_y / self.scale as f64) as usize;
        let mouse_y = self.mouse_y;
        let tab = tab_mut!(self);
        if let Some(scrollbar_offset) = self.scrollbar_offset && mouse_y >= HEADER_SIZE {
            let mouse_y = mouse_y - HEADER_SIZE;
            let height = tab.value.height() * 16 + 32 + 15;
            let total = tab.window_height - HEADER_SIZE;
            let start = total * tab.scroll() / height;
            let scrollbar_point = start + scrollbar_offset;
            let dy = mouse_y as isize - scrollbar_point as isize;
            let pixel_delta = height as isize * dy / total as isize;
            tab.modify_scroll(|scroll| (scroll as isize + pixel_delta).max(0) as usize);
        }
        self.try_extend_drag_selection();
        true
    }

    pub fn try_extend_drag_selection(&mut self) {
        let horizontal_scroll = self.horizontal_scroll();
        let left_margin = self.left_margin();
        let tab = tab_mut!(self);
        if self.last_mouse_state == ElementState::Pressed {
            if let Some(selected_text) = tab.selected_text.as_mut() && tab.last_selected_text_interaction.1 == 0 && selected_text.is_drag_selectable() {
                let cursor = selected_text.selection.unwrap_or(selected_text.cursor);
                let selection = get_cursor_idx(&selected_text.value, (self.mouse_x + horizontal_scroll) as isize - (selected_text.indices.len() * 16 + 32 + 4 + left_margin) as isize - selected_text.prefix.0.width() as isize - selected_text.keyfix.as_ref().map_or(0, |(a, _)| a.width()) as isize);
                selected_text.cursor = selection;
                selected_text.selection = Some(cursor).filter(|cursor| *cursor != selected_text.cursor);
                selected_text.interact();
            }

            if self.search_box.is_selected() && self.search_box.last_interaction.0 == 0 {
                let cursor = self.search_box.selection.unwrap_or(self.search_box.cursor);
                let selection = get_cursor_idx(&self.search_box.value, (self.mouse_x.saturating_sub(SEARCH_BOX_START_X) + self.search_box.horizontal_scroll) as isize);
                self.search_box.cursor = selection;
                self.search_box.selection = Some(cursor).filter(|cursor| *cursor != self.search_box.cursor);
                self.search_box.interact();
            }
        }
    }

    pub fn window_dimensions(&mut self, window_width: usize, window_height: usize) {
        let width_scaling = window_width as f64 / self.raw_window_width as f64;
        let height_scaling = window_height as f64 / self.raw_window_height as f64;
        self.raw_window_width = window_width;
        self.raw_window_height = window_height;
        self.raw_mouse_x = self.raw_mouse_x * width_scaling;
        self.raw_mouse_y = self.raw_mouse_y * height_scaling;
        self.set_scale(self.scale);
    }

    pub fn set_scale(&mut self, scale: f32) {
        let old_scale = self.scale;
        let scale = (scale * 10.0).round() / 10.0;
        let max_scale = usize::min(self.raw_window_width / MIN_WINDOW_WIDTH, self.raw_window_height / MIN_WINDOW_HEIGHT) as f32;
        let scale = scale.clamp(1.0, max_scale);

        self.scale = scale;
        config::set_scale(Some(scale));
        self.mouse_x = (self.raw_mouse_x / self.scale as f64) as usize;
        self.mouse_y = (self.raw_mouse_y / self.scale as f64) as usize;
        self.window_width = (self.raw_window_width as f32 / self.scale).round() as usize;
        self.window_height = (self.raw_window_height as f32 / self.scale).round() as usize;
        for tab in &mut self.tabs {
            tab.window_width = self.window_width;
            tab.window_height = self.window_height;
        }

        if old_scale != scale {
            self.notify(Notification::new(format!("Scale: {scale:.1}x (Max {max_scale}.0)"), TextColor::White, NotificationKind::Scale))
        }
    }

    #[must_use]
    pub fn scroll(&self) -> usize { tab!(self).scroll() }

    #[must_use]
    pub fn horizontal_scroll(&self) -> usize {
        tab!(self).horizontal_scroll()
    }

    fn set_tab(&mut self, idx: usize, window_properties: &mut WindowProperties) {
        self.tab = idx.min(self.tabs.len() - 1);
        // on any tab switch this should be discarded.
        self.steal_animation_data = None;
        window_properties.window_title(format!("{} - NBT Workbench", tab!(self).name).as_str());
    }

    pub fn render(&mut self, builder: &mut VertexBufferBuilder) {
        if self.raw_window_width < MIN_WINDOW_WIDTH || self.raw_window_height < MIN_WINDOW_HEIGHT { return; }

        let shift = self.shift();

        builder.draw_texture_region_z(
            (SEARCH_BOX_START_X - 3, 22),
            BASE_Z,
            LINE_NUMBER_SEPARATOR_UV,
            (2, 23),
            (2, 16),
        );
        builder.draw_texture_region_z(
            (builder.window_width() - SEARCH_BOX_END_X, 22),
            BASE_Z,
            LINE_NUMBER_SEPARATOR_UV,
            (2, 23),
            (2, 16),
        );

        if ReplaceBox::is_visible(&self.search_box, &self.replace_box) {
            builder.draw_texture_region_z(
                (SEARCH_BOX_START_X - 3, 45),
                REPLACE_BOX_Z,
                LINE_NUMBER_SEPARATOR_UV,
                (2, 25),
                (2, 16),
            );
            builder.draw_texture_region_z(
                (builder.window_width() - SEARCH_BOX_END_X, 45),
                REPLACE_BOX_Z,
                LINE_NUMBER_SEPARATOR_UV,
                (2, 25),
                (2, 16),
            );
            builder.draw_texture_region_z(
                (SEARCH_BOX_START_X - 1, 68),
                REPLACE_BOX_Z,
                HORIZONTAL_SEPARATOR_UV,
                (builder.window_width() - SEARCH_BOX_END_X - SEARCH_BOX_START_X + 1, 2),
                (14, 2),
            );
        }

        for n in 0..(builder.window_height() - HEADER_SIZE + 15) / 16 {
            let uv = if (n % 2 == 0) ^ ((builder.scroll() / 16) % 2 == 0) {
                DARK_STRIPE_UV + (1, 1)
            } else {
                LIGHT_STRIPE_UV + (1, 1)
            };
            builder.draw_texture_region_z(
                (0, n * 16 + HEADER_SIZE - (n == 0) as usize),
                BASE_Z,
                uv,
                (builder.window_width(), 16 + (n == 0) as usize),
                (14, 14),
            );
        }
        // let start = std::time::Instant::now();
        self.render_tabs(builder);
        // println!("Tabs Bar: {}ms", start.elapsed().as_millis_f64());
        let left_margin = self.left_margin();
        let tab = tab!(self);
        let horizontal_scroll = tab.horizontal_scroll;
        let ghost = if self.mouse_x + horizontal_scroll >= left_margin && self.mouse_y >= HEADER_SIZE {
            tab.held_entry.element().map(|x| {
                (
                    x,
                    Vec2u::new(
                        ((self.mouse_x + horizontal_scroll - left_margin) & !15) + left_margin,
                        ((self.mouse_y - HEADER_SIZE) & !0b0111) + HEADER_SIZE
                    ),
                )
            })
        } else {
            None
        };
        let selected_text_y = tab.selected_text
            .as_ref()
            .map(|x| x.y)
            .and_then(|x| x.checked_sub(builder.scroll()));
        let (selected_key, selected_value, selecting_key) = if let Some(SelectedText(selected)) = tab.selected_text.as_ref() && selected.editable {
            if selected.keyfix.is_some() { // Health: __20.0__
                (selected.keyfix.clone().map(|x| x.0.clone().into_boxed_str()), Some(selected.value.clone().into_boxed_str()), false)
            } else if selected.valuefix.is_some() { // __Health__: 20.0
                (Some(selected.value.clone().into_boxed_str()), selected.valuefix.clone().map(|x| x.0.clone().into_boxed_str()), true)
            } else if !selected.suffix.0.is_empty() { // __Banana__: 4 entries
                (Some(selected.value.clone().into_boxed_str()), None, false)
            } else { // __20.0__
                (None, Some(selected.value.clone().into_boxed_str()), false)
            }
        } else {
            (None, None, false)
        };
        let mut ctx = RenderContext::new(selected_text_y, selected_key, selected_value, selecting_key, ghost, left_margin, (self.mouse_x, self.mouse_y), tab.freehand_mode);
        let (mouse_x, mouse_y) = ctx.mouse_pos().into();
        if mouse_y >= HEADER_SIZE && self.action_wheel.is_none() && !ReplaceBox::is_within_bounds((mouse_x, mouse_y), builder.window_width()) {
            builder.draw_texture_region_z(
                (0, mouse_y & !15),
                BASE_Z,
                HOVERED_STRIPE_UV,
                (builder.window_width(), 16),
                (14, 14),
            );
        }
        {
            builder.draw_texture((0, 26), OPEN_FOLDER_UV, (16, 16));
            builder.draw_texture((16, 26), NEW_FILE_UV, (16, 16));
            if (0..16).contains(&mouse_x) && (26..42).contains(&mouse_y) {
                builder.draw_texture((0, 26), SELECTION_UV, (16, 16));
                builder.draw_tooltip(&["Open File (Ctrl + O)"], ctx.mouse_pos(), false);
            }
            if (16..32).contains(&mouse_x) && (26..42).contains(&mouse_y) {
                builder.draw_texture((16, 26), SELECTION_UV, (16, 16));
                if shift {
                    builder.draw_tooltip(&["Create New Region File (Ctrl + Shift + N)"], ctx.mouse_pos(), false);
                } else {
                    builder.draw_tooltip(&["Create New NBT File (Ctrl + N)"], ctx.mouse_pos(), false);
                }
            }
            builder.draw_texture_region_z(
                (33, 22),
                BASE_Z,
                LINE_NUMBER_SEPARATOR_UV,
                (2, 23),
                (2, 16),
            );
        }
        // let start = std::time::Instant::now();
        tab.render(
            builder,
            &mut ctx,
            self.scrollbar_offset.is_some(),
            self.action_wheel.is_some(),
            self.steal_animation_data.as_ref().map(|x| (now() - x.0).min(LINE_DOUBLE_CLICK_INTERVAL).as_millis() as f32 / LINE_DOUBLE_CLICK_INTERVAL.as_millis_f32()).unwrap_or(0.0)
        );
        // println!("Active Tab: {}ms", start.elapsed().as_millis_f64());
        config::get_sort_algorithm().render(builder, &mut ctx);
        if let Some(selected_text) = &tab.selected_text {
            builder.horizontal_scroll = horizontal_scroll;
            selected_text.render(builder, left_margin);
            builder.horizontal_scroll = 0;
        }
        // let start = std::time::Instant::now();
        self.render_action_wheel(builder);
        self.render_held_entry(builder);
        self.render_notifications_and_alerts(builder);
        self.render_debug_menu(builder);
        // println!("Misc: {}ms", start.elapsed().as_millis_f64());
        builder.draw_tooltips();
    }

    pub fn render_search_boxes(&self, builder: &mut VertexBufferBuilder) {
        self.search_box.render(builder, self.shift(), (self.mouse_x, self.mouse_y));
        if ReplaceBox::is_visible(&self.search_box, &self.replace_box) {
            self.replace_box.render(builder);
        }
        builder.draw_tooltips();
    }

    pub fn tick(&mut self, window_properties: &mut WindowProperties) {
        #[cfg(not(target_arch = "wasm32"))] {
            let mut alerts = vec![];
            for (idx, tab) in self.tabs.iter_mut().enumerate() {
                if let Some(path) = tab.path.as_deref() && path.is_absolute() && (now() - tab.last_interaction >= Tab::AUTOSAVE_INTERVAL) && tab.unsaved_changes && tab.value.true_height() <= Tab::AUTOSAVE_MAXIMUM_LINES {
                    if let Err(e) = tab.save(false, window_properties) {
                        alerts.push(Alert::new("Error!", TextColor::Red, e.context(format!("Failed to autosave {nth} tab", nth = nth(idx + 1))).to_string()));
                    }
                }
            }
            for alert in alerts {
                self.alert(alert);
            }
        }
        if (!tab!(self).held_entry.is_empty() || tab!(self).freehand_mode || ((tab!(self).selected_text.is_some() || self.search_box.is_selected()) && self.last_mouse_state == ElementState::Pressed)) && self.action_wheel.is_none() && self.scrollbar_offset.is_none() {
            self.try_mouse_scroll();
            self.try_search_box_scroll();
            self.try_replace_box_scroll();
            self.try_extend_drag_selection();
        }
        if self.steal_animation_data.is_some() && self.try_steal(false) {
            if self.steal_animation_data.as_ref().is_some_and(|x| (now() - x.0) >= LINE_DOUBLE_CLICK_INTERVAL) {
                self.steal();
            }
        } else {
            self.steal_animation_data = None;
        }
    }

    #[must_use]
    pub fn close(&mut self) -> usize {
        let mut failed_tabs = 0_usize;

        for tab in &mut self.tabs {
            if tab.unsaved_changes && (now() - core::mem::replace(&mut tab.last_close_attempt, now())) > Tab::TAB_CLOSE_DOUBLE_CLICK_INTERVAL {
                failed_tabs += 1;
            }
        }

        if failed_tabs > 0 {
            self.alert(Alert::new("Are you sure you want to exit?", TextColor::Yellow, format!("You have {failed_tabs} unsaved tabs.")));
        }
        failed_tabs
    }

    fn render_held_entry(&self, builder: &mut VertexBufferBuilder) {
        let shift = self.shift();

        if let Some(element) = tab!(self).held_entry.element() {
            element.render_icon(
                (
                    self.mouse_x.saturating_sub(8),
                    self.mouse_y.saturating_sub(8),
                ),
                HELD_ENTRY_Z,
                builder,
            );

            if (!element.is_primitive() || !element.is_default_state()) && element.should_render_description() || shift {
                let (text, color) = element.value();
                builder.color = color.to_raw();
                builder.draw_tooltip(&[&text], (self.mouse_x, self.mouse_y), false);
            }
        }
    }

    fn render_notifications_and_alerts(&mut self, builder: &mut VertexBufferBuilder) {
        let mut y = HEADER_SIZE;
        {
            for (_, notification_slot) in self.notifications.iter_mut() {
                if let Some(notification) = notification_slot {
                    if notification.is_invisible() {
                        *notification_slot = None;
                    } else {
                        notification.render(builder, y);
                        y += notification.height();
                    }
                }
            }
        }
        {
            let mut idx = 0;
            while let Some(alert) = self.alerts.get_mut(idx) {
                if alert.is_invisible() {
                    self.alerts.remove(idx);
                } else {
                    alert.render(builder, y);
                    y += alert.height();
                    idx += 1;
                }
            }
        }
    }

    fn render_debug_menu(&mut self, builder: &mut VertexBufferBuilder) {
        if !self.debug_menu { return }

        let tab = tab!(self);
        let lines = [
            format!("dims: {}x{}", self.window_width, self.window_height),
            format!("mouse state: {:?}", self.last_mouse_state),
            format!("mouse px coords: {}, {}", self.mouse_y, self.mouse_y),
            format!("cache cursor x: {:?}", tab.cache_cursor_x),
            format!("action wheel coords: {:?}", self.action_wheel),
            format!("sub indices: {:?}", self.subscription.as_ref().map(|subscription| &*subscription.indices)),
            format!("sub tab uuid: {:?}", self.subscription.as_ref().map(|subscription| subscription.tab_uuid)),
            format!("scale: {}", self.scale),
            format!("last SB input: y={}, since={}ms", self.search_box.last_interaction.0, (now() - self.search_box.last_interaction.1).as_millis()),
            format!("tab uuid: {}", tab.uuid),
            format!("file format: {:?}", tab.format),
            format!("undos len: {}", tab.undos.len()),
            format!("redos len: {}", tab.redos.len()),
            format!("scroll: {}", tab.scroll),
            format!("hscroll: {}", tab.horizontal_scroll),
            format!("bookmark count: {}", tab.bookmarks.len()),
            format!("select txt: {data}", data = if let Some(txt) = tab.selected_text.as_ref() { format!("y={}, cursor={}, pre={}, key={:?}, txt={}, val={:?}, suf={}", txt.y, txt.cursor, txt.prefix.0, txt.keyfix.as_ref().map(|x| &x.0), txt.value, txt.valuefix.as_ref().map(|x| &x.0), txt.suffix.0) } else { "null".to_owned() }),
            format!("held entry: {data}", data = if let Some(held) = tab.held_entry.element() { format!("h={}, th={}", held.height(), held.true_height()) } else { "null".to_owned() }),
            format!("value: h={}, th={}, depth={}", tab.value.height(), tab.value.true_height(), tab.value.max_depth()),
        ];
        for (idx, line) in lines.iter().enumerate() {
            if builder.window_height() < (idx + 1) * VertexBufferBuilder::CHAR_HEIGHT {
                continue
            }
            builder.settings((builder.window_width().saturating_sub(line.width()), builder.window_height() - (idx + 1) * VertexBufferBuilder::CHAR_HEIGHT), false, ZOffset::DEBUG_TEXT_Z);
            builder.color = TextColor::White.to_raw();
            let _ = write!(builder, "{line}");
        }
    }

    fn render_tabs(&self, builder: &mut VertexBufferBuilder) {
        let mut offset = 3;
        builder.horizontal_scroll = self.tab_scroll;
        for (idx, tab) in self.tabs.iter().enumerate() {
            let remaining_width = tab.name.width() + 48 + 3;
            let uv = if (now() - tab.last_close_attempt) <= Tab::TAB_CLOSE_DOUBLE_CLICK_INTERVAL {
                CLOSED_WIDGET_UV
            } else if idx == self.tab {
                SELECTED_WIDGET_UV
            } else if (offset..offset + 3 + remaining_width).contains(&self.mouse_x) && (3..=19).contains(&self.mouse_y) {
                HOVERED_WIDGET_UV
            } else {
                UNSELECTED_WIDGET_UV
            };
            builder.draw_texture((offset, 3), uv, (3, 16));
            if (offset..offset + 16).contains(&self.mouse_x) && (3..19).contains(&self.mouse_y) {
                builder.draw_tooltip(&[tab.value.display_name()], (self.mouse_x, self.mouse_y), false);
            }
            offset += 2;
            tab.draw_icon(builder, (offset, 2), JUST_OVERLAPPING_BASE_TEXT_Z);
            offset += 1;
            builder.draw_texture_region_z(
                (offset, 3),
                BASE_Z,
                uv + (3, 0),
                (remaining_width, 16),
                (10, 16),
            );
            builder.settings((offset + 16, 3), false, BASE_TEXT_Z);
            builder.color = match config::get_theme() { Theme::Light => TextColor::DarkGray, Theme::Dark => TextColor::White }.to_raw();
            let _ = write!(builder, "{}", tab.name);
            offset += remaining_width;
            builder.draw_texture((offset, 3), uv + (13, 0), (3, 16));
            builder.draw_texture(
                (offset - 32, 3),
                if tab.unsaved_changes {
                    SAVE_UV
                } else {
                    SAVE_GRAYSCALE_UV
                },
                (16, 16),
            );
            builder.draw_texture((offset - 16, 3), tab.format.uv(), (16, 16));
            if (offset - 32..offset - 16).contains(&self.mouse_x) && (3..19).contains(&self.mouse_y) {
                builder.draw_tooltip(&["Save"], (self.mouse_x, self.mouse_y), false);
            }
            if (offset - 16..offset).contains(&self.mouse_x) && (3..19).contains(&self.mouse_y) {
                builder.draw_tooltip(&[tab.format.into_str()], (self.mouse_x, self.mouse_y), false);
            }
            offset += 6;
        }
        builder.horizontal_scroll = 0;
        builder.draw_texture_region_z(
            (0, 21),
            BASE_Z,
            HORIZONTAL_SEPARATOR_UV,
            (builder.window_width(), 2),
            (14, 2),
        );
        builder.draw_texture_region_z(
            (0, 45),
            BASE_Z,
            HORIZONTAL_SEPARATOR_UV,
            (builder.window_width(), 2),
            (14, 2),
        );
    }

    fn render_action_wheel(&mut self, builder: &mut VertexBufferBuilder) {
        let Some((cx, cy)) = self.action_wheel else {
            return;
        };
        let cx = cx.saturating_sub(31) + 31;
        let cy = cy.saturating_sub(31) + 31;
        let left_margin = self.left_margin();
        let tab = tab_mut!(self);
        let highlight_idx = (((cy as f64 - self.mouse_y as f64).atan2(cx as f64 - self.mouse_x as f64) + core::f64::consts::FRAC_PI_8 + core::f64::consts::FRAC_PI_2 + core::f64::consts::FRAC_PI_4).rem_euclid(core::f64::consts::TAU) * core::f64::consts::FRAC_2_PI * 2.0) as usize;
        let squared_distance_from_origin = (cy as isize - self.mouse_y as isize).pow(2) + (cx as isize - self.mouse_x as isize).pow(2);
        if cy >= HEADER_SIZE {
            if cy > tab.value.height() * 16 + HEADER_SIZE { return };
            let scroll = tab.scroll();
            let InteractionInformation::Content { is_in_left_margin: false, depth, key, value, .. } = Self::get_interaction_information_raw(left_margin, 0, scroll, cx, cy, &mut tab.value) else { return };
            let min_x = depth * 16 + left_margin;
            let max_x = min_x + 32 + value.value_width() + key.map(|key| key.width() + ": ".width()).unwrap_or(0);
            if !(min_x..max_x).contains(&cx) { return };
            builder.draw_texture_z((cx - 31, cy - 31), ACTION_WHEEL_Z, TRAY_UV, (64, 64));
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
                let offset = [
                    Vec2u::new(5, 5),
                    Vec2u::new(5, 6),
                    Vec2u::new(4, 5),
                    Vec2u::new(3, 5),
                    Vec2u::new(4, 4),
                    Vec2u::new(5, 3),
                    Vec2u::new(5, 4),
                    Vec2u::new(6, 5),
                ][n];
                let dims = if n % 2 == 0 {
                    Vec2u::new(19, 19)
                } else {
                    Vec2u::new(20, 20)
                };
                builder.draw_texture_z(
                    (cx.wrapping_add(x), cy.wrapping_add(y)),
                    ACTION_WHEEL_Z,
                    uv,
                    dims,
                );
                action.render(
                    builder,
                    (
                        cx.wrapping_add(x).wrapping_add(offset.x),
                        cy.wrapping_add(y).wrapping_add(offset.y),
                    ),
                    hovered,
                );
            }
        }
    }

    pub fn try_mouse_scroll(&mut self) {
        let tab = tab_mut!(self);
        if self.mouse_x >= self.window_width - 16 {
            tab.modify_horizontal_scroll(|scroll| scroll + 16);
        } else if self.mouse_x < 16 {
            tab.modify_horizontal_scroll(|scroll| scroll.saturating_sub(16));
        }

        if self.mouse_y < HEADER_SIZE + 16 {
            tab.modify_scroll(|scroll| scroll.saturating_sub(16));
        } else if self.mouse_y
            >= usize::min(
            self.window_height - 16,
            tab.value.height() * 16 + HEADER_SIZE,
        ) {
            tab.modify_scroll(|scroll| scroll + 16);
        }
    }

    pub fn try_search_box_scroll(&mut self) {
        let search_box_x = (SEARCH_BOX_START_X + 16)..(self.window_width - (SEARCH_BOX_END_X + 64));
        if self.last_mouse_state == ElementState::Pressed && self.search_box.is_selected() && !search_box_x.contains(&self.mouse_x) {
            if self.mouse_x < search_box_x.start {
                self.search_box.horizontal_scroll = self.search_box.horizontal_scroll.saturating_sub(4);
            } else {
                self.search_box.horizontal_scroll = (self.search_box.horizontal_scroll + 4).min(self.search_box.value.width().saturating_sub(search_box_x.end - search_box_x.start));
            }
        }
    }

    pub fn try_replace_box_scroll(&mut self) {
        let replace_box_x = (SEARCH_BOX_START_X + 16)..(self.window_width - SEARCH_BOX_END_X);
        if self.last_mouse_state == ElementState::Pressed && self.replace_box.is_selected() && !replace_box_x.contains(&self.mouse_x) {
            if self.mouse_x < replace_box_x.start {
                self.replace_box.horizontal_scroll = self.replace_box.horizontal_scroll.saturating_sub(4);
            } else {
                self.replace_box.horizontal_scroll = (self.replace_box.horizontal_scroll + 4).min(self.replace_box.value.width().saturating_sub(replace_box_x.end - replace_box_x.start));
            }
        }
    }

    pub fn should_ignore_event(&self) -> bool {
        now() < self.ignore_event_end
    }

    #[allow(
        clippy::cognitive_complexity,
        clippy::match_same_arms,
        clippy::too_many_lines
    )]
    #[must_use]
    fn char_from_key(&self, key: KeyCode) -> Option<char> {
        if self.ctrl() { return None }
        let shift = self.shift();
        Some(match key {
            KeyCode::Digit1 => if shift { '!' } else { '1' },
            KeyCode::Digit2 => if shift { '@' } else { '2' },
            KeyCode::Digit3 => if shift { '#' } else { '3' },
            KeyCode::Digit4 => if shift { '$' } else { '4' },
            KeyCode::Digit5 => if shift { '%' } else { '5' },
            KeyCode::Digit6 => if shift { '^' } else { '6' },
            KeyCode::Digit7 => if shift { '&' } else { '7' },
            KeyCode::Digit8 => if shift { '*' } else { '8' },
            KeyCode::Digit9 => if shift { '(' } else { '9' },
            KeyCode::Digit0 => if shift { ')' } else { '0' },
            KeyCode::KeyA => if shift { 'A' } else { 'a' },
            KeyCode::KeyB => if shift { 'B' } else { 'b' },
            KeyCode::KeyC => if shift { 'C' } else { 'c' },
            KeyCode::KeyD => if shift { 'D' } else { 'd' },
            KeyCode::KeyE => if shift { 'E' } else { 'e' },
            KeyCode::KeyF => if shift { 'F' } else { 'f' },
            KeyCode::KeyG => if shift { 'G' } else { 'g' },
            KeyCode::KeyH => if shift { 'H' } else { 'h' },
            KeyCode::KeyI => if shift { 'I' } else { 'i' },
            KeyCode::KeyJ => if shift { 'J' } else { 'j' },
            KeyCode::KeyK => if shift { 'K' } else { 'k' },
            KeyCode::KeyL => if shift { 'L' } else { 'l' },
            KeyCode::KeyM => if shift { 'M' } else { 'm' },
            KeyCode::KeyN => if shift { 'N' } else { 'n' },
            KeyCode::KeyO => if shift { 'O' } else { 'o' },
            KeyCode::KeyP => if shift { 'P' } else { 'p' },
            KeyCode::KeyQ => if shift { 'Q' } else { 'q' },
            KeyCode::KeyR => if shift { 'R' } else { 'r' },
            KeyCode::KeyS => if shift { 'S' } else { 's' },
            KeyCode::KeyT => if shift { 'T' } else { 't' },
            KeyCode::KeyU => if shift { 'U' } else { 'u' },
            KeyCode::KeyV => if shift { 'V' } else { 'v' },
            KeyCode::KeyW => if shift { 'W' } else { 'w' },
            KeyCode::KeyX => if shift { 'X' } else { 'x' },
            KeyCode::KeyY => if shift { 'Y' } else { 'y' },
            KeyCode::KeyZ => if shift { 'Z' } else { 'z' },
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
            KeyCode::Quote => if shift { '"' } else { '\'' },
            KeyCode::Backslash => if shift { '|' } else { '\\' },
            KeyCode::Semicolon => if shift { ':' } else { ';' },
            KeyCode::Comma => if shift { '<' } else { ',' },
            KeyCode::Equal => if shift { '+' } else { '=' },
            KeyCode::Backquote => if shift { '~' } else { '`' }
            KeyCode::BracketLeft => if shift { '{' } else { '[' },
            KeyCode::Minus => if shift { '_' } else { '-' },
            KeyCode::Period => if shift { '>' } else { '.' },
            KeyCode::BracketRight => if shift { '}' } else { ']' },
            KeyCode::Slash => if shift { '?' } else { '/' },
            KeyCode::Tab => '\t',
            _ => return None,
        })
    }
}

pub const LINE_DOUBLE_CLICK_INTERVAL: Duration = Duration::from_millis(250);

#[derive(Debug)]
pub enum HeldEntry {
    Empty,
    FromAether(NbtElementAndKey),
    FromKnown(NbtElementAndKey, OwnedIndices, bool),
}

impl HeldEntry {
    #[must_use]
    pub const fn element(&self) -> Option<&NbtElement> {
        match self {
            Self::Empty => None,
            Self::FromAether((_, element)) | Self::FromKnown((_, element), _, _) => Some(element),
        }
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool { matches!(self, Self::Empty) }

    pub fn take(&mut self) -> Self {
        core::mem::replace(self, HeldEntry::Empty)
    }
}

#[derive(Copy, Clone)]
pub enum SortAlgorithm {
    None,
    Name,
    Type,
}

impl SortAlgorithm {
    pub fn render(self, builder: &mut VertexBufferBuilder, ctx: &mut RenderContext) {
        let uv = match self {
            Self::None => SORT_COMPOUND_BY_NOTHING_UV,
            Self::Name => SORT_COMPOUND_BY_NAME_UV,
            Self::Type => SORT_COMPOUND_BY_TYPE_UV,
        };

        let (mouse_x, mouse_y) = ctx.mouse_pos().into();

        let widget_uv = if (280..296).contains(&mouse_x) && (26..42).contains(&mouse_y) {
            builder.draw_tooltip(&[&format!("Compound Sorting Algorithm ({self})")], ctx.mouse_pos(), false);
            HOVERED_WIDGET_UV
        } else {
            SELECTED_WIDGET_UV
        };
        builder.draw_texture((280, 26), widget_uv, (16, 16));
        builder.draw_texture((283, 29), uv, (10, 10));
    }

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
        if let Self::None = self { return; }
        // yeah, it's hacky... but there's not much else I *can* do. plus: it works extremely well.
        for (idx, entry) in map.entries.iter_mut().enumerate() {
            entry.additional = idx;
        }
        match self {
            Self::Name => map.entries.sort_by(|a, b| ElementAction::by_name((&a.key, &a.value), (&b.key, &b.value))),
            _ => map.entries.sort_by(|a, b| ElementAction::by_type((&a.key, &a.value), (&b.key, &b.value))),
        }
        let indices = map.entries.iter().map(|entry| entry.additional).collect::<Vec<_>>();
        for (new_idx, &idx) in indices.iter().enumerate() {
            // SAFETY: these indices are valid since the length did not change and since the values written were indexes
            unsafe {
                let entry = map.entries.get_unchecked_mut(new_idx);
                *map.indices.find_mut(hash!(entry.key), |&target_idx| target_idx == idx).expect("index obviously exists") = new_idx;
            }
        }
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
    subscription_type: FileUpdateSubscriptionType,
    pub indices: OwnedIndices,
    rx: std::sync::mpsc::Receiver<Vec<u8>>,
    watcher: notify::PollWatcher,
    tab_uuid: Uuid,
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

pub enum DropFn {
    Dropped(usize, usize, Option<CompactString>, usize, Option<NbtElementAndKey>),
    Missed(NbtElementAndKey),
    InvalidType(NbtElementAndKey),
}
