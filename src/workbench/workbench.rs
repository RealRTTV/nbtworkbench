use std::fmt::{Display, Formatter, Write};
use std::mem::MaybeUninit;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::mpsc::TryRecvError;
use std::time::Duration;

use crate::assets::{ZOffset, ACTION_WHEEL_Z, BASE_TEXT_Z, BASE_Z, CLOSED_WIDGET_UV, DARK_STRIPE_UV, HEADER_SIZE, HELD_ENTRY_Z, HORIZONTAL_SEPARATOR_UV, HOVERED_STRIPE_UV, HOVERED_WIDGET_UV, JUST_OVERLAPPING_BASE_TEXT_Z, LIGHT_STRIPE_UV, LINE_NUMBER_SEPARATOR_UV, REPLACE_BOX_Z, SAVE_GRAYSCALE_UV, SAVE_UV, SELECTED_ACTION_WHEEL, SELECTED_WIDGET_UV, TRAY_UV, UNSELECTED_ACTION_WHEEL, UNSELECTED_WIDGET_UV};
use crate::elements::{CompoundMap, NbtByte, NbtByteArray, NbtChunk, NbtCompound, NbtDouble, NbtElement, NbtElementAndKey, NbtFloat, NbtInt, NbtIntArray, NbtList, NbtLong, NbtLongArray, NbtRegion, NbtShort, NbtString};
use crate::render::{RenderContext, TextColor, VertexBufferBuilder, WindowProperties, MIN_WINDOW_HEIGHT, MIN_WINDOW_WIDTH, WINDOW_HEIGHT, WINDOW_WIDTH};
use crate::serialization::{BigEndianDecoder, Decoder, UncheckedBufWriter};
use crate::tree::{add_element, close_element, expand_element, open_element, remove_element, replace_element, swap_element_same_depth, AddElementResult, Indices, MutableIndices, NavigationInformation, OwnedIndices, ParentNavigationInformationMut, RemoveElementResult, TraversalInformation, TraversalInformationMut};
use crate::util::{drop_on_separate_thread, get_clipboard, now, nth, set_clipboard, LinkedQueue, StrExt, Vec2u};
use crate::widget::{get_cursor_idx, get_cursor_left_jump_idx, get_cursor_right_jump_idx, Alert, ButtonWidget, ButtonWidgetAccumulatedResult, ButtonWidgetContext, ButtonWidgetContextMut, ExactMatchButton, FreehandModeButton, NewTabButton, Notification, NotificationKind, OpenFileButton, RefreshButton, ReplaceBox, ReplaceBoxKeyResult, ReplaceByButton, SearchBox, SearchBoxKeyResult, SearchFlagsButton, SearchModeButton, SearchOperationButton, SelectedText, SelectedTextAdditional, SelectedTextKeyResult, SortAlgorithmButton, Text, ThemeButton, SEARCH_BOX_END_X, SEARCH_BOX_START_X, TEXT_DOUBLE_CLICK_INTERVAL};
use crate::{config, mutable_indices};
use crate::{flags, get_interaction_information, hash, tab, tab_mut};

#[cfg(not(target_arch = "wasm32"))]
use std::thread::scope;
#[cfg(target_arch = "wasm32")]
use crate::wasm::fake_scope as scope;

use crate::workbench::{ElementAction, FileFormat, MarkedLine, MarkedLines, Tab, WorkbenchAction};
use anyhow::{anyhow, bail, ensure, Context, Result};
use compact_str::{format_compact, CompactString, ToCompactString};
use enum_map::EnumMap;
use fxhash::{FxBuildHasher, FxHashSet};
use uuid::Uuid;
use winit::dpi::{PhysicalPosition, PhysicalSize};
use winit::event::{ElementState, KeyEvent, MouseButton, MouseScrollDelta};
use winit::keyboard::{KeyCode, PhysicalKey};
use winit::window::Theme;

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
                let mut array = [const { MaybeUninit::<Option<Notification>>::uninit() }; enum_map::enum_len::<NotificationKind>()];
                let mut i = 0;
                while i < enum_map::enum_len::<NotificationKind>() {
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
            notifications: EnumMap::from_fn(|_| None),
            scale: 1.0,
            steal_animation_data: None,
            search_box: SearchBox::new(),
            replace_box: ReplaceBox::new(),
            ignore_event_end: Duration::ZERO,
            debug_menu: false,

            exact_match_button: ButtonWidget::new(),
            freehand_mode_button: ButtonWidget::new(),
            search_flags_button: ButtonWidget::new(),
            search_operation_button: ButtonWidget::new(),
            search_mode_button: ButtonWidget::new(),
            sort_algorithm_button: ButtonWidget::new(),
            theme_button: ButtonWidget::new(),
            refresh_button: ButtonWidget::new(),
            new_tab_button: ButtonWidget::new(),
            open_file_button: ButtonWidget::new(),
            replace_by_button: ButtonWidget::new(),
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
                    workbench.alert(e.into())
                } else {
                    break 'create_tab;
                }
            }
            workbench.add_tab(window_properties, Tab {
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
                held_entry: None,
                cache_cursor_x: None,
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

        pub fn on_open_file(&mut self, path: &Path, buf: Vec<u8>, window_properties: &mut WindowProperties) -> Result<()> {
        let (nbt, format) = Tab::parse_raw(path, buf)?;
        let tab = Tab::new(nbt, path, format, self.window_height, self.window_width)?;
        self.add_tab(window_properties, tab);
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

        pub fn on_mouse_input(&mut self, state: ElementState, button: MouseButton, window_properties: &mut WindowProperties) -> bool {
        tab_mut!(self).last_interaction = now();
        let left_margin = self.left_margin();
        let horizontal_scroll = self.horizontal_scroll();
        let shift = self.shift();
        self.last_mouse_state = state;
        
        match state {
            ElementState::Pressed => {
                self.held_mouse_keys.insert(button);

                if let MouseButton::Left | MouseButton::Right = button && self.try_deselect_selected_text(window_properties) {
                    // do not exit early
                }

                {
                    let shift = self.shift();
                    let mut ctx = ButtonWidgetContextMut::new(tab_mut!(self), &mut self.search_box, &mut self.replace_box, shift);

                    macro_rules! try_click_widget {
                        ($field:ident) => {
                            if self.$field.is_clickable(&ctx.as_ref()) && self.$field.bounds((self.window_width, self.window_height).into()).contains((self.mouse_x, self.mouse_y).into()) {
                                // hardcoded to mouse_down only for now
                                if self.$field.on_mouse_input(state, button, &mut ctx) {
                                    let ButtonWidgetAccumulatedResult { notifications, alerts, tabs, open_file_requests } = ctx.take_accumulated();
                                    for notification in notifications {
                                        self.notify(notification);
                                    }

                                    for alert in alerts {
                                        self.alert(alert);
                                    }

                                    for tab in tabs {
                                        self.add_tab(window_properties, tab);
                                    }

                                    for _ in 0..open_file_requests {
                                        self.open_file(window_properties)
                                    }

                                    return true
                                }
                            }
                        };
                    }

                    try_click_widget!(search_mode_button);
                    try_click_widget!(search_operation_button);
                    try_click_widget!(search_flags_button);
                    try_click_widget!(exact_match_button);
                    try_click_widget!(sort_algorithm_button);
                    try_click_widget!(theme_button);
                    try_click_widget!(freehand_mode_button);
                    try_click_widget!(refresh_button);
                    try_click_widget!(new_tab_button);
                    try_click_widget!(open_file_button);
                    try_click_widget!(replace_by_button);
                }

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

                if 2 < self.mouse_y && self.mouse_y < 19 && self.mouse_x > 3 {
                    self.click_tab(button, window_properties);
                } else if (24..46).contains(&self.mouse_y) && self.mouse_x < 16 {
                    self.open_file(window_properties);
                }

                if button == MouseButton::Left && self.mouse_y >= HEADER_SIZE && tab!(self).held_entry.is_some() {
                    self.drop_held_entry();
                    return true
                }

                if button == MouseButton::Left {
                    if self.bookmark_line(true) {
                        return true
                    }
                }

                if self.mouse_x + horizontal_scroll >= left_margin && self.mouse_y >= HEADER_SIZE {
                    match self.action_wheel.take() {
                        Some(_) => {}
                        None => {
                            if button == MouseButton::Right && let InteractionInformation::Content { is_in_left_margin: false, depth, y, .. } = get_interaction_information!(self) && depth + 1 == (self.mouse_x + horizontal_scroll - left_margin) / 16 {
                                self.action_wheel = Some((left_margin + depth * 16 + 16 + 6, y * 16 + HEADER_SIZE + 7));
                                return true
                            }
                        }
                    }

                    if MouseButton::Left == button {
                        if self.try_root_style_change() {
                            return true
                        }
                    }

                    if MouseButton::Left == button {
                        if self.toggle(shift, tab!(self).freehand_mode) {
                            return true
                        }
                    }

                    if button == MouseButton::Left {
                        if self.try_double_click_interaction() {
                            return true
                        }
                    }

                    if button == MouseButton::Right {
                        if self.try_select_text(false) {
                            return true
                        }
                    }

                    if button == MouseButton::Left {
                        if !self.try_steal(true) {
                            self.steal_animation_data = None;
                        }
                    }

                    if ((self.window_width - 7)..self.window_width).contains(&self.mouse_x) {
                        let tab = tab_mut!(self);
                        let height = tab.value.height() * 16 + 48;
                        let total = self.window_height - HEADER_SIZE;
                        if height - 48 > total {
                            let start = total * self.scroll() / height + HEADER_SIZE;
                            let end = start + total * total / height;
                            if (start..=end).contains(&self.mouse_y) {
                                self.scrollbar_offset = Some(self.mouse_y - start);
                                return true
                            }
                        }
                    }
                } else {
                    if !tab!(self).freehand_mode && tab!(self).held_entry.is_none() && (24..46).contains(&self.mouse_y) && button == MouseButton::Left {
                        match self.hold_entry(button) {
                            Ok(true) => return true,
                            Err(e) => self.alert(e.into()),
                            _ => {}
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
        use core::f64::consts::TAU;

        let Some((cx, cy)) = self.action_wheel.take() else { return false };
        if cy < HEADER_SIZE { return true }
        let left_margin = self.left_margin();
        let tab = tab_mut!(self);
        let scroll = tab.scroll();
        if (cy as isize - self.mouse_y as isize).pow(2) as usize + (cx as isize - self.mouse_x as isize).pow(2) as usize <= 8_usize.pow(2) { return true }
        let highlight_idx = (f64::atan2(cy as f64 - self.mouse_y as f64, cx as f64 - self.mouse_x as f64) / TAU * 8.0 + 3.5).rem_euclid(8.0) as usize;
        let Some(TraversalInformation { indices, element, .. }) = tab.value.traverse((cy - (HEADER_SIZE + 7) + scroll) / 16, Some((cx - left_margin) / 16)) else { return true };
        if let Some(action) = element.actions().get(highlight_idx).copied() && let Some(action) = action.apply(&mut tab.value, indices, &mut tab.bookmarks, mutable_indices!(self, tab), &mut self.alerts, tab.uuid) {
            tab.append_to_history(action);
        }
        true
    }

        pub fn try_subscription(&mut self) -> Result<()> {
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
                    Ok(data) => {
                        let kv = match subscription.r#type {
                            FileUpdateSubscriptionType::Snbt => {
                                let s = core::str::from_utf8(&data).context("File was not a valid UTF8 string")?;
                                let sort = config::set_sort_algorithm(SortAlgorithm::None);
                                let result = NbtElement::from_str(s);
                                config::set_sort_algorithm(sort);
                                match result {
                                    Ok(kv) => kv,
                                    Err(idx) => bail!("Failed to parse SNBT at index {idx}"),
                                }
                            },
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
                                let NavigationInformation { key, .. } = tab.value.navigate(&subscription.indices).context("Failed to navigate subscription indices")?;
                                let key = key.map(CompactString::from);
                                (key, value)
                            }
                        };
                        let action = replace_element(&mut tab.value, kv, subscription.indices.clone(), &mut tab.bookmarks, mutable_indices!(self, tab)).context("Failed to replace element")?.into_action();
                        tab.append_to_history(action);
                        tab.refresh_scrolls();
                    },
                    Err(TryRecvError::Disconnected) => {
                        self.subscription = None;
                        bail!("Could not update; file subscription disconnected.");
                    }
                    Err(TryRecvError::Empty) => {
                        // do nothing ig
                    }
                }
            } else {
                self.subscription = None;
                bail!("Could not update; file subscription tab closed.");
            }
        }

        Ok(())
    }

    fn try_double_click_interaction(&mut self) -> bool {
        let now = now();
        let shift = self.shift();

        if tab!(self).held_entry.is_some() || tab!(self).freehand_mode || now - tab!(self).last_selected_text_interaction.2 <= LINE_DOUBLE_CLICK_INTERVAL { return false };

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

        if tab!(self).held_entry.is_some() || tab!(self).freehand_mode { return false };
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
        // todo, the fact that these indices aren't seemingly (to me) stored between like a queue might pose an issue in creating a correct workbench action history model -- correct, is still buggy :(

        if tab!(self).held_entry.is_some() { return false }
        let is_grid_layout = tab!(self).value.as_region().is_some_and(NbtRegion::is_grid_layout);

        if let InteractionInformation::Content { is_in_left_margin: false, depth, x, y, indices, .. } = get_interaction_information!(self) && (depth + 1 == x || is_grid_layout) && y > 0 {
            let tab = tab_mut!(self);

            let RemoveElementResult { indices, kv: (key, mut value), replaces: _ } = match remove_element(&mut tab.value, indices, &mut tab.bookmarks, mutable_indices!(self, tab)) {
                Some(result) => result,
                None => {
                    self.alert(Alert::error("Failed to remove element"));
                    return false
                }
            };

            scope(|scope| value.shut(scope));
            tab.append_to_history(WorkbenchAction::RemoveToHeldEntry);
            tab.held_entry = Some(HeldEntry::from_indices((key, value), indices));
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
            let action = match add_element(&mut tab.value, (key, duplicate), indices, &mut tab.bookmarks, mutable_indices!(self, tab)) {
                Some(action) => action.into_action(),
                None => {
                    self.alert(Alert::error("Failed to duplicate element"));
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
            let result = match remove_element(&mut tab.value, indices, &mut tab.bookmarks, mutable_indices!(self, tab)) {
                Some(result) => result,
                None => {
                    self.alert(Alert::error("Failed to remove element"));
                    return false
                }
            };
            tab.append_to_history(result.into_action());
            true
        } else {
            false
        }
    }

    fn drop_held_entry(&mut self) -> bool {
        let horizontal_scroll = self.horizontal_scroll();
        let left_margin = self.left_margin();

        if self.mouse_y <= HEADER_SIZE { return false }
        if self.mouse_x + horizontal_scroll + 16 < left_margin { return false }
        let y = self.mouse_y - HEADER_SIZE + self.scroll();
        let x = (self.mouse_x + horizontal_scroll - left_margin) / 16 - 1;
        let tab = tab_mut!(self);
        let Some(HeldEntry { kv, indices_history }) = tab.held_entry.take() else { return false };

        if let Some(indices) = tab.value.create_drop_indices((kv.0.as_deref(), &kv.1), y, x) {
            match add_element(&mut tab.value, kv, indices, &mut tab.bookmarks, mutable_indices!(self, tab)) {
                Some(AddElementResult { indices, old_value }) => {
                    tab.value.expand_to_indices(&indices);
                    let Some(old_kv) = tab.value.get_kv_under_indices(&indices) else { return false };
                    let old_key = old_kv.0.map(|key| key.to_compact_string());
                    tab.append_to_history(WorkbenchAction::AddFromHeldEntry { indices, old_key, old_value, indices_history });
                    true
                },
                None => {
                    self.alert(Alert::error("Failed to drop held entry"));
                    false
                }
            }
        } else {
            tab.append_to_history(WorkbenchAction::DiscardHeldEntry {
                held_entry: HeldEntry {
                    kv,
                    indices_history,
                }
            });
            false
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
                            bail!("Chunks are not supported for non-region tabs");
                        } else {
                            let old_held_entry = tab.held_entry.replace(HeldEntry::from_aether((key, element)));
                            if let Some(held_entry) = old_held_entry {
                                tab.append_to_history(WorkbenchAction::DiscardHeldEntry { held_entry });
                            }
                            tab.append_to_history(WorkbenchAction::CreateHeldEntry);
                        }
                    }
                    Err(idx) => bail!("Could not parse clipboard as SNBT (failed at index {idx})"),
                }
            } else {
                let old_held_entry = tab.held_entry.replace(HeldEntry::from_aether((None, NbtElement::from_id(match x / 16 {
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
                if let Some(held_entry) = old_held_entry {
                    tab.append_to_history(WorkbenchAction::DiscardHeldEntry { held_entry })
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
                            self.alert(e.into());
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
                self.add_tab(window_properties, Tab::new_empty_tab(shift, (self.window_width, self.window_height)));
            }
        }
    }

    pub fn add_tab(&mut self, window_properties: &mut WindowProperties, tab: Tab) {
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
        let dialog = native_dialog::FileDialogBuilder::default()
            .set_location("~/Downloads")
            .add_filters(Tab::FILE_TYPE_FILTERS.iter().copied().map(|(a, b)| (a.to_owned(), b.iter().map(|x| x.to_string()).collect::<Vec<_>>())))
            .open_single_file();
        let dialog_result = dialog.show();
        self.ignore_event_end = now() + Duration::from_millis(50);
        match dialog_result {
            Err(e) => self.alert(e.into()),
            Ok(None) => {},
            Ok(Some(path)) => match std::fs::read(&path) {
                Ok(bytes) => if let Err(e) = self.on_open_file(&path, bytes, window_properties) {
                    self.alert(e.into())
                },
                Err(e) => self.alert(e.into()),
            }
        }
    }

    #[cfg(target_arch = "wasm32")]
    fn open_file(&mut self, _: &mut WindowProperties) {
        crate::wasm::try_open_dialog();
    }

    #[must_use]
    pub fn left_margin(&self) -> usize {
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
            Some(TraversalInformationMut { depth, key, element: value, line_number, true_line_number, indices }) => InteractionInformation::Content { is_in_left_margin, depth, key: key.map(|s| s.to_compact_string()), value, line_number, true_line_number, x, y, indices },
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
        tab.value.recache_along_indices(Indices::EMPTY);
        tab.refresh_scrolls();
        true
    }

    fn toggle(&mut self, expand: bool, ignore_depth: bool) -> bool {
        if let InteractionInformation::Content { is_in_left_margin: false, x, depth, value, indices, .. } = get_interaction_information!(self) && (x <= depth || ignore_depth) && value.is_complex() && value.true_height() > 1 {
            let is_open = value.is_open();
            let tab = tab_mut!(self);
            let result = if expand {
                expand_element(&mut tab.value, &indices, &mut tab.bookmarks)
            } else {
                if is_open {
                    close_element(&mut tab.value, &indices, &mut tab.bookmarks)
                } else {
                    open_element(&mut tab.value, &indices, &mut tab.bookmarks)
                }
            };
            if let None = result {
                self.alert(Alert::error("Failed to toggle hovered element"))
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

    fn try_deselect_selected_text(&mut self, window_properties: &mut WindowProperties) -> bool {
        let tab = tab_mut!(self);
        tab.write_selected_text(true, window_properties, true, true)
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

        let Some(TraversalInformation { indices, depth, key, element, .. }) = tab.value.traverse(y, None) else { return false };
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

    pub fn move_to_keyfix(&mut self, window_properties: &mut WindowProperties) {
        let tab = tab_mut!(self);
        if let Some(SelectedText(Text { value, cursor, editable: true, additional: SelectedTextAdditional { y, indices, value_color, keyfix, prefix, suffix, valuefix }, .. })) = tab.selected_text.clone()
            && let Some((keyfix, keyfix_color)) = keyfix
            && valuefix.is_none()
            && suffix.0.is_empty()
            && cursor == 0
        {
            if !tab.write_selected_text(false, window_properties, true, false) { return }
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

    pub fn move_to_valuefix(&mut self, window_properties: &mut WindowProperties) {
        let tab = tab_mut!(self);
        if let Some(SelectedText(Text { value, cursor, editable: true, additional: SelectedTextAdditional { y, indices, value_color, keyfix, prefix, suffix, valuefix }, .. })) = tab.selected_text.clone()
            && let Some((valuefix, valuefix_color)) = valuefix
            && keyfix.is_none()
            && prefix.0.is_empty()
            && cursor == value.len()
        {
            if !tab.write_selected_text(false, window_properties, true, false) { return }
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
        if parent.get_kv(a_idx).is_none() || parent.get_kv(b_idx).is_none() { return None };
        let result = match swap_element_same_depth(&mut tab.value, parent_indices.to_owned(), a_idx, b_idx, &mut tab.bookmarks, mutable_indices!(self, tab)) {
            Some(action) => action,
            None => {
                self.alert(Alert::error("Failed to swap elements"));
                return None
            }
        };

        let mut result_indices = result.parent.clone();
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

    fn move_selected_text(&mut self, window_properties: &mut WindowProperties, f: impl FnOnce(usize, &NbtElement, OwnedIndices) -> Option<usize>) -> bool {
        let left_margin = self.left_margin();
        let tab = tab_mut!(self);

        let Some(SelectedText(Text { additional: SelectedTextAdditional { y, indices, keyfix, prefix, .. }, value: str_value, cursor, .. })) = tab.selected_text.clone() else { return false };
        if !tab.write_selected_text(false, window_properties, true, false) { return false }

        let cache_cursor_x = tab.cache_cursor_x;
        let depth = indices.len();
        let y = (y - HEADER_SIZE) / 16;
        let mouse_x = cache_cursor_x.unwrap_or(depth * 16 + 32 + 4 + left_margin + keyfix.as_ref().map_or(0, |x| x.0.width()) + prefix.0.width() + str_value.split_at(cursor).0.width());

        let Some(new_y) = f(y, &tab.value, indices) else { return false };

        tab.cache_cursor_x.get_or_insert(mouse_x);
        tab.modify_scroll(|scroll| scroll.min(new_y * 16));
        tab.refresh_scrolls();

        self.set_selected_text_at_y(new_y, mouse_x, true);
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
        self.move_selected_text(window_properties, |y, root, indices| Some(if ctrl && let Some((last_idx, parent_indices)) = indices.split_last() {
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
        let Some(SelectedText(Text { additional: SelectedTextAdditional { indices, .. }, .. })) = tab.selected_text.as_ref() else { return };
        let alert = if let None = close_element(&mut tab.value, indices, &mut tab.bookmarks) {
            Some(Alert::error("Failed to close selected element"))
        } else {
            None
        };
        tab.refresh_scrolls();
        if let Some(alert) = alert {
            self.alert(alert);
        }
    }

    pub fn force_open(&mut self) {
        let expand = self.shift();
        let tab = tab_mut!(self);
        let Some(SelectedText(Text { additional: SelectedTextAdditional { indices, .. }, .. })) = tab.selected_text.as_ref() else { return };
        let result = if expand {
            expand_element(&mut tab.value, indices, &mut tab.bookmarks)
        } else {
            open_element(&mut tab.value, indices, &mut tab.bookmarks)
        };
        let alert = if let None = result {
            Some(Alert::error("Failed to open selected element"))
        } else {
            None
        };
        tab.refresh_scrolls();
        if let Some(alert) = alert {
            self.alert(alert);
        }
    }

    pub fn refresh_selected_text_horizontal_scroll(&mut self) {
        let left_margin = self.left_margin();
        let tab = tab_mut!(self);

        let free_space = 48 + left_margin;
        if let Some(selected_text) = tab.selected_text.as_ref() {
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
                        result @ (SearchBoxKeyResult::Search | SearchBoxKeyResult::SearchCountOnly) => {
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
                            let (notification, bulk) = self.replace_box.replace(&mut tab.bookmarks, mutable_indices!(self, tab), &mut tab.value, &self.search_box);
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
                            tab.cache_cursor_x = None;
                            self.refresh_selected_text_horizontal_scroll();
                            return true;
                        }
                        SelectedTextKeyResult::Escape => {
                            tab.selected_text = None;
                            tab.cache_cursor_x = None;
                            return true;
                        }
                        SelectedTextKeyResult::Finish => {
                            tab.cache_cursor_x = None;
                            // we just won't let you leave if you didn't fix it ;)
                            tab.write_selected_text(false, window_properties, true, false);
                            return true;
                        }
                        SelectedTextKeyResult::MoveToKeyfix => {
                            tab.cache_cursor_x = None;
                            self.move_to_keyfix(window_properties);
                            self.refresh_selected_text_horizontal_scroll();
                            return true;
                        }
                        SelectedTextKeyResult::MoveToValuefix => {
                            tab.cache_cursor_x = None;
                            self.move_to_valuefix(window_properties);
                            self.refresh_selected_text_horizontal_scroll();
                            return true;
                        }
                        SelectedTextKeyResult::Up(ctrl) => {
                            self.selected_text_up(ctrl, window_properties);
                            self.refresh_selected_text_horizontal_scroll();
                            return true;
                        }
                        SelectedTextKeyResult::Down(ctrl) => {
                            self.selected_text_down(ctrl, window_properties);
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
                if key == KeyCode::Escape && flags == flags!() && let Some(held_entry) = tab.held_entry.take() {
                    tab.append_to_history(WorkbenchAction::DiscardHeldEntry { held_entry });
                    return true;
                }
                if (key == KeyCode::Enter || key == KeyCode::NumpadEnter) && tab.selected_text.is_none() && flags == flags!() {
                    return if tab.held_entry.is_some() {
                        self.drop_held_entry()
                    } else {
                        self.try_select_text(true);
                        true
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
                        self.alert(e.into())
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
                    self.add_tab(window_properties, Tab::new_empty_tab((flags & flags!(Shift)) > 0, (self.window_width, self.window_height)));
                    return true;
                }
                if key == KeyCode::KeyO && flags == flags!(Ctrl) {
                    self.open_file(window_properties);
                    return true;
                }
                if key == KeyCode::KeyS && flags & (!flags!(Shift)) == flags!(Ctrl) {
                    return if let Err(e) = tab.save((flags & flags!(Shift)) > 0, window_properties) {
                        self.alert(e.into());
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
                        match action.undo(&mut tab.value, &mut tab.bookmarks, mutable_indices!(self, tab), &mut tab.path, &mut tab.name, &mut tab.held_entry, window_properties).context("Failed undo") {
                            Ok(action) => tab.redos.push(action),
                            Err(e) => self.alert(e.into()),
                        }
                        return true;
                    }
                }
                if key == KeyCode::KeyY && flags == flags!(Ctrl) || key == KeyCode::KeyZ && flags == flags!(Ctrl + Shift) {
                    if let Some(action) = tab.redos.pop() {
                        match action.undo(&mut tab.value, &mut tab.bookmarks, mutable_indices!(self, tab), &mut tab.path, &mut tab.name, &mut tab.held_entry, window_properties).context("Failed redo") {
                            Ok(action) => tab.undos.push(action),
                            Err(e) => self.alert(e.into()),
                        }
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
                    let kv = if key == KeyCode::Digit1 {
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
                            self.alert(Alert::error("Failed to get clipboard"));
                            return true;
                        };
                        match NbtElement::from_str(&clipboard) {
                            Ok((key, value)) => (key, value),
                            Err(idx) => {
                                self.alert(Alert::error(format!("Could not parse clipboard as SNBT (failed at index {idx})")));
                                return true;
                            }
                        }
                    } else {
                        return true;
                    };
                    let old_held_entry = tab.held_entry.replace(HeldEntry::from_aether(kv));
                    if let Some(held_entry) = old_held_entry {
                        tab.append_to_history(WorkbenchAction::DiscardHeldEntry {
                            held_entry,
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
        window_properties.set_window_title(format!("{} - NBT Workbench", tab!(self).name).as_str());
    }

    // todo: replace commented std::time::Instant::now() with debug pie for ms to complete and pct
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
            tab.held_entry.as_ref().map(|entry| {
                (
                    &entry.kv.1,
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
            builder.draw_texture_region_z(
                (33, 22),
                BASE_Z,
                LINE_NUMBER_SEPARATOR_UV,
                (2, 23),
                (2, 16),
            );
        }
        {
            // let start = std::time::Instant::now();
            tab.render(
                builder,
                &mut ctx,
                self.scrollbar_offset.is_some(),
                self.action_wheel.is_some(),
                self.steal_animation_data.as_ref().map(|x| (now() - x.0).min(LINE_DOUBLE_CLICK_INTERVAL).as_millis() as f32 / LINE_DOUBLE_CLICK_INTERVAL.as_millis_f32()).unwrap_or(0.0)
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
            let ctx = ButtonWidgetContext::new(tab, &self.search_box, &self.replace_box, shift);

            macro_rules! render_button {
                ($field:ident) => {
                    if self.$field.is_visible(&ctx) {
                        self.$field.render(builder, (self.mouse_x, self.mouse_y).into(), (self.window_width, self.window_height).into(), &ctx, &self.held_mouse_keys);
                    }
                };
            }

            render_button!(search_flags_button);
            render_button!(search_operation_button);
            render_button!(search_mode_button);
            render_button!(exact_match_button);
            render_button!(sort_algorithm_button);
            render_button!(theme_button);
            render_button!(freehand_mode_button);
            render_button!(refresh_button);
            render_button!(new_tab_button);
            render_button!(open_file_button);
            render_button!(replace_by_button);
            // println!("Buttons: {}ms", start.elapsed().as_millis_f64());
        }

        {
            // let start = std::time::Instant::now();
            self.render_action_wheel(builder);
            self.render_held_entry(builder);
            self.render_notifications_and_alerts(builder);
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

    pub fn tick(&mut self, window_properties: &mut WindowProperties) {
        #[cfg(not(target_arch = "wasm32"))] {
            let mut alerts = vec![];
            for (idx, tab) in self.tabs.iter_mut().enumerate() {
                if let Some(path) = tab.path.as_deref() && path.is_absolute() && (now() - tab.last_interaction >= Tab::AUTOSAVE_INTERVAL) && tab.unsaved_changes && tab.value.true_height() <= Tab::AUTOSAVE_MAXIMUM_LINES {
                    if let Err(e) = tab.save(false, window_properties) {
                        alerts.push(e.context(format!("Failed to autosave {nth} tab", nth = nth(idx + 1))).into());
                    }
                }
            }
            for alert in alerts {
                self.alert(alert);
            }
        }
        if (tab!(self).held_entry.is_some() || tab!(self).freehand_mode || ((tab!(self).selected_text.is_some() || self.search_box.is_selected()) && self.last_mouse_state == ElementState::Pressed)) && self.action_wheel.is_none() && self.scrollbar_offset.is_none() {
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
            self.alert(Alert::new("Are you sure you want to exit?", TextColor::Yellow, format!("You have {failed_tabs} unsaved tab{tab_suffix}.", tab_suffix = if failed_tabs == 1 { "" } else { "s" })));
        }
        failed_tabs
    }

    fn render_held_entry(&self, builder: &mut VertexBufferBuilder) {
        let shift = self.shift();

        if let Some(held_entry) = &tab!(self).held_entry {
            let element = &held_entry.kv.1;
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
            format!("held entry: {data}", data = if let Some(held) = &tab.held_entry { format!("h={}, th={}", held.kv.1.height(), held.kv.1.true_height()) } else { "null".to_owned() }),
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
        use std::f64::consts::TAU;

        let Some((cx, cy)) = self.action_wheel else { return };
        let cx = cx.saturating_sub(31) + 31;
        let cy = cy.saturating_sub(31) + 31;
        let left_margin = self.left_margin();
        let tab = tab_mut!(self);
        let highlight_idx = (f64::atan2(cy as f64 - self.mouse_y as f64, cx as f64 - self.mouse_x as f64) / TAU * 8.0 + 3.5).rem_euclid(8.0) as usize;
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
        if self.mouse_x >= self.window_width - 16 && self.mouse_y >= HEADER_SIZE {
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
pub struct HeldEntry {
    pub(super) kv: NbtElementAndKey,
    pub(super) indices_history: LinkedQueue<OwnedIndices>,
}

impl HeldEntry {
    #[must_use]
    pub fn from_aether(kv: NbtElementAndKey) -> Self {
        Self {
            kv,
            indices_history: LinkedQueue::new(),
        }
    }

    #[must_use]
    pub fn from_indices(kv: NbtElementAndKey, indices: OwnedIndices) -> Self {
        Self {
            kv,
            indices_history: {
                let mut queue = LinkedQueue::new();
                queue.push(indices);
                queue
            }
        }
    }
}

#[derive(Copy, Clone)]
pub enum SortAlgorithm {
    None,
    Name,
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
    r#type: FileUpdateSubscriptionType,
    pub indices: OwnedIndices,
    rx: std::sync::mpsc::Receiver<Vec<u8>>,
    watcher: notify::PollWatcher,
    tab_uuid: Uuid,
}

impl FileUpdateSubscription {
    #[must_use]
    pub fn new(r#type: FileUpdateSubscriptionType, indices: OwnedIndices, rx: std::sync::mpsc::Receiver<Vec<u8>>, watcher: notify::PollWatcher, tab_uuid: Uuid) -> Self {
        Self {
            r#type,
            indices,
            rx,
            watcher,
            tab_uuid,
        }
    }
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
