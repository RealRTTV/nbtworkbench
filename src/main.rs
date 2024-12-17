#![allow(
	semicolon_in_expressions_from_macros,
	internal_features,
	incomplete_features,
)]
#![feature(
    array_chunks,
    box_patterns,
    core_intrinsics,
    iter_array_chunks,
    iter_next_chunk,
    optimize_attribute,
	panic_update_hook,
    stmt_expr_attributes,
	float_next_up_down,
	variant_count,
	sync_unsafe_cell,
	duration_millis_float,
	let_chains
)]
#![windows_subsystem = "windows"]

extern crate core;

use std::cell::UnsafeCell;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Write};
use std::mem::MaybeUninit;
use std::rc::Rc;
use std::time::Duration;
use anyhow::{anyhow, Context};
use compact_str::{CompactString, ToCompactString};
use regex::{Regex, RegexBuilder};
use static_assertions::const_assert_eq;
use winit::dpi::PhysicalSize;
use winit::window::Window;

use elements::element::NbtElement;
use vertex_buffer_builder::VertexBufferBuilder;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::wasm_bindgen;

use crate::assets::{BASE_TEXT_Z, BASE_Z, BOOKMARK_UV, BOOKMARK_Z, END_LINE_NUMBER_SEPARATOR_UV, HEADER_SIZE, HIDDEN_BOOKMARK_UV, HOVERED_WIDGET_UV, INSERTION_CHUNK_UV, INSERTION_UV, INVALID_STRIPE_UV, LINE_NUMBER_SEPARATOR_UV, LINE_NUMBER_Z, SCROLLBAR_BOOKMARK_Z, SELECTED_TOGGLE_OFF_UV, SELECTED_TOGGLE_ON_UV, SELECTED_WIDGET_UV, SORT_COMPOUND_BY_NAME_UV, SORT_COMPOUND_BY_NOTHING_UV, SORT_COMPOUND_BY_TYPE_UV, TEXT_UNDERLINE_UV, TOGGLE_Z, UNSELECTED_TOGGLE_OFF_UV, UNSELECTED_TOGGLE_ON_UV};
use crate::color::TextColor;
use crate::elements::compound::CompoundMap;
use crate::elements::element::{NbtByteArray, NbtIntArray, NbtLongArray};
use crate::marked_line::{MarkedLine, MarkedLineSlice, MarkedLines};
use crate::tree_travel::Navigate;
use crate::vertex_buffer_builder::Vec2u;
use crate::workbench::Workbench;
use crate::workbench_action::WorkbenchAction;

mod alert;
mod assets;
mod be_decoder;
mod marked_line;
#[cfg(not(target_arch = "wasm32"))]
mod cli;
mod color;
mod element_action;
mod encoder;
mod formatter;
mod le_decoder;
mod search_box;
mod selected_text;
mod shader;
mod tab;
mod text;
mod text_shader;
mod tree_travel;
mod vertex_buffer_builder;
mod window;
mod workbench;
mod workbench_action;
mod notification;
mod config;
pub mod elements {
	pub mod array;
	pub mod chunk;
	pub mod compound;
	pub mod element;
	pub mod list;
	pub mod primitive;
	pub mod string;
	pub mod null;
}

#[macro_export]
macro_rules! flags {
	() => {
		0b000_u8
	};
	(Ctrl) => {
		0b001_u8
	};
	(Shift) => {
		0b010_u8
	};
	(Ctrl + Shift) => {
		0b011_u8
	};
	(Alt) => {
		0b100_u8
	};
	(Ctrl + Alt) => {
		0b101_u8
	};
	(Shift + Alt) => {
		0b110_u8
	};
	(Ctrl + Shift + Alt) => {
		0b111_u8
	};
}

#[macro_export]
macro_rules! hash {
	($data:expr) => {{
		let mut hasher = ::fxhash::FxHasher::default();
		::std::hash::Hasher::write(&mut hasher, $data.as_bytes());
		::std::hash::Hasher::finish(&hasher)
	}};
}

#[macro_export]
macro_rules! tab {
    ($self:ident) => {
		#[allow(unused_unsafe)]
		unsafe { $self.tabs.get_unchecked($self.tab) }
	};
}

#[macro_export]
macro_rules! tab_mut {
    ($self:ident) => {
		#[allow(unused_unsafe)]
		unsafe { $self.tabs.get_unchecked_mut($self.tab) }
	};
}

#[macro_export]
macro_rules! get_interaction_information {
    ($self:ident) => {{
		let left_margin = $self.left_margin();
		let horizontal_scroll = $self.horizontal_scroll();
		let scroll = $self.scroll();
		$crate::workbench::Workbench::get_interaction_information_raw(left_margin, horizontal_scroll, scroll, $self.mouse_x, $self.mouse_y, &mut tab_mut!($self).value)
	}};
}

#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {{
		eprintln!($($arg)*);
	}};
}

#[cfg(target_arch = "wasm32")]
#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {
		::web_sys::console::error_1(&wasm_bindgen::JsValue::from(&format!($($arg)*)));
	};
}

#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! log {
    ($($arg:tt)*) => {{
		println!($($arg)*);
	}};
}

#[cfg(target_arch = "wasm32")]
#[macro_export]
macro_rules! log {
    ($($arg:tt)*) => {
		::web_sys::console::log_1(&wasm_bindgen::JsValue::from(&format!($($arg)*)));
	};
}

#[wasm_bindgen(module = "/web/script.js")]
#[cfg(target_arch = "wasm32")]
extern "C" {
	#[wasm_bindgen(js_name = "getClipboard")]
	fn get_clipboard() -> Option<String>;

	#[wasm_bindgen(js_name = "onInput")]
	fn on_input();

	#[wasm_bindgen(js_name = "tryOpenDialog")]
	fn try_open_dialog();

	#[wasm_bindgen(js_name = "save")]
	fn save(name: &str, bytes: Vec<u8>);

	#[wasm_bindgen(js_name = "onPanic")]
	fn on_panic(msg: String);
}

pub static mut WORKBENCH: UnsafeCell<Workbench> = UnsafeCell::new(unsafe { Workbench::uninit() });
pub static mut WINDOW_PROPERTIES: UnsafeCell<WindowProperties> = UnsafeCell::new(WindowProperties::Fake);
pub const TEXT_DOUBLE_CLICK_INTERVAL: Duration = Duration::from_millis(250);
pub const LINE_DOUBLE_CLICK_INTERVAL: Duration = Duration::from_millis(250);
pub const TAB_CLOSE_DOUBLE_CLICK_INTERVAL: Duration = Duration::from_millis(2000);

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn open_file(name: String, bytes: Vec<u8>) {
	use crate::alert::Alert;

	let workbench = unsafe { WORKBENCH.get_mut() };

	if let Err(e) = workbench.on_open_file(name.as_str().as_ref(), bytes, unsafe { WINDOW_PROPERTIES.get_mut() }) {
		workbench.alert(Alert::new("Error!", TextColor::Red, e.to_string()));
	}
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn close() -> usize {
	let workbench = unsafe { WORKBENCH.get_mut() };
	workbench.close()
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen(start))]
#[cfg(target_arch = "wasm32")]
pub fn wasm_main() {
	config::read();
	std::panic::set_hook(Box::new(|info| {
		on_panic(info.to_string());
	}));
	wasm_bindgen_futures::spawn_local(async move {
		window::run().await;
	});
}

/// # Refactor
/// * render trees using [`RenderLine`](RenderLine) struct/enum
/// * rendering code is duplicated af
/// # Long-Term Goals
/// * smart screen
/// * [chunk](elements::chunk::NbtChunk) section rendering
/// # Minor Features
/// * [`last_modified`](elements::chunk::NbtChunk) field actually gets the ability to be set
/// * sort for regions
/// # Major Features
/// * macros
#[cfg(not(target_arch = "wasm32"))]
pub fn main() -> ! {
	config::read();
	#[cfg(target_os = "windows")] unsafe {
		winapi::um::wincon::AttachConsole(winapi::um::wincon::ATTACH_PARENT_PROCESS);
	}

	let first_arg = std::env::args().nth(1);
	if let Some("find") = first_arg.as_deref() {
		cli::find()
	} else if let Some("reformat") = first_arg.as_deref() {
		cli::reformat()
	} else if let Some("--version" | "-v") = first_arg.as_deref() {
		println!("{}", env!("CARGO_PKG_VERSION"));
		std::process::exit(0);
	} else if let Some("-?" | "/?" | "--help" | "-h") = first_arg.as_deref() {
		println!(r#"
Usage:
  nbtworkbench --version|-v
  nbtworkbench -?|-h|--help|/?
  nbtworkbench find <path> [(--mode|-m)=(normal|regex|snbt)] [(--search|-s)=(key|value|all)] [--exact-match|-em] <query>
  nbtworkbench reformat (--format|-f)=<format> [(--out-dir|-d)=<out-dir>] [(--out-ext|-e)=<out-ext>] <path>

Options:
  --version, -v       Displays the version of nbtworkbench you're running.
  -?, -h, --help, /?  Displays this dialog.
  --mode, -m          Changes the `find` mode to take the <query> field as either, a containing substring, a regex (match whole), or snbt. [default: normal]
  --search, -s        Searches for results matching the <query> in either, the key, the value, or both (note that substrings and regex search the same pattern in both key and value, while the regex uses it's key field to match equal strings). [default: all]
  --format, -f        Specifies the format to be reformatted to; either `nbt`, `snbt`, `dat/dat_old/gzip`, `zlib`, 'lnbt' (little endian nbt), or 'lhnbt' (little endian nbt with header).
  --out-dir, -d       Specifies the output directory. [default: ./]
  --out-ext, -e       Specifies the output file extension (if not specified, it will infer from --format)"#);
		std::process::exit(0)
	} else {
		pollster::block_on(window::run())
	}
}

pub type NbtElementAndKey = (Option<CompactString>, NbtElement);

pub enum DropFn {
	Dropped(usize, usize, Option<CompactString>, usize, Option<NbtElementAndKey>),
	Missed(NbtElementAndKey),
	InvalidType(NbtElementAndKey),
}

/// Yoinked from `itertools`.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Position {
	Only,
	First,
	Middle,
	Last,
}

#[derive(Debug)]
pub enum HeldEntry {
	Empty,
	FromAether(NbtElementAndKey),
	FromKnown(NbtElementAndKey, Box<[usize]>, bool),
}

impl HeldEntry {
	#[inline]
	#[must_use]
	pub const fn element(&self) -> Option<&NbtElement> {
		match self {
			Self::Empty => None,
			Self::FromAether((_, element)) | Self::FromKnown((_, element), _, _) => Some(element),
		}
	}

	#[inline]
	#[must_use]
	pub const fn is_empty(&self) -> bool { matches!(self, Self::Empty) }

	#[inline]
	pub fn take(&mut self) -> Self {
		core::mem::replace(self, HeldEntry::Empty)
	}
}

#[must_use]
#[cfg(not(target_arch = "wasm32"))]
pub fn get_clipboard() -> Option<String> {
	cli_clipboard::get_contents().ok()
}

#[cfg(not(target_arch = "wasm32"))]
pub fn set_clipboard(value: String) -> bool {
	cli_clipboard::set_contents(value).is_ok()
}

#[cfg(target_arch = "wasm32")]
pub fn set_clipboard(value: String) -> bool {
	web_sys::window().map(|window| window.navigator()).and_then(|navigator| navigator.clipboard()).map(|clipboard| clipboard.write_text(&value)).is_some()
}

#[must_use]
pub fn create_regex(mut str: String, case_sensitive: bool) -> Option<Regex> {
	let flags = 'a: {
		if !str.starts_with("/") {
			break 'a 0;
		}

		str = str.split_off(1);

		let mut flags = 0_u8;
		while let Some(char) = str.pop() {
			match char {
				'i' => flags |= 0b000001,
				'g' => flags |= 0b000010,
				'm' => flags |= 0b000100,
				's' => flags |= 0b001000,
				'u' => flags |= 0b010000,
				'y' => flags |= 0b100000,
				'/' => break,
				_ => return None
			}
		}
		flags
	};

	RegexBuilder::new(&str)
		.case_insensitive((flags & 0b1 > 0) || !case_sensitive)
		.multi_line(flags & 0b100 > 0)
		.dot_matches_new_line(flags & 0b1000 > 0)
		.unicode(flags & 0b10000 > 0)
		.swap_greed(flags & 0b10000 > 0)
		.build()
		.ok()
}

#[must_use]
#[cfg(not(target_arch = "wasm32"))]
pub fn since_epoch() -> Duration {
	std::time::SystemTime::UNIX_EPOCH.elapsed().unwrap_or_else(|e| e.duration())
}

#[must_use]
#[cfg(target_arch = "wasm32")]
pub fn since_epoch() -> Duration {
	Duration::from_millis(web_sys::js_sys::Date::now() as u64)
}

pub fn split_lines<const MAX_WIDTH: usize>(s: String) -> Vec<String> {
	let mut lines = Vec::new();
	let mut current_line = String::new();
	let mut is_previous_byte_ascii_whitespace = true;
	for word in s.as_bytes().split_inclusive(|byte| {
		let is_ascii_whitespace = byte.is_ascii_whitespace();
		let is_previous_byte_ascii_whitespace = core::mem::replace(&mut is_previous_byte_ascii_whitespace, is_ascii_whitespace);
		!is_previous_byte_ascii_whitespace && is_ascii_whitespace
	}).filter(|slice| !slice.is_empty()).map(|slice| unsafe { std::str::from_utf8_unchecked(slice) }) {
		if current_line.width() + word.width() > MAX_WIDTH {
			lines.push(current_line.trim_ascii_end().to_string());
			current_line = String::new();
		}
		current_line += word;
	}
	let trimmed = current_line.trim_ascii_end();
	if !trimmed.is_empty() {
		lines.push(trimmed.to_string());
	}
	
	lines
}

pub fn nth(n: usize) -> String {
	let mut buf = String::with_capacity(n.checked_ilog10().map_or(1, |x| x + 1) as usize + 2);
	let _ = write!(&mut buf, "{n}");
	if n / 10 % 10 == 1 {
		buf.push_str("th");
	} else {
		match n % 10 {
			1 => buf.push_str("st"),
			2 => buf.push_str("nd"),
			3 => buf.push_str("rd"),
			_ => buf.push_str("th"),
		}
	}
	buf
}

pub fn sum_indices<I: Iterator<Item = usize>>(indices: I, mut root: &NbtElement) -> usize {
	let mut total = 0;
	let mut indices = indices.peekable();
	while let Some(idx) = indices.next() {
		root = if let NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID = root.id() {
			total += 1 + idx;
			break;
		} else if let Some(list) = root.as_list() {
			total += 1 + list
				.children()
				.take(idx)
				.map(NbtElement::height)
				.sum::<usize>();
			if let Some(root) = list.get(idx) {
				root
			} else {
				break;
			}
		} else if let Some(compound) = root.as_compound() {
			total += 1 + compound
				.children()
				.take(idx)
				.map(|(_, b)| b)
				.map(NbtElement::height)
				.sum::<usize>();
			if let Some((_, root)) = compound.get(idx) {
				root
			} else {
				break;
			}
		} else if let Some(chunk) = root.as_chunk() {
			total += 1 + chunk
				.children()
				.take(idx)
				.map(|(_, b)| b)
				.map(NbtElement::height)
				.sum::<usize>();
			if let Some((_, root)) = chunk.get(idx) {
				root
			} else {
				break;
			}
		} else if let Some(region) = root.as_region() {
			total += 1 + region
				.children()
				.take(idx)
				.map(NbtElement::height)
				.sum::<usize>();
			if let Some(root) = region.get(idx) {
				root
			} else {
				break;
			}
		} else {
			total += root.height();
			if indices.peek().is_some() {
				panic!("tried to index non-indexable")
			} else {
				break;
			}
		};
	}
	total
}

pub fn recache_along_indices(indices: &[usize], parent: &mut NbtElement) {
	if let Some(region) = parent.as_region_mut() {
		if let Some((&idx, rest)) = indices.split_first() {
			recache_along_indices(rest, region.get_mut(idx).expect("expected valid index"));
		}
		region.recache();
	} else if let Some(array) = parent.as_byte_array_mut() {
		array.recache();
	} else if let Some(array) = parent.as_int_array_mut() {
		array.recache();
	} else if let Some(array) = parent.as_long_array_mut() {
		array.recache();
	} else if let Some(list) = parent.as_list_mut() {
		if let Some((&idx, rest)) = indices.split_first() {
			recache_along_indices(rest, list.get_mut(idx).expect("expected valid index"));
		}
		list.recache();
	} else if let Some(compound) = parent.as_compound_mut() {
		if let Some((&idx, rest)) = indices.split_first() {
			recache_along_indices(rest, compound.get_mut(idx).expect("expected valid index").1, );
		}
		compound.recache();
	} else if let Some(chunk) = parent.as_chunk_mut() {
		if let Some((&idx, rest)) = indices.split_first() {
			recache_along_indices(rest, chunk.get_mut(idx).expect("expected valid index").1, );
		}
		chunk.recache();
	}
}

#[inline]
#[must_use]
pub fn encompasses_or_equal<T: Ord>(outer: &[T], inner: &[T]) -> bool {
	outer.len() <= inner.len() && outer == &inner[..outer.len()]
}

#[inline]
#[must_use]
pub fn encompasses<T: Ord>(outer: &[T], inner: &[T]) -> bool {
	outer.len() < inner.len() && outer == &inner[..outer.len()]
}

#[inline]
#[must_use]
pub const fn is_utf8_char_boundary(x: u8) -> bool { (x as i8) >= -0x40 }

// importantly, no underscores
#[inline]
#[must_use]
pub fn is_jump_char_boundary(x: u8) -> bool { b" \t\r\n/\\()\"'-.,:;<>~!@#$%^&*|+=[]{}~?|".contains(&x) }

pub enum WindowProperties {
	Real(Rc<Window>),
	Fake,
}

impl WindowProperties {
	pub const fn new(window: Rc<Window>) -> Self {
		Self::Real(window)
	}

	pub fn window_title(&mut self, title: &str) -> &mut Self {
		if let Self::Real(window) = self {
			window.set_title(title);
			#[cfg(target_arch = "wasm32")]
			if let Some(document) = web_sys::window().and_then(|window| window.document()) {
				let _ = document.set_title(title);
			}
		}
		self
	}

	pub fn get_window_size(&self) -> Option<PhysicalSize<u32>> {
		match self {
			Self::Real(window) => Some(window.inner_size()),
			Self::Fake => None,
		}
	}
}

pub struct FileUpdateSubscription {
	subscription_type: FileUpdateSubscriptionType,
	indices: Box<[usize]>,
	rx: std::sync::mpsc::Receiver<Vec<u8>>,
	watcher: notify::PollWatcher,
	tab_uuid: uuid::Uuid,
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

		let widget_uv = if (280..296).contains(&ctx.mouse_x) && (26..42).contains(&ctx.mouse_y) {
			builder.draw_tooltip(&[&format!("Compound Sorting Algorithm ({self})")], (ctx.mouse_x, ctx.mouse_y), false);
			HOVERED_WIDGET_UV
		} else {
			SELECTED_WIDGET_UV
		};
		builder.draw_texture((280, 26), widget_uv, (16, 16));
		builder.draw_texture((283, 29), uv, (10, 10));
	}

	pub fn cycle(self) -> Self {
		match self {
			Self::None => Self::Name,
			Self::Name => Self::Type,
			Self::Type => Self::None,
		}
	}

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
			Self::Name => map.entries.sort_by(|a, b| element_action::ElementAction::by_name((&a.key, &a.value), (&b.key, &b.value))),
			_ => map.entries.sort_by(|a, b| element_action::ElementAction::by_type((&a.key, &a.value), (&b.key, &b.value))),
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

pub enum LineNumberKind {
	Generic,
	Grid(usize, usize),
}

pub struct RenderContext<'a> {
	selecting_key: bool,
	selected_y: usize,
	selected_key: Option<Box<str>>,
	selected_value: Option<Box<str>>,
	extend_error: bool,
	invalid_key_error: bool,
	invalid_value_error: bool,
	key_duplicate_error: bool,
	ghost: Option<(&'a NbtElement, usize, usize)>,
	left_margin: usize,
	mouse_x: usize,
	mouse_y: usize,
	line_number: usize,
	// if you can only select one line at a time, the most that we can have is two red line numbers (from a duplicate key)
	red_line_numbers: [usize; 2],
	pub x_offset: usize,
	pub y_offset: usize,
	// must be sorted least to greatest
	line_numbers: Vec<usize>,
	freehand: bool,
}

impl<'a> RenderContext<'a> {
	#[must_use]
	#[allow(clippy::type_complexity)] // forbidden is fine to be like that, c'mon
	pub fn new(selected_y: usize, selected_key: Option<Box<str>>, selected_value: Option<Box<str>>, selecting_key: bool, ghost: Option<(&'a NbtElement, usize, usize)>, left_margin: usize, mouse: (usize, usize), freehand: bool) -> Self {
		Self {
			selecting_key,
			selected_y,
			selected_key,
			selected_value,
			extend_error: false,
			invalid_key_error: false,
			invalid_value_error: false,
			key_duplicate_error: false,
			ghost,
			left_margin,
			mouse_x: mouse.0,
			mouse_y: mouse.1,
			line_number: 1,
			red_line_numbers: [0, 0],
			x_offset: 16 + left_margin,
			y_offset: HEADER_SIZE,
			line_numbers: vec![],
			freehand,
		}
	}

	#[must_use]
	pub const fn pos(&self) -> Vec2u { Vec2u::new(self.x_offset, self.y_offset) }

	#[inline]
	pub fn check_for_key_duplicate<F: FnOnce(&str, Option<&str>) -> bool>(&mut self, f: F, extend: bool) {
		if let Some(selected_key) = self.selected_key.as_ref()
			&& self.selecting_key
		{
			self.key_duplicate_error = f(selected_key, self.selected_value.as_ref().map(Box::as_ref));
			self.extend_error = extend;
		}
	}

	#[inline]
	pub fn check_for_invalid_key<F: FnOnce(&str) -> bool>(&mut self, f: F) {
		let (_, y) = self.pos().into();
		if let Some(selected_key) = self.selected_key.as_ref()
			&& self.selected_y == y
			&& self.selecting_key {
			self.invalid_key_error = f(selected_key);
		}
	}

	#[inline]
	pub fn check_for_invalid_value<F: FnOnce(&str) -> bool>(&mut self, f: F) {
		let (_, y) = self.pos().into();
		if let Some(selected_value) = self.selected_value.as_ref()
			&& self.selected_y == y
			&& !self.selecting_key
		{
			self.invalid_value_error = f(selected_value);
		}
	}

	#[inline]
	pub fn draw_toggle(&self, pos: impl Into<(usize, usize)>, open: bool, builder: &mut VertexBufferBuilder) {
		let pos = pos.into();
		let x = (pos.0 - self.left_margin) / 16;
		let y = (pos.1 - HEADER_SIZE) / 16;
		let hovered = if (self.mouse_x >= self.left_margin) & (self.mouse_y >= HEADER_SIZE) {
			((x >= (self.mouse_x - self.left_margin) / 16) || self.freehand) & (y == (self.mouse_y - HEADER_SIZE) / 16)
		} else {
			false
		};
		let uv = match (open, hovered) {
			(false, false) => UNSELECTED_TOGGLE_OFF_UV,
			(false, true) => SELECTED_TOGGLE_OFF_UV,
			(true, false) => UNSELECTED_TOGGLE_ON_UV,
			(true, true) => SELECTED_TOGGLE_ON_UV,
		};
		builder.draw_texture_z(Vec2u::from(pos) + (3, 5), TOGGLE_Z, uv, (8, 8));
	}

	#[inline]
	#[must_use]
	pub fn forbid(&self, pos: impl Into<(usize, usize)>) -> bool {
		let (_, y) = pos.into();
		if y == self.selected_y {
			false
		} else {
			true
		}
	}

	#[inline]
	pub fn render_errors(&mut self, pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder) {
		let (x, y) = pos.into();
		if (self.key_duplicate_error | self.invalid_key_error | self.invalid_value_error) && self.selected_y == y {
			self.red_line_numbers[0] = self.selected_y;
			self.draw_error_underline(x, y, builder);
		}
	}

	#[inline]
	pub fn draw_error_underline_width(&self, x: usize, y: usize, overridden_width: usize, builder: &mut VertexBufferBuilder) {
		let horizontal_scroll_before = core::mem::replace(&mut builder.horizontal_scroll, 0);
		builder.draw_texture_region_z(
			(0, y),
			BASE_Z,
			INVALID_STRIPE_UV + (1, 1),
			(builder.window_width(), 16),
			(14, 14),
		);
		builder.horizontal_scroll = horizontal_scroll_before;
		builder.draw_texture_region_z(
			(x + 20, y + 14),
			BASE_Z,
			TEXT_UNDERLINE_UV,
			(overridden_width, 2),
			(16, 2),
		);
	}

	#[inline]
	pub fn draw_error_underline(&self, x: usize, y: usize, builder: &mut VertexBufferBuilder) {
		let key_width = self.selected_key.as_deref().map(str::width).unwrap_or(0);
		let value_width = self.selected_value.as_deref().map(str::width).unwrap_or(0);
		let (overridden_width, x_shift) = if self.selected_key.is_some() {
			if self.extend_error {
				(key_width + value_width + ": ".width(), 0)
			} else if self.selecting_key {
				(key_width, 0)
			} else {
				(value_width, key_width + ": ".width())
			}
		} else {
			(value_width, 0)
		};
		self.draw_error_underline_width(x + x_shift, y, overridden_width, builder);
	}

	#[inline]
	pub fn skip_line_numbers(&mut self, n: usize) { self.line_number = self.line_number.wrapping_add(n); }

	#[inline]
	pub fn line_number(&mut self) {
		self.line_numbers.push(self.line_number);
		self.line_number += 1;
	}

	#[inline]
	pub fn render_line_numbers(&self, builder: &mut VertexBufferBuilder, mut bookmarks: &MarkedLineSlice) {
		let start = self.line_numbers.first();
		while let Some((head, rest)) = bookmarks.split_first() {
			if start.is_some_and(|&start| start > head.true_line_number()) {
				bookmarks = rest;
			} else {
				break;
			}
		}
		let mut y = HEADER_SIZE;
		for (idx, &line_number) in self.line_numbers.iter().enumerate() {
			let next_line_number = self.line_numbers.get(idx + 1).copied();

			let color = if (self.red_line_numbers[0] == y) | (self.red_line_numbers[1] == y) {
				if idx % 2 == 0 {
					0xC33C3C
				} else {
					TextColor::Red.to_raw()
				}
			} else {
				if idx % 2 == 0 {
					0x777777
				} else {
					TextColor::Gray.to_raw()
				}
			};
			let color = core::mem::replace(&mut builder.color, color);
			builder.settings(
				(
					self.left_margin - line_number.ilog10() as usize * 8 - 16,
					y,
				),
				false,
				BASE_TEXT_Z,
			);
			let _ = write!(builder, "{line_number}");
			builder.color = color;

			if let Some((first, rest)) = bookmarks.split_first() && line_number == first.true_line_number() {
				bookmarks = rest;
				builder.draw_texture_region_z(
					(1, y + 2),
					BOOKMARK_Z,
					first.uv(),
					(builder.text_coords.0 + 1, 12),
					(16, 16),
				);
			}
			let mut hidden_bookmarks = 0_usize;
			while let Some((first, rest)) = bookmarks.split_first() && next_line_number.is_none_or(|next_line_number| line_number <= first.true_line_number() && first.true_line_number() < next_line_number) {
				bookmarks = rest;
				if hidden_bookmarks < 5 {
					builder.draw_texture_region_z(
						(1, y + 15),
						BOOKMARK_Z,
						first.uv(),
						(builder.text_coords.0 + 1, 2),
						(16, 16),
					);
				}
				hidden_bookmarks += 1;
			}

			let uv = if idx + 1 == self.line_numbers.len() {
				END_LINE_NUMBER_SEPARATOR_UV
			} else {
				LINE_NUMBER_SEPARATOR_UV
			};
			builder.draw_texture_z((builder.text_coords.0 + 4, y), LINE_NUMBER_Z, uv, (2, 16));
			y += 16;
		}
	}

	#[inline]
	pub fn render_grid_line_numbers(&self, builder: &mut VertexBufferBuilder, mut bookmarks: &MarkedLineSlice) {
		let scroll = builder.scroll();

		let last_line_number = (self.line_numbers.len() > 1) as usize * 32 + 1;
		for line_number in 1..=last_line_number {
			let uv = if line_number == last_line_number {
				END_LINE_NUMBER_SEPARATOR_UV
			} else {
				LINE_NUMBER_SEPARATOR_UV
			};
			if 16 * line_number >= scroll + 16 {
				let color = if line_number % 2 == 1 {
					0x777777
				} else {
					TextColor::Gray.to_raw()
				};
				let color = core::mem::replace(&mut builder.color, color);
				builder.settings(
					(
						self.left_margin - line_number.ilog10() as usize * 8 - 16,
						HEADER_SIZE + 16 * line_number - 16 - scroll,
					),
					false,
					BASE_TEXT_Z,
				);
				let _ = write!(builder, "{line_number}");
				builder.color = color;
				builder.draw_texture_z((builder.text_coords.0 + 4, HEADER_SIZE + 16 * line_number - 16 - scroll), LINE_NUMBER_Z, uv, (2, 16));
			}
		}

		if let Some((first, rest)) = bookmarks.split_first() && first.true_line_number() == 1 {
			bookmarks = rest;
			if scroll < 16 {
				builder.draw_texture_region_z(
					(1, HEADER_SIZE + 2),
					BOOKMARK_Z,
					first.uv(),
					(self.left_margin - 7, 12),
					(16, 16),
				);
			}
		}

		for (idx, &line_number) in self.line_numbers.iter().enumerate() {
			let next_line_number = self.line_numbers.get(idx + 1).copied();
			let x = idx % 32;
			let z = idx / 32;
			let pos = Vec2u::new(self.left_margin + 16 + 16 + x * 16, HEADER_SIZE + 16 + z * 16);
			if let Some((first, rest)) = bookmarks.split_first() && first.true_line_number() == line_number {
				bookmarks = rest;
				if pos.y >= scroll + HEADER_SIZE {
					builder.draw_texture_region_z(
						pos - (0, scroll),
						BOOKMARK_Z,
						first.uv(),
						(16, 16),
						(16, 16),
					);
				}
			}
			let mut hidden_bookmarks = 0_usize;
			while let Some((first, rest)) = bookmarks.split_first() && next_line_number.is_none_or(|next_line_number| line_number <= first.true_line_number() && first.true_line_number() < next_line_number) {
				bookmarks = rest;
				if hidden_bookmarks < 5 {
					if pos.y >= scroll + HEADER_SIZE {
						builder.draw_texture_region_z(
							pos + (0, 14) - (0, scroll),
							BOOKMARK_Z,
							first.uv(),
							(16, 2),
							(16, 16),
						);
					}
				}
				hidden_bookmarks += 1;
			}
		}
	}

	#[inline]
	pub fn render_key_value_errors(&mut self, builder: &mut VertexBufferBuilder) {
		if self.mouse_y < HEADER_SIZE { return }
		let y = ((self.mouse_y - HEADER_SIZE) & !15) + HEADER_SIZE;
		if self
			.red_line_numbers
			.into_iter()
			.any(|red_line_number| red_line_number == y)
		{
			let mut errors = vec![];
			if self.invalid_value_error {
				errors.push("Error! The currently entered value is not valid for this type.");
			}
			if self.invalid_key_error {
				errors.push("Error! The currently entered key is not valid for this type.");
			}
			if self.key_duplicate_error {
				errors.push("Error! The current key is a duplicate of another one.");
			}
			let color_before = core::mem::replace(&mut builder.color, TextColor::Red.to_raw());
			builder.draw_tooltip(&errors, (self.mouse_x, self.mouse_y), false);
			builder.color = color_before;
		}
	}

	#[inline]
	pub fn render_scrollbar_bookmarks(&self, builder: &mut VertexBufferBuilder, bookmarks: &MarkedLineSlice, root: &NbtElement) {
		let height = root.height();
		let mut hidden_bookmarks_at_y = 0_usize;
		let mut hidden_bookmark_y = 0;
		let mut bookmarks_at_y = 0_usize;
		let mut bookmark_y = 0;
		for bookmark in bookmarks.iter() {
			let y = HEADER_SIZE + (bookmark.line_number() * (builder.window_height() - HEADER_SIZE)) / height;
			if bookmark.uv() == BOOKMARK_UV {
				if bookmarks_at_y < 5 {
					builder.draw_texture_z(
						(builder.window_width() - 8, y),
						SCROLLBAR_BOOKMARK_Z,
						BOOKMARK_UV,
						(8, 2),
					);
				}

				if y == bookmark_y {
					bookmarks_at_y += 1;
				} else {
					bookmark_y = y;
					bookmarks_at_y = 1;
				}
			} else {
				if hidden_bookmarks_at_y < 5 {
					builder.draw_texture_z(
						(builder.window_width() - 8, y),
						SCROLLBAR_BOOKMARK_Z,
						HIDDEN_BOOKMARK_UV,
						(8, 2),
					);
				}

				if y == hidden_bookmark_y {
					hidden_bookmarks_at_y += 1;
				} else {
					hidden_bookmark_y = y;
					hidden_bookmarks_at_y = 1;
				}
			}
		}
	}

	pub fn draw_held_entry_bar<F: FnOnce(usize, usize) -> bool, G: FnOnce(&NbtElement) -> bool>(&mut self, pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder, f: F, g: G) -> bool {
		let (x_offset, y_offset) = pos.into();
		if let Some((element, x, y)) = self.ghost && f(x, y) && g(element) {
			builder.draw_texture_region_z((self.left_margin - 2, y_offset - 1), BASE_Z, INSERTION_UV, (x_offset + 18 - self.left_margin, 2), (16, 2));
			true
		} else {
			false
		}
	}

	pub fn draw_held_entry_chunk<F: FnOnce(usize, usize) -> bool, G: FnOnce(&NbtElement) -> bool>(&mut self, pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder, f: F, g: G) -> bool {
		let (x_offset, y_offset) = pos.into();
		if let Some((element, x, y)) = self.ghost && f(x, y) && g(element) {
			builder.draw_texture_region_z((self.left_margin - 2, y_offset), BASE_Z, INSERTION_CHUNK_UV, (x_offset + 18 - self.left_margin, 16), (16, 16));
			true
		} else {
			false
		}
	}

	pub fn draw_held_entry_grid_chunk<F: FnOnce(usize, usize) -> bool, G: FnOnce(&NbtElement) -> bool>(&mut self, pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder, f: F, g: G) -> bool {
		let (x_offset, y_offset) = pos.into();
		if let Some((element, x, y)) = self.ghost && f(x, y) && g(element) {
			builder.draw_texture_region_z((x_offset, y_offset), BASE_Z, INSERTION_CHUNK_UV, (16, 16), (16, 16));
			true
		} else {
			false
		}
	}
}

pub struct LinkedQueue<T> {
	tail: Option<Box<SinglyLinkedNode<T>>>,
	len: usize,
}

// perf enhancement
impl<T> Drop for LinkedQueue<T> {
	fn drop(&mut self) {
		while let Some(box SinglyLinkedNode { value: _, mut prev }) = self.tail.take() {
			// take is not required, but then intellij gets upset.
			self.tail = prev.take();
		}
	}
}

impl<T: Clone> Clone for LinkedQueue<T> {
	fn clone(&self) -> Self {
		let mut new = Self::new();
		for t in self.iter().cloned().collect::<Vec<_>>() {
			new.push(t);
		}
		new
	}
}

impl<T> LinkedQueue<T> {
	#[must_use]
	pub const fn new() -> Self { Self { tail: None, len: 0 } }

	pub fn push(&mut self, value: T) {
		self.tail = Some(Box::new(SinglyLinkedNode {
			value,
			prev: self.tail.take(),
		}));
		self.len += 1;
	}

	#[must_use]
	pub fn pop(&mut self) -> Option<T> {
		if let Some(box SinglyLinkedNode { value, prev: tail }) = self.tail.take() {
			self.tail = tail;
			self.len -= 1;
			Some(value)
		} else {
			None
		}
	}

	#[must_use]
	pub fn get(&self) -> Option<&T> { self.tail.as_ref().map(|x| &x.value) }

	#[must_use]
	pub fn get_mut(&mut self) -> Option<&mut T> { self.tail.as_mut().map(|x| &mut x.value) }

	#[must_use]
	pub const fn is_empty(&self) -> bool { self.len == 0 }

	pub fn clear(&mut self) {
		while let Some(box SinglyLinkedNode { value: _, mut prev }) = self.tail.take() {
			// take is not required, but then intellij gets upset.
			self.tail = prev.take();
		}
		self.len = 0;
	}

	#[must_use]
	pub fn iter(&self) -> LinkedQueueIter<'_, T> {
		LinkedQueueIter {
			tail: &self.tail,
		}
	}
}

pub struct LinkedQueueIter<'a, T> {
	tail: &'a Option<Box<SinglyLinkedNode<T>>>,
}

impl<'a, T> Iterator for LinkedQueueIter<'a, T> {
	type Item = &'a T;

	fn next(&mut self) -> Option<Self::Item> {
		if let Some(SinglyLinkedNode { value, prev }) = self.tail.as_deref() {
			self.tail = prev;
			Some(value)
		} else {
			None
		}
	}
}

pub struct SinglyLinkedNode<T> {
	value: T,
	prev: Option<Box<SinglyLinkedNode<T>>>,
}

pub fn smoothstep64(x: f64) -> f64 {
	let x = x.clamp(0.0, 1.0);
	3.0 * x * x - 2.0 * x * x * x
}

#[must_use]
pub const fn valid_unescaped_char(byte: u8) -> bool { matches!(byte, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'-' | b'.' | b'+') }

#[derive(Clone)]
pub struct RemoveElementResult {
	indices: Box<[usize]>,
	kv: NbtElementAndKey,
	replaces: bool,
}

impl RemoveElementResult {
	pub fn into_raw(self) -> (Box<[usize]>, NbtElementAndKey, bool) {
		(self.indices, self.kv, self.replaces)
	}

	pub fn into_action(self) -> WorkbenchAction {
		if self.replaces {
			WorkbenchAction::Replace {
				indices: self.indices,
				value: self.kv,
			}
		} else {
			WorkbenchAction::Remove {
				element: self.kv,
				indices: self.indices,
			}
		}
	}
}

#[derive(Clone)]
pub struct SwapElementResult {
	parent: Box<[usize]>,
	a: usize,
	b: usize,
}

impl SwapElementResult {
	pub fn into_raw(self) -> (Box<[usize]>, usize, usize) {
		(self.parent, self.a, self.b)
	}

	pub fn into_action(self) -> WorkbenchAction {
		WorkbenchAction::Swap {
			parent: self.parent,
			a: self.a,
			b: self.b,
		}
	}
}

/// Properly adds an element under the specified indices, updating the following relevant data
/// - Subscription Indices
/// - Bookmarked Lines
/// - Heights and True Heights
/// - Workbench Actions
/// - Horizontal Scroll
///
/// # Examples
/// ```rust
/// let workbench = ...;
/// let tab = tab_mut!(workbench);
/// let action = add_element(
///     &mut tab.value,
///     None,
///     NbtElement::from_str(
///         r#"{"registry":"minecraft:item","value":"minecraft:stone"}"#
///     ).unwrap().1,
///     Box::new([0]),
///     &mut tab.bookmarks,
///     &mut self.subscription
/// )?;
/// tab.append_to_history(action);
/// ```
pub fn add_element(root: &mut NbtElement, key: Option<CompactString>, value: NbtElement, indices: Box<[usize]>, bookmarks: &mut MarkedLines, subscription: &mut Option<FileUpdateSubscription>) -> anyhow::Result<WorkbenchAction> {
	let (&last, rem) = indices.split_last().expect("You can't remove the head!");

	let (_, _, parent, mut line_number) = Navigate::new(rem.iter().copied(), root).last();
	let (old_parent_height, old_parent_true_height) = (parent.height(), parent.true_height());
	// SAFETY: we have updated all the relevant data
	let old_value = match unsafe { parent.insert(last, (key, value)) } {
		Ok(Some(old)) => Some(old),
		Ok(None) => None,
		Err(_) => return Err(anyhow!("Invalid type to insert into parent")),
	};
	let (parent_height, parent_true_height) = (parent.height(), parent.true_height());
	let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
	let (_old_height, old_true_height) = (old_value.as_ref().map(NbtElement::height), old_value.as_ref().map(NbtElement::true_height));
	let been_replaced = old_true_height.is_some();

	for n in 0..last {
		line_number += parent[n].true_height();
	}
	line_number += 1;
	bookmarks.remove(line_number..line_number + old_true_height.unwrap_or(0));
	bookmarks[line_number..].increment(diff, true_diff);

	if let Some(subscription) = subscription && encompasses_or_equal(rem, &subscription.indices) {
		if subscription.indices[rem.len()] <= last && !been_replaced {
			subscription.indices[rem.len()] += 1;
		}
	}

	recache_along_indices(rem, root);

	let action = if let Some(old_value) = old_value {
		WorkbenchAction::Replace { indices, value: (None, old_value) }
	} else {
		WorkbenchAction::Add { indices }
	};

	Ok(action)
}

/// Properly removes an element under the specified indices, updating the following relevant data
/// - Subscription Indices
/// - Bookmarked Lines
/// - Heights and True Heights
/// - Workbench Actions
/// - Horizontal Scroll
///
/// # Examples
/// ```rust
/// let workbench = ...;
/// let tab = tab_mut!(workbench);
/// let result = remove_element(
///     &mut tab.value,
///     Box::new([0]),
///     &mut tab.bookmarks,
///     &mut self.subscription
/// )?;
/// tab.append_to_history(result.into_action());
/// ```
pub fn remove_element(root: &mut NbtElement, indices: Box<[usize]>, bookmarks: &mut MarkedLines, subscription: &mut Option<FileUpdateSubscription>) -> anyhow::Result<RemoveElementResult> {
	let (&last, rem) = indices.split_last().context("Cannot remove the root")?;
	let (_, _, parent, mut line_number) = Navigate::new(rem.iter().copied(), root).last();
	for n in 0..last {
		line_number += parent[n].true_height();
	}
	line_number += 1;
	let (old_parent_height, old_parent_true_height) = (parent.height(), parent.true_height());
	// SAFETY: we have updated all the relevant data
	let (key, value) = unsafe { parent.remove(last) }.context("Could not remove element")?;
	let (height, true_height) = (value.height(), value.true_height());
	let (parent_height, parent_true_height) = (parent.height(), parent.true_height());
	let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
	let been_replaced = !(height == diff && true_height == true_diff);
	bookmarks.remove(line_number..line_number);
	bookmarks[line_number..].decrement(diff, true_diff);

	if let Some(inner_subscription) = subscription {
		if encompasses_or_equal(&indices, &inner_subscription.indices) {
			*subscription = None;
		} else if encompasses(rem, &inner_subscription.indices) {
			if inner_subscription.indices[rem.len()] >= last && !been_replaced {
				inner_subscription.indices[rem.len()] -= 1;
			}
		}
	}

	recache_along_indices(rem, root);

	Ok(RemoveElementResult {
		indices,
		kv: (key, value),
		replaces: been_replaced,
	})
}

/// Properly replaces an element under the specified indices, updating the following relevant data
/// - Subscription Indices
/// - Bookmarked Lines
/// - Heights and True Heights
/// - Workbench Actions
/// - Horizontal Scroll
///
/// # Examples
/// ```rust
/// let workbench = ...;
/// let tab = tab_mut!(workbench);
/// let result = replace_element(
///     &mut tab.value,
///     NbtElement::from_str(
///         r#"{"registry":"minecraft:item","value":"minecraft:stone"}"#
///     ).unwrap(),
///     Box::new([0]),
///     &mut tab.bookmarks,
///     &mut self.subscription
/// )?;
/// tab.append_to_history(result.into_action());
/// ```
pub fn replace_element(root: &mut NbtElement, value: NbtElementAndKey, indices: Box<[usize]>, bookmarks: &mut MarkedLines, subscription: &mut Option<FileUpdateSubscription>) -> anyhow::Result<RemoveElementResult> {
	let Some((&last, rem)) = indices.split_last() else {
		return if root.id() == value.1.id() {
			bookmarks.remove(..);

			Ok(RemoveElementResult {
				indices: Box::new([]),
				kv: (None, core::mem::replace(root, value.1)),
				replaces: true,
			})
		} else {
			Err(anyhow!("Root element type cannot be changed"))
		}
	};

	let (_, _, parent, mut line_number) = Navigate::new(rem.iter().copied(), root).last();
	for n in 0..last {
		line_number += parent[n].true_height();
	}
	line_number += 1;

	let (old_parent_height, old_parent_true_height) = (parent.height(), parent.true_height());
	// SAFETY: we have updated all the relevant data
	let (old_key, old_value) = unsafe { parent.replace_key_value(last, value) }.context("Failed to replace element")?;
	let (_old_height, old_true_height) = (old_value.height(), old_value.true_height());
	let (parent_height, parent_true_height) = (parent.height(), parent.true_height());
	let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
	bookmarks.remove(line_number..line_number + old_true_height);
	bookmarks[line_number..].increment(diff, true_diff);

	if let Some(inner_subscription) = subscription {
		if encompasses_or_equal(&indices, &inner_subscription.indices) {
			*subscription = None;
		}
	}

	recache_along_indices(rem, root);

	Ok(RemoveElementResult {
		indices,
		kv: (old_key, old_value),
		replaces: true,
	})
}

/// Properly swaps two elements under their specified indices (requires them to be at the same depth), updating the following relevant data
/// - Subscription Indices
/// - Bookmarked Lines
/// - Heights and True Heights
/// - Workbench Actions
/// - Horizontal Scroll
pub fn swap_elements(root: &mut NbtElement, parent_indices: Box<[usize]>, a: usize, b: usize, bookmarks: &mut MarkedLines, subscription: &mut Option<FileUpdateSubscription>) -> anyhow::Result<SwapElementResult> {
	let (a, b) = if a <= b { (a, b) } else { (b, a) };
	let parent_y = sum_indices(parent_indices.iter().copied(), root);
	let (_, _, parent, parent_line_number) = Navigate::new(parent_indices.iter().copied(), root).last();

	let mut a_line_number = parent_line_number;
	let mut a_y = parent_y;
	let (a_height, a_true_height) = (parent[a].height(), parent[a].true_height());
	for n in 0..a {
		let sibling = &parent[n];
		a_line_number += sibling.true_height();
		a_y += sibling.height();
	}
	a_line_number += 1;
	a_y += 1;

	let mut b_line_number = parent_line_number;
	let mut b_y = parent_y;
	let (b_height, b_true_height) = (parent[b].height(), parent[b].true_height());
	for n in 0..b {
		let sibling = &parent[n];
		b_line_number += sibling.true_height();
		b_y += sibling.height();
	}
	b_line_number += 1;
	b_y += 1;

	let mut a_bookmarks = bookmarks.remove(a_line_number..a_line_number + a_true_height);
	let mut b_bookmarks = bookmarks.remove(b_line_number..b_line_number + b_true_height);
	MarkedLineSlice::from_marked_lines_mut(&mut a_bookmarks).decrement(a_y, a_line_number);
	MarkedLineSlice::from_marked_lines_mut(&mut b_bookmarks).decrement(b_y, b_line_number);

	bookmarks[b_line_number..].decrement(b_height, b_true_height);
	bookmarks[a_line_number..].decrement(a_height, a_true_height);

	MarkedLineSlice::from_marked_lines_mut(&mut a_bookmarks).increment(b_y, b_line_number);
	MarkedLineSlice::from_marked_lines_mut(&mut b_bookmarks).increment(a_y, a_line_number);
	bookmarks.add_bookmarks(a_bookmarks);
	bookmarks.add_bookmarks(b_bookmarks);

	if let Some(subscription_inner) = subscription && encompasses(&parent_indices, &subscription_inner.indices) {
		let sibling = &mut subscription_inner.indices[parent_indices.len()];
		if *sibling == a {
			*sibling = b;
		} else if *sibling == b {
			*sibling = a;
		}
	}

	parent.swap(a, b);

	recache_along_indices(&parent_indices, root);

	Ok(SwapElementResult {
		parent: parent_indices,
		a,
		b,
	})
}

#[must_use]
pub fn combined_two_sorted<T: Ord>(a: Box<[T]>, b: Box<[T]>) -> Vec<T> {
	let mut a = unsafe { core::mem::transmute::<_, Box<[MaybeUninit<T>]>>(a) };
	let mut a_idx = 0;
	let mut b = unsafe { core::mem::transmute::<_, Box<[MaybeUninit<T>]>>(b) };
	let mut b_idx = 0;
	let mut out = Vec::with_capacity(a.len() + b.len());
	let spare = out.spare_capacity_mut();
	let mut idx = 0;

	while a_idx < a.len() && b_idx < b.len() {
		let a = &mut a[a_idx];
		let b = &mut b[b_idx];

		// SAFETY: the values are all initialized initially, once this is uninit memory, we go to the next `idx` so we never read it again
		match unsafe { a.assume_init_ref().cmp(b.assume_init_ref()) } {
			Ordering::Less => {
				spare[idx].write(unsafe { core::mem::replace(a, MaybeUninit::uninit()).assume_init() });
				a_idx += 1;
				idx += 1;
			}
			Ordering::Equal => {
				spare[idx].write(unsafe { core::mem::replace(a, MaybeUninit::uninit()).assume_init() });
				drop(unsafe { core::mem::replace(b, MaybeUninit::uninit()).assume_init() });
				a_idx += 1;
				b_idx += 1;
				idx += 1;
			}
			Ordering::Greater => {
				spare[idx].write(unsafe { core::mem::replace(b, MaybeUninit::uninit()).assume_init() });
				b_idx += 1;
				idx += 1;
			}
		}
	}

	unsafe { spare.as_mut_ptr().add(idx).copy_from_nonoverlapping(a.as_ptr().add(a_idx), a.len() - a_idx) }
	idx += a.len() - a_idx;
	unsafe { spare.as_mut_ptr().add(idx).copy_from_nonoverlapping(b.as_ptr().add(b_idx), b.len() - b_idx) }
	idx += b.len() - b_idx;

	// SAFETY: all the values used have been copied over, all the unused values have been dropped.
	drop(a);
	drop(b);

	// we have written `idx` times
	unsafe { out.set_len(idx); }
	out
}

pub trait StrExt {
	fn snbt_string_read(&self) -> Result<(CompactString, &str), usize>;

	fn needs_escape(&self) -> bool;

	fn width(&self) -> usize;
}

impl StrExt for str {
	#[inline]
	#[optimize(speed)]
	#[allow(clippy::too_many_lines)]
	fn snbt_string_read(mut self: &Self) -> Result<(CompactString, &Self), usize> {
		const MAPPING: [Option<u8>; 256] = {
			let mut initial = [Option::<u8>::None; 256];
			initial[b'0' as usize] = Some(0);
			initial[b'1' as usize] = Some(1);
			initial[b'2' as usize] = Some(2);
			initial[b'3' as usize] = Some(3);
			initial[b'4' as usize] = Some(4);
			initial[b'5' as usize] = Some(5);
			initial[b'6' as usize] = Some(6);
			initial[b'7' as usize] = Some(7);
			initial[b'8' as usize] = Some(8);
			initial[b'9' as usize] = Some(9);
			initial[b'a' as usize] = Some(10);
			initial[b'b' as usize] = Some(11);
			initial[b'c' as usize] = Some(12);
			initial[b'd' as usize] = Some(13);
			initial[b'e' as usize] = Some(14);
			initial[b'f' as usize] = Some(15);
			initial[b'A' as usize] = Some(10);
			initial[b'B' as usize] = Some(11);
			initial[b'C' as usize] = Some(12);
			initial[b'D' as usize] = Some(13);
			initial[b'E' as usize] = Some(14);
			initial[b'F' as usize] = Some(15);
			initial
		};

		if !self.starts_with('"') && !self.starts_with('\'') {
			let end_idx = self
				.char_indices()
				.find(|(_, c)| !valid_unescaped_char(*c as u8))
				.map_or(self.len(), |(idx, _)| idx);
			let (s, s2) = unsafe {
				(
					self.get_unchecked(..end_idx),
					self.get_unchecked(end_idx..self.len()),
				)
			};
			if s.needs_escape() { return Err(s2.len()) }
			Ok((s.to_compact_string(), s2))
		} else {
			let enclosing = self.as_bytes().first().copied().ok_or(self.len())?;
			self = unsafe { self.get_unchecked(1..) };
			let (end, len) = 'a: {
				let mut backslash = false;
				let mut sub = 0;
				let mut iter = self.bytes().enumerate();
				while let Some((idx, byte)) = iter.next() {
					if backslash {
						if byte == b'x' {
							if let Ok([(_, a), _]) = iter.next_chunk::<2>()
								&& let Some(a) = MAPPING[a as usize]
							{
								if a < 8 {
									sub += 3;
								} else {
									sub += 2;
								}
							} else {
								return Err(self.len() - idx);
							}
						} else if byte == b'u' {
							if let Ok([(_, _), (_, b), (_, c), _]) = iter.next_chunk::<4>()
								&& let Some(b) = MAPPING[b as usize]
								&& let Some(c) = MAPPING[c as usize]
							{
								if b < 8 {
									if c < 8 {
										sub += 5;
									} else {
										sub += 4;
									}
								} else {
									sub += 3;
								}
							} else {
								return Err(self.len() - idx);
							}
						} else {
							sub += 1;
						}
					}
					if byte == enclosing {
						if backslash {
							backslash = false;
						} else {
							break 'a (idx, idx - sub);
						}
					} else if byte == b'\\' {
						backslash = !backslash;
					} else {
						backslash = false;
					}
				}
				return Err(self.len());
			};
			let mut out = CompactString::with_capacity(len);
			let ptr = out.as_mut_ptr();
			unsafe {
				out.set_len(len);
			}
			let mut buf_len = 0;
			let mut backslash = false;
			let mut iter = self.bytes();
			while let Some(mut byte) = iter.next() {
				if byte == b'\\' {
					if backslash {
						backslash = false;
					} else {
						backslash = true;
						continue;
					}
				} else if byte == enclosing {
					if backslash {
						backslash = false;
					} else {
						break;
					}
				} else if byte == b'n' {
					if backslash {
						backslash = false;
						byte = b'\n';
					}
				} else if byte == b'r' {
					if backslash {
						backslash = false;
						byte = b'\r';
					}
				} else if byte == b'0' {
					if backslash {
						backslash = false;
						byte = b'\0';
					}
				} else if byte == b'x' {
					if backslash {
						backslash = false;
						if let Ok([a, b]) = iter.next_chunk::<2>()
							&& let Some(a) = MAPPING[a as usize]
							&& let Some(b) = MAPPING[b as usize]
						{
							let char = ((a << 4) | b) as char;
							let len = char.len_utf8();
							char.encode_utf8(unsafe { core::slice::from_raw_parts_mut(ptr.add(buf_len), len) });
							buf_len += len;
							continue;
						} else {
							return Err(self.len());
						}
					}
				} else if byte == b'u' {
					if backslash {
						backslash = false;
						if let Ok([a, b, c, d]) = iter.next_chunk::<4>()
							&& let Some(a) = MAPPING[a as usize]
							&& let Some(b) = MAPPING[b as usize]
							&& let Some(c) = MAPPING[c as usize]
							&& let Some(d) = MAPPING[d as usize]
							&& let Some(char) = char::from_u32(((a as u32) << 12) | ((b as u32) << 8) | ((c as u32) << 4) | (d as u32))
						{
							let len = char.len_utf8();
							char.encode_utf8(unsafe { core::slice::from_raw_parts_mut(ptr.add(buf_len), len) });
							buf_len += len;
							continue;
						} else {
							return Err(self.len());
						}
					}
				} else if backslash {
					return Err(self.len());
				}

				unsafe {
					*ptr.add(buf_len) = byte;
					buf_len += 1;
				}
			}

			if self.len() < end + 1 { return Err(self.len()) };
			unsafe { Ok((out, self.get_unchecked((end + 1)..))) }
		}
	}

	fn needs_escape(&self) -> bool { self.as_bytes().first().is_some_and(u8::is_ascii_digit) || !self.bytes().all(valid_unescaped_char) }

	fn width(&self) -> usize {
		self.chars().map(CharExt::width).sum()
	}
}

pub trait CharExt {
	fn width(self) -> usize;
}

impl CharExt for char {
	fn width(self) -> usize {
		if (self as u32) < 56832 {
			VertexBufferBuilder::CHAR_WIDTH[self as usize] as usize
		} else {
			VertexBufferBuilder::CHAR_WIDTH[56829] as usize
		}
	}
}

pub const fn width_ascii(s: &str) -> usize {
	let mut width = 0;
	let mut i = 0;
	while i < s.len() {
		if s.as_bytes()[i].is_ascii() {
			width += VertexBufferBuilder::CHAR_WIDTH[s.as_bytes()[i] as usize] as usize;
			i += 1;
		}
	}
	width
}

pub trait OptionExt<T> {
	#[allow(clippy::wrong_self_convention)] // then why is is_some_and like that, huh?
	fn is_none_or(self, f: impl FnOnce(T) -> bool) -> bool;
}

impl<T> OptionExt<T> for Option<T> {
	fn is_none_or(self, f: impl FnOnce(T) -> bool) -> bool { self.map_or(true, f) }
}

/// # Safety
///
/// * This code better be unreachable otherwise it's UB without `debug_assertions`, just a panic with them, however.
///
/// # Panics
///
/// * When `debug_assertions` are true, it panics with the respective `msg`
#[allow(unused_variables)] // intellij being freaky
pub unsafe fn panic_unchecked(msg: &str) -> ! {
	#[cfg(debug_assertions)]
	panic!("{msg}");

	#[cfg(not(debug_assertions))]
	core::hint::unreachable_unchecked()
}

const_assert_eq!(
	VertexBufferBuilder::CHAR_WIDTH[b':' as usize],
	VertexBufferBuilder::CHAR_WIDTH[b',' as usize]
);
