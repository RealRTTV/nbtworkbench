#![allow(semicolon_in_expressions_from_macros, internal_features, incomplete_features, clippy::cast_lossless, clippy::cast_possible_truncation, clippy::cast_precision_loss, clippy::cast_sign_loss, clippy::cast_possible_wrap)]
#![warn(clippy::pedantic)]
#![deny(clippy::too_many_lines, unused_must_use)]
#![feature(
	iter_array_chunks,
	duration_millis_float,
	never_type,
	associated_type_defaults,
	box_patterns,
	allocator_api,
	likely_unlikely,
	try_with_capacity,
	iter_next_chunk,
	inherent_associated_types,
	vec_push_within_capacity,
	maybe_uninit_array_assume_init,
)]
#![windows_subsystem = "windows"]

extern crate core;

#[cfg(not(target_arch = "wasm32"))] pub mod cli;
pub mod config;
pub mod elements;
pub mod history;
pub mod render;
pub mod serialization;
pub mod tree;
pub mod util;
#[cfg(target_arch = "wasm32")] pub mod wasm;
pub mod workbench;

#[macro_export]
macro_rules! flags {
	() => { 0b000_u8 };
	(Ctrl) => { 0b001_u8 };
	(Shift) => { 0b010_u8 };
	(Ctrl + Shift) => { 0b011_u8 };
	(Alt) => { 0b100_u8 };
	(Ctrl + Alt) => { 0b101_u8 };
	(Shift + Alt) => { 0b110_u8 };
	(Ctrl + Shift + Alt) => { 0b111_u8 };
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
macro_rules! get_interaction_information {
	($self:ident) => {{
		let tab = $self.tabs.active_tab_mut();
		let consts = tab.consts();
		$crate::workbench::Workbench::get_interaction_information_raw(consts, $self.mouse.coords, &mut tab.root)
	}};
}

#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {{
		eprintln!($($arg)*);
	}};
}

#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! log {
    ($($arg:tt)*) => {{
		println!($($arg)*);
	}};
}

#[macro_export]
macro_rules! mutable_indices {
	($tab:ident) => {
		&mut $crate::tree::MutableIndices::new(&mut $tab.subscription, &mut $tab.selected_text, &mut $tab.bookmarks)
	};
}

pub static mut WORKBENCH: workbench::Workbench = unsafe { workbench::Workbench::uninit() };

pub fn mutable_window_properties() -> parking_lot::MutexGuard<'static, render::window::MutableWindowProperties> {
	static WINDOW_PROPERTIES: parking_lot::Mutex<render::window::MutableWindowProperties> = parking_lot::Mutex::new(render::window::MutableWindowProperties::Fake);

	WINDOW_PROPERTIES.lock()
}

// TODO: GO OVER EACH FUNCTION IN WORKBENCH.RS AND TAB.RS AND CONVERT ALL SELF REFERENCES INTO FIELD REFERENCES ONLY: EXAMPLES INCLUDE, SELECTED TEXT. EX: `Workbench::bookmark_line(...)`

/// # Refactor
/// * render trees using [`RenderLine`](RenderLine) struct/enum
/// * rendering code is duplicated af
/// * rename `line_number` and `true_line_number` to `y` and `line_number` respectively
/// * add high-quality Safety rustdoc to **all** created unsafe fns
/// * if you want to optimize something, optimize [`NbtElement::recache`]
/// * remove all magic constants
/// * refactor rendering to use `u32` instead of `usize`
/// * minimize usage of `anyhow`
/// # Long-Term Goals
/// * smart screen
/// * add multi-cursor
/// * [chunk](elements::chunk::NbtChunk) section rendering
/// # Minor Features
/// * use another scissor rect to make the left margin always exist while scrolling
/// * change to Vec2i-based system for rendering, and do the rem_euclid for each coord to the window dims
/// * [`last_modified`](elements::chunk::NbtChunk) field actually gets the ability to be set
/// # Major Features
/// * macros
#[cfg(not(target_arch = "wasm32"))]
pub fn main() -> ! {
	#[cfg(target_os = "windows")]
	unsafe {
		winapi::um::wincon::AttachConsole(winapi::um::wincon::ATTACH_PARENT_PROCESS)
	};

	config::read();

	match std::env::args().nth(1).as_deref() {
		Some("find") => cli::find(),
		Some("replace") => cli::replace(),
		Some("reformat") => cli::reformat(),
		Some("--version" | "-v") => {
			println!("{}", env!("CARGO_PKG_VERSION"));
			std::process::exit(0);
		}
		Some("-?" | "/?" | "--help" | "-h") => cli::help(),
		_ => pollster::block_on(render::window::run()),
	}
}
