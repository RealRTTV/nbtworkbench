#![allow(semicolon_in_expressions_from_macros, internal_features, incomplete_features, unsafe_op_in_unsafe_fn)]
#![deny(unused_must_use)]
#![warn(clippy::pedantic)]
#![feature(
	allocator_api,
	assert_matches,
	associated_type_defaults,
	cold_path,
	duration_millis_float,
	if_let_guard,
	inherent_associated_types,
	let_chains,
	likely_unlikely,
	maybe_uninit_array_assume_init,
	never_type,
	panic_update_hook,
	ptr_as_ref_unchecked,
	try_trait_v2,
	try_with_capacity,
	vec_push_within_capacity,
	array_chunks,
	box_patterns,
	iter_array_chunks,
	iter_next_chunk,
	stmt_expr_attributes
)]
#![windows_subsystem = "windows"]

extern crate core;

pub mod action_result;
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
pub fn window_properties() -> parking_lot::MutexGuard<'static, render::window::WindowProperties> {
	static WINDOW_PROPERTIES: parking_lot::Mutex<render::window::WindowProperties> = parking_lot::Mutex::new(render::window::WindowProperties::Fake);

	WINDOW_PROPERTIES.lock()
}

// TODO: GO OVER EACH FUNCTION IN WORKBENCH.RS AND TAB.RS AND CONVERT ALL SELF REFERENCES INTO FIELD REFERENCES ONLY: EXAMPLES INCLUDE, SELECTED TEXT. EX: `Workbench::bookmark_line(...)`

/// # Refactor
/// * render trees using [`RenderLine`](RenderLine) struct/enum
/// * rendering code is duplicated af
/// * rename `line_number` and `true_line_number` to `y` and `line_number` respectively
/// * add high-quality Safety rustdoc to **all** created unsafe fns
/// * if you want to optimize something, optimize [`NbtElement::recache`]
/// * add HStack and VStack equivelents for rendering
/// * remove all magic constants
/// * refactor rendering to use `u32` instead of `usize`
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
