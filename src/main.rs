#![allow(
	semicolon_in_expressions_from_macros,
	internal_features,
	incomplete_features,
)]
#![feature(
	allocator_api,
	cold_path,
	duration_millis_float,
	let_chains,
	likely_unlikely,
	maybe_uninit_array_assume_init,
	panic_update_hook,
    array_chunks,
    box_patterns,
    core_intrinsics,
    iter_array_chunks,
    iter_next_chunk,
    stmt_expr_attributes,
)]
#![windows_subsystem = "windows"]

extern crate core;

#[cfg(not(target_arch = "wasm32"))]
pub mod cli;
pub mod config;
pub mod render;
pub mod elements;
pub mod serialization;
pub mod workbench;
#[cfg(target_arch = "wasm32")]
pub mod wasm;
pub mod util;
pub mod tree;

pub use render::assets;
pub use render::widget;

use static_assertions::const_assert_eq;

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

#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! log {
    ($($arg:tt)*) => {{
		println!($($arg)*);
	}};
}

#[macro_export]
macro_rules! mutable_indices {
    ($workbench:ident, $tab:ident) => {
		&mut MutableIndices::new(&mut $workbench.subscription, &mut $tab.selected_text)
	};
}


pub static mut WORKBENCH: workbench::Workbench = unsafe { workbench::Workbench::uninit() };
pub static mut WINDOW_PROPERTIES: render::WindowProperties = render::WindowProperties::Fake;

/// # Refactor
/// * render trees using [`RenderLine`](RenderLine) struct/enum
/// * rendering code is duplicated af
/// * rename line_number and true_line_number to y and line_number respectively
/// # Long-Term Goals
/// * smart screen
/// * add multi-cursor
/// * [chunk](elements::chunk::NbtChunk) section rendering
/// # Minor Features
/// * [`last_modified`](elements::chunk::NbtChunk) field actually gets the ability to be set
/// * add set intersection operations for line number searching (+ dedicated negate button)
/// * add option for [ReplaceBox](render::widget::ReplaceBox) to use existing highlighted line numbers or hits for search box
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
	} else if let Some("replace") = first_arg.as_deref() {
		cli::replace()
	} else if let Some("reformat") = first_arg.as_deref() {
		cli::reformat()
	} else if let Some("--version" | "-v") = first_arg.as_deref() {
		println!("{}", env!("CARGO_PKG_VERSION"));
		std::process::exit(0);
	} else if let Some("-?" | "/?" | "--help" | "-h") = first_arg.as_deref() {
		cli::help();
	} else {
		pollster::block_on(render::run())
	}
}

// required so chunk coordinates function with the hardcoded spacing offset
const_assert_eq!(
	render::VertexBufferBuilder::CHAR_WIDTH[b':' as usize],
	render::VertexBufferBuilder::CHAR_WIDTH[b',' as usize]
);
