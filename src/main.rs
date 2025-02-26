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

pub use render::assets;
pub use render::widget;

use static_assertions::const_assert_eq;
use render::run;

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


pub static mut WORKBENCH: workbench::Workbench = unsafe { workbench::Workbench::uninit() };
pub static mut WINDOW_PROPERTIES: render::WindowProperties = render::WindowProperties::Fake;

/// # Refactor
/// * render trees using [`RenderLine`](RenderLine) struct/enum
/// * rendering code is duplicated af
/// # Long-Term Goals
/// * smart screen
/// * [chunk](elements::chunk::NbtChunk) section rendering
/// # Minor Features
/// * [`last_modified`](elements::chunk::NbtChunk) field actually gets the ability to be set
/// # Major Features
/// * macros
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
  nbtworkbench find <path> [(--mode|-m)=(normal|regex|snbt)] [(--search|-s)=(key|value|any)] [--exact-match|-em] <query>
  nbtworkbench reformat (--format|-f)=<format> [(--out-dir|-d)=<out-dir>] [(--out-ext|-e)=<out-ext>] <path>

Options:
  --version, -v       Displays the version of nbtworkbench you're running.
  -?, -h, --help, /?  Displays this dialog.
  --mode, -m          Changes the `find` mode to take the <query> field as either, a containing substring, a regex (match whole), or snbt. [default: normal]
  --search, -s        Searches for results matching the <query> in either, the key, the value, or both (note that substrings and regex search the same pattern in both key and value, while the regex uses it's key field to match equal strings). [default: any]
  --format, -f        Specifies the format to be reformatted to; either `nbt`, `snbt`, `dat/dat_old/gzip`, `zlib`, 'lnbt' (little endian nbt), or 'lhnbt' (little endian nbt with header).
  --out-dir, -d       Specifies the output directory. [default: ./]
  --out-ext, -e       Specifies the output file extension (if not specified, it will infer from --format)"#);
		std::process::exit(0)
	} else {
		pollster::block_on(run())
	}
}

// required so chunk coordinates function with the hardcoded spacing offset
const_assert_eq!(
	render::VertexBufferBuilder::CHAR_WIDTH[b':' as usize],
	render::VertexBufferBuilder::CHAR_WIDTH[b',' as usize]
);
