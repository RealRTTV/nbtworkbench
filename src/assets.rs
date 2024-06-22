use std::cell::LazyCell;
use std::mem::ManuallyDrop;
use std::ops::Deref;

use winit::window::Theme;
use zune_png::zune_core::options::DecoderOptions;

pub use ZOffset::*;

use crate::since_epoch;
use crate::vertex_buffer_builder::Vec2u;

pub const HEADER_SIZE: usize = 48;

pub const DARK_ATLAS_ENCODED: &[u8] = include_bytes!("assets/dark_atlas.png");
pub const LIGHT_ATLAS_ENCODED: &[u8] = include_bytes!("assets/light_atlas.png");
pub const ATLAS_WIDTH: usize = 256;
pub const ATLAS_HEIGHT: usize = 256;
pub const UNICODE_LEN: usize = 1_818_624;

pub const ICON_WIDTH: usize = 64;
pub const ICON_HEIGHT: usize = 64;

const OTHERSIDE_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/discs/otherside.hex");
const PIGSTEP_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/discs/pigstep.hex");
const MELLOHI_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/discs/mellohi.hex");
const FIVE_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/discs/5.hex");
const WARD_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/discs/ward.hex");
const ELEVEN_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/discs/11.hex");
const RELIC_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/discs/relic.hex");
const STAL_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/discs/stal.hex");

pub const CONNECTION_UV: Vec2u = Vec2u::new(64, 64);
pub const UNKNOWN_NBT_UV: Vec2u = Vec2u::new(112, 32);
pub const UNKNOWN_NBT_GHOST_UV: Vec2u = Vec2u::new(112, 48);
pub const SELECTION_UV: Vec2u = Vec2u::new(0, 80);
pub const UNSELECTED_WIDGET_UV: Vec2u = Vec2u::new(80, 64);
pub const SELECTED_WIDGET_UV: Vec2u = Vec2u::new(96, 64);
pub const HOVERED_WIDGET_UV: Vec2u = Vec2u::new(112, 64);
pub const CLOSED_WIDGET_UV: Vec2u = Vec2u::new(80, 144);
pub const UNSELECTED_ACTION_WHEEL: [Vec2u; 8] = [
	Vec2u::new(168, 19),
	Vec2u::new(128, 40),
	Vec2u::new(168, 38),
	Vec2u::new(128, 60),
	Vec2u::new(168, 57),
	Vec2u::new(128, 0),
	Vec2u::new(168, 0),
	Vec2u::new(128, 20),
];
pub const SELECTED_ACTION_WHEEL: [Vec2u; 8] = [
	Vec2u::new(187, 19),
	Vec2u::new(148, 40),
	Vec2u::new(187, 38),
	Vec2u::new(148, 60),
	Vec2u::new(187, 57),
	Vec2u::new(148, 0),
	Vec2u::new(187, 0),
	Vec2u::new(148, 20),
];
pub const TRAY_UV: Vec2u = Vec2u::new(128, 80);
pub const SAVE_UV: Vec2u = Vec2u::new(16, 64);
pub const SAVE_GRAYSCALE_UV: Vec2u = Vec2u::new(32, 64);
pub const NBT_FILE_TYPE_UV: Vec2u = Vec2u::new(32, 80);
pub const GZIP_FILE_TYPE_UV: Vec2u = Vec2u::new(48, 80);
pub const ZLIB_FILE_TYPE_UV: Vec2u = Vec2u::new(64, 80);
pub const SNBT_FILE_TYPE_UV: Vec2u = Vec2u::new(80, 80);
pub const MCA_FILE_TYPE_UV: Vec2u = Vec2u::new(96, 80);
pub const LITTLE_ENDIAN_NBT_FILE_TYPE_UV: Vec2u = Vec2u::new(152, 160);
pub const LITTLE_ENDIAN_HEADER_NBT_FILE_TYPE_UV: Vec2u = Vec2u::new(168, 160);
pub const OPEN_FOLDER_UV: Vec2u = Vec2u::new(112, 80);
pub const UNSELECTED_TOGGLE_ON_UV: Vec2u = Vec2u::new(0, 64);
pub const UNSELECTED_TOGGLE_OFF_UV: Vec2u = Vec2u::new(8, 64);
pub const SELECTED_TOGGLE_ON_UV: Vec2u = Vec2u::new(0, 72);
pub const SELECTED_TOGGLE_OFF_UV: Vec2u = Vec2u::new(8, 72);
pub const UNHELD_SCROLLBAR_UV: Vec2u = Vec2u::new(48, 64);
pub const HELD_SCROLLBAR_UV: Vec2u = Vec2u::new(54, 64);
pub const LINE_NUMBER_SEPARATOR_UV: Vec2u = Vec2u::new(60, 64);
pub const END_LINE_NUMBER_SEPARATOR_UV: Vec2u = Vec2u::new(62, 64);
pub const HORIZONTAL_SEPARATOR_UV: Vec2u = Vec2u::new(17, 80);
pub const TEXT_UNDERLINE_UV: Vec2u = Vec2u::new(16, 82);
pub const INSERTION_UV: Vec2u = Vec2u::new(16, 84);
pub const TOOLTIP_UV: Vec2u = Vec2u::new(96, 144);
pub const BOOKMARK_UV: Vec2u = Vec2u::new(112, 96);
pub const HIDDEN_BOOKMARK_UV: Vec2u = Vec2u::new(96, 128);
pub const LIGHT_STRIPE_UV: Vec2u = Vec2u::new(96, 96);
pub const DARK_STRIPE_UV: Vec2u = Vec2u::new(96, 112);
pub const HOVERED_STRIPE_UV: Vec2u = Vec2u::new(112, 128);
pub const INVALID_STRIPE_UV: Vec2u = Vec2u::new(112, 112);
pub const COPY_RAW_UV: Vec2u = Vec2u::new(3, 131);
pub const COPY_FORMATTED_UV: Vec2u = Vec2u::new(19, 131);
#[cfg(not(target_arch = "wasm32"))]
pub const OPEN_ARRAY_IN_HEX_UV: Vec2u = Vec2u::new(35, 131);
#[cfg(not(target_arch = "wasm32"))]
pub const OPEN_IN_TXT: Vec2u = Vec2u::new(51, 131);
pub const SORT_COMPOUND_BY_NAME: Vec2u = Vec2u::new(67, 131);
pub const SORT_COMPOUND_BY_TYPE: Vec2u = Vec2u::new(83, 131);
pub const SORT_COMPOUND_BY_NOTHING: Vec2u = Vec2u::new(3, 163);
pub const FREEHAND_MODE_UV: Vec2u = Vec2u::new(0, 144);
pub const ENABLED_FREEHAND_MODE_UV: Vec2u = Vec2u::new(16, 144);
pub const STEAL_ANIMATION_OVERLAY_UV: Vec2u = Vec2u::new(64, 144);

pub const BYTE_UV: Vec2u = Vec2u::new(0, 0);
pub const SHORT_UV: Vec2u = Vec2u::new(16, 0);
pub const INT_UV: Vec2u = Vec2u::new(32, 0);
pub const LONG_UV: Vec2u = Vec2u::new(48, 0);
pub const FLOAT_UV: Vec2u = Vec2u::new(64, 0);
pub const DOUBLE_UV: Vec2u = Vec2u::new(80, 0);
pub const BYTE_ARRAY_UV: Vec2u = Vec2u::new(96, 0);
pub const STRING_UV: Vec2u = Vec2u::new(16, 32);
pub const LIST_UV: Vec2u = Vec2u::new(32, 32);
pub const COMPOUND_ROOT_UV: Vec2u = Vec2u::new(80, 32);
pub const COMPOUND_UV: Vec2u = Vec2u::new(48, 32);
pub const INT_ARRAY_UV: Vec2u = Vec2u::new(112, 0);
pub const LONG_ARRAY_UV: Vec2u = Vec2u::new(0, 32);
pub const REGION_UV: Vec2u = Vec2u::new(96, 32);
pub const CHUNK_UV: Vec2u = Vec2u::new(64, 32);

pub const BYTE_GRAYSCALE_UV: Vec2u = Vec2u::new(0, 16);
pub const SHORT_GRAYSCALE_UV: Vec2u = Vec2u::new(16, 16);
pub const INT_GRAYSCALE_UV: Vec2u = Vec2u::new(32, 16);
pub const LONG_GRAYSCALE_UV: Vec2u = Vec2u::new(48, 16);
pub const FLOAT_GRAYSCALE_UV: Vec2u = Vec2u::new(64, 16);
pub const DOUBLE_GRAYSCALE_UV: Vec2u = Vec2u::new(80, 16);
pub const BYTE_ARRAY_GHOST_UV: Vec2u = Vec2u::new(96, 16);
pub const STRING_GHOST_UV: Vec2u = Vec2u::new(16, 48);
pub const LIST_GHOST_UV: Vec2u = Vec2u::new(32, 48);
pub const COMPOUND_GHOST_UV: Vec2u = Vec2u::new(48, 48);
pub const INT_ARRAY_GHOST_UV: Vec2u = Vec2u::new(112, 16);
pub const LONG_ARRAY_GHOST_UV: Vec2u = Vec2u::new(0, 48);
pub const CHUNK_GHOST_UV: Vec2u = Vec2u::new(64, 48);
pub const ALERT_UV: Vec2u = Vec2u::new(112, 144);
pub const NOTIFICATION_UV: Vec2u = Vec2u::new(112, 184);
pub const BACKDROP_UV: Vec2u = Vec2u::new(16, 160);
pub const ADD_SEARCH_BOOKMARKS_UV: Vec2u = Vec2u::new(48, 160);
pub const REMOVE_SEARCH_BOOKMARKS_UV: Vec2u = Vec2u::new(64, 160);
pub const SEARCH_KEYS_UV: Vec2u = Vec2u::new(80, 160);
pub const SEARCH_VALUES_UV: Vec2u = Vec2u::new(80, 176);
pub const SEARCH_KEYS_AND_VALUES_UV: Vec2u = Vec2u::new(80, 192);
pub const STRING_SEARCH_MODE_UV: Vec2u = Vec2u::new(96, 160);
pub const REGEX_SEARCH_MODE_UV: Vec2u = Vec2u::new(96, 176);
pub const SNBT_SEARCH_MODE_UV: Vec2u = Vec2u::new(96, 192);
pub const NEW_FILE_UV: Vec2u = Vec2u::new(96, 48);
pub const REFRESH_UV: Vec2u = Vec2u::new(152, 144);
pub const DISABLED_REFRESH_UV: Vec2u = Vec2u::new(168, 144);
pub const LIGHTBULB_UV: Vec2u = Vec2u::new(32, 160);
pub const DIM_LIGHTBULB_UV: Vec2u = Vec2u::new(32, 176);

#[repr(u8)]
#[allow(non_camel_case_types)]
#[derive(Copy, Clone)]
pub enum ZOffset {
	SEARCH_BOX_Z = 50,
	SEARCH_BOX_SELECTION_Z = 51,
	JUST_UNDERLAPPING_BASE_Z = 79,
    BASE_Z = 80,
    JUST_OVERLAPPING_BASE_Z = 81,
    BASE_TEXT_Z = 90,
    JUST_OVERLAPPING_BASE_TEXT_Z = 91,
    TOGGLE_Z = 100,
    LINE_NUMBER_Z = 130,
    LINE_NUMBER_CONNECTOR_Z = 131,
    BOOKMARK_Z = 140,
    SELECTED_TEXT_Z = 170,
	SELECTED_TEXT_SELECTION_Z = 171,
    ACTION_WHEEL_Z = 190,
    SCROLLBAR_BOOKMARK_Z = 199,
    SCROLLBAR_Z = 200,
    HELD_ENTRY_Z = 210,
    NOTIFICATION_Z = 240,
    NOTIFICATION_TEXT_Z = 241,
    TOOLTIP_Z = 255,
}

static mut DARK_ATLAS_CELL: LazyCell<Vec<u8>> = LazyCell::new(|| zune_png::PngDecoder::new_with_options(DARK_ATLAS_ENCODED, DecoderOptions::new_fast().png_set_confirm_crc(false)).decode_raw().unwrap());
static mut LIGHT_ATLAS_CELL: LazyCell<Vec<u8>> = LazyCell::new(|| zune_png::PngDecoder::new_with_options(LIGHT_ATLAS_ENCODED, DecoderOptions::new_fast().png_set_confirm_crc(false)).decode_raw().unwrap());

pub fn atlas(theme: Theme) -> &'static [u8] {
	match theme {
		Theme::Light => unsafe { LIGHT_ATLAS_CELL.deref().as_slice() }
		Theme::Dark => unsafe { DARK_ATLAS_CELL.deref().as_slice() }
	}
}

#[allow(clippy::cast_ptr_alignment)]
pub fn icon() -> Vec<u8> {
	let original = match (since_epoch().as_millis() & 7) as u8 {
		// it's a good random only because its used once
		0 => OTHERSIDE_MUSIC_DISC_ICON,
		1 => PIGSTEP_MUSIC_DISC_ICON,
		2 => MELLOHI_MUSIC_DISC_ICON,
		3 => FIVE_MUSIC_DISC_ICON,
		4 => WARD_MUSIC_DISC_ICON,
		5 => ELEVEN_MUSIC_DISC_ICON,
		6 => RELIC_MUSIC_DISC_ICON,
		7 => STAL_MUSIC_DISC_ICON,
		_ => unsafe { core::hint::unreachable_unchecked() },
	};
	let mut scaled = Box::<[i32]>::new_uninit_slice(4096);
	for y in 0..16 {
		for x in 0..16 {
			unsafe {
				let value = original
					.as_ptr()
					.cast::<i32>()
					.add(y * 16 + x)
					.read_unaligned();
				let offset = y * 256 + x * 4;
				scaled
					.as_mut_ptr()
					.add(offset)
					.cast::<(i32, i32, i32, i32)>()
					.write((value, value, value, value));
			}
		}
		unsafe {
			let ptr = scaled.as_ptr().cast::<[i32; 64]>().add(y * 4);
			scaled
				.as_mut_ptr()
				.cast::<[i32; 64]>()
				.add(y * 4 + 1)
				.cast::<u8>()
				.copy_from_nonoverlapping(ptr.cast(), 256);
			scaled
				.as_mut_ptr()
				.cast::<[i32; 64]>()
				.add(y * 4 + 2)
				.cast::<u8>()
				.copy_from_nonoverlapping(ptr.cast(), 512);
		}
	}
	let mut scaled = ManuallyDrop::new(core::hint::black_box(scaled));
	unsafe { Vec::from_raw_parts(scaled.as_mut_ptr().cast::<u8>(), 16384, 16384) }
}
