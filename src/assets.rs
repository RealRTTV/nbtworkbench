#[cfg(target_feature = "avx")]
use core::arch::x86_64::*;
use std::mem::ManuallyDrop;
use std::time::SystemTime;
use crate::vertex_buffer_builder::Vec2u;

pub const HEADER_SIZE: usize = 48;

pub const ATLAS: &[u8] = include_bytes!("assets/atlas.hex");
pub const ATLAS_WIDTH: usize = 128;
pub const ATLAS_HEIGHT: usize = 128;

pub const UNICODE: &[u8] = include_bytes!("assets/unicode.hex");

pub const ICON_WIDTH: usize = 64;
pub const ICON_HEIGHT: usize = 64;

const OTHERSIDE_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/otherside.hex");
const PIGSTEP_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/pigstep.hex");
const MELLOHI_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/mellohi.hex");
const FIVE_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/5.hex");
const WARD_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/ward.hex");
const ELEVEN_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/11.hex");
const RELIC_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/relic.hex");
const STAL_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/stal.hex");

pub const CONNECTION_UV: Vec2u = Vec2u::new(64, 64);
pub const UNKNOWN_NBT_UV: Vec2u = Vec2u::new(112, 32);
pub const UNKNOWN_NBT_GHOST_UV: Vec2u = Vec2u::new(112, 48);
pub const SELECTION_UV: Vec2u = Vec2u::new(0, 80);
pub const UNSELECTED_WIDGET_UV: Vec2u = Vec2u::new(80, 64);
pub const SELECTED_WIDGET_UV: Vec2u = Vec2u::new(96, 64);
pub const EDITED_UV: Vec2u = Vec2u::new(16, 64);
pub const UNEDITED_UV: Vec2u = Vec2u::new(32, 64);
pub const NBT_FILE_TYPE_UV: Vec2u = Vec2u::new(32, 80);
pub const GZIP_FILE_TYPE_UV: Vec2u = Vec2u::new(48, 80);
pub const ZLIB_FILE_TYPE_UV: Vec2u = Vec2u::new(64, 80);
pub const SNBT_FILE_TYPE_UV: Vec2u = Vec2u::new(80, 80);
pub const MCA_FILE_TYPE_UV: Vec2u = Vec2u::new(96, 80);
pub const UNSELECTED_TOGGLE_OFF_UV: Vec2u = Vec2u::new(0, 64);
pub const UNSELECTED_TOGGLE_ON_UV: Vec2u = Vec2u::new(8, 64);
pub const SELECTED_TOGGLE_OFF_UV: Vec2u = Vec2u::new(0, 72);
pub const SELECTED_TOGGLE_ON_UV: Vec2u = Vec2u::new(8, 72);
pub const UNHELD_SCROLLBAR_UV: Vec2u = Vec2u::new(48, 64);
pub const HELD_SCROLLBAR_UV: Vec2u = Vec2u::new(54, 64);
pub const REMOVE_UV: Vec2u = Vec2u::new(0, 96);
pub const ADD_UV: Vec2u = Vec2u::new(16, 96);
pub const RENAME_UV: Vec2u = Vec2u::new(32, 96);
pub const MOVE_UV: Vec2u = Vec2u::new(48, 96);
pub const LINE_NUMBER_SEPARATOR_UV: Vec2u = Vec2u::new(60, 64);
pub const END_LINE_NUMBER_SEPARATOR_UV: Vec2u = Vec2u::new(62, 64);
pub const HORIZONTAL_SEPARATOR_UV: Vec2u = Vec2u::new(16, 80);
pub const TEXT_UNDERLINE_UV: Vec2u = Vec2u::new(16, 82);

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

pub const BYTE_GHOST_UV: Vec2u = Vec2u::new(0, 16);
pub const SHORT_GHOST_UV: Vec2u = Vec2u::new(16, 16);
pub const INT_GHOST_UV: Vec2u = Vec2u::new(32, 16);
pub const LONG_GHOST_UV: Vec2u = Vec2u::new(48, 16);
pub const FLOAT_GHOST_UV: Vec2u = Vec2u::new(64, 16);
pub const DOUBLE_GHOST_UV: Vec2u = Vec2u::new(80, 16);
pub const BYTE_ARRAY_GHOST_UV: Vec2u = Vec2u::new(96, 16);
pub const STRING_GHOST_UV: Vec2u = Vec2u::new(16, 48);
pub const LIST_GHOST_UV: Vec2u = Vec2u::new(32, 48);
pub const COMPOUND_GHOST_UV: Vec2u = Vec2u::new(48, 48);
pub const INT_ARRAY_GHOST_UV: Vec2u = Vec2u::new(112, 16);
pub const LONG_ARRAY_GHOST_UV: Vec2u = Vec2u::new(0, 48);
pub const REGION_GHOST_UV: Vec2u = Vec2u::new(96, 48);
pub const CHUNK_GHOST_UV: Vec2u = Vec2u::new(64, 48);

#[allow(clippy::cast_ptr_alignment)]
pub fn icon() -> Vec<u8> {
	let original = match (SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_micros() & 7) as u8 {
		// its a good random only because its used once
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
	#[cfg(target_feature = "avx")]
	for y in 0..16 {
		for x in 0..16 {
			unsafe {
				let register = _mm_set1_epi32(original.as_ptr().cast::<i32>().add(y * 16 + x).read_unaligned());
				let offset = y * 256 + x * 4;
				_mm_storeu_epi32(scaled.as_mut_ptr().add(offset + 64 * 0), register);
				_mm_storeu_epi32(scaled.as_mut_ptr().add(offset + 64 * 1), register);
				_mm_storeu_epi32(scaled.as_mut_ptr().add(offset + 64 * 2), register);
				_mm_storeu_epi32(scaled.as_mut_ptr().add(offset + 64 * 3), register);
			}
		}
	}
	#[cfg(not(target_feature = "avx"))]
	for y in 0..16 {
		for x in 0..16 {
			unsafe {
				let value = original.as_ptr().cast::<i32>().add(y * 16 + x).read_unaligned();
				let offset = y * 256 + x * 4;
				scaled.as_mut_ptr().add(offset).cast::<(i32, i32, i32, i32)>().write((value, value, value, value));
				scaled.as_mut_ptr().add(offset + 64).copy_from_nonoverlapping(scaled.as_ptr().add(offset), 4);
				scaled.as_mut_ptr().add(offset + 128).copy_from_nonoverlapping(scaled.as_ptr().add(offset), 4);
				scaled.as_mut_ptr().add(offset + 192).copy_from_nonoverlapping(scaled.as_ptr().add(offset), 4);
			}
		}
	}
	let mut scaled = ManuallyDrop::new(scaled);
	unsafe { Vec::from_raw_parts(scaled.as_mut_ptr().cast::<u8>(), 16384, 16384) }
}
