#[cfg(target_feature = "avx")]
use core::arch::x86_64::*;
use std::mem::ManuallyDrop;
use std::time::SystemTime;

pub const HEADER_SIZE: usize = 48;

pub const ATLAS: &[u8] = include_bytes!("assets/atlas.hex");
pub const ATLAS_WIDTH: usize = 128;
pub const ATLAS_HEIGHT: usize = 128;

pub const UNICODE: &[u8] = include_bytes!("assets/unicode.hex");

pub const ICON_WIDTH: usize = 64;
pub const ICON_HEIGHT: usize = 64;

pub const STRING_UV: (usize, usize) = (16, 16);
pub const LIST_UV: (usize, usize) = (32, 16);
pub const COMPOUND_UV: (usize, usize) = (48, 16);

const OTHERSIDE_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/otherside.hex");
const PIGSTEP_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/pigstep.hex");
const MELLOHI_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/mellohi.hex");
const FIVE_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/5.hex");
const WARD_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/ward.hex");
const ELEVEN_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/11.hex");
const RELIC_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/relic.hex");
const STAL_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/stal.hex");

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
