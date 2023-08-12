#![feature(maybe_uninit_uninit_array)]
#![feature(maybe_uninit_array_assume_init)]

use std::fs::write;
use std::mem::MaybeUninit;

const UNICODE: &[u8] = include_bytes!("src/assets/unicode.hex");
const ATLAS: &[u8] = include_bytes!(r"src/assets/atlas.png");

#[allow(unused_variables)] // intellij being freaky
fn main() {
	write(r"src\assets\atlas.hex", zune_png::PngDecoder::new(ATLAS).decode_raw().unwrap()).unwrap();

	let mut char_widths: [MaybeUninit<u8>; 56832] = MaybeUninit::uninit_array();
	for (idx, maybe) in char_widths.iter_mut().enumerate() {
		maybe.write('a: {
			for x_pixel in (0..16).rev() {
				for y_pixel in (0..16).rev() {
					if ((UNICODE[idx * 32 + y_pixel * 2 + x_pixel / 8] >> (7 - x_pixel % 8)) & 1) == 1 {
						break 'a x_pixel + 2;
					}
				}
			}
			5_usize
		} as u8);
	}
	// SAFETY: all values are written to
	let char_widths = unsafe { MaybeUninit::array_assume_init(char_widths) };
	write(r"src\assets\char_widths.hex", char_widths).unwrap();

	if cfg!(target_os = "windows") {
		if let Err(e) = winres::WindowsResource::new()
			.set_icon_with_id("src/assets/icon_16.ico", "16")
			.set_icon_with_id("src/assets/icon_32.ico", "32")
			.set_icon_with_id("src/assets/icon_48.ico", "48")
			.set_icon_with_id("src/assets/icon_64.ico", "64")
			.set_icon_with_id("src/assets/icon_128.ico", "128")
			.set_icon_with_id("src/assets/icon_256.ico", "!")
			.compile()
		{
			panic!("{e}");
		}

		println!("cargo:rustc-link-arg=/STACK:10485760")
	}
}
