#![feature(maybe_uninit_uninit_array)]
#![feature(maybe_uninit_array_assume_init)]

use flate2::Compression;
use std::fs::write;
use std::io::Read;
use std::mem::MaybeUninit;

const UNICODE: &[u8] = include_bytes!("src/assets/build/unicode.hex");

fn main() {
	{
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
		write(r"src\assets\char_widths.hex", unsafe { MaybeUninit::array_assume_init(char_widths) }).unwrap();
	}

	{
		let mut buf = Vec::with_capacity(UNICODE.len());
		let mut encoder = flate2::read::ZlibEncoder::new(UNICODE, Compression::best());
		let _ = encoder.read_to_end(&mut buf);
		write(r"src/assets/unicode.hex.zib", &buf).unwrap();
	}

	if std::env::var("CARGO_CFG_TARGET_OS").unwrap() == "windows" {
		if let Err(e) = winres::WindowsResource::new()
			.set_icon_with_id(r"src/assets/build/icon_128.ico", "2")
			.set_icon_with_id(r"src/assets/build/icon_64.ico", "3")
			.set_icon_with_id(r"src/assets/build/icon_48.ico", "4")
			.set_icon_with_id(r"src/assets/build/icon_32.ico", "5")
			.set_icon_with_id(r"src/assets/build/icon_16.ico", "6")
			.set_icon_with_id(r"src/assets/build/icon_256.ico", "!")
			.compile()
		{
			eprintln!("Error! {e}");
			std::process::exit(1);
		}
	}
}
