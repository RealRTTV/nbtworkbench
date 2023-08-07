#![feature(maybe_uninit_uninit_array)]
#![feature(maybe_uninit_array_assume_init)]

use std::fs::{read, write};
use std::mem::MaybeUninit;

const UNICODE: &[u8] = include_bytes!("src/assets/unicode.hex");

fn main() {
    let buf = read(r"C:\Users\kt\Desktop\Rust Coding\nbtworkbench\src\assets\atlas.png").unwrap();
    let raw = zune_png::PngDecoder::new(&buf).decode_raw().unwrap();
    write(r"C:\Users\kt\Desktop\Rust Coding\nbtworkbench\src\assets\atlas.hex", &raw).unwrap();

    let mut char_widths: [MaybeUninit<u8>; 56832] = MaybeUninit::uninit_array();
    for (idx, maybe) in char_widths.iter_mut().enumerate() {
        maybe.write(furthest_pixel(idx as u16) as u8);
    }
    // SAFETY: all values are written to
    let char_widths = unsafe { MaybeUninit::array_assume_init(char_widths) };
    write(r"C:\Users\kt\Desktop\Rust Coding\nbtworkbench\src\assets\char_widths.hex", &char_widths).unwrap();
}

pub fn furthest_pixel(char: u16) -> usize {
    for x_pixel in (0..16).rev() {
        for y_pixel in (0..16).rev() {
            if ((UNICODE[char as usize * 32 + y_pixel * 2 + x_pixel / 8] >> (7 - x_pixel % 8)) & 1) == 1 {
                return x_pixel + 2;
            }
        }
    }
    5 // space
}
