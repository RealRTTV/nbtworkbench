use std::time::SystemTime;

pub const ATLAS: &[u8] = include_bytes!("assets/atlas.hex");
pub const ATLAS_WIDTH: u32 = 256;
pub const ATLAS_HEIGHT: u32 = 256;

pub const ICON_WIDTH: u32 = 128;
pub const ICON_HEIGHT: u32 = 128;

pub const HEADER_SIZE: u32 = 48;

const OTHERSIDE_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/otherside.hex");
const WAIT_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/wait.hex");
const MELLOHI_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/mellohi.hex");
const CHIRP_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/chirp.hex");
const WARD_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/ward.hex");
const ELEVEN_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/11.hex");
const MALL_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/mall.hex");
const STAL_MUSIC_DISC_ICON: &[u8] = include_bytes!("assets/stal.hex");

pub fn icon() -> Vec<u8> {
    let mut vec = Vec::with_capacity(65536);
    let original = match (SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_micros() & 7) as u8 { // its a good random only because its used once
        0 => OTHERSIDE_MUSIC_DISC_ICON,
        1 => WAIT_MUSIC_DISC_ICON,
        2 => MELLOHI_MUSIC_DISC_ICON,
        3 => CHIRP_MUSIC_DISC_ICON,
        4 => WARD_MUSIC_DISC_ICON,
        5 => ELEVEN_MUSIC_DISC_ICON,
        6 => MALL_MUSIC_DISC_ICON,
        _ => STAL_MUSIC_DISC_ICON
    };
    let mut scaled = [[0u32;128];128];
    for x in 0..16 {
        let mut row = [0u32;128];

        for i in 0..16 {
            for j in 0..8 {
                row[i * 8 + j] = ((original[(x * 16 + i) * 4] as u32) << 24) | ((original[(x * 16 + i) * 4 + 1] as u32) << 16) | ((original[(x * 16 + i) * 4 + 2] as u32) << 8) | (original[(x * 16 + i) * 4 + 3] as u32)
            }
        }

        for i in 0..8 {
            scaled[x * 8 + i] = row;
        }
    }
    for row in scaled {
        for element in row {
            vec.push((element >> 24) as u8);
            vec.push((element >> 16) as u8);
            vec.push((element >> 8) as u8);
            vec.push(element as u8);
        }
    }
    vec
}
