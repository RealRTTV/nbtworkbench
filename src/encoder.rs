#[inline]
pub fn write_u8(writer: &mut Vec<u8>, byte: u8) {
    writer.push(byte);
}

#[inline]
pub fn write_i8(writer: &mut Vec<u8>, byte: i8) {
    unsafe { write_u8(writer, *(&byte as *const i8 as *const u8)); }
}

pub fn write_u16(writer: &mut Vec<u8>, short: u16) {
    writer.push(((short >> 8) & 255) as u8);
    writer.push((short & 255) as u8);
}

#[inline]
pub fn write_i16(writer: &mut Vec<u8>, short: i16) {
    unsafe { write_u16(writer, *(&short as *const i16 as *const u16)); }
}

pub fn write_u32(writer: &mut Vec<u8>, int: u32) {
    writer.push(((int >> 24) & 255) as u8);
    writer.push(((int >> 16) & 255) as u8);
    writer.push(((int >> 8) & 255) as u8);
    writer.push((int & 255) as u8);
}

#[inline]
pub fn write_i32(writer: &mut Vec<u8>, int: i32) {
    unsafe { write_u32(writer, *(&int as *const i32 as *const u32)); }
}

pub fn write_u64(writer: &mut Vec<u8>, long: u64) {
    writer.push(((long >> 56) & 255) as u8);
    writer.push(((long >> 48) & 255) as u8);
    writer.push(((long >> 40) & 255) as u8);
    writer.push(((long >> 32) & 255) as u8);
    writer.push(((long >> 24) & 255) as u8);
    writer.push(((long >> 16) & 255) as u8);
    writer.push(((long >> 8) & 255) as u8);
    writer.push((long & 255) as u8);
}

#[inline]
pub fn write_i64(writer: &mut Vec<u8>, long: i64) {
    unsafe { write_u64(writer, *(&long as *const i64 as *const u64)); }
}

pub fn write_f32(writer: &mut Vec<u8>, float: f32) {
    unsafe {
        let float = *(&float as *const f32 as *const u32);
        writer.push(((float >> 24) & 255) as u8);
        writer.push(((float >> 16) & 255) as u8);
        writer.push(((float >> 8) & 255) as u8);
        writer.push((float & 255) as u8);
    }
}

pub fn write_f64(writer: &mut Vec<u8>, double: f64) {
    unsafe {
        let double = *(&double as *const f64 as *const u64);
        writer.push(((double >> 56) & 255) as u8);
        writer.push(((double >> 48) & 255) as u8);
        writer.push(((double >> 40) & 255) as u8);
        writer.push(((double >> 32) & 255) as u8);
        writer.push(((double >> 24) & 255) as u8);
        writer.push(((double >> 16) & 255) as u8);
        writer.push(((double >> 8) & 255) as u8);
        writer.push((double & 255) as u8);
    }
}

pub fn write_string(writer: &mut Vec<u8>, str: &str) {
    write_u16(writer, str.len() as u16);
    str.as_bytes().iter().for_each(|x| writer.push(*x));
}
