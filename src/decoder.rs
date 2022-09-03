use std::slice::Iter;

#[inline]
pub fn read_u8(iter: &mut Iter<u8>) -> Option<u8> {
    Some(*(iter.next()?))
}

#[inline]
pub fn read_i8(iter: &mut Iter<u8>) -> Option<i8> {
    unsafe { Some(*(&(read_u8(iter)?) as *const u8 as *const i8)) }
}

pub fn read_u16(iter: &mut Iter<u8>) -> Option<u16> {
    let first = (*(iter.next()?) as u16) << 8u16;
    let second = *(iter.next()?) as u16;
    Some(first | second)
}

#[inline]
pub fn read_i16(iter: &mut Iter<u8>) -> Option<i16> {
    unsafe { Some(*(&(read_u16(iter)?) as *const u16 as *const i16)) }
}

pub fn read_u32(iter: &mut Iter<u8>) -> Option<u32> {
    let first = (*(iter.next()?) as u32) << 24u32;
    let second = (*(iter.next()?) as u32) << 16u32;
    let third = (*(iter.next()?) as u32) << 8u32;
    let fourth = *(iter.next()?) as u32;
    Some(first | second | third | fourth)
}

#[inline]
pub fn read_i32(iter: &mut Iter<u8>) -> Option<i32> {
    unsafe { Some(*(&(read_u32(iter)?) as *const u32 as *const i32)) }
}

pub fn read_u64(iter: &mut Iter<u8>) -> Option<u64> {
    let first = (*(iter.next()?) as u64) << 56;
    let second = (*(iter.next()?) as u64) << 48;
    let third = (*(iter.next()?) as u64) << 40;
    let fourth = (*(iter.next()?) as u64) << 32;
    let fifth = (*(iter.next()?) as u64) << 24;
    let sixth = (*(iter.next()?) as u64) << 16;
    let seventh = (*(iter.next()?) as u64) << 8;
    let eighth = *(iter.next()?) as u64;
    Some(first | second | third | fourth | fifth | sixth | seventh | eighth)
}

#[inline]
pub fn read_i64(iter: &mut Iter<u8>) -> Option<i64> {
    unsafe { Some(*(&(read_u64(iter)?) as *const u64 as *const i64)) }
}

#[inline]
pub fn read_f32(iter: &mut Iter<u8>) -> Option<f32> {
    unsafe { Some(*(&(read_u32(iter)?) as *const u32 as *const f32)) }
}

#[inline]
pub fn read_f64(iter: &mut Iter<u8>) -> Option<f64> {
    unsafe { Some(*(&(read_u64(iter)?) as *const u64 as *const f64)) }
}

pub fn read_string(iter: &mut Iter<u8>) -> Option<String> {
    let utflen = read_u16(iter)?;

    let mut builder = String::with_capacity(utflen as usize);

    let mut i = 0;
    while i < utflen as usize {
        let char = read_u8(iter)?;
        match char >> 4 {
            0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 => builder.push(char as char),
            12 | 13 => {
                let char2 = read_u8(iter)?;
                builder.push(char::from_u32(((char as u32 & 0x1f) << 6) | (char2 as u32 & 0x3f))?);
                i += 1;
            },
            14 => {
                let char2 = read_u8(iter)?;
                let char3 = read_u8(iter)?;
                builder.push(char::from_u32(((char as u32 & 0x0f) << 12) | ((char2 as u32 & 0x3f) << 6) | (char3 as u32 & 0x3f))?);
                i += 2;
            }
            _ => {
                panic!();
            }
        }

        i += 1;
    }

    Some(builder)
}
