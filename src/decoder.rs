pub struct Decoder {
    data: *const u8,
    remaining_len: usize
}

impl Decoder {
    #[inline]
    pub fn new(data: &[u8]) -> Decoder {
        Decoder { remaining_len: data.len(), data: data.as_ptr() }
    }

    #[inline]
    pub fn assert_len(&mut self, remaining_len: usize) {
        if self.remaining_len < remaining_len {
            panic!()
        } else {
            self.remaining_len -= remaining_len;
        }
    }

    #[inline]
    pub unsafe fn u8(&mut self) -> u8 {
        let val = *self.data;
        self.data = self.data.add(1);
        val
    }

    #[inline]
    pub unsafe fn u16(&mut self) -> u16 {
        ((self.u8() as u16) << 8) | (self.u8() as u16)
    }

    #[inline]
    pub unsafe fn u32(&mut self) -> u32 {
        ((self.u8() as u32) << 24) | ((self.u8() as u32) << 16) | ((self.u8() as u32) << 8) | self.u8() as u32
    }

    #[inline]
    pub unsafe fn u64(&mut self) -> u64 {
        ((self.u8() as u64) << 56) | ((self.u8() as u64) << 48) | ((self.u8() as u64) << 40) | ((self.u8() as u64) << 32) | ((self.u8() as u64) << 24) | ((self.u8() as u64) << 16) | ((self.u8() as u64) << 8) | self.u8() as u64
    }

    #[inline]
    pub unsafe fn i8(&mut self) -> i8 {
        *(&self.u8() as *const u8 as *const i8)
    }

    #[inline]
    pub unsafe fn i16(&mut self) -> i16 {
        *(&self.u16() as *const u16 as *const i16)
    }

    #[inline]
    pub unsafe fn i32(&mut self) -> i32 {
        *(&self.u32() as *const u32 as *const i32)
    }

    #[inline]
    pub unsafe fn i64(&mut self) -> i64 {
        *(&self.u64() as *const u64 as *const i64)
    }

    #[inline]
    pub unsafe fn f32(&mut self) -> f32 {
        *(&self.u32() as *const u32 as *const f32)
    }

    #[inline]
    pub unsafe fn f64(&mut self) -> f64 {
        *(&self.u64() as *const u64 as *const f64)
    }

    #[inline]
    pub unsafe fn skip(&mut self, amount: usize) {
        self.data = self.data.add(amount);
    }

    #[inline]
    pub unsafe fn string(&mut self) -> String {
        let utflen = self.u16() as usize;
        self.assert_len(utflen);

        let mut builder = String::with_capacity(utflen);

        let mut i = 0;
        while i < utflen {
            let char = self.u8();
            match char >> 4 {
                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 => builder.push(char as char),
                12 | 13 => {
                    let char2 = self.u8();
                    builder.push(char::from_u32(((char as u32 & 0x1f) << 6) | (char2 as u32 & 0x3f)).unwrap()); // dont unwrap unchecked
                    i += 1;
                },
                14 => {
                    let char2 = self.u8();
                    let char3 = self.u8();
                    builder.push(char::from_u32(((char as u32 & 0x0f) << 12) | ((char2 as u32 & 0x3f) << 6) | (char3 as u32 & 0x3f)).unwrap()); // dont unwrap unchecked
                    i += 2;
                }
                _ => {
                    panic!();
                }
            }

            i += 1;
        }

        builder
    }
}
