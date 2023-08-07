use std::intrinsics::likely;

pub struct Decoder {
    pub data: *const u8,
    end: *const u8,
}

impl Decoder {
    #[inline]
    pub fn new(data: &[u8]) -> Decoder {
        Decoder { end: unsafe { data.as_ptr().add(data.len()) }, data: data.as_ptr() }
    }

    #[inline(always)]
    pub fn assert_len(&mut self, remaining_len: usize) -> Option<()> {
        if unsafe { likely((self.data.add(remaining_len) as usize) < self.end as usize) } {
            Some(())
        } else {
            None
        }
    }

    #[inline(always)]
    pub unsafe fn read_bytes<const N: usize>(&mut self) -> Option<[u8; N]> {
        let array = self.data.cast::<[u8; N]>().read();
        self.data = self.data.add(N);
        Some(array)
    }

    #[inline]
    pub unsafe fn u8(&mut self) -> u8 {
        let val = self.data.read();
        self.data = self.data.add(1);
        val
    }

    #[inline]
    pub unsafe fn u16(&mut self) -> u16 {
        let val = self.data.cast::<u16>().read_unaligned().to_be();
        self.data = self.data.add(2);
        val
    }

    #[inline]
    pub unsafe fn u32(&mut self) -> u32 {
        let val = self.data.cast::<u32>().read_unaligned().to_be();
        self.data = self.data.add(4);
        val
    }

    #[inline]
    pub unsafe fn u64(&mut self) -> u64 {
        let val = self.data.cast::<u64>().read_unaligned().to_be();
        self.data = self.data.add(8);
        val
    }

    #[inline]
    pub unsafe fn i8(&mut self) -> i8 {
        core::mem::transmute(self.u8())
    }

    #[inline]
    pub unsafe fn i16(&mut self) -> i16 {
        core::mem::transmute(self.u16())
    }

    #[inline]
    pub unsafe fn i32(&mut self) -> i32 {
        core::mem::transmute(self.u32())
    }

    #[inline]
    pub unsafe fn i64(&mut self) -> i64 {
        core::mem::transmute(self.u64())
    }

    #[inline]
    pub unsafe fn f32(&mut self) -> f32 {
        core::mem::transmute(self.u32())
    }

    #[inline]
    pub unsafe fn f64(&mut self) -> f64 {
        core::mem::transmute(self.u64())
    }

    #[inline]
    pub unsafe fn skip(&mut self, amount: usize) {
        self.data = self.data.add(amount);
    }

    #[inline]
    pub unsafe fn string(&mut self) -> Option<Box<str>> {
        let len = self.u16() as usize;
        self.assert_len(len)?;

        let mut str = String::with_capacity(len);
        str.as_mut_ptr().cast::<u8>().copy_from_nonoverlapping(self.data, len);
        str.as_mut_vec().set_len(len);
        self.data = self.data.add(len);
        Some(str.into_boxed_str())
    }
}
