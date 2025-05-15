use crate::config;
use crate::elements::CompoundMap;
use compact_str::CompactString;
use std::hint::likely;
use std::marker::PhantomData;
use crate::elements::nbt_parse_result::NbtParseResult;

pub trait Decoder<'a> {
    fn new(data: &'a [u8]) -> Self where Self: Sized;
    
    fn assert_len(&self, remaining_len: usize) -> NbtParseResult<()>;

    fn sort(&self, map: &mut CompoundMap);

    unsafe fn read_ne_bytes<const N: usize>(&mut self) -> [u8; N];

    unsafe fn u8(&mut self) -> u8;

    unsafe fn u16(&mut self) -> u16;

    unsafe fn u32(&mut self) -> u32;
    
    unsafe fn u64(&mut self) -> u64;

    unsafe fn i8(&mut self) -> i8;

    unsafe fn i16(&mut self) -> i16;

    unsafe fn i32(&mut self) -> i32;

    unsafe fn i64(&mut self) -> i64;
    
    unsafe fn f32(&mut self) -> f32;

    unsafe fn f64(&mut self) -> f64;
    
    unsafe fn skip(&mut self, amount: usize);
    
    unsafe fn string(&mut self) -> NbtParseResult<CompactString>;
}

pub struct BigEndianDecoder<'a> {
    data: *const u8,
    end: *const u8,
    _marker: PhantomData<&'a ()>,
}

#[allow(improper_ctypes_definitions)]
impl<'a> BigEndianDecoder<'a> {
    unsafe fn read_bytes<const N: usize>(&mut self) -> [u8; N] {
        let array = self.data.cast::<[u8; N]>().read();
        self.data = self.data.add(N);
        array
    }
}

impl<'a> Decoder<'a> for BigEndianDecoder<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self {
            end: unsafe { data.as_ptr().add(data.len()) },
            data: data.as_ptr(),
            _marker: PhantomData,
        }
    }

    fn assert_len(&self, remaining_len: usize) -> NbtParseResult<()> {
        use crate::elements::nbt_parse_result::*;
        
        // <= end because it will read *until* that byte
        if unsafe { likely((self.data.add(remaining_len) as usize) <= self.end as usize) } {
            ok(())
        } else {
            err("Out of bounds")
        }
    }

       fn sort(&self, map: &mut CompoundMap) {
        // SAFETY: we can only call this on init of the compound
        unsafe { config::get_sort_algorithm().sort(map) }
    }

    #[allow(unused_mut)]
    unsafe fn read_ne_bytes<const N: usize>(&mut self) -> [u8; N] {
        let mut bytes = self.read_bytes::<N>();
        #[cfg(not(target_endian = "big"))]
        bytes.reverse();
        bytes
    }

    unsafe fn u8(&mut self) -> u8 { u8::from_be_bytes(self.read_bytes()) }

    unsafe fn u16(&mut self) -> u16 { u16::from_be_bytes(self.read_bytes()) }

    unsafe fn u32(&mut self) -> u32 { u32::from_be_bytes(self.read_bytes()) }

    unsafe fn u64(&mut self) -> u64 { u64::from_be_bytes(self.read_bytes()) }

    unsafe fn i8(&mut self) -> i8 { i8::from_be_bytes(self.read_bytes()) }

    unsafe fn i16(&mut self) -> i16 { i16::from_be_bytes(self.read_bytes()) }

    unsafe fn i32(&mut self) -> i32 { i32::from_be_bytes(self.read_bytes()) }

    unsafe fn i64(&mut self) -> i64 { i64::from_be_bytes(self.read_bytes()) }

    unsafe fn f32(&mut self) -> f32 { f32::from_be_bytes(self.read_bytes()) }

    unsafe fn f64(&mut self) -> f64 { f64::from_be_bytes(self.read_bytes()) }

    unsafe fn skip(&mut self, amount: usize) { self.data = self.data.add(amount); }

    unsafe fn string(&mut self) -> NbtParseResult<CompactString> {
        use crate::elements::nbt_parse_result::*;
        
        let len = self.u16() as usize;
        self.assert_len(len)?;

        let out = CompactString::from_utf8_lossy(core::slice::from_raw_parts(self.data, len));
        self.data = self.data.add(len);
        ok(out)
    }
}

pub struct LittleEndianDecoder<'a> {
    data: *const u8,
    end: *const u8,
    _marker: PhantomData<&'a ()>,
    header: bool,
}

impl<'a> LittleEndianDecoder<'a> {
    unsafe fn read_bytes<const N: usize>(&mut self) -> [u8; N] {
        let array = self.data.cast::<[u8; N]>().read();
        self.data = self.data.add(N);
        array
    }

       #[must_use]
    fn remaining_len(&self) -> usize {
        self.end as usize - self.data as usize
    }

       #[must_use]
    pub fn header(&self) -> bool {
        self.header
    }
}

#[allow(improper_ctypes_definitions)]
impl<'a> Decoder<'a> for LittleEndianDecoder<'a> {
    fn new(data: &'a [u8]) -> Self {
        use crate::elements::nbt_parse_result::*;   
           
        let mut this = Self {
            end: unsafe { data.as_ptr().add(data.len()) },
            data: data.as_ptr(),
            _marker: PhantomData,
            header: false,
        };
        unsafe {
            if is_ok(&this.assert_len(8)) && this.data.add(4).cast::<u32>().read_unaligned() as usize == this.remaining_len() - 8 {
                // what the hell is this version for
                let _version = this.u32();
                let _remaining_length = this.u32() as usize;
                this.header = true;
            }
        }
        this
    }

    fn assert_len(&self, remaining_len: usize) -> NbtParseResult<()> {
        use crate::elements::nbt_parse_result::*;
        
        // <= end because it will read *until* that byte
        if unsafe { likely((self.data.add(remaining_len) as usize) <= self.end as usize) } {
            ok(())
        } else {
            err("Out of bounds")
        }
    }

       fn sort(&self, map: &mut CompoundMap) {
        // SAFETY: we can only call this on init of the compound
        unsafe { config::get_sort_algorithm().sort(map) }
    }

    #[allow(unused_mut)]
    unsafe fn read_ne_bytes<const N: usize>(&mut self) -> [u8; N] {
        let mut bytes = self.read_bytes::<N>();
        #[cfg(not(target_endian = "little"))]
        bytes.reverse();
        bytes
    }

    unsafe fn u8(&mut self) -> u8 { u8::from_le_bytes(self.read_bytes()) }

    unsafe fn u16(&mut self) -> u16 { u16::from_le_bytes(self.read_bytes()) }

    unsafe fn u32(&mut self) -> u32 { u32::from_le_bytes(self.read_bytes()) }

    unsafe fn u64(&mut self) -> u64 { u64::from_le_bytes(self.read_bytes()) }

    unsafe fn i8(&mut self) -> i8 { i8::from_le_bytes(self.read_bytes()) }

    unsafe fn i16(&mut self) -> i16 { i16::from_le_bytes(self.read_bytes()) }

    unsafe fn i32(&mut self) -> i32 { i32::from_le_bytes(self.read_bytes()) }

    unsafe fn i64(&mut self) -> i64 { i64::from_le_bytes(self.read_bytes()) }

    unsafe fn f32(&mut self) -> f32 { f32::from_le_bytes(self.read_bytes()) }

    unsafe fn f64(&mut self) -> f64 { f64::from_le_bytes(self.read_bytes()) }

    unsafe fn skip(&mut self, amount: usize) { self.data = self.data.add(amount); }

    unsafe fn string(&mut self) -> NbtParseResult<CompactString> {
        use crate::elements::nbt_parse_result::*;
        
        let len = self.u16() as usize;
        self.assert_len(len)?;

        let out = CompactString::from_utf8_lossy(core::slice::from_raw_parts(self.data, len));
        self.data = self.data.add(len);
        ok(out)
    }
}
