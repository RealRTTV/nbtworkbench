use std::fmt::{Display, Formatter};
use std::io::BufWriter;
use std::string::String;
#[cfg(debug_assertions)]
use std::time::Instant;

use crate::{array, DeleteFn, DropFn, elements, primitive, RenderContext, StealFn, ToggleFn, TrySelectTextFn, UnescapeStart, VertexBufferBuilder};
use crate::decoder::Decoder;
use crate::elements::compound::NbtCompound;
use crate::elements::list::NbtList;
use crate::elements::string::NbtString;
use std::{fmt, fmt::Write};
use std::hint::unreachable_unchecked;
use std::intrinsics::likely;

primitive!((0 0), { Some('b') }, NbtByte, i8, 1);
primitive!((16 0), { Some('s') }, NbtShort, i16, 2);
primitive!((32 0), { None::<char> }, NbtInt, i32, 3);
primitive!((48 0), { Some('L') }, NbtLong, i64, 4);
primitive!((64 0), { Some('f') }, NbtFloat, f32, 5);
primitive!((80 0), { Some('d') }, NbtDouble, f64, 6);
array!(Byte, NbtByte, NbtByteArray, i8, 7, 1, 'B', (96 0), (0 0));
array!(Int, NbtInt, NbtIntArray, i32, 11, 3, 'I', (112 0), (32 0));
array!(Long, NbtLong, NbtLongArray, i64, 12, 4, 'L', (0 16), (48 0));

#[derive(Clone)]
pub enum NbtElement {
    Null,
    Byte(NbtByte),
    Short(NbtShort),
    Int(NbtInt),
    Long(NbtLong),
    Float(NbtFloat),
    Double(NbtDouble),
    ByteArray(NbtByteArray),
    String(NbtString),
    List(NbtList),
    Compound(NbtCompound),
    IntArray(NbtIntArray),
    LongArray(NbtLongArray),
}

impl NbtElement {
    pub fn from_str(mut s: &str) -> Option<Result<(Box<str>, Self), Self>> {
        if s.is_empty() { return None }

        let prefix = match s.snbt_string_read() {
            Some((prefix, s2)) => {
                match s2.strip_prefix(':') {
                    Some(s2) => {
                        s = s2;
                        Some(prefix.into_boxed_str())
                    }
                    None => None
                }
            }
            None => None
        };
        let element = Self::from_str0(s).map(|(_, x)| x)?;
        Some(match prefix {
            Some(key) => Ok((key, element)),
            None => Err(element)
        })
    }

    pub(in crate::elements) fn from_str0(s: &str) -> Option<(&str, Self)> {
        if s.starts_with("[B;") { return NbtByteArray::from_str0(s).map(|(s, x)| (s, Self::ByteArray(x))) }
        if s.starts_with("[I;") { return NbtIntArray::from_str0(s).map(|(s, x)| (s, Self::IntArray(x))) }
        if s.starts_with("[L;") { return NbtLongArray::from_str0(s).map(|(s, x)| (s, Self::LongArray(x))) }
        if s.starts_with('[') { return NbtList::from_str0(s).map(|(s, x)| (s, Self::List(x))) }
        if s.starts_with('{') { return NbtCompound::from_str0(s).map(|(s, x)| (s, Self::Compound(x))) }
        if s.starts_with('"') { return NbtString::from_str0(s).map(|(s, x)| (s, Self::String(x))) }

        let digit_end_idx = {
            let mut s = s;
            let mut digit_end_idx = 0;

            if s.starts_with("+") || s.starts_with("-") {
                s = &s[1..];
                digit_end_idx += 1;
            }

            let int_part = s.bytes().take_while(u8::is_ascii_digit).count();
            s = &s[int_part..];
            digit_end_idx += int_part;
            if int_part == 0 && !s.starts_with('.') {
                return None
            }
            if s.starts_with('.') {
                digit_end_idx += 1;
                s = &s[1..];
                let frac_part = s.bytes().take_while(u8::is_ascii_digit).count();
                digit_end_idx += frac_part;
                // s = &s[frac_part..];
            }

            digit_end_idx
        };
        if digit_end_idx > 0 {
            let suffix = (&s[digit_end_idx..]).as_bytes().get(0).copied();
            return match suffix {
                Some(b'b') => Some((&s[(digit_end_idx + 1)..], Self::Byte(NbtByte { value: (&s[..digit_end_idx]).parse().ok()? }))),
                Some(b's') => Some((&s[(digit_end_idx + 1)..], Self::Short(NbtShort { value: (&s[..digit_end_idx]).parse().ok()? }))),
                Some(b'L') => Some((&s[(digit_end_idx + 1)..], Self::Long(NbtLong { value: (&s[..digit_end_idx]).parse().ok()? }))),
                Some(b'f') => Some((&s[(digit_end_idx + 1)..], Self::Float(NbtFloat { value: (&s[..digit_end_idx]).parse().ok()? }))),
                Some(b'd') => Some((&s[(digit_end_idx + 1)..], Self::Double(NbtDouble { value: (&s[..digit_end_idx]).parse().ok()? }))),
                _ => Some((&s[digit_end_idx..], Self::Int(NbtInt { value: (&s[..digit_end_idx]).parse().ok()? }))),
            }
        }

        // please not nbt null, that's just, no.
        None
    }

    pub fn from_bytes(element: u8, decoder: &mut Decoder) -> Option<Self> {
        Some(match element {
            0 => NbtElement::Null,
            1 => NbtElement::Byte(NbtByte::from_bytes(decoder)?),
            2 => NbtElement::Short(NbtShort::from_bytes(decoder)?),
            3 => NbtElement::Int(NbtInt::from_bytes(decoder)?),
            4 => NbtElement::Long(NbtLong::from_bytes(decoder)?),
            5 => NbtElement::Float(NbtFloat::from_bytes(decoder)?),
            6 => NbtElement::Double(NbtDouble::from_bytes(decoder)?),
            7 => NbtElement::ByteArray(NbtByteArray::from_bytes(decoder)?),
            8 => NbtElement::String(NbtString::from_bytes(decoder)?),
            9 => NbtElement::List(NbtList::from_bytes(decoder)?),
            10 => NbtElement::Compound(NbtCompound::from_bytes(decoder)?),
            11 => NbtElement::IntArray(NbtIntArray::from_bytes(decoder)?),
            12 => NbtElement::LongArray(NbtLongArray::from_bytes(decoder)?),
            _ => return None
        })
    }

    pub fn to_bytes<W: std::io::Write>(&self, writer: &mut W) {
        let _ = match self {
            NbtElement::Null => { let _ = writer.write(&[0x00]); },
            NbtElement::Byte(byte) => byte.to_bytes(writer),
            NbtElement::Short(short) => short.to_bytes(writer),
            NbtElement::Int(int) => int.to_bytes(writer),
            NbtElement::Long(long) => long.to_bytes(writer),
            NbtElement::Float(float) => float.to_bytes(writer),
            NbtElement::Double(double) => double.to_bytes(writer),
            NbtElement::ByteArray(bytes) => bytes.to_bytes(writer),
            NbtElement::String(string) => string.to_bytes(writer),
            NbtElement::List(list) => list.to_bytes(writer),
            NbtElement::Compound(compound) => compound.to_bytes(writer),
            NbtElement::IntArray(ints) => ints.to_bytes(writer),
            NbtElement::LongArray(longs) => longs.to_bytes(writer),
        };
    }

    #[inline]
    pub fn id(&self) -> u8 {
        match self {
            NbtElement::Null => 0,
            NbtElement::Byte(_) => NbtByte::ID,
            NbtElement::Short(_) => NbtShort::ID,
            NbtElement::Int(_) => NbtInt::ID,
            NbtElement::Long(_) => NbtLong::ID,
            NbtElement::Float(_) => NbtFloat::ID,
            NbtElement::Double(_) => NbtDouble::ID,
            NbtElement::ByteArray(_) => NbtByteArray::ID,
            NbtElement::String(_) => NbtString::ID,
            NbtElement::List(_) => NbtList::ID,
            NbtElement::Compound(_) => NbtCompound::ID,
            NbtElement::IntArray(_) => NbtIntArray::ID,
            NbtElement::LongArray(_) => NbtLongArray::ID,
        }
    }

    #[inline]
    pub fn from_id(id: u8) -> NbtElement {
        match id {
            0 => NbtElement::Null,
            1 => NbtElement::Byte(Default::default()),
            2 => NbtElement::Short(Default::default()),
            3 => NbtElement::Int(Default::default()),
            4 => NbtElement::Long(Default::default()),
            5 => NbtElement::Float(Default::default()),
            6 => NbtElement::Double(Default::default()),
            7 => NbtElement::ByteArray(NbtByteArray::new()),
            8 => NbtElement::String(NbtString::new(String::new().into_boxed_str())),
            9 => NbtElement::List(NbtList::new(vec![], 0x00)),
            10 => NbtElement::Compound(NbtCompound::new()),
            11 => NbtElement::IntArray(NbtIntArray::new()),
            _ => NbtElement::LongArray(NbtLongArray::new())
        }
    }

    #[inline]
    pub fn data_addr(&self) -> *const () {
        match self {
            NbtElement::Null => &() as *const _,
            NbtElement::Byte(byte) => byte as *const _ as *const _,
            NbtElement::Short(short) => short as *const _ as *const _,
            NbtElement::Int(int) => int as *const _ as *const _,
            NbtElement::Long(long) => long as *const _ as *const _,
            NbtElement::Float(float) => float as *const _ as *const _,
            NbtElement::Double(double) => double as *const _ as *const _,
            NbtElement::ByteArray(byte_array) => byte_array as *const _ as *const _,
            NbtElement::String(string) => string as *const _ as *const _,
            NbtElement::List(list) => list as *const _ as *const _,
            NbtElement::Compound(compound) => compound as *const _ as *const _,
            NbtElement::IntArray(int_array) => int_array as *const _ as *const _,
            NbtElement::LongArray(long_array) => long_array as *const _ as *const _,
        }
    }

    #[inline]
    pub fn data_addr_mut(&mut self) -> *mut () {
        match self {
            NbtElement::Null => &mut () as *mut _,
            NbtElement::Byte(byte) => byte as *mut _ as *mut _,
            NbtElement::Short(short) => short as *mut _ as *mut _,
            NbtElement::Int(int) => int as *mut _ as *mut _,
            NbtElement::Long(long) => long as *mut _ as *mut _,
            NbtElement::Float(float) => float as *mut _ as *mut _,
            NbtElement::Double(double) => double as *mut _ as *mut _,
            NbtElement::ByteArray(byte_array) => byte_array as *mut _ as *mut _,
            NbtElement::String(string) => string as *mut _ as *mut _,
            NbtElement::List(list) => list as *mut _ as *mut _,
            NbtElement::Compound(compound) => compound as *mut _ as *mut _,
            NbtElement::IntArray(int_array) => int_array as *mut _ as *mut _,
            NbtElement::LongArray(long_array) => long_array as *mut _ as *mut _,
        }
    }

    #[inline]
    pub fn from_file(bytes: &[u8]) -> Option<Self> {
        #[cfg(debug_assertions)]
            let start = Instant::now();
        let mut decoder = Decoder::new(bytes);
        decoder.assert_len(3)?;
        unsafe {
            if decoder.u8() != 0x0A { return None }
            let skip = decoder.u16() as usize;
            decoder.skip(skip)
        }
        let nbt = NbtElement::Compound(NbtCompound::from_bytes(&mut decoder)?);
        #[cfg(debug_assertions)]
        println!("{}ms for file read", Instant::now().duration_since(start).as_nanos() as f64 / 1_000_000.0);
        Some(nbt)
    }

    #[inline]
    pub fn to_file(&self) -> Option<Vec<u8>> {
        use std::io::Write;

        #[cfg(debug_assertions)]
            let start = Instant::now();
        let mut writer = BufWriter::with_capacity(65536, vec![]);
        let _ = writer.write(&[0x0A, 0x00, 0x00]);
        self.to_bytes(&mut writer);
        #[cfg(debug_assertions)]
        println!("{}ms for file write", Instant::now().duration_since(start).as_nanos() as f64 / 1_000_000.0);
        writer.into_inner().ok()
    }

    #[inline]
    pub fn render(&self, x: &mut usize, y: &mut usize, remaining_scroll: &mut usize, builder: &mut VertexBufferBuilder, str: Option<&str>, tail: bool, line_number: &mut usize, ctx: &RenderContext) {
        match self {
            NbtElement::Null => *y += 16,
            NbtElement::Byte(byte) => byte.render(builder, x, y, str, line_number, ctx),
            NbtElement::Short(short) => short.render(builder, x, y, str, line_number, ctx),
            NbtElement::Int(int) => int.render(builder, x, y, str, line_number, ctx),
            NbtElement::Long(long) => long.render(builder, x, y, str, line_number, ctx),
            NbtElement::Float(float) => float.render(builder, x, y, str, line_number, ctx),
            NbtElement::Double(double) => double.render(builder, x, y, str, line_number, ctx),
            NbtElement::ByteArray(byte_array) => byte_array.render(builder, x, y, str, remaining_scroll, tail, line_number, ctx),
            NbtElement::String(string) => string.render(builder, x, y, str, line_number, ctx),
            NbtElement::List(list) => list.render(builder, x, y, str, remaining_scroll, tail, line_number, ctx),
            NbtElement::Compound(compound) => compound.render(builder, x, y, str, remaining_scroll, tail, line_number, ctx),
            NbtElement::IntArray(int_array) => int_array.render(builder, x, y, str, remaining_scroll, tail, line_number, ctx),
            NbtElement::LongArray(long_array) => long_array.render(builder, x, y, str, remaining_scroll, tail, line_number, ctx),
        }
    }

    #[inline]
    pub fn can_accept(&self, id: u8) -> bool {
        match self {
            NbtElement::Null => false,
            NbtElement::Byte(_) => false,
            NbtElement::Short(_) => false,
            NbtElement::Int(_) => false,
            NbtElement::Long(_) => false,
            NbtElement::Float(_) => false,
            NbtElement::Double(_) => false,
            NbtElement::ByteArray(_) => id == 1,
            NbtElement::String(_) => false,
            NbtElement::List(list) => id == list.id(),
            NbtElement::Compound(_) => true,
            NbtElement::IntArray(_) => id == 3,
            NbtElement::LongArray(_) => id == 4,
        }
    }

    #[inline]
    pub fn len(&self) -> Option<usize> {
        Some(match self {
            NbtElement::ByteArray(bytes) => bytes.len(),
            NbtElement::List(list) => list.len(),
            NbtElement::Compound(compound) => compound.len(),
            NbtElement::IntArray(ints) => ints.len(),
            NbtElement::LongArray(longs) => longs.len(),
            _ => return None
        })
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len().map(|x| x > 0).unwrap_or(false)
    }

    #[inline]
    pub fn render_icon(id: u8, x: usize, y: usize, builder: &mut VertexBufferBuilder) {
        match id {
            0 => {},
            1 => NbtByte::render_icon(x, y, builder),
            2 => NbtShort::render_icon(x, y, builder),
            3 => NbtInt::render_icon(x, y, builder),
            4 => NbtLong::render_icon(x, y, builder),
            5 => NbtFloat::render_icon(x, y, builder),
            6 => NbtDouble::render_icon(x, y, builder),
            7 => NbtByteArray::render_icon(x, y, builder),
            8 => elements::string::render_icon(x, y, builder),
            9 => elements::list::render_icon(x, y, builder),
            10 => elements::compound::render_icon(x, y, builder),
            11 => NbtIntArray::render_icon(x, y, builder),
            _ => NbtLongArray::render_icon(x, y, builder)
        }
    }

    #[inline]
    pub fn height(&self) -> usize {
        match self {
            NbtElement::ByteArray(bytes) => bytes.height(),
            NbtElement::List(list) => list.height(),
            NbtElement::Compound(compound) => compound.height(),
            NbtElement::IntArray(ints) => ints.height(),
            NbtElement::LongArray(longs) => longs.height(),
            _ => 1
        }
    }

    #[inline]
    pub fn set_value(&mut self, value: Box<str>) -> Option<Box<str>> {
        Some(match self {
            NbtElement::Byte(byte) => {
                let before = byte.to_string().into();
                byte.set(value.parse().ok());
                before
            },
            NbtElement::Short(short) => {
                let before = short.to_string().into();
                short.set(value.parse().ok());
                before
            },
            NbtElement::Int(int) => {
                let before = int.to_string().into();
                int.set(value.parse().ok());
                before
            },
            NbtElement::Long(long) => {
                let before = long.to_string().into();
                long.set(value.parse().ok());
                before
            },
            NbtElement::Float(float) => {
                let before = float.to_string().into();
                float.set(value.parse().ok());
                before
            },
            NbtElement::Double(double) => {
                let before = double.to_string().into();
                double.set(value.parse().ok());
                before
            },
            NbtElement::String(string) => {
                unsafe { core::mem::transmute::<_, Box<str>>(core::mem::replace(string, NbtString::new(value))) }
            },
            _ => return None
        })
    }

    pub fn true_height(&self) -> usize {
        match self {
            NbtElement::ByteArray(bytes) => bytes.true_height(),
            NbtElement::List(list) => list.true_height(),
            NbtElement::Compound(compound) => compound.true_height(),
            NbtElement::IntArray(ints) => ints.true_height(),
            NbtElement::LongArray(longs) => longs.true_height(),
            _ => 1
        }
    }

    #[inline]
    #[must_use]
    pub fn toggle(&mut self) -> Option<()> {
        match self {
            NbtElement::ByteArray(bytes) => bytes.toggle(),
            NbtElement::List(list) => list.toggle(),
            NbtElement::Compound(compound) => compound.toggle(),
            NbtElement::IntArray(ints) => ints.toggle(),
            NbtElement::LongArray(longs) => longs.toggle(),
            _ => None
        }
    }

    #[inline]
    pub fn open(&self) -> bool {
        match self {
            NbtElement::ByteArray(bytes) => bytes.open(),
            NbtElement::List(list) => list.open(),
            NbtElement::Compound(compound) => compound.open(),
            NbtElement::IntArray(ints) => ints.open(),
            NbtElement::LongArray(longs) => longs.open(),
            _ => false
        }
    }

    #[inline]
    pub fn increment(&mut self, amount: usize, true_amount: usize) {
        match self {
            NbtElement::ByteArray(bytes) => bytes.increment(amount, true_amount),
            NbtElement::List(list) => list.increment(amount, true_amount),
            NbtElement::Compound(compound) => compound.increment(amount, true_amount),
            NbtElement::IntArray(ints) => ints.increment(amount, true_amount),
            NbtElement::LongArray(longs) => longs.increment(amount, true_amount),
            _ => {}
        }
    }

    #[inline]
    pub fn decrement(&mut self, amount: usize, true_amount: usize) {
        match self {
            NbtElement::ByteArray(bytes) => bytes.decrement(amount, true_amount),
            NbtElement::List(list) => list.decrement(amount, true_amount),
            NbtElement::Compound(compound) => compound.decrement(amount, true_amount),
            NbtElement::IntArray(ints) => ints.decrement(amount, true_amount),
            NbtElement::LongArray(longs) => longs.decrement(amount, true_amount),
            _ => {}
        }
    }

    #[inline]
    pub fn value(&self) -> (Box<str>, bool) {
        match self {
            NbtElement::Null => ("".to_owned().into_boxed_str(), false),
            NbtElement::Byte(x) => (x.value.to_string().into_boxed_str(), true),
            NbtElement::Short(x) => (x.value.to_string().into_boxed_str(), true),
            NbtElement::Int(x) => (x.value.to_string().into_boxed_str(), true),
            NbtElement::Long(x) => (x.value.to_string().into_boxed_str(), true),
            NbtElement::Float(x) => (x.value.to_string().into_boxed_str(), true),
            NbtElement::Double(x) => (x.value.to_string().into_boxed_str(), true),
            NbtElement::ByteArray(array) => (format!("{} {}", array.len(), if array.len() == 1 { "entry" } else { "entries" }).into_boxed_str(), false),
            NbtElement::String(string) => (string.unwrap().to_owned().into_boxed_str(), true),
            NbtElement::List(list) => (format!("{} {}", list.len(), if list.len() == 1 { "entry" } else { "entries" }).into_boxed_str(), false),
            NbtElement::Compound(compound) => (format!("{} {}", compound.len(), if compound.len() == 1 { "entry" } else { "entries" }).into_boxed_str(), false),
            NbtElement::IntArray(array) => (format!("{} {}", array.len(), if array.len() == 1 { "entry" } else { "entries" }).into_boxed_str(), false),
            NbtElement::LongArray(array) => (format!("{} {}", array.len(), if array.len() == 1 { "entry" } else { "entries" }).into_boxed_str(), false),
        }
    }

    /// # Safety
    ///
    /// * 'y' parameter must be in range 0..self.height()
    ///
    /// * Final value must be valid for parent element, i.e. element = [NbtInt] only (for [NbtIntArray])
    ///
    /// * Indices must be valid
    #[inline]
    pub unsafe fn drop_simple(&mut self, y: &mut usize, key: Option<Box<str>>, element: Self, idx: usize, indices: &mut Vec<usize>) -> usize {
        match self {
            NbtElement::ByteArray(array) => array.drop_simple(y, key, element, idx, indices),
            NbtElement::List(list) => list.drop_simple(y, key, element, idx, indices),
            NbtElement::Compound(comppund) => comppund.drop_simple(y, key, element, idx, indices),
            NbtElement::IntArray(array) => array.drop_simple(y, key, element, idx, indices),
            NbtElement::LongArray(array) => array.drop_simple(y, key, element, idx, indices),
            _ => unreachable_unchecked()
        }
    }

    #[inline]
    pub fn drop(&mut self, key: Option<Box<str>>, element: Self, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> DropFn {
        let height = self.height() * 16;
        match self {
            _ if *y >= height + 8 => {
                *y -= height;
                DropFn::Missed(key, element)
            },
            NbtElement::ByteArray(bytes) => bytes.drop(key, element, y, depth, target_depth, indices),
            NbtElement::List(list) => list.drop(key, element, y, depth, target_depth, indices),
            NbtElement::Compound(compound) => compound.drop(key, element, y, depth, target_depth, indices),
            NbtElement::IntArray(ints) => ints.drop(key, element, y, depth, target_depth, indices),
            NbtElement::LongArray(longs) => longs.drop(key, element, y, depth, target_depth, indices),
            _ => {
                if *y < 16 {
                    *y = 0;
                } else {
                    *y -= 16;
                }
                DropFn::Missed(key, element)
            },
        }
    }

    /// # Safety
    ///
    /// 'y' parameter must be in range ..self.height()
    #[inline]
    pub unsafe fn try_select_text(&self, y: &mut usize, indices: &mut Vec<usize>) -> TrySelectTextFn {
        match self {
            NbtElement::ByteArray(array) => array.try_select_text(y, indices),
            NbtElement::List(list) => list.try_select_text(y, indices),
            NbtElement::Compound(compound) => compound.try_select_text(y, indices),
            NbtElement::IntArray(array) => array.try_select_text(y, indices),
            NbtElement::LongArray(array) => array.try_select_text(y, indices),
            _ => unsafe { unreachable_unchecked() }
        }
    }

    /// Duplicates the hovered element below
    /// # Safety
    ///
    /// 'y' must be within the range 1..self.height()
    #[inline]
    pub unsafe fn duplicate(&mut self, y: &mut usize, indices: &mut Vec<usize>) {
        match self {
            NbtElement::ByteArray(array) => array.duplicate(y, indices),
            NbtElement::List(list) => list.duplicate(y, indices),
            NbtElement::Compound(compound) => compound.duplicate(y, indices),
            NbtElement::IntArray(array) => array.duplicate(y, indices),
            NbtElement::LongArray(array) => array.duplicate(y, indices),
            _ => unreachable_unchecked()
        }
    }

    #[inline]
    pub fn copy(&self, y: &mut usize, parent: Option<String>) {
        match self {
            NbtElement::Null => { let _ = cli_clipboard::set_contents(format!("{}null", parent.map(|x| x + ":").unwrap_or(String::new()))); },
            NbtElement::ByteArray(array) => array.copy(y, parent),
            NbtElement::List(list) => list.copy(y, parent),
            NbtElement::Compound(compound) => compound.copy(y, parent),
            NbtElement::IntArray(array) => array.copy(y, parent),
            NbtElement::LongArray(array) => array.copy(y, parent),
            x => { let _ = cli_clipboard::set_contents(format!("{}{x}", parent.map(|x| x + ":").unwrap_or(String::new()))); },
        }
    }

    #[inline]
    pub fn toggle_fn(&mut self, y: &mut usize, target_depth: usize, depth: usize, indices: &mut Vec<usize>) -> ToggleFn {
        match self {
            NbtElement::ByteArray(array) => array.toggle_fn(y, target_depth, depth, indices),
            NbtElement::List(list) => list.toggle_fn(y, target_depth, depth, indices),
            NbtElement::Compound(compound) => compound.toggle_fn(y, target_depth, depth, indices),
            NbtElement::IntArray(array) => array.toggle_fn(y, target_depth, depth, indices),
            NbtElement::LongArray(array) => array.toggle_fn(y, target_depth, depth, indices),
            _ => Err(()),
        }
    }

    /// Like [steal](NbtElement::steal) but without depth checks.
    ///
    /// # Safety
    ///
    /// 'y' parameter must be in range 1..self.height()
    #[inline]
    pub unsafe fn delete(&mut self, y: &mut usize, indices: &mut Vec<usize>) -> DeleteFn {
        match self {
            NbtElement::ByteArray(array) => array.delete(y, indices),
            NbtElement::List(list) => list.delete(y, indices),
            NbtElement::Compound(compound) => compound.delete(y, indices),
            NbtElement::IntArray(array) => array.delete(y, indices),
            NbtElement::LongArray(array) => array.delete(y, indices),
            _ => unsafe { unreachable_unchecked() },
        }
    }

    /// Like [delete](NbtElement::delete) but with depth checks.
    #[inline]
    pub fn steal(&mut self, y: &mut usize, depth: usize, target_depth: usize, indices: &mut Vec<usize>) -> StealFn {
        match self {
            NbtElement::ByteArray(array) => array.steal(y, depth, target_depth, indices),
            NbtElement::List(list) => list.steal(y, depth, target_depth, indices),
            NbtElement::Compound(compound) => compound.steal(y, depth, target_depth, indices),
            NbtElement::IntArray(array) => array.steal(y, depth, target_depth, indices),
            NbtElement::LongArray(array) => array.steal(y, depth, target_depth, indices),
            _ => unsafe { unreachable_unchecked() },
        }
    }

    #[inline]
    pub fn insert(&mut self, idx: usize, value: Self) -> Option<NbtElement> {
        match self {
            NbtElement::ByteArray(bytes) => bytes.insert(idx, value),
            NbtElement::List(list) => list.insert(idx, value),
            NbtElement::Compound(compound) => compound.insert_full(idx, "_".to_owned(), value),
            NbtElement::IntArray(ints) => ints.insert(idx, value),
            NbtElement::LongArray(longs) => longs.insert(idx, value),
            _ => Some(value)
        }
    }
}

impl Display for NbtElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            NbtElement::Null => write!(f, "null"),
            NbtElement::Byte(byte) => write!(f, "{byte}"),
            NbtElement::Short(short) => write!(f, "{short}"),
            NbtElement::Int(int) => write!(f, "{int}"),
            NbtElement::Long(long) => write!(f, "{long}"),
            NbtElement::Float(float) => write!(f, "{float}"),
            NbtElement::Double(double) => write!(f, "{double}"),
            NbtElement::ByteArray(bytes) => write!(f, "{bytes}"),
            NbtElement::String(string) => write!(f, "{string}"),
            NbtElement::List(list) => write!(f, "{list}"),
            NbtElement::Compound(compound) => write!(f, "{compound}"),
            NbtElement::IntArray(ints) => write!(f, "{ints}"),
            NbtElement::LongArray(longs) => write!(f, "{longs}"),
        }
    }
}
