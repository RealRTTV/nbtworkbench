use std::panic::catch_unwind;
use std::slice::IterMut;
use std::string::String;
use std::time::Instant;

use crate::{DeleteFn, DropFn, LeftClickFn, elements, VertexBufferBuilder};
use crate::decoder::Decoder;
use crate::elements::array::NbtArray;
use crate::elements::compound::NbtCompound;
use crate::elements::element_type::NbtElement::*;
use crate::elements::list::NbtList;
use crate::elements::primitive::NbtPrimitive;
use crate::elements::string::NbtString;

#[repr(C)]
#[repr(u8)]
pub enum NbtElement {
    Null,
    Byte(NbtPrimitive<i8, 0, 0>),
    Short(NbtPrimitive<i16, 16, 0>),
    Int(NbtPrimitive<i32, 32, 0>),
    Long(NbtPrimitive<i64, 48, 0>),
    Float(NbtPrimitive<f32, 64, 0>),
    Double(NbtPrimitive<f64, 80, 0>),
    ByteArray(NbtArray<1, 'B', 96, 0, 1>),
    String(NbtString),
    List(NbtList),
    Compound(NbtCompound),
    IntArray(NbtArray<3, 'I', 112, 0, 4>),
    LongArray(NbtArray<4, 'L', 0, 16, 8>),
}

impl NbtElement {
    pub fn from_bytes(element: u8, decoder: &mut Decoder) -> Self {
        match element {
            0 => Null,
            1 => Byte(NbtPrimitive { value: unsafe { decoder.assert_len(1); decoder.i8() }}),
            2 => Short(NbtPrimitive { value: unsafe { decoder.assert_len(2); decoder.i16() }}),
            3 => Int(NbtPrimitive { value: unsafe { decoder.assert_len(4); decoder.i32() }}),
            4 => Long(NbtPrimitive { value: unsafe { decoder.assert_len(8); decoder.i64() }}),
            5 => Float(NbtPrimitive { value: unsafe { decoder.assert_len(4); decoder.f32() }}),
            6 => Double(NbtPrimitive { value: unsafe { decoder.assert_len(8); decoder.f64() }}),
            7 => ByteArray(NbtArray::from_bytes(decoder, |decoder| Byte(NbtPrimitive { value: unsafe { decoder.i8() }}))),
            8 => String(NbtString::from_bytes(decoder)),
            9 => List(NbtList::from_bytes(decoder)),
            10 => Compound(NbtCompound::from_bytes(decoder)),
            11 => IntArray(NbtArray::from_bytes(decoder, |decoder| Int(NbtPrimitive { value: unsafe { decoder.i32() }}))),
            12 => LongArray(NbtArray::from_bytes(decoder, |decoder| Long(NbtPrimitive { value: unsafe { decoder.i64() }}))),
            _ => panic!()
        }
    }

    pub fn to_bytes(&self, writer: &mut Vec<u8>) {
        match self {
            Null => writer.push(0),
            Byte(byte) => byte.to_bytes(writer),
            Short(short) => short.to_bytes(writer),
            Int(int) => int.to_bytes(writer),
            Long(long) => long.to_bytes(writer),
            Float(float) => float.to_bytes(writer),
            Double(double) => double.to_bytes(writer),
            ByteArray(bytes) => bytes.to_bytes(writer),
            String(string) => string.to_bytes(writer),
            List(list) => list.to_bytes(writer),
            Compound(compound) => compound.to_bytes(writer),
            IntArray(ints) => ints.to_bytes(writer),
            LongArray(longs) => longs.to_bytes(writer),
        }
    }

    #[inline]
    pub fn id(&self) -> u8 {
        match self {
            Null => 0,
            Byte(_) => 1,
            Short(_) => 2,
            Int(_) => 3,
            Long(_) => 4,
            Float(_) => 5,
            Double(_) => 6,
            ByteArray(_) => 7,
            String(_) => 8,
            List(_) => 9,
            Compound(_) => 10,
            IntArray(_) => 11,
            LongArray(_) => 12,
        }
    }

    #[inline]
    pub fn from_id(id: u8) -> NbtElement {
        match id {
            0 => Null,
            1 => Byte(unsafe { std::mem::zeroed() }),
            2 => Short(unsafe { std::mem::zeroed() }),
            3 => Int(unsafe { std::mem::zeroed() }),
            4 => Long(unsafe { std::mem::zeroed() }),
            5 => Float(unsafe { std::mem::zeroed() }),
            6 => Double(unsafe { std::mem::zeroed() }),
            7 => ByteArray(NbtArray::new()),
            8 => String(NbtString::new("".to_owned())),
            9 => List(NbtList::new(Vec::new(), 0x1)),
            10 => Compound(NbtCompound::new()),
            11 => IntArray(NbtArray::new()),
            _ => LongArray(NbtArray::new())
        }
    }

    #[inline]
    pub fn from_file(bytes: &[u8]) -> Option<Self> {
        let start = Instant::now();
        let res = catch_unwind(|| {
            let mut decoder = Decoder::new(bytes);
            decoder.assert_len(3);
            unsafe { decoder.skip(3); }
            Compound(NbtCompound::from_bytes(&mut decoder))
        }).ok();
        println!("{}ms", Instant::now().duration_since(start).as_nanos() as f64 / 1_000_000.0);
        res
    }

    #[inline]
    pub fn array_iter_mut(&mut self) -> Option<IterMut<'_, NbtElement>> {
        Some(match self {
            ByteArray(array) => array.iter_mut(),
            IntArray(array) => array.iter_mut(),
            LongArray(array) => array.iter_mut(),
            _ => return None
        })
    }

    #[inline]
    pub fn to_file(&self) -> Vec<u8> {
        let mut writer = Vec::new();
        writer.push(0x0A);
        writer.push(0x00);
        writer.push(0x00);
        self.to_bytes(&mut writer);
        writer
    }

    #[inline]
    pub fn render(&self, x: &mut u32, y: &mut u32, remaining_scroll: &mut u32, builder: &mut VertexBufferBuilder, str: Option<&str>, tail: bool, forbidden_y: Option<u32>) {
        match self {
            Null => *x += 8,
            Byte(byte) => byte.render(builder, x, y, str, forbidden_y),
            Short(short) => short.render(builder, x, y, str, forbidden_y),
            Int(int) => int.render(builder, x, y, str, forbidden_y),
            Long(long) => long.render(builder, x, y, str, forbidden_y),
            Float(float) => float.render(builder, x, y, str, forbidden_y),
            Double(double) => double.render(builder, x, y, str, forbidden_y),
            ByteArray(byte_array) => byte_array.render(builder, x, y, str, remaining_scroll, tail, forbidden_y),
            String(string) => string.render(builder, x, y, str, forbidden_y),
            List(list) => list.render(builder, x, y, str, remaining_scroll, tail, forbidden_y),
            Compound(compound) => compound.render(builder, x, y, str, remaining_scroll, tail, forbidden_y),
            IntArray(int_array) => int_array.render(builder, x, y, str, remaining_scroll, tail, forbidden_y),
            LongArray(long_array) => long_array.render(builder, x, y, str, remaining_scroll, tail, forbidden_y),
        }
    }

    #[inline]
    pub fn can_accept(&self, id: u8) -> bool {
        match self {
            Null => false,
            Byte(_) => false,
            Short(_) => false,
            Int(_) => false,
            Long(_) => false,
            Float(_) => false,
            Double(_) => false,
            ByteArray(_) => id == 1,
            String(_) => false,
            List(list) => id == list.id(),
            Compound(_) => true,
            IntArray(_) => id == 3,
            LongArray(_) => id == 4,
        }
    }

    #[inline]
    pub fn value_render(&self) -> String {
        match self {
            Null => String::new(),
            Byte(_) => format!(": {}", self.value().unwrap()),
            Short(_) => format!(": {}", self.value().unwrap()),
            Int(_) => format!(": {}", self.value().unwrap()),
            Long(_) => format!(": {}", self.value().unwrap()),
            Float(_) => format!(": {}", self.value().unwrap()),
            Double(_) => format!(": {}", self.value().unwrap()),
            ByteArray(bytes) => format!(": {} entr{}", bytes.len(), if bytes.len() == 1 { "y" } else { "ies" }),
            String(_) => ": ".to_owned() + &self.value().unwrap(),
            List(list) => format!(": {} entr{}", list.len(), if list.len() == 1 { "y" } else { "ies" }),
            Compound(compound) => format!(": {} entr{}", compound.len(), if compound.len() == 1 { "y" } else { "ies" }),
            IntArray(ints) => format!(": {} entr{}", ints.len(), if ints.len() == 1 { "y" } else { "ies" }),
            LongArray(longs) => format!(": {} entr{}", longs.len(), if longs.len() == 1 { "y" } else { "ies" }),
        }
    }

    #[inline]
    pub fn len(&self) -> Option<u32> {
        Some(match self {
            ByteArray(bytes) => bytes.len(),
            List(list) => list.len(),
            Compound(compound) => compound.len(),
            IntArray(ints) => ints.len(),
            LongArray(longs) => longs.len(),
            _ => return None
        } as u32)
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len().map(|x| x > 0).unwrap_or(false)
    }

    #[inline]
    pub fn value(&self) -> Option<std::string::String> {
        Some(match self {
            Byte(byte) => byte.to_string(),
            Short(short) => short.to_string(),
            Int(int) => int.to_string(),
            Long(long) => long.to_string(),
            Float(float) => float.to_string(),
            Double(double) => double.to_string(),
            String(string) => string.unwrap().to_owned(),
            _ => return None
        })
    }

    #[inline]
    pub fn render_icon(id: u8, x: u32, y: u32, builder: &mut VertexBufferBuilder) {
        match id {
            0 => {}
            1 => NbtPrimitive::<u8, 0, 0>::render_icon(x, y, builder),
            2 => NbtPrimitive::<u8, 16, 0>::render_icon(x, y, builder),
            3 => NbtPrimitive::<u8, 32, 0>::render_icon(x, y, builder),
            4 => NbtPrimitive::<u8, 48, 0>::render_icon(x, y, builder),
            5 => NbtPrimitive::<u8, 64, 0>::render_icon(x, y, builder),
            6 => NbtPrimitive::<u8, 80, 0>::render_icon(x, y, builder),
            7 => NbtPrimitive::<u8, 96, 0>::render_icon(x, y, builder),
            8 => elements::string::render_icon(x, y, builder),
            9 => elements::list::render_icon(x, y, builder),
            10 => elements::compound::render_icon(x, y, builder),
            11 => NbtPrimitive::<u8, 112, 0>::render_icon(x, y, builder),
            _ => NbtPrimitive::<u8, 0, 16>::render_icon(x, y, builder)
        }
    }

    #[inline]
    pub fn height(&self) -> u32 {
        match self {
            ByteArray(bytes) => bytes.height(),
            List(list) => list.height(),
            Compound(compound) => compound.height(),
            IntArray(ints) => ints.height(),
            LongArray(longs) => longs.height(),
            _ => 1
        }
    }

    #[inline]
    pub fn stack<F: FnMut(&mut NbtElement, u32), G: FnOnce(&mut NbtElement, u32, u32)>(&mut self, y: &mut u32, depth: &mut u32, index: u32, parent: &mut F, tail: G) -> Option<G> {
        if *y >= self.height() {
            *y -= self.height();
            Some(tail)
        } else {
            match self {
                ByteArray(_) => NbtArray::<1, 'B', 96, 0, 1>::stack(self, y, depth, index, parent, tail),
                LongArray(_) => NbtArray::<4, 'L', 112, 0, 8>::stack(self, y, depth, index, parent, tail),
                IntArray(_) => NbtArray::<3, 'I', 0, 16, 4>::stack(self, y, depth, index, parent, tail),
                List(_) => NbtList::stack(self, y, depth, index, parent, tail),
                Compound(_) => NbtCompound::stack(self, y, depth, index, parent, tail),
                x => {
                    tail(x, *depth, index);
                    None
                }
            }
        }
    }

    #[inline]
    pub fn left_click(&mut self, y: &mut u32, depth: u32, mouse_x: u32, index: u32, other_value: Option<std::string::String>) -> LeftClickFn {
        match self {
            ByteArray(array) => array.left_click(y, depth, mouse_x, index, other_value),
            List(list) => list.left_click(y, depth, mouse_x, index, other_value),
            Compound(compound) => compound.left_click(y, depth, mouse_x, index, other_value),
            IntArray(array) => array.left_click(y, depth, mouse_x, index, other_value),
            LongArray(array) => array.left_click(y, depth, mouse_x, index, other_value),
            x => {
                if mouse_x / 16 == depth { // cannot click on primitive elements
                    *y -= 1;
                    None
                } else if mouse_x / 16 >= depth + 1 { // ooh probably a text selection
                    Some(Err((depth * 16 + 40, *y, index, x.value(), mouse_x, other_value)))
                } else { // too small to do anything
                    *y -= 1;
                    None
                }
            }
        }
    }

    #[inline]
    pub fn set_value(&mut self, value: &str) -> Option<std::string::String> {
        Some(match self {
            Byte(byte) => {
                let before = byte.to_string();
                byte.set(value.parse().ok());
                before
            },
            Short(short) => {
                let before = short.to_string();
                short.set(value.parse().ok());
                before
            },
            Int(int) => {
                let before = int.to_string();
                int.set(value.parse().ok());
                before
            },
            Long(long) => {
                let before = long.to_string();
                long.set(value.parse().ok());
                before
            },
            Float(float) => {
                let before = float.to_string();
                float.set(value.parse().ok());
                before
            },
            Double(double) => {
                let before = double.to_string();
                double.set(value.parse().ok());
                before
            },
            String(string) => {
                let before = string.unwrap().to_string();
                string.set(value.to_owned());
                before
            },
            _ => return None
        })
    }

    #[inline]
    pub fn child_height(&self, index: u32) -> u32 {
        match self {
            ByteArray(bytes) => bytes.child_height(index),
            List(list) => list.child_height(index),
            Compound(compound) => compound.child_height(index),
            IntArray(ints) => ints.child_height(index),
            LongArray(longs) => longs.child_height(index),
            _ => 0
        }
    }

    #[inline]
    pub fn delete_index(&mut self, index: u32) -> Option<NbtElement> {
        match self {
            ByteArray(bytes) => bytes.delete_index(index),
            List(list) => list.delete_index(index),
            Compound(compound) => compound.delete_index(index),
            IntArray(ints) => ints.delete_index(index),
            LongArray(longs) => longs.delete_index(index),
            _ => None
        }
    }

    #[inline]
    pub fn toggle(&mut self) -> bool {
        match self {
            ByteArray(bytes) => bytes.toggle(),
            List(list) => list.toggle(),
            Compound(compound) => compound.toggle(),
            IntArray(ints) => ints.toggle(),
            LongArray(longs) => longs.toggle(),
            _ => false
        }
    }

    #[inline]
    pub fn open(&self) -> bool {
        match self {
            ByteArray(bytes) => bytes.open(),
            List(list) => list.open(),
            Compound(compound) => compound.open(),
            IntArray(ints) => ints.open(),
            LongArray(longs) => longs.open(),
            _ => false
        }
    }

    #[inline]
    pub fn increment(&mut self, amount: u32) {
        match self {
            ByteArray(bytes) => bytes.increment(amount),
            List(list) => list.increment(amount),
            Compound(compound) => compound.increment(amount),
            IntArray(ints) => ints.increment(amount),
            LongArray(longs) => longs.increment(amount),
            _ => {}
        }
    }

    #[inline]
    pub fn decrement(&mut self, amount: u32) {
        match self {
            ByteArray(bytes) => bytes.decrement(amount),
            List(list) => list.decrement(amount),
            Compound(compound) => compound.decrement(amount),
            IntArray(ints) => ints.decrement(amount),
            LongArray(longs) => longs.decrement(amount),
            _ => {}
        }
    }

    #[inline]
    pub fn drop(&mut self, element: Self, y: &mut u32, parent_y: u32) -> DropFn {
        match self {
            ByteArray(bytes) => bytes.drop(element, y, parent_y),
            List(list) => list.drop(element, y, parent_y),
            Compound(compound) => compound.drop(element, y, parent_y),
            IntArray(ints) => ints.drop(element, y, parent_y),
            LongArray(longs) => longs.drop(element, y, parent_y),
            _ => Ok(element)
        }
    }

    #[inline]
    pub fn delete(&mut self, y: &mut u32, depth: u32) -> DeleteFn {
        match self {
            ByteArray(array) => array.delete(y, depth),
            List(list) => list.delete(y, depth),
            Compound(compound) => compound.delete(y, depth),
            IntArray(array) => array.delete(y, depth),
            LongArray(array) => array.delete(y, depth),
            _ if *y == 0 => Some(None),
            _ => {
                *y -= 1;
                None
            }
        }
    }

    #[inline]
    pub fn drop_index(&mut self, index: u32, other: Self) -> bool {
        match self {
            ByteArray(bytes) => bytes.drop_index(index, other),
            List(list) => list.drop_index(index, other),
            Compound(compound) => compound.drop_index(index, "_".to_owned(), other),
            IntArray(ints) => ints.drop_index(index, other),
            LongArray(longs) => longs.drop_index(index, other),
            _ => false
        }
    }
}

impl ToString for NbtElement {
    fn to_string(&self) -> std::string::String {
        match self {
            Null => "null".to_owned(),
            Byte(byte) => byte.to_string(),
            Short(short) => short.to_string(),
            Int(int) => int.to_string(),
            Long(long) => long.to_string(),
            Float(float) => float.to_string(),
            Double(double) => double.to_string(),
            ByteArray(bytes) => bytes.to_string(),
            String(string) => string.to_string(),
            List(list) => list.to_string(),
            Compound(compound) => compound.to_string(),
            IntArray(ints) => ints.to_string(),
            LongArray(longs) => longs.to_string()
        }
    }
}
