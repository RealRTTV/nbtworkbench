use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::hint::likely;
use std::slice::{Iter, IterMut};
#[cfg(not(target_arch = "wasm32"))] use std::thread::{Scope, scope};

use crate::elements::compound::NbtCompound;
use crate::elements::element::id_to_string_name;
use crate::elements::result::NbtParseResult;
use crate::elements::{ComplexNbtElementVariant, Matches, NbtElement, NbtElementVariant};
use crate::render::TreeRenderContext;
use crate::render::assets::{LIST_GHOST_UV, LIST_UV};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::selected_text::SelectedText;
use crate::serialization::decoder::Decoder;
use crate::serialization::encoder::UncheckedBufWriter;
use crate::serialization::formatter::{PrettyDisplay, PrettyFormatter};
use crate::util::Vec2u;
#[cfg(target_arch = "wasm32")]
use crate::wasm::{FakeScope as Scope, fake_scope as scope};

#[repr(C)]
pub struct NbtList {
	pub elements: Box<Vec<NbtElement>>,
	height: u32,
	true_height: u32,
	end_x: u32,
	elements_bitset: u16,
	open: bool,
	_id: u8,
}

impl Matches for NbtList {
	fn matches(&self, other: &Self) -> bool { if self.is_empty() { other.is_empty() } else { self.elements.iter().all(|a| other.elements.iter().any(|b| a.matches(b))) } }
}

impl PartialEq for NbtList {
	fn eq(&self, other: &Self) -> bool { self.elements.as_slice().eq(other.elements.as_slice()) }
}

impl Default for NbtList {
	fn default() -> Self {
		Self {
			elements: Box::new(vec![]),
			height: 1,
			true_height: 1,
			end_x: 0,
			elements_bitset: 0,
			open: false,
			_id: Self::ID,
		}
	}
}

impl Clone for NbtList {
	fn clone(&self) -> Self {
		let mut vec = unsafe { Vec::try_with_capacity(self.len()).unwrap_unchecked() };
		for src in self.elements.iter() {
			unsafe { vec.push_within_capacity(src.clone()).unwrap_unchecked() };
		}
		Self {
			elements: unsafe { Box::try_new(vec).unwrap_unchecked() },
			height: self.height,
			true_height: self.true_height,
			end_x: self.end_x,
			elements_bitset: self.elements_bitset,
			open: self.open,
			_id: Self::ID,
		}
	}
}

impl Display for NbtList {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		let heterogeneous = self.is_heterogeneous();
		write!(f, "[")?;
		for (idx, element) in self.children().enumerate() {
			if heterogeneous {
				write!(f, "{{'':{element}}}")?;
			} else {
				write!(f, "{element}")?;
			}
			if likely(idx < self.len() - 1) {
				write!(f, ",")?;
			}
		}
		write!(f, "]")
	}
}

impl PrettyDisplay for NbtList {
	fn pretty_fmt(&self, f: &mut PrettyFormatter) {
		if self.is_empty() {
			f.write_str("[]")
		} else {
			let len = self.len();
			let heterogeneous = self.is_heterogeneous();
			f.write_str("[\n");
			f.increase();
			for (idx, element) in self.children().enumerate() {
				f.indent();
				if heterogeneous && element.is_compound() {
					f.write_str("{ '': ");
					element.pretty_fmt(f);
					f.write_str(" }");
				} else {
					element.pretty_fmt(f);
				}
				if idx + 1 < len {
					f.write_str(",\n");
				} else {
					f.write_str("\n");
				}
			}
			f.decrease();
			f.indent();
			f.write_str("]");
		}
	}
}

impl NbtList {
	#[must_use]
	pub const fn is_heterogeneous(&self) -> bool { !self.elements_bitset.is_power_of_two() }

	#[must_use]
	pub const fn child_id(&self) -> u8 { if self.is_heterogeneous() { 0 } else { self.elements_bitset.trailing_zeros() as u8 } }

	#[must_use]
	pub const fn serialize_id(&self) -> u8 { if self.is_heterogeneous() { NbtCompound::ID } else { self.elements_bitset.trailing_zeros() as u8 } }

	pub fn recache_elements_bitset(&mut self) {
		let mut elements_bitset = 0_u16;
		for element in self.children() {
			elements_bitset |= 1 << element.id();
		}
		self.elements_bitset = elements_bitset;
	}
}

impl NbtElementVariant for NbtList {
	const ID: u8 = 9;
	const UV: Vec2u = LIST_UV;
	const GHOST_UV: Vec2u = LIST_GHOST_UV;
	const VALUE_COLOR: TextColor = TextColor::TreeValueDesc;
	const SEPERATOR_COLOR: TextColor = Self::VALUE_COLOR;

	fn from_str0(mut s: &str) -> Result<(&str, Self), usize>
	where Self: Sized {
		s = s.strip_prefix('[').ok_or(s.len())?.trim_start();
		let mut list = Self::new(vec![]);
		while !s.starts_with(']') {
			let (s2, mut element) = NbtElement::from_str0(s, NbtElement::parse_int)?;
			// SAFETY: no caches have been made
			element = unsafe { element.try_compound_singleton_into_inner().unwrap_or_else(|element| element) };
			unsafe { list.insert(list.len(), element) }.map_err(|_| s.len())?;
			s = s2.trim_start();
			if let Some(s2) = s.strip_prefix(',') {
				s = s2.trim_start();
			} else if s.starts_with(']') {
				break;
			}
		}
		let s = s.strip_prefix(']').ok_or(s.len())?;
		list.elements.shrink_to_fit();
		list.recache();
		Ok((s, list))
	}

	fn from_bytes<'a, D: Decoder<'a>>(decoder: &mut D, _: Self::ExtraParseInfo) -> NbtParseResult<Self>
	where Self: Sized {
		use super::result::*;

		decoder.assert_len(5)?;
		let element = unsafe { decoder.u8() };
		let len = unsafe { decoder.u32() } as usize;
		let mut vec = from_result(Vec::try_with_capacity(len))?;
		for _ in 0..len {
			let mut element = NbtElement::from_bytes(element, decoder)?;
			// SAFETY: no caches have been made
			element = unsafe { element.try_compound_singleton_into_inner().unwrap_or_else(|element| element) };
			from_opt(vec.push_within_capacity(element).ok(), "Vec was larger that stated")?;
		}
		let mut list = Self {
			elements: unsafe { Box::try_new(vec).unwrap_unchecked() },
			height: 1,
			true_height: 1,
			end_x: 0,
			elements_bitset: 1 << element,
			open: false,
			_id: Self::ID,
		};
		list.recache();
		ok(list)
	}

	fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) {
		let heterogeneous = self.is_heterogeneous();
		writer.write(&[self.serialize_id()]);
		writer.write(&(self.len() as u32).to_be_bytes());
		for element in self.elements.iter() {
			if heterogeneous {
				writer.write(&[element.id()]);
				element.to_be_bytes(writer);
				writer.write(&[0x00]);
			} else {
				element.to_be_bytes(writer);
			}
		}
	}

	fn to_le_bytes(&self, writer: &mut UncheckedBufWriter) {
		let heterogeneous = self.is_heterogeneous();
		writer.write(&[self.serialize_id()]);
		writer.write(&(self.len() as u32).to_le_bytes());
		for element in self.elements.iter() {
			if heterogeneous {
				writer.write(&[element.id()]);
				element.to_le_bytes(writer);
				writer.write(&[0x00]);
			} else {
				element.to_le_bytes(writer);
			}
		}
	}

	fn render(&self, builder: &mut VertexBufferBuilder, key: Option<&str>, tail: bool, ctx: &mut TreeRenderContext) {
		ctx.render_complex_head(self, builder, key, TreeRenderContext::draw_held_entry_bar);
		ctx.render_complex_body(self, builder, tail, TreeRenderContext::draw_held_entry_bar, TreeRenderContext::draw_held_entry_bar);
	}

	fn value(&self) -> Cow<'_, str> {
		let item = id_to_string_name(self.child_id(), self.len());
		Cow::Owned(format!("{} {item}", self.len()))
	}
}

impl ComplexNbtElementVariant for NbtList {
	type Entry = NbtElement;

	fn new(entries: Vec<Self::Entry>) -> Self
	where Self: Sized {
		let mut this = Self {
			height: 0,
			true_height: 0,
			elements: Box::new(entries),
			open: false,
			elements_bitset: 0,
			end_x: 0,
			_id: Self::ID,
		};
		this.recache_elements_bitset();
		this
	}

	fn height(&self) -> usize { self.height as usize }

	fn true_height(&self) -> usize { self.true_height as usize }

	fn len(&self) -> usize { self.elements.len() }

	fn can_insert(&self, value: &NbtElement) -> bool { !value.is_chunk() }

	fn is_open(&self) -> bool { self.open }

	fn end_x(&self) -> usize { self.end_x as usize }

	unsafe fn toggle(&mut self) {
		self.open = !self.open && !self.is_empty();
		if !self.open && !self.is_empty() {
			scope(|scope| unsafe { self.shut(scope) });
		}
	}

	unsafe fn insert(&mut self, idx: usize, entry: Self::Entry) -> Result<Option<Self::Entry>, Self::Entry> {
		if self.can_insert(&entry) {
			unsafe {
				self.elements.try_reserve_exact(1).unwrap_unchecked();
			}
			self.elements.insert(idx, entry);
			Ok(None)
		} else {
			Err(entry)
		}
	}

	unsafe fn remove(&mut self, idx: usize) -> Option<Self::Entry> {
		if idx >= self.elements.len() {
			return None
		};
		let removed = self.elements.remove(idx);
		self.elements.shrink_to_fit();
		Some(removed)
	}

	unsafe fn replace(&mut self, idx: usize, entry: Self::Entry) -> Result<Option<Self::Entry>, Self::Entry> {
		if !self.can_insert(&entry) || idx >= self.len() {
			return Err(entry);
		}
		Ok(Some(core::mem::replace(&mut self.elements[idx], entry)))
	}

	unsafe fn swap(&mut self, a: usize, b: usize) { self.elements.swap(a, b); }

	unsafe fn shut<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		self.open = false;
		self.height = 1;
		for element in self.children_mut() {
			if element.is_open() {
				unsafe { element.shut(scope) };
			}
		}
	}

	unsafe fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		self.open = !self.is_empty();
		self.height = self.true_height;
		for element in self.children_mut() {
			unsafe { element.expand(scope) };
		}
	}

	fn recache(&mut self) {
		let mut height = 1;
		let mut true_height = 1;
		let mut end_x = 0;

		for child in self.children() {
			height += child.height() as u32;
			true_height += child.true_height() as u32;
			end_x = usize::max(end_x, NbtElement::DEPTH_INCREMENT_WIDTH + SelectedText::PREFIXING_SPACE_WIDTH + child.value_width());
			end_x = usize::max(end_x, NbtElement::DEPTH_INCREMENT_WIDTH + child.end_x());
		}

		self.height = if self.is_open() { height } else { 1 };
		self.true_height = true_height;
		self.end_x = if self.is_open() { end_x as u32 } else { 0 };

		self.recache_elements_bitset();
	}

	fn get(&self, idx: usize) -> Option<&Self::Entry> { self.elements.get(idx) }

	fn get_mut(&mut self, idx: usize) -> Option<&mut Self::Entry> { self.elements.get_mut(idx) }

	unsafe fn get_unchecked(&self, idx: usize) -> &Self::Entry { self.elements.get_unchecked(idx) }

	unsafe fn get_unchecked_mut(&mut self, idx: usize) -> &mut Self::Entry { self.elements.get_unchecked_mut(idx) }

	fn children(&self) -> Iter<'_, Self::Entry> { self.elements.iter() }

	fn children_mut(&mut self) -> IterMut<'_, Self::Entry> { self.elements.iter_mut() }
}
