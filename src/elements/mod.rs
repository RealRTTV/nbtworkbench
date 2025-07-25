pub mod array;
pub mod chunk;
pub mod compound;
pub mod element;
pub mod list;
pub mod primitive;
pub mod region;
pub mod string;

use std::borrow::Cow;
use std::fmt::Display;
use std::slice;
use std::str::FromStr;
#[cfg(not(target_arch = "wasm32"))] use std::thread::Scope;

pub use primitive::*;

use crate::elements::element::NbtElement;
use crate::elements::result::NbtParseResult;
use crate::render::TreeRenderContext;
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::serialization::decoder::Decoder;
use crate::serialization::encoder::UncheckedBufWriter;
use crate::serialization::formatter::PrettyDisplay;
use crate::util::Vec2u;
#[cfg(target_arch = "wasm32")] use crate::wasm::FakeScope as Scope;
use crate::workbench::marked_line::MarkedLines;

/// # Shorthands
/// * `kv`
pub type NbtElementAndKey = (Option<compact_str::CompactString>, NbtElement);

/// # Shorthands
/// * `kv`
pub type NbtElementAndKeyRef<'a> = (Option<&'a str>, &'a NbtElement);

/// # Shorthands
/// * `kv`
pub type NbtElementAndKeyRefMut<'a> = (Option<&'a str>, &'a mut NbtElement);

impl From<NbtElement> for NbtElementAndKey {
	fn from(value: NbtElement) -> Self { (None, value) }
}

impl<'a> From<&'a NbtElement> for NbtElementAndKeyRef<'a> {
	fn from(value: &'a NbtElement) -> Self { (None, value) }
}

impl<'a> From<&'a mut NbtElement> for NbtElementAndKeyRefMut<'a> {
	fn from(value: &'a mut NbtElement) -> Self { (None, value) }
}

pub mod result {
	use std::error::Error;

	#[cfg(debug_assertions)]
	pub type NbtParseResult<T> = anyhow::Result<T>;

	#[cfg(not(debug_assertions))]
	#[must_use]
	pub type NbtParseResult<T> = Option<T>;

	#[must_use]
	#[cfg_attr(not(debug_assertions), inline(always))]
	pub fn ok<T>(nbt: T) -> NbtParseResult<T> {
		#[cfg(debug_assertions)]
		return Ok(nbt);
		#[cfg(not(debug_assertions))]
		return Some(nbt);
	}

	#[must_use]
	#[cfg_attr(not(debug_assertions), inline(always))]
	#[cfg_attr(not(debug_assertions), allow(unused_variables))]
	pub fn err<T>(msg: &'static str) -> NbtParseResult<T> {
		#[cfg(debug_assertions)]
		return Err(anyhow::Error::msg(msg));
		#[cfg(not(debug_assertions))]
		return None;
	}

	#[must_use]
	#[cfg_attr(not(debug_assertions), inline(always))]
	pub fn is_ok<T>(result: &NbtParseResult<T>) -> bool {
		#[cfg(debug_assertions)]
		return result.is_ok();
		#[cfg(not(debug_assertions))]
		return result.is_some();
	}

	#[must_use]
	#[cfg_attr(not(debug_assertions), inline(always))]
	pub fn is_err<T>(result: &NbtParseResult<T>) -> bool {
		#[cfg(debug_assertions)]
		return result.is_err();
		#[cfg(not(debug_assertions))]
		return result.is_none();
	}

	#[must_use]
	#[cfg_attr(not(debug_assertions), inline(always))]
	#[cfg_attr(not(debug_assertions), allow(unused_variables))]
	pub fn from_opt<T>(opt: Option<T>, msg: &'static str) -> NbtParseResult<T> {
		#[cfg(debug_assertions)]
		return opt.ok_or(anyhow::Error::msg(msg));
		#[cfg(not(debug_assertions))]
		return opt;
	}

	#[must_use]
	#[cfg_attr(not(debug_assertions), inline(always))]
	pub fn from_result<T, E>(result: Result<T, E>) -> NbtParseResult<T>
	where E: Error + Send + Sync + 'static {
		#[cfg(debug_assertions)]
		return result.map_err(|e| anyhow::Error::new(e));
		#[cfg(not(debug_assertions))]
		return result.ok();
	}

	#[must_use]
	#[cfg_attr(not(debug_assertions), inline(always))]
	pub fn into_opt<T>(result: NbtParseResult<T>) -> Option<T> {
		#[cfg(debug_assertions)]
		return result.ok();
		#[cfg(not(debug_assertions))]
		return result;
	}

	#[must_use]
	#[cfg_attr(not(debug_assertions), inline(always))]
	pub fn into_result<T, E>(result: NbtParseResult<T>, f: impl FnOnce(Option<anyhow::Error>) -> E) -> Result<T, E> {
		#[cfg(debug_assertions)]
		return result.map_err(|e| f(Some(e)));
		#[cfg(not(debug_assertions))]
		return result.ok_or_else(|| f(None));
	}

	#[must_use]
	#[cfg_attr(not(debug_assertions), inline(always))]
	pub fn map<T, U>(result: NbtParseResult<T>, f: impl FnOnce(T) -> U) -> NbtParseResult<U> {
		#[cfg(debug_assertions)]
		return result.map(f);
		#[cfg(not(debug_assertions))]
		return result.map(f);
	}
}

pub trait Matches {
	#[must_use]
	fn matches(&self, other: &Self) -> bool;
}

pub trait NbtElementVariant: Clone + PartialEq + Display + Matches + Default + PrettyDisplay {
	type ExtraParseInfo = ();

	const ID: u8;
	const UV: Vec2u;
	const GHOST_UV: Vec2u;
	const VALUE_COLOR: TextColor;
	const SEPERATOR_COLOR: TextColor;

	fn from_str0(s: &str) -> Result<(&str, Self), usize>
	where Self: Sized;

	fn from_bytes<'a, D: Decoder<'a>>(decoder: &mut D, extra: Self::ExtraParseInfo) -> NbtParseResult<Self>
	where Self: Sized;

	fn to_be_bytes(&self, writer: &mut UncheckedBufWriter);

	fn to_le_bytes(&self, writer: &mut UncheckedBufWriter);

	fn render(&self, builder: &mut VertexBufferBuilder, key: Option<&str>, tail: bool, ctx: &mut TreeRenderContext);

	#[must_use]
	fn value(&self) -> Cow<'_, str>;

	#[must_use]
	fn uv(&self) -> Vec2u { Self::UV }
}

pub trait PrimitiveNbtElementVariant: NbtElementVariant {
	type InnerType: FromStr;

	#[must_use]
	fn new(inner: Self::InnerType) -> Self
	where Self: Sized;

	#[must_use]
	fn height(&self) -> usize { 1 }

	#[must_use]
	fn true_height(&self) -> usize { 1 }
}

pub trait ComplexNbtElementVariant: NbtElementVariant {
	type Entry;

	const ROOT_UV: Vec2u = Self::UV;

	#[must_use]
	fn new(entries: Vec<Self::Entry>) -> Self
	where Self: Sized;

	#[must_use]
	fn height(&self) -> usize;

	#[must_use]
	fn true_height(&self) -> usize;

	#[must_use]
	fn len(&self) -> usize;

	#[must_use]
	fn is_empty(&self) -> bool { self.len() == 0 }

	#[must_use]
	fn can_insert(&self, value: &NbtElement) -> bool;

	#[must_use]
	fn is_open(&self) -> bool;

	#[must_use]
	fn end_x(&self) -> usize;

	unsafe fn toggle(&mut self);

	unsafe fn insert(&mut self, idx: usize, entry: Self::Entry) -> Result<Option<Self::Entry>, Self::Entry>;

	#[must_use]
	unsafe fn remove(&mut self, idx: usize) -> Option<Self::Entry>;

	unsafe fn replace(&mut self, idx: usize, entry: Self::Entry) -> Result<Option<Self::Entry>, Self::Entry>;

	unsafe fn swap(&mut self, a: usize, b: usize);

	unsafe fn shut<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>);

	unsafe fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>);

	fn recache(&mut self);

	fn on_style_change(&mut self, _bookmarks: &mut MarkedLines) -> bool { false }

	#[must_use]
	fn get(&self, idx: usize) -> Option<&Self::Entry>;

	#[must_use]
	fn get_mut(&mut self, idx: usize) -> Option<&mut Self::Entry>;

	#[must_use]
	unsafe fn get_unchecked(&self, idx: usize) -> &Self::Entry;

	#[must_use]
	unsafe fn get_unchecked_mut(&mut self, idx: usize) -> &mut Self::Entry;

	fn children(&self) -> slice::Iter<'_, Self::Entry>;

	fn children_mut(&mut self) -> slice::IterMut<'_, Self::Entry>;
}
