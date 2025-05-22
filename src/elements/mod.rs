mod array;
mod chunk;
mod compound;
mod element;
mod list;
mod null;
mod primitive;
mod string;

pub use array::*;
pub use chunk::*;
pub use compound::*;
pub use element::*;
pub use list::*;
pub use null::*;
pub use primitive::*;
pub use string::*;

pub type NbtElementAndKey = (Option<compact_str::CompactString>, NbtElement);

pub type NbtElementAndKeyRef<'a> = (Option<&'a str>, &'a NbtElement);

pub mod result {
	use std::error::Error;

	#[cfg(debug_assertions)]
	pub type NbtParseResult<T> = anyhow::Result<T>;

	#[cfg(not(debug_assertions))]
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
}
