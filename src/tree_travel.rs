use std::iter::Peekable;

use compact_str::{CompactString, ToCompactString};

use crate::{OptionExt, panic_unchecked, Position};
use crate::elements::element::{NbtByteArray, NbtElement, NbtIntArray, NbtLongArray, NbtPatternMut};

/// Navigates a tree from the given route of an indices list, will cause UB on invalid lists. Typically used for operations which are saved and not direct clicking interaction.
#[must_use]
pub struct Navigate<'a, I: IntoIterator + ExactSizeIterator> {
	iter: Peekable<I::IntoIter>,
	at_head: bool,
	node: Option<(usize, Option<CompactString>, &'a mut NbtElement)>,
	line_number: usize,
}

impl<'a, I: Iterator<Item = usize> + ExactSizeIterator> Navigate<'a, I> {
	pub fn new(iter: impl IntoIterator<IntoIter = I> + ExactSizeIterator, root: &'a mut NbtElement) -> Self {
		Self {
			iter: iter.into_iter().peekable(),
			at_head: true,
			node: Some((0, None, root)),
			line_number: 1,
		}
	}

	fn step(&mut self) -> Option<()> {
		let (_, _, node) = self.node.take()?;
		let idx = self.iter.next()?;
		let (key, node) = {
			match node.as_pattern_mut() {
				NbtPatternMut::Compound(compound) => {
					self.line_number += 1 + compound
						.children()
						.take(idx)
						.map(|(_, b)| b)
						.map(NbtElement::true_height)
						.sum::<usize>();
					compound
						.get_mut(idx)
						.map(|(a, b)| (Some(a.to_compact_string()), b))
				}
				NbtPatternMut::List(list) => {
					self.line_number += 1 + list
						.children()
						.take(idx)
						.map(NbtElement::true_height)
						.sum::<usize>();
					list.get_mut(idx).map(|b| (None, b))
				}
				NbtPatternMut::ByteArray(array) => {
					self.line_number += 1 + idx;
					array.get_mut(idx).map(|b| (None, b))
				}
				NbtPatternMut::IntArray(array) => {
					self.line_number += 1 + idx;
					array.get_mut(idx).map(|b| (None, b))
				}
				NbtPatternMut::LongArray(array) => {
					self.line_number += 1 + idx;
					array.get_mut(idx).map(|b| (None, b))
				}
				NbtPatternMut::Region(region) => {
					self.line_number += 1 + region
						.children()
						.take(idx)
						.map(NbtElement::true_height)
						.sum::<usize>();
					region.get_mut(idx).map(|b| (None, b))
				}
				NbtPatternMut::Chunk(chunk) => {
					self.line_number += 1 + chunk
						.children()
						.take(idx)
						.map(|(_, b)| b)
						.map(NbtElement::true_height)
						.sum::<usize>();
					chunk
						.get_mut(idx)
						.map(|(a, b)| (Some(a.to_compact_string()), b))
				}
				_ => None,
			}?
		};
		self.node = Some((idx, key, node));
		Some(())
	}

	#[allow(clippy::should_implement_trait)] // no
	pub fn next(&mut self) -> Option<(Position, usize, Option<&str>, &mut NbtElement, usize)> {
		let head = core::mem::replace(&mut self.at_head, false);

		if !head {
			self.step();
		}

		let tail = self.iter.peek().is_none();

		let position = match (head, tail) {
			(true, false) => Position::First,
			(false, false) => Position::Middle,
			(false, true) => Position::Last,
			(true, true) => Position::Only,
		};

		let (idx, key, element) = self.node.as_mut()?;

		Some((position, *idx, key.as_deref(), *element, self.line_number))
	}

	#[must_use]
	pub fn last(mut self) -> (usize, Option<CompactString>, &'a mut NbtElement, usize) {
		while self.iter.peek().is_some() {
			self.step();
		}

		unsafe {
			self.node
				.map(|(a, b, c)| (a, b, c, self.line_number))
				.panic_unchecked("List cannot be empty")
		}
	}
}

/// Navigates through an [`NbtElement`] tree using a y value.
#[must_use]
pub struct Traverse<'a> {
	node: Option<(usize, Option<CompactString>, &'a mut NbtElement)>,
	y: usize,
	cut: bool,
	head: bool,
	line_number: usize,
}

impl<'a> Traverse<'a> {
	pub fn new(y: usize, root: &'a mut NbtElement) -> Self {
		Self {
			cut: y >= root.height() || y == 0,
			node: Some((0, None, root)),
			y,
			head: true,
			line_number: 1,
		}
	}

	fn step(&mut self) -> Option<()> {
		let (_, _, node) = self.node.take()?;

		let (idx, key, new) = 'm: {
			match node.as_pattern_mut() {
				NbtPatternMut::Region(region) => {
					self.y -= 1;
					for (idx, value) in region.children_mut().enumerate() {
						let height = value.height();
						if self.y >= height {
							self.line_number += value.true_height();
							self.y -= height;
							continue;
						} else {
							self.cut = self.y == 0;
							self.line_number += 1;
							break 'm (idx, None, value);
						}
					}
				}
				NbtPatternMut::Chunk(chunk) => {
					self.y -= 1;
					for (idx, (key, value)) in chunk.children_mut().enumerate() {
						let height = value.height();
						if self.y >= height {
							self.line_number += value.true_height();
							self.y -= height;
							continue;
						} else {
							self.cut = self.y == 0;
							self.line_number += 1;
							break 'm (idx, Some(key.to_compact_string()), value);
						}
					}
				}
				NbtPatternMut::Compound(compound) => {
					self.y -= 1;
					for (idx, (key, value)) in compound.children_mut().enumerate() {
						let height = value.height();
						if self.y >= height {
							self.line_number += value.true_height();
							self.y -= height;
							continue;
						} else {
							self.cut = self.y == 0;
							self.line_number += 1;
							break 'm (idx, Some(key.to_compact_string()), value);
						}
					}
				}
				NbtPatternMut::List(list) => {
					self.y -= 1;
					for (idx, value) in list.children_mut().enumerate() {
						let height = value.height();
						if self.y >= height {
							self.line_number += value.true_height();
							self.y -= height;
							continue;
						} else {
							self.cut = self.y == 0;
							self.line_number += 1;
							break 'm (idx, None, value);
						}
					}
				}
				NbtPatternMut::ByteArray(array) => {
					self.cut = true;
					let idx = core::mem::replace(&mut self.y, 0) - 1;
					self.line_number += idx + 1;
					break 'm (idx, None, array.get_mut(idx)?);
				}
				NbtPatternMut::IntArray(array) => {
					self.cut = true;
					let idx = core::mem::replace(&mut self.y, 0) - 1;
					self.line_number += idx + 1;
					break 'm (idx, None, array.get_mut(idx)?);
				}
				NbtPatternMut::LongArray(array) => {
					self.cut = true;
					let idx = core::mem::replace(&mut self.y, 0) - 1;
					self.line_number += idx + 1;
					break 'm (idx, None, array.get_mut(idx)?);
				}
				_ => return None,
			}
			unsafe { panic_unchecked("could not find value in any (skipped everything somehow)") }
		};

		self.node = Some((idx, key, new));

		Some(())
	}

	#[must_use]
	pub fn last(mut self) -> Option<(usize, Option<CompactString>, &'a mut NbtElement, usize)> {
		if self.cut && !self.head { return None }
		while self.y > 0 {
			self.step();
		}
		self.node.map(|(a, b, c)| (a, b, c, self.line_number))
	}

	pub const fn enumerate(self) -> EnumeratedTraverse<'a> {
		EnumeratedTraverse {
			inner: self,
			depth: 0,
		}
	}
}

#[must_use]
pub struct EnumeratedTraverse<'a> {
	inner: Traverse<'a>,
	depth: usize,
}

impl<'a> EnumeratedTraverse<'a> {
	#[must_use]
	#[allow(clippy::type_complexity)] // literally can't otherwise the compiler crashes... yeah...
	pub fn last(
		mut self,
	) -> (
		usize,
		(usize, Option<CompactString>, &'a mut NbtElement, usize),
	) {
		while self.inner.y > 0 {
			self.depth += 1;
			self.inner.step();
		}
		unsafe {
			self.inner
				.node
				.map(|(a, b, c)| (self.depth, (a, b, c, self.inner.line_number)))
				.panic_unchecked("head is always initialized")
		}
	}
}

/// Navigates through an [`NbtElement`] tree using a y value.
/// This traversal will return parents with their child indices and keys with their parent's element.
#[must_use]
pub struct TraverseParents<'a> {
	node: Option<&'a mut NbtElement>,
	y: usize,
	cut: bool,
	head: bool,
	line_number: usize,
}

impl<'a> TraverseParents<'a> {
	pub fn new(y: usize, root: &'a mut NbtElement) -> Self {
		Self {
			cut: y >= root.height() || y == 0,
			node: Some(root),
			y,
			head: true,
			line_number: 1,
		}
	}

	fn step(&mut self) -> Option<()> {
		self.node = Some('m: {
			let node = self.node.take()?;
			match node.as_pattern_mut() {
				NbtPatternMut::Region(region) => {
					self.y -= 1;
					for value in region.children_mut() {
						let height = value.height();
						if self.y >= height {
							self.y -= height;
							self.line_number += value.true_height();
							continue;
						} else {
							self.line_number += 1;
							break 'm value;
						}
					}
				}
				NbtPatternMut::List(list) => {
					self.y -= 1;
					for value in list.children_mut() {
						let height = value.height();
						if self.y >= height {
							self.y -= height;
							self.line_number += value.true_height();
							continue;
						} else {
							self.line_number += 1;
							break 'm value;
						}
					}
				}
				NbtPatternMut::Compound(compound) => {
					self.y -= 1;
					for (_, value) in compound.children_mut() {
						let height = value.height();
						if self.y >= height {
							self.y -= height;
							self.line_number += value.true_height();
							continue;
						} else {
							self.line_number += 1;
							break 'm value;
						}
					}
				}
				NbtPatternMut::Chunk(chunk) => {
					self.y -= 1;
					for (_, value) in chunk.children_mut() {
						let height = value.height();
						if self.y >= height {
							self.y -= height;
							self.line_number += value.true_height();
							continue;
						} else {
							self.line_number += 1;
							break 'm value;
						}
					}
				}
				NbtPatternMut::ByteArray(array) => {
					self.line_number += self.y;
					break 'm array.get_mut(core::mem::replace(&mut self.y, 0) - 1)?;
				}
				NbtPatternMut::IntArray(array) => {
					self.line_number += self.y;
					break 'm array.get_mut(core::mem::replace(&mut self.y, 0) - 1)?;
				}
				NbtPatternMut::LongArray(array) => {
					self.line_number += self.y;
					break 'm array.get_mut(core::mem::replace(&mut self.y, 0) - 1)?;
				}
				_ => return None,
			}
			unsafe { panic_unchecked("Skipped everything in TraverseParents (somehow)") }
		});

		Some(())
	}

	fn extras(&self) -> (usize, Option<CompactString>, bool) {
		let node = unsafe {
			self.node
				.as_ref()
				.panic_unchecked("Expected a value for parent element")
		};
		if let Some(region) = node.as_region() {
			let mut remaining_y = self.y - 1;
			for (idx, element) in region.children().enumerate() {
				if remaining_y >= element.height() {
					remaining_y -= element.height();
					continue;
				} else {
					return (idx, None, remaining_y == 0);
				}
			}
		} else if let Some(list) = node.as_list() {
			let mut remaining_y = self.y - 1;
			for (idx, element) in list.children().enumerate() {
				if remaining_y >= element.height() {
					remaining_y -= element.height();
					continue;
				} else {
					return (idx, None, remaining_y == 0);
				}
			}
		} else if let Some(chunk) = node.as_chunk() {
			let mut remaining_y = self.y - 1;
			for (idx, (key, element)) in chunk.children().enumerate() {
				if remaining_y >= element.height() {
					remaining_y -= element.height();
					continue;
				} else {
					return (idx, Some(key.to_compact_string()), remaining_y == 0);
				}
			}
		} else if let Some(compound) = node.as_compound() {
			let mut remaining_y = self.y - 1;
			for (idx, (key, element)) in compound.children().enumerate() {
				if remaining_y >= element.height() {
					remaining_y -= element.height();
					continue;
				} else {
					return (idx, Some(key.to_compact_string()), remaining_y == 0);
				}
			}
		} else if let NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID = node.id() {
			return (self.y - 1, None, true);
		}
		unsafe { panic_unchecked("Expected parent element to be complex") }
	}

	#[allow(clippy::should_implement_trait)] // no
	pub fn next(
		&mut self,
	) -> Option<(
		Position,
		usize,
		Option<CompactString>,
		&mut NbtElement,
		usize,
	)> {
		if self.cut { return None }

		let head = core::mem::replace(&mut self.head, false);

		if !head {
			self.step();
		}

		let (idx, key, is_last) = self.extras();
		self.cut = is_last;
		let position = match (head, self.cut) {
			(true, false) => Position::First,
			(false, false) => Position::Middle,
			(false, true) => Position::Last,
			(true, true) => Position::Only,
		};

		Some((
			position,
			idx,
			key,
			&mut **self.node.as_mut()?,
			self.line_number,
		))
	}
}
