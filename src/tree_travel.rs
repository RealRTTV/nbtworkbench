use compact_str::{CompactString, ToCompactString};
use std::iter::Peekable;

use crate::elements::chunk::{NbtChunk, NbtRegion};
use crate::elements::compound::NbtCompound;
use crate::elements::element_type::{NbtByteArray, NbtElement, NbtIntArray, NbtLongArray};
use crate::elements::list::NbtList;
use crate::{panic_unchecked, OptionExt, Position};

/// Navigates a tree from the given route of an indices list, will cause UB on invalid lists. Typically used for operations which are saved and not direct clicking interaction.
#[must_use]
pub struct Navigate<'a, I: IntoIterator + ExactSizeIterator> {
	iter: Peekable<I::IntoIter>,
	at_head: bool,
	node: Option<(usize, Option<&'a str>, &'a mut NbtElement)>,
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
		let (key, node) = unsafe {
			if node.id() == NbtCompound::ID {
				for (_, child) in node.data.compound.children().take(idx) {
					self.line_number += child.true_height();
				}
				self.line_number += 1;
				node.data.compound.get_mut(idx).map(|(a, b)| (Some(a), b))
			} else {
				for child in node.children().unwrap_unchecked().unwrap_unchecked().take(idx) {
					self.line_number += child.true_height();
				}
				self.line_number += 1;
				node.get_mut(idx).map(|x| (None, x))
			}
			.panic_unchecked("Invalid index in indices iterator")
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

		Some((position, *idx, *key, *element, self.line_number))
	}

	#[must_use]
	pub fn last(mut self) -> (usize, Option<&'a str>, &'a mut NbtElement, usize) {
		while self.iter.peek().is_some() {
			self.step();
		}

		unsafe { self.node.map(|(a, b, c)| (a, b, c, self.line_number)).panic_unchecked("List cannot be empty") }
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

		let (idx, key, new) = unsafe {
			'm: {
				match node.id() {
					NbtRegion::ID => {
						self.y -= 1;
						for (idx, value) in node.data.region.children_mut().enumerate() {
							let height = value.height();
							if self.y >= height {
								self.line_number += value.true_height();
								self.y -= height;
								continue;
							} else {
								self.cut = self.y == 0;
								self.line_number += 1;
								break 'm (idx, Some(value.data.chunk.z.to_compact_string()), value);
							}
						}
						panic_unchecked("Expected element to contain next element")
					}
					NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => {
						self.cut = true;
						let idx = core::mem::replace(&mut self.y, 0) - 1;
						self.line_number += idx + 1;
						(idx, None, node.data.byte_array.get_mut(idx).panic_unchecked("Expected element to contain next element"))
					}
					NbtList::ID => {
						self.y -= 1;
						for (idx, value) in node.data.list.children_mut().enumerate() {
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
						panic_unchecked("Expected element to contain next element")
					}
					NbtCompound::ID => {
						self.y -= 1;
						for (idx, (key, value)) in node.data.compound.children_mut().enumerate() {
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
						panic_unchecked("Expected element to contain next element")
					}
					NbtChunk::ID => {
						self.y -= 1;
						for (idx, (key, value)) in node.data.chunk.inner.children_mut().enumerate() {
							let height = value.height();
							if self.y >= height {
								self.line_number += value.true_height();
								self.y -= height;
								continue;
							} else {
								self.cut = self.y == 0;
								self.line_number += 1;
								break 'm (idx, Some(key.into()), value);
							}
						}
						panic_unchecked("Expected element to contain next element")
					}
					_ => panic_unchecked("Invalid type for tree traversal"),
				}
			}
		};

		self.node = Some((idx, key, new));

		Some(())
	}

	#[allow(clippy::should_implement_trait)] // no
	pub fn next(&mut self) -> Option<(Position, usize, Option<CompactString>, &mut NbtElement, usize)> {
		if self.cut {
			if self.head {
				self.head = false;
				return self.node.as_mut().map(|(idx, key, value)| (Position::Only, *idx, key.clone(), &mut **value, self.line_number));
			}
			return None;
		}

		let head = core::mem::replace(&mut self.head, false);

		if !head {
			self.step();
		}

		let tail = self.cut;
		let position = match (head, tail) {
			(true, false) => Position::First,
			(false, false) => Position::Middle,
			(false, true) => Position::Last,
			(true, true) => Position::Only,
		};

		let (idx, key, element) = self.node.as_mut()?;
		Some((position, *idx, key.clone(), *element, self.line_number))
	}

	#[must_use]
	pub fn last(mut self) -> Option<(usize, Option<CompactString>, &'a mut NbtElement, usize)> {
		if self.cut && !self.head {
			return None;
		}
		while self.y > 0 {
			self.step();
		}
		self.node.map(|(a, b, c)| (a, b, c, self.line_number))
	}

	pub const fn enumerate(self) -> EnumeratedTraverse<'a> {
		EnumeratedTraverse { inner: self, depth: 0 }
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
	pub fn last(mut self) -> (usize, (usize, Option<CompactString>, &'a mut NbtElement, usize)) {
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
		self.node = Some(unsafe {
			'm: {
				let node = self.node.take()?;
				match node.id() {
					NbtRegion::ID => {
						self.y -= 1;
						for value in node.data.region.children_mut() {
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
						panic_unchecked("Expected element to contain next element")
					}
					NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => {
						self.line_number += self.y;
						node.data
							.byte_array
							.get_mut(core::mem::replace(&mut self.y, 0) - 1)
							.panic_unchecked("Expected element to contain next element")
					}
					NbtList::ID => {
						self.y -= 1;
						for value in node.data.list.children_mut() {
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
						panic_unchecked("Expected element to contain next element")
					}
					NbtCompound::ID => {
						self.y -= 1;
						for (_, value) in node.data.compound.children_mut() {
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
						panic_unchecked("Expected element to contain next element")
					}
					NbtChunk::ID => {
						self.y -= 1;
						for (_, value) in node.data.chunk.inner.children_mut() {
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
						panic_unchecked("Expected element to contain next element")
					}
					_ => panic_unchecked("Invalid type for tree traversal"),
				}
			}
		});

		Some(())
	}

	fn extras(&self) -> (usize, Option<CompactString>, bool) {
		let node = unsafe { self.node.as_ref().panic_unchecked("Expected a value for parent element") };
		unsafe {
			match node.id() {
				NbtRegion::ID => {
					let mut remaining_y = self.y - 1;
					for (idx, element) in node.data.region.children().enumerate() {
						if remaining_y >= element.height() {
							remaining_y -= element.height();
							continue;
						} else {
							return (idx, Some(element.as_chunk_unchecked().x.to_compact_string()), remaining_y == 0);
						}
					}
					panic_unchecked("Expected parent element to contain next element")
				}
				NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID => (self.y - 1, None, true),
				NbtCompound::ID => {
					let mut remaining_y = self.y - 1;
					for (idx, (key, element)) in node.data.compound.children().enumerate() {
						if remaining_y >= element.height() {
							remaining_y -= element.height();
							continue;
						} else {
							return (idx, Some(key.into()), remaining_y == 0);
						}
					}
					panic_unchecked("Expected parent element to contain next element")
				}
				NbtChunk::ID => {
					let mut remaining_y = self.y - 1;
					for (idx, (key, element)) in node.data.chunk.inner.children().enumerate() {
						if remaining_y >= element.height() {
							remaining_y -= element.height();
							continue;
						} else {
							return (idx, Some(key.into()), remaining_y == 0);
						}
					}
					panic_unchecked("Expected parent element to contain next element")
				}
				NbtList::ID => {
					let mut remaining_y = self.y - 1;
					for (idx, element) in node.data.list.children().enumerate() {
						if remaining_y >= element.height() {
							remaining_y -= element.height();
							continue;
						} else {
							return (idx, None, remaining_y == 0);
						}
					}
					panic_unchecked("Expected parent element to contain next element")
				}
				_ => panic_unchecked("Expected parent element to be complex"),
			}
		}
	}

	#[allow(clippy::should_implement_trait)] // no
	pub fn next(&mut self) -> Option<(Position, usize, Option<CompactString>, &mut NbtElement, usize)> {
		if self.cut {
			return None;
		}

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

		Some((position, idx, key, &mut **self.node.as_mut()?, self.line_number))
	}
}
