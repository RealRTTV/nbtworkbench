use crate::elements::element_type::NbtElement;
use crate::{panic_unchecked, OptionExt, Position};
use std::iter::Peekable;

/// Navigates a tree from the given route of an indices list, will cause UB on invalid lists. Typically used for operations which are saved and not direct clicking interaction.
#[must_use]
pub struct Navigate<'a, I: IntoIterator + ExactSizeIterator> {
	iter: Peekable<I::IntoIter>,
	at_head: bool,
	node: Option<(usize, Option<&'a str>, &'a mut NbtElement)>,
}

impl<'a, I: Iterator<Item = usize> + ExactSizeIterator> Navigate<'a, I> {
	pub fn new(iter: impl IntoIterator<IntoIter = I> + ExactSizeIterator, root: &'a mut NbtElement) -> Self {
		Self {
			iter: iter.into_iter().peekable(),
			at_head: true,
			node: Some((0, None, root)),
		}
	}

	fn step(&mut self) -> Option<()> {
		let (_, _, node) = self.node.take()?;
		let idx = self.iter.next()?;
		let (key, node) = unsafe {
			match node {
				NbtElement::Compound(compound) => compound.get_mut(idx).map(|(a, b)| (Some(a), b)),
				element => element.get_mut(idx).map(|x| (None, x)),
			}
			.panic_unchecked("Invalid index in indices iterator")
		};
		self.node = Some((idx, key, node));
		Some(())
	}

	#[allow(clippy::should_implement_trait)] // no
	pub fn next(&mut self) -> Option<(Position, usize, Option<&str>, &mut NbtElement)> {
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

		Some((position, *idx, *key, *element))
	}

	#[must_use]
	pub fn last(mut self) -> (usize, Option<&'a str>, &'a mut NbtElement) {
		while self.iter.peek().is_some() {
			self.step();
		}

		unsafe { self.node.panic_unchecked("List cannot be empty") }
	}
}

/// Navigates through an [`NbtElement`] tree using a y value.
#[must_use]
pub struct Traverse<'a> {
	node: Option<(usize, Option<&'a str>, &'a mut NbtElement)>,
	y: usize,
	cut: bool,
	head: bool,
}

impl<'a> Traverse<'a> {
	pub fn new(y: usize, root: &'a mut NbtElement) -> Self {
		Self {
			cut: y >= root.height() || y == 0,
			node: Some((0, None, root)),
			y,
			head: true,
		}
	}

	fn step(&mut self) -> Option<()> {
		let (_, _, node) = self.node.take()?;

		let (idx, key, new) = 'm: {
			match node {
				NbtElement::ByteArray(array) => {
					self.cut = true;
					let idx = core::mem::replace(&mut self.y, 0) - 1;
					(idx, None, unsafe { array.get_mut(idx).panic_unchecked("Expected element to contain next element") })
				}
				NbtElement::List(list) => {
					self.y -= 1;
					for (idx, value) in list.children_mut().enumerate() {
						let height = value.height();
						if self.y >= height {
							self.y -= height;
							continue;
						} else {
							self.cut = self.y == 0;
							break 'm (idx, None, value);
						}
					}
					unsafe { panic_unchecked("Expected element to contain next element") }
				}
				NbtElement::Compound(compound) => {
					self.y -= 1;
					for (idx, (key, value)) in compound.children_mut().enumerate() {
						let height = value.height();
						if self.y >= height {
							self.y -= height;
							continue;
						} else {
							self.cut = self.y == 0;
							break 'm (idx, Some(key.as_ref()), value);
						}
					}
					unsafe { panic_unchecked("Expected element to contain next element") }
				}
				NbtElement::IntArray(array) => {
					self.cut = true;
					let idx = core::mem::replace(&mut self.y, 0) - 1;
					(idx, None, unsafe { array.get_mut(idx).panic_unchecked("Expected element to contain next element") })
				}
				NbtElement::LongArray(array) => {
					self.cut = true;
					let idx = core::mem::replace(&mut self.y, 0) - 1;
					(idx, None, unsafe { array.get_mut(idx).panic_unchecked("Expected element to contain next element") })
				}
				_ => unsafe { panic_unchecked("Invalid type for tree traversal") },
			}
		};

		self.node = Some((idx, key, new));

		Some(())
	}

	#[allow(clippy::should_implement_trait)] // no
	pub fn next(&mut self) -> Option<(Position, usize, Option<&str>, &mut NbtElement)> {
		if self.cut {
			if self.head {
				self.head = false;
				return self.node.as_mut().map(|(a, b, c)| (Position::Only, *a, *b, &mut **c));
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
		Some((position, *idx, *key, *element))
	}

	#[must_use]
	pub fn last(mut self) -> Option<(usize, Option<&'a str>, &'a mut NbtElement)> {
		if self.cut && !self.head {
			return None;
		}
		while self.y > 0 {
			self.step();
		}
		self.node
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
	pub fn last(mut self) -> (usize, (usize, Option<&'a str>, &'a mut NbtElement)) {
		while self.inner.y > 0 {
			self.depth += 1;
			self.inner.step();
		}
		unsafe { self.inner.node.map(|x| (self.depth, x)).panic_unchecked("head is always initialized") }
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
}

impl<'a> TraverseParents<'a> {
	pub fn new(y: usize, root: &'a mut NbtElement) -> Self {
		Self {
			cut: y >= root.height() || y == 0,
			node: Some(root),
			y,
			head: true,
		}
	}

	fn step(&mut self) -> Option<()> {
		self.node = Some('m: {
			match self.node.take()? {
				NbtElement::ByteArray(array) => unsafe { array.get_mut(core::mem::replace(&mut self.y, 0) - 1).panic_unchecked("Expected element to contain next element") },
				NbtElement::List(list) => {
					self.y -= 1;
					for value in list.children_mut() {
						let height = value.height();
						if self.y >= height {
							self.y -= height;
							continue;
						} else {
							break 'm value;
						}
					}
					unsafe { panic_unchecked("Expected element to contain next element") }
				}
				NbtElement::Compound(compound) => {
					self.y -= 1;
					for (_, value) in compound.children_mut() {
						let height = value.height();
						if self.y >= height {
							self.y -= height;
							continue;
						} else {
							break 'm value;
						}
					}
					unsafe { panic_unchecked("Expected element to contain next element") }
				}
				NbtElement::IntArray(array) => unsafe { array.get_mut(core::mem::replace(&mut self.y, 0) - 1).panic_unchecked("Expected element to contain next element") },
				NbtElement::LongArray(array) => unsafe { array.get_mut(core::mem::replace(&mut self.y, 0) - 1).panic_unchecked("Expected element to contain next element") },
				_ => unsafe { panic_unchecked("Invalid type for tree traversal") },
			}
		});

		Some(())
	}

	fn extras(&self) -> (usize, Option<Box<str>>, bool) {
		match self.node.as_ref() {
			Some(NbtElement::ByteArray(_) | NbtElement::IntArray(_) | NbtElement::LongArray(_)) => (self.y - 1, None, true),
			Some(NbtElement::Compound(compound)) => {
				let mut remaining_y = self.y - 1;
				for (idx, (key, element)) in compound.children().enumerate() {
					if remaining_y >= element.height() {
						remaining_y -= element.height();
						continue;
					} else {
						return (idx, Some(key.clone()), remaining_y == 0);
					}
				}
				unsafe { panic_unchecked("Expected parent element to contain next element") }
			}
			Some(NbtElement::List(list)) => {
				let mut remaining_y = self.y - 1;
				for (idx, element) in list.children().enumerate() {
					if remaining_y >= element.height() {
						remaining_y -= element.height();
						continue;
					} else {
						return (idx, None, remaining_y == 0);
					}
				}
				unsafe { panic_unchecked("Expected parent element to contain next element") }
			}
			_ => unsafe { panic_unchecked("Expected parent element to be complex") },
		}
	}

	#[allow(clippy::should_implement_trait)] // no
	pub fn next(&mut self) -> Option<(Position, usize, Option<Box<str>>, &mut NbtElement)> {
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

		Some((position, idx, key, &mut **self.node.as_mut()?))
	}
}
