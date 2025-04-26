use std::ffi::OsStr;
use std::iter::Peekable;
use std::path::PathBuf;
use compact_str::{CompactString, ToCompactString};

use crate::elements::{NbtByteArray, NbtElement, NbtElementAndKey, NbtIntArray, NbtLongArray, NbtPatternMut};
use crate::render::WindowProperties;
use crate::util::{encompasses, encompasses_or_equal};
use crate::widget::SelectedText;
use crate::workbench::{FileUpdateSubscription, MarkedLineSlice, MarkedLines, WorkbenchAction};

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

	#[must_use]
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

		self.node
			.map(|(a, b, c)| (a, b, c, self.line_number))
			.expect("List cannot be empty")
	}
}

/// Navigates through an [`NbtElement`] tree using a y value.
#[must_use]
pub struct Traverse<'a> {
	node: Option<(usize, Option<CompactString>, &'a mut NbtElement)>,
	x: usize,
	y: usize,
	killed: bool,
	line_number: usize,
	depth: usize,
	indices: Vec<usize>,
}

impl<'a> Traverse<'a> {
	pub fn new(x: usize, y: usize, root: &'a mut NbtElement) -> Self {
		Self {
			killed: y >= root.height() || y == 0,
			node: Some((0, None, root)),
			x,
			y,
			line_number: 1,
			depth: 0,
			indices: Vec::new(),
		}
	}

	fn step(&mut self) -> Option<()> {
		let (_, _, node) = self.node.take()?;

		let (idx, key, new) = 'm: {
			match node.as_pattern_mut() {
				NbtPatternMut::Region(region) => {
					self.y -= 1;
					if region.is_grid_layout() {
						if self.x < 2 {
							self.killed = true;
							self.node = None;
							return None;
						}
						self.x -= 2;
						if self.x >= 32 {
							self.killed = true;
							self.node = None;
							return None;
						}
						let idx = self.y * 32 + self.x;
						for chunk in region.children().take(idx) {
							self.line_number += chunk.true_height();
						}
						self.line_number += 1;
						self.y = 0;
						self.killed = true;
						break 'm (idx, None, &mut region.chunks[idx]);
					} else {
						for (idx, value) in region.children_mut().enumerate() {
							let height = value.height();
							if self.y >= height {
								self.line_number += value.true_height();
								self.y -= height;
								continue;
							} else {
								self.killed = self.y == 0;
								self.line_number += 1;
								break 'm (idx, None, value);
							}
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
							self.killed = self.y == 0;
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
							self.killed = self.y == 0;
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
							self.killed = self.y == 0;
							self.line_number += 1;
							break 'm (idx, None, value);
						}
					}
				}
				NbtPatternMut::ByteArray(array) => {
					self.killed = true;
					let idx = core::mem::replace(&mut self.y, 0) - 1;
					self.line_number += idx + 1;
					break 'm (idx, None, array.get_mut(idx)?);
				}
				NbtPatternMut::IntArray(array) => {
					self.killed = true;
					let idx = core::mem::replace(&mut self.y, 0) - 1;
					self.line_number += idx + 1;
					break 'm (idx, None, array.get_mut(idx)?);
				}
				NbtPatternMut::LongArray(array) => {
					self.killed = true;
					let idx = core::mem::replace(&mut self.y, 0) - 1;
					self.line_number += idx + 1;
					break 'm (idx, None, array.get_mut(idx)?);
				}
				_ => return None,
			}

			self.killed = true;
			self.node = None;
			return None;
		};

		self.indices.push(idx);

		self.node = Some((idx, key, new));

		Some(())
	}

	#[must_use]
	#[allow(clippy::type_complexity)] // literally can't otherwise the compiler crashes... yeah...
	pub fn last(mut self) -> Option<(usize, (usize, Option<CompactString>, &'a mut NbtElement, usize, Box<[usize]>))> {
		while self.y > 0 && !self.killed {
			self.depth += 1;
			self.step();
		}
		self.node.map(|(a, b, c)| (self.depth, (a, b, c, self.line_number, self.indices.into_boxed_slice())))
	}
}

/// Navigates through an [`NbtElement`] tree using a y value.
/// This traversal will return parents with their child indices and keys with their parent's element.
#[must_use]
pub struct TraverseParents<'a> {
	node: Option<&'a mut NbtElement>,
	x: usize,
	y: usize,
	killed: bool,
	head: bool,
	line_number: usize,
}

impl<'a> TraverseParents<'a> {
	pub fn new(x: usize, y: usize, root: &'a mut NbtElement) -> Self {
		Self {
			killed: y >= root.height() || y == 0,
			node: Some(root),
			x,
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
					if region.is_grid_layout() {
						return None;
					} else {
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
			self.killed = true;
			self.node = None;
			return None
		});

		Some(())
	}

	#[must_use]
	fn extras(&mut self) -> (usize, Option<CompactString>, bool) {
		let Some(node) = self.node.as_ref() else { self.killed = true; return (0, None, true) };
		if let Some(region) = node.as_region() {
			if region.is_grid_layout() {
				self.y -= 1;
				if self.x < 2 {
					self.killed = true;
					return (0, None, true)
				}
				self.x -= 2;
				if self.x >= 32 {
					self.killed = true;
					return (0, None, true)
				}
				let idx = self.y * 32 + self.x;
				self.y = 0;
				return (idx, None, true)
			} else {
				let mut remaining_y = self.y - 1;
				for (idx, element) in region.children().enumerate() {
					if remaining_y >= element.height() {
						remaining_y -= element.height();
						continue;
					} else {
						return (idx, None, remaining_y == 0);
					}
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
		panic!("Expected parent element to be complex")
	}

	#[must_use]
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
		if self.killed { return None }

		let head = core::mem::replace(&mut self.head, false);

		if !head {
			self.step();
		}

		let (idx, key, is_last) = self.extras();
		if self.killed { return None }
		self.killed = is_last;
		let position = match (head, self.killed) {
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

pub enum DropFn {
	Dropped(usize, usize, Option<CompactString>, usize, Option<NbtElementAndKey>),
	Missed(NbtElementAndKey),
	InvalidType(NbtElementAndKey),
}

/// Yoinked from `itertools`.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Position {
	Only,
	First,
	Middle,
	Last,
}

#[must_use]
pub fn sum_indices<I: Iterator<Item = usize>>(indices: I, mut root: &NbtElement) -> usize {
	let mut total = 0;
	let mut indices = indices.peekable();
	while let Some(idx) = indices.next() {
		root = if let NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID = root.id() {
			total += 1 + idx;
			break;
		} else if let Some(list) = root.as_list() {
			total += 1 + list
				.children()
				.take(idx)
				.map(NbtElement::height)
				.sum::<usize>();
			if let Some(root) = list.get(idx) {
				root
			} else {
				break;
			}
		} else if let Some(compound) = root.as_compound() {
			total += 1 + compound
				.children()
				.take(idx)
				.map(|(_, b)| b)
				.map(NbtElement::height)
				.sum::<usize>();
			if let Some((_, root)) = compound.get(idx) {
				root
			} else {
				break;
			}
		} else if let Some(chunk) = root.as_chunk() {
			total += 1 + chunk
				.children()
				.take(idx)
				.map(|(_, b)| b)
				.map(NbtElement::height)
				.sum::<usize>();
			if let Some((_, root)) = chunk.get(idx) {
				root
			} else {
				break;
			}
		} else if let Some(region) = root.as_region() {
			total += 1 + region
				.children()
				.take(idx)
				.map(NbtElement::height)
				.sum::<usize>();
			if let Some(root) = region.get(idx) {
				root
			} else {
				break;
			}
		} else {
			total += root.height();
			if indices.peek().is_some() {
				panic!("tried to index non-indexable")
			} else {
				break;
			}
		};
	}
	total
}

pub fn recache_along_indices(indices: &[usize], parent: &mut NbtElement) {
	if let Some(region) = parent.as_region_mut() {
		if let Some((&idx, rest)) = indices.split_first() {
			recache_along_indices(rest, region.get_mut(idx).expect("expected valid index"));
		}
		region.recache();
	} else if let Some(array) = parent.as_byte_array_mut() {
		array.recache();
	} else if let Some(array) = parent.as_int_array_mut() {
		array.recache();
	} else if let Some(array) = parent.as_long_array_mut() {
		array.recache();
	} else if let Some(list) = parent.as_list_mut() {
		if let Some((&idx, rest)) = indices.split_first() {
			recache_along_indices(rest, list.get_mut(idx).expect("expected valid index"));
		}
		list.recache();
	} else if let Some(compound) = parent.as_compound_mut() {
		if let Some((&idx, rest)) = indices.split_first() {
			recache_along_indices(rest, compound.get_mut(idx).expect("expected valid index").1, );
		}
		compound.recache();
	} else if let Some(chunk) = parent.as_chunk_mut() {
		if let Some((&idx, rest)) = indices.split_first() {
			recache_along_indices(rest, chunk.get_mut(idx).expect("expected valid index").1, );
		}
		chunk.recache();
	}
}

pub struct MutableIndices<'m2> {
	is_empty: bool,
	subscription: &'m2 mut Option<FileUpdateSubscription>,
	selected_text: &'m2 mut Option<SelectedText>,
}

impl<'m1, 'm2: 'm1> MutableIndices<'m2> {
	#[must_use]
	pub fn new(subscription: &'m2 mut Option<FileUpdateSubscription>, selected_text: &'m2 mut Option<SelectedText>) -> Self {
		Self {
			is_empty: false,
			subscription,
			selected_text,
		}
	}

	#[must_use]
	pub fn empty() -> &'static mut Self {
		static mut EMPTY_SUBSCRIPTION: Option<FileUpdateSubscription> = None;
		static mut EMPTY_SELECTED_TEXT: Option<SelectedText> = None;
		static mut EMPTY: MutableIndices<'static> = MutableIndices {
			is_empty: true,
			subscription: unsafe { &mut EMPTY_SUBSCRIPTION },
			selected_text: unsafe { &mut EMPTY_SELECTED_TEXT },
		};

		unsafe { &mut EMPTY }
	}

	pub fn apply<F: FnMut(&mut Box<[usize]>) -> bool>(&mut self, mut f: F) {
		if self.is_empty {
			return;
		}

		if let Some(subscription) = self.subscription.as_mut() && f(&mut subscription.indices) {
			self.subscription.take();
		}

		if let Some(selected_text) = self.selected_text.as_mut() && f(&mut selected_text.indices) {
			self.subscription.take();
		}
	}

	/// required to be here because it is the only mutable reference to `FileUpdateSubscription` available when *_element-ing
	pub fn set_subscription(&mut self, subscription: Option<FileUpdateSubscription>) {
		if self.is_empty {
			return;
		}

		*self.subscription = subscription;
	}

	pub fn set_selected_text_indices(&mut self, indices: Box<[usize]>) {
		if self.is_empty {
			return;
		}

		if let Some(selected_text) = self.selected_text.as_mut() {
			selected_text.indices = indices;
		}
	}
}

/// Properly adds an element under the specified indices, updating the following relevant data
/// - Subscription Indices
/// - Bookmarked Lines
/// - Heights and True Heights
/// - Workbench Actions
/// - Horizontal Scroll
///
/// # Examples
/// ```rust
/// let workbench = ...;
/// let tab = tab_mut!(workbench);
/// let action = add_element(
///     &mut tab.value,
///     None,
///     NbtElement::from_str(
///         r#"{"registry":"minecraft:item","value":"minecraft:stone"}"#
///     ).unwrap().1,
///     Box::new([0]),
///     &mut tab.bookmarks,
///     &mut self.subscription
/// )?;
/// tab.append_to_history(action);
/// ```
pub fn add_element<'m1, 'm2: 'm1>(root: &mut NbtElement, key: Option<CompactString>, value: NbtElement, indices: Box<[usize]>, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<WorkbenchAction> {
	let (&last, rem) = indices.split_last().expect("You can't remove the head!");

	let (_, _, parent, mut line_number) = Navigate::new(rem.iter().copied(), root).last();
	let (old_parent_height, old_parent_true_height) = (parent.height(), parent.true_height());
	// SAFETY: we have updated all the relevant data
	let old_value = match unsafe { parent.insert(last, (key, value)) } {
		Ok(Some(old)) => Some(old),
		Ok(None) => None,
		Err(_) => return None,
	};
	let (parent_height, parent_true_height) = (parent.height(), parent.true_height());
	let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
	let (_old_height, old_true_height) = (old_value.as_ref().map(NbtElement::height), old_value.as_ref().map(NbtElement::true_height));
	let been_replaced = old_true_height.is_some();

	for n in 0..last {
		line_number += parent[n].true_height();
	}
	line_number += 1;
	bookmarks.remove(line_number..line_number + old_true_height.unwrap_or(0));
	bookmarks[line_number..].increment(diff, true_diff);

	mutable_indices.apply(|indices| {
		if encompasses_or_equal(rem, &indices) {
			if indices[rem.len()] <= last && !been_replaced {
				indices[rem.len()] += 1;
			}
		}
		false
	});

	recache_along_indices(rem, root);

	let action = if let Some(old_value) = old_value {
		WorkbenchAction::Replace { indices, value: (None, old_value) }
	} else {
		WorkbenchAction::Add { indices }
	};

	Some(action)
}

/// Properly removes an element under the specified indices, updating the following relevant data
/// - Subscription Indices
/// - Bookmarked Lines
/// - Heights and True Heights
/// - Workbench Actions
/// - Horizontal Scroll
///
/// # Examples
/// ```rust
/// let workbench = ...;
/// let tab = tab_mut!(workbench);
/// let result = remove_element(
///     &mut tab.value,
///     Box::new([0]),
///     &mut tab.bookmarks,
///     &mut workbench.subscription
/// )?;
/// tab.append_to_history(result.into_action());
/// ```
pub fn remove_element<'m1, 'm2: 'm1>(root: &mut NbtElement, indices: Box<[usize]>, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<RemoveElementResult> {
	let (&last, rem) = indices.split_last()?;
	let (_, _, parent, mut line_number) = Navigate::new(rem.iter().copied(), root).last();
	for n in 0..last {
		line_number += parent[n].true_height();
	}
	line_number += 1;
	let (old_parent_height, old_parent_true_height) = (parent.height(), parent.true_height());
	// SAFETY: we have updated all the relevant data
	let (key, value) = unsafe { parent.remove(last) }?;
	let (height, true_height) = (value.height(), value.true_height());
	let (parent_height, parent_true_height) = (parent.height(), parent.true_height());
	let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
	let been_replaced = !(height == diff && true_height == true_diff);
	bookmarks.remove(line_number..line_number);
	bookmarks[line_number..].decrement(diff, true_diff);

	mutable_indices.apply(|mutable_indices| {
		if encompasses_or_equal(&indices, &mutable_indices) {
			return true
		} else if encompasses(rem, &mutable_indices) {
			if mutable_indices[rem.len()] >= last && !been_replaced {
				mutable_indices[rem.len()] -= 1;
			}
		}
		false
	});

	recache_along_indices(rem, root);

	Some(RemoveElementResult {
		indices,
		kv: (key, value),
		replaces: been_replaced,
	})
}

/// Properly replaces an element under the specified indices, updating the following relevant data
/// - Subscription Indices
/// - Bookmarked Lines
/// - Heights and True Heights
/// - Workbench Actions
/// - Horizontal Scroll
///
/// # Examples
/// ```rust
/// let workbench = ...;
/// let tab = tab_mut!(workbench);
/// let result = replace_element(
///     &mut tab.value,
///     NbtElement::from_str(
///         r#"{"registry":"minecraft:item","value":"minecraft:stone"}"#
///     ).unwrap(),
///     Box::new([0]),
///     &mut tab.bookmarks,
///     &mut workbench.subscription
/// )?;
/// tab.append_to_history(result.into_action());
/// ```
pub fn replace_element<'m1, 'm2: 'm1>(root: &mut NbtElement, value: NbtElementAndKey, indices: Box<[usize]>, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<RemoveElementResult> {
	let Some((&last, rem)) = indices.split_last() else {
		return if root.id() == value.1.id() {
			bookmarks.remove(..);

			Some(RemoveElementResult {
				indices: Box::new([]),
				kv: (None, core::mem::replace(root, value.1)),
				replaces: true,
			})
		} else {
			None
		}
	};

	let (_, _, parent, mut line_number) = Navigate::new(rem.iter().copied(), root).last();
	for n in 0..last {
		line_number += parent[n].true_height();
	}
	line_number += 1;

	let (old_parent_height, old_parent_true_height) = (parent.height(), parent.true_height());
	// SAFETY: we have updated all the relevant data
	let (old_key, old_value) = unsafe { parent.replace_key_value(last, value) }?;
	let (_old_height, old_true_height) = (old_value.height(), old_value.true_height());
	let (parent_height, parent_true_height) = (parent.height(), parent.true_height());
	let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
	bookmarks.remove(line_number..line_number + old_true_height);
	bookmarks[line_number..].increment(diff, true_diff);

	mutable_indices.apply(|mutable_indices| encompasses_or_equal(&indices, &mutable_indices));

	recache_along_indices(rem, root);

	Some(RemoveElementResult {
		indices,
		kv: (old_key, old_value),
		replaces: true,
	})
}

/// Properly swaps two elements under their specified indices (requires them to be at the same depth), updating the following relevant data
/// - Subscription Indices
/// - Bookmarked Lines
/// - Heights and True Heights
/// - Workbench Actions
/// - Horizontal Scroll
///
/// # Examples
/// ```rust
/// let workbench = ...;
/// let tab = tab_mut!(workbench);
/// let action = swap_element(
///     &mut tab.value,
///     None,
///     Box::new([]),
///     0,
///     1,
///     &mut tab.bookmarks,
///     &mut self.subscription
/// ).into_action();
/// tab.append_to_history(action);
/// ```
pub fn same_depth_swap_element<'m1, 'm2: 'm1>(root: &mut NbtElement, parent_indices: Box<[usize]>, a: usize, b: usize, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> SwapElementResult {
	let (a, b) = if a <= b { (a, b) } else { (b, a) };
	let parent_y = sum_indices(parent_indices.iter().copied(), root);
	let (_, _, parent, parent_line_number) = Navigate::new(parent_indices.iter().copied(), root).last();

	let mut a_line_number = parent_line_number;
	let mut a_y = parent_y;
	let (a_height, a_true_height) = (parent[a].height(), parent[a].true_height());
	for n in 0..a {
		let sibling = &parent[n];
		a_line_number += sibling.true_height();
		a_y += sibling.height();
	}
	a_line_number += 1;
	a_y += 1;

	let mut b_line_number = parent_line_number;
	let mut b_y = parent_y;
	let (b_height, b_true_height) = (parent[b].height(), parent[b].true_height());
	for n in 0..b {
		let sibling = &parent[n];
		b_line_number += sibling.true_height();
		b_y += sibling.height();
	}
	b_line_number += 1;
	b_y += 1;

	let mut a_bookmarks = bookmarks.remove(a_line_number..a_line_number + a_true_height);
	let mut b_bookmarks = bookmarks.remove(b_line_number..b_line_number + b_true_height);
	MarkedLineSlice::from_marked_lines_mut(&mut a_bookmarks).decrement(a_y, a_line_number);
	MarkedLineSlice::from_marked_lines_mut(&mut b_bookmarks).decrement(b_y, b_line_number);

	bookmarks[b_line_number..].decrement(b_height, b_true_height);
	bookmarks[a_line_number..].decrement(a_height, a_true_height);

	MarkedLineSlice::from_marked_lines_mut(&mut a_bookmarks).increment(b_y, b_line_number);
	MarkedLineSlice::from_marked_lines_mut(&mut b_bookmarks).increment(a_y, a_line_number);
	bookmarks.add_bookmarks(a_bookmarks);
	bookmarks.add_bookmarks(b_bookmarks);

	mutable_indices.apply(|indices| {
		if encompasses(&parent_indices, &indices) {
			let sibling = &mut indices[parent_indices.len()];
			if *sibling == a {
				*sibling = b;
			} else if *sibling == b {
				*sibling = a;
			}
		}
		false
	});

	parent.swap(a, b);

	recache_along_indices(&parent_indices, root);

	SwapElementResult {
		parent: parent_indices,
		a,
		b,
	}
}

/// Properly renames an element under its specified indices, keeping these caches correct:
/// - Subscription Indices
/// - Bookmarked Lines
/// - Heights and True Heights
/// - Workbench Actions
/// - Horizontal Scroll
///
/// # Examples
/// ```rust
/// let workbench = ...;
/// let tab = tab_mut!(workbench);
/// let action = rename_element(
///     &mut tab.value,
///     Box::new([]),
///     Some("id".to_compact_string()),
///     Some("minecraft:stone".to_compact_string()),
///     &mut tab.path,
///     &mut tab.name,
///     window_properties,
/// )?.into_action();
/// tab.append_to_history(action);
/// ```
pub fn rename_element(root: &mut NbtElement, indices: Box<[usize]>, key: Option<CompactString>, value: Option<CompactString>, path: &mut Option<PathBuf>, name: &mut Box<str>, window_properties: &mut WindowProperties) -> Option<RenameElementResult> {
	if key.is_none() && value.is_none() {
		return None;
	}

	if let Some((&last, rem)) = indices.split_last() {
		let element = Navigate::new(rem.iter().copied(), root).last().2;
		let old_key = if let Some(key) = key {
			if let Some(compound) = element.as_compound_mut() {
				Some(compound.entries.update_key(last, key.clone()).unwrap_or(key))
			} else if let Some(chunk) = element.as_chunk_mut() {
				Some(chunk.entries.update_key(last, key.clone()).unwrap_or(key))
			} else {
				None
			}
		} else {
			None
		};

		let old_value = if let Some(value) = value {
			// no drops dw, well except for the value, but that's a simple thing dw
			let child = element
				.get_mut(last)
				.expect("Last index was valid");
			let (previous, success) = child
				.set_value(value)
				.expect("Type of indices tail can accept value writes");
			if !success || previous == child.value().0 { return None; }
			Some(previous)
		} else {
			None
		};

		Some(RenameElementResult {
			indices,
			key: old_key,
			value: old_value,
		})
	} else if let Some(key) = key && value.is_none() {
		if path.as_ref().map(|path| path.as_os_str().to_string_lossy()).as_deref().unwrap_or(&name) == key {
			return None;
		}
		let buf = PathBuf::from(key);
		if let Some(new_name) = buf
			.file_name()
			.and_then(OsStr::to_str)
			.map(ToOwned::to_owned)
		{
			window_properties.window_title(&format!("{new_name} - NBT Workbench"));
			let old_name = core::mem::replace(name, new_name.into_boxed_str());
			Some(RenameElementResult {
				indices: Box::new([]),
				key: None,
				value: Some(
					path.replace(buf)
						.as_deref()
						.and_then(|path| path.to_str())
						.map(|str| str.to_compact_string())
						.unwrap_or_else(|| old_name.to_compact_string()),
				),
			})
		} else {
			None
		}
	} else {
		None
	}
}

#[derive(Clone)]
pub struct RemoveElementResult {
	indices: Box<[usize]>,
	kv: NbtElementAndKey,
	replaces: bool,
}

impl RemoveElementResult {
	#[must_use]
	pub fn into_raw(self) -> (Box<[usize]>, NbtElementAndKey, bool) {
		(self.indices, self.kv, self.replaces)
	}

	#[must_use]
	pub fn into_action(self) -> WorkbenchAction {
		if self.replaces {
			WorkbenchAction::Replace {
				indices: self.indices,
				value: self.kv,
			}
		} else {
			WorkbenchAction::Remove {
				element: self.kv,
				indices: self.indices,
			}
		}
	}
}

#[derive(Clone)]
pub struct SwapElementResult {
	parent: Box<[usize]>,
	a: usize,
	b: usize,
}

impl SwapElementResult {
	#[must_use]
	pub fn as_raw(&self) -> (&[usize], usize, usize) { (&self.parent, self.a, self.b) }

	#[must_use]
	pub fn into_raw(self) -> (Box<[usize]>, usize, usize) {
		(self.parent, self.a, self.b)
	}

	#[must_use]
	pub fn into_action(self) -> WorkbenchAction {
		WorkbenchAction::Swap {
			parent: self.parent,
			a: self.a,
			b: self.b,
		}
	}
}

#[derive(Clone)]
pub struct RenameElementResult {
	indices: Box<[usize]>,
	key: Option<CompactString>,
	value: Option<CompactString>
}

impl RenameElementResult {
	#[must_use]
	pub fn into_raw(self) -> (Box<[usize]>, Option<CompactString>, Option<CompactString>) {
		(self.indices, self.key, self.value)
	}

	#[must_use]
	pub fn into_action(self) -> WorkbenchAction {
		WorkbenchAction::Rename {
			indices: self.indices,
			key: self.key,
			value: self.value,
		}
	}
}
