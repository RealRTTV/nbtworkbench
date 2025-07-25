#![allow(dead_code)]

use compact_str::{CompactString, ToCompactString};
use thiserror::Error;

use crate::elements::element::NbtElement;
use crate::tree::{Indices, OwnedIndices};

pub struct NavigationInformation<'a> {
	pub idx: Option<usize>,
	pub key: Option<&'a str>,
	pub element: &'a NbtElement,
	pub line_number: usize,
	pub true_line_number: usize,
}

impl<'a> NavigationInformation<'a> {
	pub fn from(mut element: &'a NbtElement, indices: &Indices) -> Result<Self, NavigationError> {
		let mut line_number = 0;
		let mut true_line_number = 1;
		let mut key = None;

		for idx in indices {
			let len = element.len().ok_or_else(|| NavigationError::ParentWasPrimitive { indices: indices.to_owned() })?;

			if idx >= len {
				return Err(NavigationError::IndexOutOfBounds { idx, indices: indices.to_owned() }.into());
			}

			line_number += 1;
			true_line_number += 1;
			for jdx in 0..idx {
				let sibling = &element[jdx];
				line_number += sibling.height();
				true_line_number += sibling.true_height();
			}

			let (k, v) = element.get(idx).ok_or_else(|| NavigationError::IndexOutOfBounds { idx, indices: indices.to_owned() })?;
			key = k;
			element = v;
		}

		Ok(Self {
			idx: indices.last(),
			key,
			element,
			line_number,
			true_line_number,
		})
	}
}

pub struct NavigationInformationMut<'a> {
	pub idx: Option<usize>,
	pub key: Option<&'a str>,
	pub element: &'a mut NbtElement,
	pub line_number: usize,
	pub true_line_number: usize,
}

impl<'a> NavigationInformationMut<'a> {
	pub fn from(mut element: &'a mut NbtElement, indices: &Indices) -> Result<Self, NavigationError> {
		let mut line_number = 0;
		let mut true_line_number = 1;
		let mut key = None;

		for idx in indices {
			let len = element.len().ok_or_else(|| NavigationError::ParentWasPrimitive { indices: indices.to_owned() })?;

			if idx >= len {
				return Err(NavigationError::IndexOutOfBounds { idx, indices: indices.to_owned() }.into());
			}

			line_number += 1;
			true_line_number += 1;
			for jdx in 0..idx {
				let sibling = &element[jdx];
				line_number += sibling.height();
				true_line_number += sibling.true_height();
			}

			let (k, v) = element.get_mut(idx).ok_or_else(|| NavigationError::IndexOutOfBounds { idx, indices: indices.to_owned() })?;
			key = k;
			element = v;
		}

		Ok(Self {
			idx: indices.last(),
			key,
			element,
			line_number,
			true_line_number,
		})
	}
}

#[derive(Error, Debug)]
pub enum NavigationError {
	#[error("Tried to index parent node @ {indices} but found out it was primitive.")]
	ParentWasPrimitive { indices: OwnedIndices },
	#[error("Tried to index {nth} child node from parent @ {indices}.", nth = crate::util::nth(.idx + 1))]
	IndexOutOfBounds { idx: usize, indices: OwnedIndices },
}

pub struct ParentNavigationInformation<'nbt, 'indices> {
	pub idx: usize,
	pub key: Option<&'nbt str>,
	pub parent: &'nbt NbtElement,
	pub line_number: usize,
	pub true_line_number: usize,
	pub parent_indices: &'indices Indices,
}

impl<'nbt, 'indices> ParentNavigationInformation<'nbt, 'indices> {
	pub fn from(mut parent: &'nbt NbtElement, indices: &'indices Indices) -> Result<Self, ParentNavigationError> {
		let (last, parent_indices) = indices.split_last().ok_or(ParentNavigationError::EmptyIndices)?;

		let mut line_number = 0;
		let mut true_line_number = 1;

		for idx in parent_indices {
			let len = parent.len().ok_or_else(|| NavigationError::ParentWasPrimitive { indices: parent_indices.to_owned() })?;

			if last >= len {
				return Err(NavigationError::IndexOutOfBounds { idx: last, indices: parent_indices.to_owned() }.into());
			}

			line_number += 1;
			true_line_number += 1;
			for jdx in 0..idx {
				let sibling = &parent[jdx];
				line_number += sibling.height();
				true_line_number += sibling.true_height();
			}

			parent = &parent[idx];
		}

		{
			let len = parent.len().ok_or_else(|| NavigationError::ParentWasPrimitive { indices: parent_indices.to_owned() })?;

			if last >= len {
				return Err(NavigationError::IndexOutOfBounds { idx: last, indices: parent_indices.to_owned() }.into());
			}

			line_number += 1;
			true_line_number += 1;
			for jdx in 0..last {
				let sibling = &parent[jdx];
				line_number += sibling.height();
				true_line_number += sibling.true_height();
			}
		}

		Ok(Self {
			idx: last,
			key: parent.get(last).and_then(|(a, _)| a),
			parent,
			line_number,
			true_line_number,
			parent_indices,
		})
	}
}

pub struct ParentNavigationInformationMut<'nbt, 'indices> {
	pub idx: usize,
	pub key: Option<CompactString>,
	pub parent: &'nbt mut NbtElement,
	pub line_number: usize,
	pub true_line_number: usize,
	pub parent_indices: &'indices Indices,
}

impl<'nbt, 'indices> ParentNavigationInformationMut<'nbt, 'indices> {
	pub fn from(mut parent: &'nbt mut NbtElement, indices: &'indices Indices) -> Result<Self, ParentNavigationError> {
		let (last, parent_indices) = indices.split_last().ok_or(ParentNavigationError::EmptyIndices)?;

		let mut line_number = 0;
		let mut true_line_number = 1;

		for idx in parent_indices {
			let len = parent.len().ok_or_else(|| NavigationError::ParentWasPrimitive { indices: parent_indices.to_owned() })?;

			if last >= len {
				return Err(NavigationError::IndexOutOfBounds { idx: last, indices: parent_indices.to_owned() }.into());
			}

			line_number += 1;
			true_line_number += 1;
			for jdx in 0..idx {
				let sibling = &parent[jdx];
				line_number += sibling.height();
				true_line_number += sibling.true_height();
			}

			parent = &mut parent[idx];
		}

		{
			let len = parent.len().ok_or_else(|| NavigationError::ParentWasPrimitive { indices: parent_indices.to_owned() })?;

			if last >= len {
				return Err(NavigationError::IndexOutOfBounds { idx: last, indices: parent_indices.to_owned() }.into());
			}

			line_number += 1;
			true_line_number += 1;
			for jdx in 0..last {
				let sibling = &parent[jdx];
				line_number += sibling.height();
				true_line_number += sibling.true_height();
			}
		}

		Ok(Self {
			idx: last,
			key: parent.get(last).and_then(|(a, _)| a.map(|x| x.to_compact_string())),
			parent,
			line_number,
			true_line_number,
			parent_indices,
		})
	}
}

#[derive(Error, Debug)]
pub enum ParentNavigationError {
	#[error("Indices were empty, root has no parent.")]
	EmptyIndices,
	#[error(transparent)]
	Navigation(#[from] NavigationError),
}

pub struct IterativeNavigationInformationMut<'nbt, 'indices> {
	head: bool,
	element: &'nbt mut NbtElement,
	line_number: usize,
	true_line_number: usize,
	indices: &'indices Indices,
}

impl<'nbt, 'indices> IterativeNavigationInformationMut<'nbt, 'indices> {
	/// # Safety
	///
	/// All usages of the &mut [NbtElement] reference must not modify the indices of its children,
	/// For example, [super::add_element] is not permitted, however [super::open_element] is.
	///
	/// Additionally, these values cannot be stored anywhere in memory because of aliasing.
	#[must_use]
	pub unsafe fn new(element: &'nbt mut NbtElement, indices: &'indices Indices) -> Self {
		Self {
			head: false,
			element,
			line_number: 0,
			true_line_number: 1,
			indices,
		}
	}
}

pub struct IterativeNavigationInformationMutItem<'nbt> {
	pub element: &'nbt mut NbtElement,
	pub line_number: usize,
	pub true_line_number: usize,
	pub idx: Option<usize>,
}

impl<'nbt, 'indices> Iterator for IterativeNavigationInformationMut<'nbt, 'indices> {
	type Item = IterativeNavigationInformationMutItem<'nbt>;

	fn next(&mut self) -> Option<Self::Item> {
		if !core::mem::replace(&mut self.head, true) {
			return Some(Self::Item {
				element: unsafe { std::ptr::read(&raw const self.element) },
				line_number: self.line_number,
				true_line_number: self.true_line_number,
				idx: None,
			})
		}

		let (idx, rest) = self.indices.split_first()?;
		self.indices = rest;

		self.line_number += 1;
		self.true_line_number += 1;
		for jdx in 0..idx {
			let sibling = self.element[jdx].as_nonnull()?;
			self.line_number += sibling.height();
			self.true_line_number += sibling.true_height();
		}

		let (element, child_element) = unsafe {
			let duplicate_reference = std::ptr::read(&raw const self.element);
			let child_reference = core::mem::transmute::<_, &'nbt mut NbtElement>(self.element[idx].as_nonnull_mut()?);
			(duplicate_reference, child_reference)
		};

		self.element = child_element;

		Some(Self::Item {
			element,
			line_number: self.line_number,
			true_line_number: self.true_line_number,
			idx: Some(idx),
		})
	}
}

pub struct ParentIterativeNavigationInformationMut<'nbt, 'indices> {
	head: bool,
	element: &'nbt mut NbtElement,
	line_number: usize,
	true_line_number: usize,
	indices: &'indices Indices,
}

impl<'nbt, 'indices> ParentIterativeNavigationInformationMut<'nbt, 'indices> {
	/// # Safety
	///
	/// All usages of the &mut [`NbtElement`] reference must not modify the indices of its children,
	/// For example, [`super::add_element`] is not permitted, however [`super::open_element`] is.
	///
	/// Additionally, these values cannot be stored anywhere in memory because of aliasing.
	#[must_use]
	pub unsafe fn new(element: &'nbt mut NbtElement, indices: &'indices Indices) -> Self {
		Self {
			head: false,
			element,
			line_number: 0,
			true_line_number: 1,
			indices,
		}
	}
}

impl<'nbt, 'indices> Iterator for ParentIterativeNavigationInformationMut<'nbt, 'indices> {
	type Item = IterativeNavigationInformationMutItem<'nbt>;

	fn next(&mut self) -> Option<Self::Item> {
		if !core::mem::replace(&mut self.head, true) {
			return Some(Self::Item {
				element: unsafe { std::ptr::read(&raw const self.element) },
				line_number: self.line_number,
				true_line_number: self.true_line_number,
				idx: None,
			})
		}

		let (idx, rest) = self.indices.split_first()?;
		self.indices = rest;

		// this is the final node, but since we're only iterating parents, we exclude this one.
		if rest.is_root() {
			return None;
		}

		self.line_number += 1;
		self.true_line_number += 1;
		for jdx in 0..idx {
			let sibling = self.element[jdx].as_nonnull()?;
			self.line_number += sibling.height();
			self.true_line_number += sibling.true_height();
		}

		let (element, child_element) = unsafe {
			let duplicate_reference = std::ptr::read(&raw const self.element);
			let child_reference = core::mem::transmute::<_, &'nbt mut NbtElement>(self.element[idx].as_nonnull_mut()?);
			(duplicate_reference, child_reference)
		};

		self.element = child_element;

		Some(Self::Item {
			element,
			line_number: self.line_number,
			true_line_number: self.true_line_number,
			idx: Some(idx),
		})
	}
}
