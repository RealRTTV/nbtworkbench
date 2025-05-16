#![allow(dead_code)]

use compact_str::{CompactString, ToCompactString};

use crate::elements::NbtElement;
use crate::tree::Indices;

pub struct NavigationInformation<'a> {
	pub idx: Option<usize>,
	pub key: Option<&'a str>,
	pub element: &'a NbtElement,
	pub line_number: usize,
	pub true_line_number: usize,
}

impl<'a> NavigationInformation<'a> {
	#[must_use]
	pub fn from(mut element: &'a NbtElement, indices: &Indices) -> Option<Self> {
		let mut line_number = 0;
		let mut true_line_number = 1;
		let mut key = None;

		for idx in indices {
			line_number += 1;
			true_line_number += 1;
			for jdx in 0..idx {
				let sibling = &element[jdx];
				line_number += sibling.height();
				true_line_number += sibling.true_height();
			}

			let (k, v) = element.get_kv(idx)?;
			key = k;
			element = v;
		}

		Some(Self {
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
	#[must_use]
	pub fn from(mut element: &'a mut NbtElement, indices: &Indices) -> Option<Self> {
		let mut line_number = 0;
		let mut true_line_number = 1;
		let mut key = None;

		for idx in indices {
			line_number += 1;
			true_line_number += 1;
			for jdx in 0..idx {
				let sibling = &element[jdx];
				line_number += sibling.height();
				true_line_number += sibling.true_height();
			}

			let (k, v) = element.get_mut(idx)?;
			key = k;
			element = v;
		}

		Some(Self {
			idx: indices.last(),
			key,
			element,
			line_number,
			true_line_number,
		})
	}
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
	#[must_use]
	pub fn from(mut parent: &'nbt NbtElement, indices: &'indices Indices) -> Option<Self> {
		let (last, parent_indices) = indices.split_last()?;

		let mut line_number = 0;
		let mut true_line_number = 1;

		for idx in parent_indices {
			line_number += 1;
			true_line_number += 1;
			for jdx in 0..idx {
				let sibling = parent[jdx].as_nonnull()?;
				line_number += sibling.height();
				true_line_number += sibling.true_height();
			}

			parent = parent[idx].as_nonnull()?;
		}

		line_number += 1;
		true_line_number += 1;
		for jdx in 0..last {
			let sibling = parent[jdx].as_nonnull()?;
			line_number += sibling.height();
			true_line_number += sibling.true_height();
		}

		Some(Self {
			idx: last,
			key: parent.get_kv(last).and_then(|(a, _)| a),
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
	#[must_use]
	pub fn from(mut parent: &'nbt mut NbtElement, indices: &'indices Indices) -> Option<Self> {
		let (last, parent_indices) = indices.split_last()?;

		let mut line_number = 0;
		let mut true_line_number = 1;

		for idx in parent_indices {
			line_number += 1;
			true_line_number += 1;
			for jdx in 0..idx {
				let sibling = parent[jdx].as_nonnull()?;
				line_number += sibling.height();
				true_line_number += sibling.true_height();
			}

			parent = parent[idx].as_nonnull_mut()?;
		}

		line_number += 1;
		true_line_number += 1;
		for jdx in 0..last {
			let sibling = parent[jdx].as_nonnull()?;
			line_number += sibling.height();
			true_line_number += sibling.true_height();
		}

		Some(Self {
			idx: last,
			key: parent
				.get_kv(last)
				.and_then(|(a, _)| a.map(|x| x.to_compact_string())),
			parent,
			line_number,
			true_line_number,
			parent_indices,
		})
	}
}
