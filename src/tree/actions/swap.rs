use thiserror::Error;

use crate::{
	elements::element::NbtElement,
	history::WorkbenchAction,
	tree::{
		MutableIndices,
		indices::{Indices, OwnedIndices},
		navigate::{NavigationError, NavigationInformation, NavigationInformationMut},
	},
	workbench::marked_line::MarkedLineSlice,
};

#[rustfmt::skip]
pub fn swap_element_same_depth<'m1, 'm2: 'm1>(
	root: &mut NbtElement,
	parent_indices: OwnedIndices,
	a: usize,
	b: usize,
	mi: &'m1 mut MutableIndices<'m2>,
) -> Result<SwapElementResultSameDepth, SwapElementErrorSameDepth> {
	let (a, b) = if a <= b { (a, b) } else { (b, a) };
	let NavigationInformationMut {
		element: parent,
		true_line_number: parent_true_line_number,
		line_number: parent_line_number,
		..
	} = root.navigate_mut(&parent_indices)?;
	let NavigationInformation {
		element: child_a,
		true_line_number: mut a_true_line_number,
		line_number: mut a_line_number,
		..
	} = parent.navigate(Indices::from_slice(&[a]))?;
	let NavigationInformation {
		element: child_b,
		true_line_number: mut b_true_line_number,
		line_number: mut b_line_number,
		..
	} = parent.navigate(Indices::from_slice(&[b]))?;
	let (a_height, a_true_height) = child_a.heights();
	let (b_height, b_true_height) = child_b.heights();
	a_true_line_number += parent_true_line_number;
	a_line_number += parent_line_number;
	b_true_line_number += parent_true_line_number;
	b_line_number += parent_line_number;

	let mut a_bookmarks = mi.bookmarks.remove(a_true_line_number..a_true_line_number + a_true_height);
	let mut b_bookmarks = mi.bookmarks.remove(b_true_line_number..b_true_line_number + b_true_height);
	MarkedLineSlice::from_marked_lines_mut(&mut a_bookmarks).decrement(a_line_number, a_true_line_number);
	MarkedLineSlice::from_marked_lines_mut(&mut b_bookmarks).decrement(b_line_number, b_true_line_number);

	mi.bookmarks[b_true_line_number..].decrement(b_height, b_true_height);
	mi.bookmarks[a_true_line_number..].decrement(a_height, a_true_height);

	MarkedLineSlice::from_marked_lines_mut(&mut a_bookmarks).increment(b_line_number, b_true_line_number);
	MarkedLineSlice::from_marked_lines_mut(&mut b_bookmarks).increment(a_line_number, a_true_line_number);
	*mi.bookmarks |= a_bookmarks;
	*mi.bookmarks |= b_bookmarks;

	mi.apply(|indices, _ci| {
		if parent_indices.encompasses(indices) {
			let sibling = &mut indices[parent_indices.len()];
			if *sibling == a {
				*sibling = b;
			} else if *sibling == b {
				*sibling = a;
			}
		}
	});

	// SAFETY: we have updated all the relevant data
	unsafe {
		parent.swap(a, b);
	}

	root.recache_along_indices(&parent_indices);

	Ok(SwapElementResultSameDepth { parent: parent_indices, a, b })
}

#[derive(Clone)]
pub struct SwapElementResultSameDepth {
	pub parent: OwnedIndices,
	pub a: usize,
	pub b: usize,
}

#[allow(dead_code)]
impl SwapElementResultSameDepth {
	pub fn into_action(self) -> WorkbenchAction { WorkbenchAction::Swap { parent: self.parent, a: self.a, b: self.b } }
}

#[derive(Error, Debug)]
pub enum SwapElementErrorSameDepth {
	#[error(transparent)]
	Navigation(#[from] NavigationError),
}
