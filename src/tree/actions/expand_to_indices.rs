use thiserror::Error;
use crate::elements::NbtElement;
use crate::tree::{Indices, IterativeNavigationInformationMutItem, RecacheBookmarkError};
use crate::workbench::MarkedLines;

pub fn expand_element_to_indices(root: &mut NbtElement, indices: &Indices, bookmarks: &mut MarkedLines) -> Result<(), ExpandElementToIndicesError> {
	// SAFETY: only NbtElement::toggle is being called
	for IterativeNavigationInformationMutItem { element, line_number, true_line_number, .. } in unsafe { root.navigate_parents_iteratively_mut(indices) } {
		if element.is_complex() && !element.is_open() {
			let height_before = element.height();
			unsafe { element.toggle() };
			let height_gained = element.height() - height_before;
			super::recache_bookmarks_on_open(element, bookmarks, height_gained, line_number, true_line_number)?;
		}
	}

	root.recache_along_indices(indices);

	Ok(())
}

#[derive(Error, Debug)]
pub enum ExpandElementToIndicesError {
	#[error(transparent)]
	RecacheBookmarks(#[from] RecacheBookmarkError)
}
