use crate::elements::NbtElement;
use crate::tree::{Indices, IterativeNavigationInformationMutItem};
use crate::workbench::MarkedLines;

#[must_use]
pub fn expand_element_to_indices(root: &mut NbtElement, indices: &Indices, bookmarks: &mut MarkedLines) -> Option<()> {
	// SAFETY: only NbtElement::toggle is being called
	for IterativeNavigationInformationMutItem { element, line_number, true_line_number, .. } in unsafe { root.navigate_parents_iteratively_mut(indices) } {
		if element.is_complex() && !element.is_open() {
			let height_before = element.height();
			unsafe { element.toggle() };
			let height_gained = element.height() - height_before;
			super::open::recache_bookmarks(element, bookmarks, height_gained, line_number, true_line_number)?;
		}
	}

	root.recache_along_indices(indices);

	Some(())
}
