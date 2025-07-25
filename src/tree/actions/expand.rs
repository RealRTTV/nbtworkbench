use thiserror::Error;

use crate::elements::element::NbtElement;
use crate::tree::MutableIndices;
use crate::tree::indices::Indices;
use crate::tree::navigate::{NavigationError, NavigationInformationMut};
use crate::workbench::marked_line::MarkedLines;

#[rustfmt::skip]
pub fn expand_element<'m1, 'm2: 'm1>(
	root: &mut NbtElement,
	indices: &Indices,
	mi: &'m1 mut MutableIndices<'m2>
) -> Result<(), ExpandElementError> {
	let NavigationInformationMut { element, true_line_number, line_number, .. } = root.navigate_mut(&indices)?;
	let true_height = element.true_height();
	let height_before = element.height();
	#[cfg(not(target_arch = "wasm32"))]
	// SAFETY: we are literally updating all the relevant information
	std::thread::scope(|scope| unsafe { element.expand(scope) });
	#[cfg(target_arch = "wasm32")]
	element.expand();
	let height_after = element.height();
	let height_gained = height_after - height_before;

	for bookmark in &mut mi.bookmarks[true_line_number + 1..true_line_number + true_height] {
		let bookmark_true_line_number = bookmark.true_line_number();
		*bookmark = bookmark.open(line_number + bookmark_true_line_number - true_line_number);
	}
	mi.bookmarks[true_line_number + true_height..].increment(height_gained, 0);

	root.recache_along_indices(&indices);
	mi.recache_all_line_number_caches_from_indices(root);

	Ok(())
}

#[derive(Debug, Error)]
pub enum ExpandElementError {
	#[error(transparent)]
	Navigation(#[from] NavigationError),
}
