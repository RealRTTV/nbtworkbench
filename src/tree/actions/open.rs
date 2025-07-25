use thiserror::Error;

use crate::elements::element::NbtElement;
use crate::tree::actions::RecacheBookmarkError;
use crate::tree::indices::Indices;
use crate::tree::MutableIndices;
use crate::tree::navigate::{NavigationError, NavigationInformationMut};
use crate::workbench::marked_line::MarkedLines;

#[rustfmt::skip]
pub fn open_element<'m1, 'm2: 'm1>(
	root: &mut NbtElement,
	indices: &Indices,
	mi: &'m1 mut MutableIndices<'m2>
) -> Result<(), OpenElementError> {
	let NavigationInformationMut { element, true_line_number, line_number, .. } = root.navigate_mut(&indices)?;
	let height_before = element.height();
	if element.is_open() {
		return Ok(())
	};
	unsafe { element.toggle() };
	let height_after = element.height();
	let height_gained = height_after - height_before;

	super::recache_bookmarks_on_open(element, mi.bookmarks, height_gained, line_number, true_line_number)?;

	root.recache_along_indices(&indices);
	mi.recache_all_line_number_caches_from_indices(root);

	Ok(())
}

#[derive(Debug, Error)]
pub enum OpenElementError {
	#[error(transparent)]
	Navigation(#[from] NavigationError),
	#[error(transparent)]
	RecacheBookmark(#[from] RecacheBookmarkError),
}
