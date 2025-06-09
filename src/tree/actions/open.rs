use thiserror::Error;
use crate::assets::HIDDEN_BOOKMARK_UV;
use crate::elements::NbtElement;
use crate::tree::{Indices, NavigationError, NavigationInformationMut, RecacheBookmarkError};
use crate::workbench::MarkedLines;

pub fn open_element(root: &mut NbtElement, indices: &Indices, bookmarks: &mut MarkedLines) -> Result<(), OpenElementError> {
	let NavigationInformationMut { element, true_line_number, line_number, .. } = root.navigate_mut(&indices)?;
	let height_before = element.height();
	if element.is_open() {
		return Ok(())
	};
	unsafe { element.toggle() };
	let height_after = element.height();
	let height_gained = height_after - height_before;

	super::recache_bookmarks_on_open(element, bookmarks, height_gained, line_number, true_line_number)?;

	root.recache_along_indices(&indices);

	Ok(())
}

#[derive(Error, Debug)]
pub enum OpenElementError {
	#[error(transparent)]
	Navigation(#[from] NavigationError),
	#[error(transparent)]
	RecacheBookmark(#[from] RecacheBookmarkError),
}
