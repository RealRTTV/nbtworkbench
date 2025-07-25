#[cfg(not(target_arch = "wasm32"))] use std::thread::scope;

use thiserror::Error;

use crate::elements::element::NbtElement;
use crate::tree::indices::Indices;
use crate::tree::MutableIndices;
use crate::tree::navigate::{NavigationError, NavigationInformationMut};
#[cfg(target_arch = "wasm32")]
use crate::wasm::{FakeScope as Scope, fake_scope as scope};
use crate::workbench::marked_line::MarkedLines;

#[rustfmt::skip]
pub fn close_element<'m1, 'm2: 'm1>(
	root: &mut NbtElement,
	indices: &Indices,
	mi: &'m1 mut MutableIndices<'m2>
) -> Result<(), CloseElementError> {
	let NavigationInformationMut { element, true_line_number, line_number, .. } = root.navigate_mut(&indices)?;
	if element.is_primitive() {
		return Err(CloseElementError::ElementIsPrimitive {
			element: element.display_name(),
			true_line_number,
			line_number,
		})
	}
	let true_height = element.true_height();
	let height_before = element.height();
	if !element.is_open() {
		return Ok(())
	};
	// SAFETY: we are literally updating all the relevant information
	scope(|scope| unsafe { element.shut(scope) });
	let height_after = element.height();
	let height_lost = height_before - height_after;

	for bookmark in &mut mi.bookmarks[true_line_number + 1..true_line_number + true_height] {
		*bookmark = bookmark.hidden(line_number);
	}
	mi.bookmarks[true_line_number + true_height..].decrement(height_lost, 0);

	root.recache_along_indices(&indices);
	mi.recache_all_line_number_caches_from_indices(root);

	Ok(())
}

#[derive(Debug, Error)]
pub enum CloseElementError {
	#[error(transparent)]
	Navigation(#[from] NavigationError),
	#[error("{element} @ (line number = {line_number}, true line number = {true_line_number}) is primitive and therefore cannot be opened or closed.")]
	ElementIsPrimitive { element: &'static str, line_number: usize, true_line_number: usize },
}
