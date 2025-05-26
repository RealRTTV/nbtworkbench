#[cfg(not(target_arch = "wasm32"))] use std::thread::scope;

use crate::elements::NbtElement;
use crate::tree::{Indices, NavigationInformationMut};
#[cfg(target_arch = "wasm32")]
use crate::wasm::{fake_scope as scope, FakeScope as Scope};
use crate::workbench::MarkedLines;

#[must_use]
pub fn close_element(root: &mut NbtElement, indices: &Indices, bookmarks: &mut MarkedLines) -> Option<()> {
	let Some(NavigationInformationMut { element, true_line_number, line_number, .. }) = root.navigate_mut(&indices) else {
		return None
	};
	if !element.is_open() || element.is_primitive() {
		return None
	}
	let true_height = element.true_height();
	let height_before = element.height();
	if !element.is_open() {
		return Some(())
	};
	// SAFETY: we are literally updating all the relevant information
	scope(|scope| unsafe { element.shut(scope) });
	let height_after = element.height();
	let height_lost = height_before - height_after;

	for bookmark in &mut bookmarks[true_line_number + 1..true_line_number + true_height] {
		*bookmark = bookmark.hidden(line_number);
	}
	bookmarks[true_line_number + true_height..].decrement(height_lost, 0);

	root.recache_along_indices(&indices);

	Some(())
}
