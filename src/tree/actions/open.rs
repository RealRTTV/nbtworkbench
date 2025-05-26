use crate::assets::HIDDEN_BOOKMARK_UV;
use crate::elements::NbtElement;
use crate::tree::{Indices, NavigationInformationMut};
use crate::workbench::MarkedLines;

#[must_use]
pub fn open_element(root: &mut NbtElement, indices: &Indices, bookmarks: &mut MarkedLines) -> Option<()> {
	let NavigationInformationMut { element, true_line_number, line_number, .. } = root.navigate_mut(&indices)?;
	let height_before = element.height();
	if element.is_open() {
		return Some(())
	};
	unsafe { element.toggle() };
	let height_after = element.height();
	let height_gained = height_after - height_before;

	recache_bookmarks(element, bookmarks, height_gained, line_number, true_line_number);

	root.recache_along_indices(&indices);

	Some(())
}

pub(super) fn recache_bookmarks(element: &NbtElement, bookmarks: &mut MarkedLines, height_gained: usize, line_number: usize, true_line_number: usize) -> Option<()> {
	let true_height = element.true_height();

	let mut current_child_line_number = None;

	let mut next_child_idx = 0;
	let mut next_child_true_line_number = true_line_number + 1;
	let mut next_child_line_number = line_number + 1;

	for bookmark in &mut bookmarks[true_line_number + 1..true_line_number + true_height] {
		if bookmark.uv() != HIDDEN_BOOKMARK_UV {
			return None
		}

		let bookmark_true_line_number = bookmark.true_line_number();
		if bookmark_true_line_number == next_child_true_line_number {
			current_child_line_number = Some(next_child_line_number);

			*bookmark = bookmark.open(current_child_line_number?);

			let child = &element[next_child_idx];
			let (child_height, child_true_height) = child.heights();
			next_child_idx += 1;
			next_child_true_line_number += child_true_height;
			next_child_line_number += child_height;
		} else {
			let current_child_line_number = current_child_line_number?;
			*bookmark = bookmark.hidden(current_child_line_number);
		}
	}
	bookmarks[true_line_number + true_height..].increment(height_gained, 0);

	Some(())
}
