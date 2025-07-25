#![allow(unused_imports)]

use thiserror::Error;

use crate::elements::element::NbtElement;
use crate::tree::actions::expand::ExpandElementError;
use crate::tree::actions::open::OpenElementError;
use crate::workbench::marked_line::MarkedLines;

pub mod add;
pub mod remove;
pub mod rename;
pub mod reorder;
pub mod replace;
pub mod swap;

pub mod close;
pub mod expand;
pub mod expand_to_indices;
pub mod open;

fn recache_bookmarks_on_open(element: &NbtElement, bookmarks: &mut MarkedLines, height_gained: usize, mut line_number: usize, mut true_line_number: usize) -> Result<(), RecacheBookmarkError> {
	line_number += 1;
	true_line_number += 1;
	for child in element.values().ok_or(RecacheBookmarkError::ElementWasPrimitive {
		element: element.display_name(),
		line_number,
		true_line_number,
	})? {
		let (height, true_height) = child.heights();
		if let Some(bookmark) = bookmarks.get_mut(true_line_number) {
			*bookmark = bookmark.open(line_number);
		}
		for bookmark in &mut bookmarks[true_line_number + 1..true_line_number + true_height] {
			*bookmark = bookmark.hidden(line_number);
		}
		line_number += height;
		true_line_number += true_height;
	}
	bookmarks[true_line_number..].increment(height_gained, 0);
	Ok(())
}

#[derive(Debug, Error)]
pub enum RecacheBookmarkError {
	#[error("{element} was primitive when expected to recache the bookmarks of the children")]
	ElementWasPrimitive { element: &'static str, line_number: usize, true_line_number: usize },
}

#[derive(Debug, Error)]
pub enum AmbiguiousOpenElementError {
	#[error(transparent)]
	Open(#[from] OpenElementError),
	#[error(transparent)]
	Expand(#[from] ExpandElementError),
}
