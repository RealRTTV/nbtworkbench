#![allow(unused_imports)]

use thiserror::Error;
use super::*;

mod add;
mod remove;
mod rename;
mod reorder;
mod replace;
mod swap;

mod close;
mod expand;
mod expand_to_indices;
mod open;

pub use add::{add_element, AddElementResult, AddElementError};
pub use remove::{remove_element, RemoveElementResult, RemoveElementError};
pub use rename::{rename_element, RenameElementResult, RenameElementError};
pub use reorder::{reorder_element, ReorderElementResult, ReorderElementError};
pub use replace::{replace_element, ReplaceElementResult, ReplaceElementError};
pub use swap::{swap_element_same_depth, SwapElementResultSameDepth, SwapElementErrorSameDepth};

pub use close::{close_element, CloseElementError};
pub use expand::{expand_element, ExpandElementError};
pub use expand_to_indices::{expand_element_to_indices, ExpandElementToIndicesError};
pub use open::{open_element, OpenElementError};

use crate::workbench::{MarkedLine, MarkedLines};

fn recache_bookmarks_on_open(element: &NbtElement, bookmarks: &mut MarkedLines, height_gained: usize, mut line_number: usize, mut true_line_number: usize) -> Result<(), RecacheBookmarkError> {
    line_number += 1;
    true_line_number += 1;
    for child in element.values().ok_or(RecacheBookmarkError::ElementWasPrimitive { element: element.display_name(), line_number, true_line_number })? {
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

#[derive(Error, Debug)]
pub enum RecacheBookmarkError {
    #[error("{element} was primitive when expected to recache the bookmarks of the children")]
    ElementWasPrimitive { element: &'static str, line_number: usize, true_line_number: usize },
}
