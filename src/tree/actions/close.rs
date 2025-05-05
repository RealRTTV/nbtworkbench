use crate::assets::{HEADER_SIZE, HIDDEN_BOOKMARK_UV};
use crate::elements::NbtElement;
use crate::tree::{Indices, NavigationInformationMut};
use crate::workbench::{MarkedLine, MarkedLines};

#[must_use]
pub fn close_element(root: &mut NbtElement, indices: &Indices, bookmarks: &mut MarkedLines) -> Option<()> {
    let Some(NavigationInformationMut { element, true_line_number, line_number, .. }) = root.navigate_mut(&indices) else { return None };
    if !element.is_open() || element.is_primitive() { return None }
    let true_height = element.true_height();
    let height_before = element.height();
    if !element.is_open() { return Some(()) };
    element.shut();
    let height_after = element.height();
    let height_lost = height_before - height_after;
    
    for bookmark in &mut bookmarks[true_line_number + 1..true_line_number + true_height] {
        *bookmark = bookmark.hidden(line_number);
    }
    bookmarks[true_line_number + true_height..].decrement(height_lost, 0);
    
    root.recache_along_indices(&indices);
    
    Some(())
}