use crate::assets::HIDDEN_BOOKMARK_UV;
use crate::elements::NbtElement;
use crate::tree::{Indices, NavigationInformationMut};
use crate::workbench::MarkedLines;

#[must_use]
pub fn expand_element(root: &mut NbtElement, indices: &Indices, bookmarks: &mut MarkedLines) -> Option<()> {
    let NavigationInformationMut { element, true_line_number, line_number, .. } = root.navigate_mut(&indices)?;
    let true_height = element.true_height();
    let height_before = element.height();
    #[cfg(not(target_arch = "wasm32"))]
    std::thread::scope(|scope| element.expand(scope));
    #[cfg(target_arch = "wasm32")]
    element.expand();
    let height_after = element.height();
    let height_gained = height_after - height_before;
    
    for bookmark in &mut bookmarks[true_line_number + 1..true_line_number + true_height] {
        if bookmark.uv() != HIDDEN_BOOKMARK_UV { return None }
        let bookmark_true_line_number = bookmark.true_line_number();
        *bookmark = bookmark.open(line_number + bookmark_true_line_number - true_line_number);
    }
    bookmarks[true_line_number + true_height..].increment(height_gained, 0);
    
    root.recache_along_indices(&indices);
    
    Some(())
}