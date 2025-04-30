use crate::elements::NbtElement;
use crate::tree::{Indices, OwnedIndices};
use crate::util::encompasses;
use crate::workbench::{MarkedLineSlice, MarkedLines, WorkbenchAction};
use super::{recache_along_indices, sum_indices, MutableIndices, Navigate};

/// Properly swaps two elements under their specified indices (requires them to be at the same depth), updating the following relevant data
/// - Mutable Indices
/// - Bookmarked Lines
/// - Heights and True Heights
/// - User Actions
/// - Horizontal Scroll
///
/// # Examples
/// ```rust
/// let workbench = ...;
/// let tab = tab_mut!(workbench);
/// let action = swap_element(
///     &mut tab.value,
///     None,
///     Box::new([]),
///     0,
///     1,
///     &mut tab.bookmarks,
///     &mut self.subscription
/// ).into_action();
/// tab.append_to_history(action);
/// ```
pub fn swap_element_same_depth<'m1, 'm2: 'm1>(root: &mut NbtElement, parent_indices: OwnedIndices, a: usize, b: usize, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> SwapElementResultSameDepth {
    let (a, b) = if a <= b { (a, b) } else { (b, a) };
    let parent_y = sum_indices(&parent_indices, root);
    let (_, _, parent, parent_line_number) = Navigate::new(&parent_indices, root).last();

    let mut a_line_number = parent_line_number;
    let mut a_y = parent_y;
    let (a_height, a_true_height) = (parent[a].height(), parent[a].true_height());
    for n in 0..a {
        let sibling = &parent[n];
        a_line_number += sibling.true_height();
        a_y += sibling.height();
    }
    a_line_number += 1;
    a_y += 1;

    let mut b_line_number = parent_line_number;
    let mut b_y = parent_y;
    let (b_height, b_true_height) = (parent[b].height(), parent[b].true_height());
    for n in 0..b {
        let sibling = &parent[n];
        b_line_number += sibling.true_height();
        b_y += sibling.height();
    }
    b_line_number += 1;
    b_y += 1;

    let mut a_bookmarks = bookmarks.remove(a_line_number..a_line_number + a_true_height);
    let mut b_bookmarks = bookmarks.remove(b_line_number..b_line_number + b_true_height);
    MarkedLineSlice::from_marked_lines_mut(&mut a_bookmarks).decrement(a_y, a_line_number);
    MarkedLineSlice::from_marked_lines_mut(&mut b_bookmarks).decrement(b_y, b_line_number);

    bookmarks[b_line_number..].decrement(b_height, b_true_height);
    bookmarks[a_line_number..].decrement(a_height, a_true_height);

    MarkedLineSlice::from_marked_lines_mut(&mut a_bookmarks).increment(b_y, b_line_number);
    MarkedLineSlice::from_marked_lines_mut(&mut b_bookmarks).increment(a_y, a_line_number);
    bookmarks.add_bookmarks(a_bookmarks);
    bookmarks.add_bookmarks(b_bookmarks);

    mutable_indices.apply(|indices| {
        if parent_indices.encompasses(indices) {
            let sibling = &mut indices[parent_indices.len()];
            if *sibling == a {
                *sibling = b;
            } else if *sibling == b {
                *sibling = a;
            }
        }
        false
    });

    parent.swap(a, b);

    recache_along_indices(&parent_indices, root);

    SwapElementResultSameDepth {
        parent: parent_indices,
        a,
        b,
    }
}

#[derive(Clone)]
pub struct SwapElementResultSameDepth {
    parent: OwnedIndices,
    a: usize,
    b: usize,
}

#[allow(dead_code)]
impl SwapElementResultSameDepth {
    #[must_use]
    pub fn as_raw(&self) -> (&Indices, usize, usize) { (&self.parent, self.a, self.b) }

    #[must_use]
    pub fn into_raw(self) -> (OwnedIndices, usize, usize) {
        (self.parent, self.a, self.b)
    }

    #[must_use]
    pub fn into_action(self) -> WorkbenchAction {
        WorkbenchAction::Swap {
            parent: self.parent,
            a: self.a,
            b: self.b,
        }
    }
}
