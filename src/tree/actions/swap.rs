use crate::elements::NbtElement;
use crate::tree::{Indices, NavigationInformation, NavigationInformationMut, OwnedIndices};
use crate::util::encompasses;
use crate::workbench::{MarkedLineSlice, MarkedLines, WorkbenchAction};
use super::{recache_along_indices, line_number_at, MutableIndices, Navigate};

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
///     Indices::from_slice(&[]).to_owned(),
///     0,
///     1,
///     &mut tab.bookmarks,
///     &mut self.subscription
/// ).into_action();
/// tab.append_to_history(action);
/// ```
pub fn swap_element_same_depth<'m1, 'm2: 'm1>(root: &mut NbtElement, parent_indices: OwnedIndices, a: usize, b: usize, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<SwapElementResultSameDepth> {
    let (a, b) = if a <= b { (a, b) } else { (b, a) };
    let NavigationInformationMut { element: parent, true_line_number: parent_true_line_number, line_number: parent_line_number, ..  } = root.navigate_mut(&parent_indices)?;
    let NavigationInformation { element: child_a, true_line_number: mut a_true_line_number, line_number: mut a_line_number, .. } = parent.navigate(Indices::from_slice(&[a]))?;
    let NavigationInformation { element: child_b, true_line_number: mut b_true_line_number, line_number: mut b_line_number, .. } = parent.navigate(Indices::from_slice(&[b]))?;
    let (a_height, a_true_height) = (child_a.height(), child_a.true_height());
    let (b_height, b_true_height) = (child_b.height(), child_b.true_height());
    a_true_line_number += parent_true_line_number;
    a_line_number += parent_line_number;
    b_true_line_number += parent_true_line_number;
    b_line_number += parent_line_number;

    let mut a_bookmarks = bookmarks.remove(a_true_line_number..a_true_line_number + a_true_height);
    let mut b_bookmarks = bookmarks.remove(b_true_line_number..b_true_line_number + b_true_height);
    MarkedLineSlice::from_marked_lines_mut(&mut a_bookmarks).decrement(a_line_number, a_true_line_number);
    MarkedLineSlice::from_marked_lines_mut(&mut b_bookmarks).decrement(b_line_number, b_true_line_number);

    bookmarks[b_true_line_number..].decrement(b_height, b_true_height);
    bookmarks[a_true_line_number..].decrement(a_height, a_true_height);

    MarkedLineSlice::from_marked_lines_mut(&mut a_bookmarks).increment(b_line_number, b_true_line_number);
    MarkedLineSlice::from_marked_lines_mut(&mut b_bookmarks).increment(a_line_number, a_true_line_number);
    bookmarks.add_bookmarks(a_bookmarks);
    bookmarks.add_bookmarks(b_bookmarks);

    mutable_indices.apply(|indices, _ci| {
        if parent_indices.encompasses(indices) {
            let sibling = &mut indices[parent_indices.len()];
            if *sibling == a {
                *sibling = b;
            } else if *sibling == b {
                *sibling = a;
            }
        }
    });

    parent.swap(a, b);

    recache_along_indices(&parent_indices, root);

    Some(SwapElementResultSameDepth {
        parent: parent_indices,
        a,
        b,
    })
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
