use crate::elements::{NbtElement, NbtElementAndKey};
use crate::tree::{OwnedIndices, ParentNavigationInformationMut};
use crate::util::{encompasses, encompasses_or_equal};
use crate::workbench::{MarkedLines, WorkbenchAction};
use super::{recache_along_indices, MutableIndices, Navigate};


/// Properly removes an element under the specified indices, updating the following relevant data
/// - Mutable Indices
/// - Bookmarked Lines
/// - Heights and True Heights
/// - Workbench Actions
/// - Horizontal Scroll
///
/// # Examples
/// ```rust
/// let workbench = ...;
/// let tab = tab_mut!(workbench);
/// let result = remove_element(
///     &mut tab.value,
///     Indices::from_slice(&[0]).to_owned(),
///     &mut tab.bookmarks,
///     &mut workbench.subscription
/// )?;
/// tab.append_to_history(result.into_action());
/// ```
#[must_use]
pub fn remove_element<'m1, 'm2: 'm1>(root: &mut NbtElement, indices: OwnedIndices, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<RemoveElementResult> {
    let ParentNavigationInformationMut { true_line_number, parent, idx, parent_indices, .. } = root.navigate_parent_mut(&indices)?;
    let (old_parent_height, old_parent_true_height) = (parent.height(), parent.true_height());
    // SAFETY: we have updated all the relevant data
    let (key, value) = unsafe { parent.remove(idx) }?;
    let (height, true_height) = (value.height(), value.true_height());
    let (parent_height, parent_true_height) = (parent.height(), parent.true_height());
    let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
    // exists because of regions
    let been_replaced = !(height == diff && true_height == true_diff);
    bookmarks.remove(true_line_number..true_line_number);
    bookmarks[true_line_number..].decrement(diff, true_diff);

    mutable_indices.apply(|mutable_indices, ci| {
        if indices.encompasses_or_equal(mutable_indices) {
            ci.remove();
        } else if parent_indices.encompasses(mutable_indices) {
            if mutable_indices[parent_indices.len()] >= idx && !been_replaced {
                mutable_indices[parent_indices.len()] -= 1;
            }
        }
    });

    root.recache_along_indices(&parent_indices);

    Some(RemoveElementResult {
        indices,
        kv: (key, value),
        replaces: been_replaced,
    })
}

#[derive(Clone)]
pub struct RemoveElementResult {
    indices: OwnedIndices,
    kv: NbtElementAndKey,
    replaces: bool,
}

impl RemoveElementResult {
    #[must_use]
    pub fn into_raw(self) -> (OwnedIndices, NbtElementAndKey, bool) {
        (self.indices, self.kv, self.replaces)
    }

    #[must_use]
    pub fn into_action(self) -> WorkbenchAction {
        if self.replaces {
            WorkbenchAction::Replace {
                indices: self.indices,
                value: self.kv,
            }
        } else {
            WorkbenchAction::Remove {
                element: self.kv,
                indices: self.indices,
            }
        }
    }
}
