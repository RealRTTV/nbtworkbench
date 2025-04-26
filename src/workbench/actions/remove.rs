use crate::elements::{NbtElement, NbtElementAndKey};
use crate::util::{encompasses, encompasses_or_equal};
use crate::workbench::{recache_along_indices, MarkedLines, MutableIndices, Navigate, WorkbenchAction};

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
///     Box::new([0]),
///     &mut tab.bookmarks,
///     &mut workbench.subscription
/// )?;
/// tab.append_to_history(result.into_action());
/// ```
pub fn remove_element<'m1, 'm2: 'm1>(root: &mut NbtElement, indices: Box<[usize]>, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<RemoveElementResult> {
    let (&last, rem) = indices.split_last()?;
    let (_, _, parent, mut line_number) = Navigate::new(rem.iter().copied(), root).last();
    for n in 0..last {
        line_number += parent[n].true_height();
    }
    line_number += 1;
    let (old_parent_height, old_parent_true_height) = (parent.height(), parent.true_height());
    // SAFETY: we have updated all the relevant data
    let (key, value) = unsafe { parent.remove(last) }?;
    let (height, true_height) = (value.height(), value.true_height());
    let (parent_height, parent_true_height) = (parent.height(), parent.true_height());
    let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
    let been_replaced = !(height == diff && true_height == true_diff);
    bookmarks.remove(line_number..line_number);
    bookmarks[line_number..].decrement(diff, true_diff);

    mutable_indices.apply(|mutable_indices| {
        if encompasses_or_equal(&indices, &mutable_indices) {
            return true
        } else if encompasses(rem, &mutable_indices) {
            if mutable_indices[rem.len()] >= last && !been_replaced {
                mutable_indices[rem.len()] -= 1;
            }
        }
        false
    });

    recache_along_indices(rem, root);

    Some(RemoveElementResult {
        indices,
        kv: (key, value),
        replaces: been_replaced,
    })
}

#[derive(Clone)]
pub struct RemoveElementResult {
    indices: Box<[usize]>,
    kv: NbtElementAndKey,
    replaces: bool,
}

impl RemoveElementResult {
    #[must_use]
    pub fn into_raw(self) -> (Box<[usize]>, NbtElementAndKey, bool) {
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
