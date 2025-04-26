use crate::elements::{NbtElement, NbtElementAndKey};
use crate::util::encompasses_or_equal;
use crate::workbench::{MarkedLines, WorkbenchAction};
use super::{recache_along_indices, MutableIndices, Navigate};

/// Properly replaces an element under the specified indices, updating the following relevant data
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
/// let result = replace_element(
///     &mut tab.value,
///     NbtElement::from_str(
///         r#"{"registry":"minecraft:item","value":"minecraft:stone"}"#
///     ).unwrap(),
///     Box::new([0]),
///     &mut tab.bookmarks,
///     &mut workbench.subscription
/// )?;
/// tab.append_to_history(result.into_action());
/// ```
pub fn replace_element<'m1, 'm2: 'm1>(root: &mut NbtElement, value: NbtElementAndKey, indices: Box<[usize]>, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<ReplaceElementResult> {
    let Some((&last, rem)) = indices.split_last() else {
        return if root.id() == value.1.id() {
            bookmarks.remove(..);

            Some(ReplaceElementResult {
                indices: Box::new([]),
                kv: (None, core::mem::replace(root, value.1)),
                replaces: true,
            })
        } else {
            None
        }
    };

    let (_, _, parent, mut line_number) = Navigate::new(rem.iter().copied(), root).last();
    for n in 0..last {
        line_number += parent[n].true_height();
    }
    line_number += 1;

    let (old_parent_height, old_parent_true_height) = (parent.height(), parent.true_height());
    // SAFETY: we have updated all the relevant data
    let (old_key, old_value) = unsafe { parent.replace_key_value(last, value) }?;
    let (_old_height, old_true_height) = (old_value.height(), old_value.true_height());
    let (parent_height, parent_true_height) = (parent.height(), parent.true_height());
    let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
    bookmarks.remove(line_number..line_number + old_true_height);
    bookmarks[line_number..].increment(diff, true_diff);

    mutable_indices.apply(|mutable_indices| encompasses_or_equal(&indices, &mutable_indices));

    recache_along_indices(rem, root);

    Some(ReplaceElementResult {
        indices,
        kv: (old_key, old_value),
        replaces: true,
    })
}

#[derive(Clone)]
pub struct ReplaceElementResult {
    indices: Box<[usize]>,
    kv: NbtElementAndKey,
    replaces: bool,
}

impl ReplaceElementResult {
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