use crate::elements::{NbtElement, NbtElementAndKey};
use crate::tree::{OwnedIndices, ParentNavigationInformationMut};
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
pub fn replace_element<'m1, 'm2: 'm1>(root: &mut NbtElement, value: NbtElementAndKey, indices: OwnedIndices, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<ReplaceElementResult> {
    let Some(ParentNavigationInformationMut { parent, true_line_number, .. }) = root.navigate_parent_mut(&indices) else {
        return if root.id() == value.1.id() {
            bookmarks.remove(..);

            Some(ReplaceElementResult {
                indices,
                kv: (None, core::mem::replace(root, value.1)),
                replaces: true,
            })
        } else {
            None
        }
    };

    let (old_parent_height, old_parent_true_height) = (parent.height(), parent.true_height());
    // SAFETY: we have updated all the relevant data
    let (old_key, old_value) = unsafe { parent.replace_key_value(last, value) }?;
    let (_old_height, old_true_height) = (old_value.height(), old_value.true_height());
    let (parent_height, parent_true_height) = (parent.height(), parent.true_height());
    let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
    bookmarks.remove(true_line_number..true_line_number + old_true_height);
    bookmarks[true_line_number..].increment(diff, true_diff);

    mutable_indices.apply(|mutable_indices, ci| {
        if indices.encompasses_or_equal(mutable_indices) {
            ci.remove();
        }
    });

    recache_along_indices(&indices, root);

    Some(ReplaceElementResult {
        indices,
        kv: (old_key, old_value),
        replaces: true,
    })
}

#[derive(Clone)]
pub struct ReplaceElementResult {
    indices: OwnedIndices,
    kv: NbtElementAndKey,
    replaces: bool,
}

impl ReplaceElementResult {
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