use compact_str::CompactString;
use crate::elements::NbtElement;
use crate::tree::{Indices, OwnedIndices, ParentNavigationInformationMut};
use crate::util::encompasses_or_equal;
use crate::workbench::{MarkedLines, WorkbenchAction};
use super::super::{recache_along_indices, MutableIndices, Navigate};

/// Properly adds an element under the specified indices, updating the following relevant data
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
/// let action = add_element(
///     &mut tab.value,
///     None,
///     NbtElement::from_str(
///         r#"{"registry":"minecraft:item","value":"minecraft:stone"}"#
///     ).unwrap().1,
///     Box::new([0]),
///     &mut tab.bookmarks,
///     &mut self.subscription
/// )?;
/// tab.append_to_history(action);
/// ```
pub fn add_element<'m1, 'm2: 'm1>(root: &mut NbtElement, key: Option<CompactString>, value: NbtElement, indices: OwnedIndices, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<WorkbenchAction> {
    let ParentNavigationInformationMut { true_line_number, parent, idx, parent_indices, .. } = root.navigate_parent_mut(&indices)?;
    let (old_parent_height, old_parent_true_height) = (parent.height(), parent.true_height());
    // SAFETY: we have updated all the relevant data
    let old_value = match unsafe { parent.insert(idx, (key, value)) } {
        Ok(Some(old)) => Some(old),
        Ok(None) => None,
        Err(_) => return None,
    };
    let (parent_height, parent_true_height) = (parent.height(), parent.true_height());
    let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
    let (_old_height, old_true_height) = (old_value.as_ref().map(NbtElement::height), old_value.as_ref().map(NbtElement::true_height));
    let been_replaced = old_true_height.is_some();
    
    bookmarks.remove(true_line_number..true_line_number + old_true_height.unwrap_or(0));
    bookmarks[true_line_number..].increment(diff, true_diff);

    mutable_indices.apply(|indices| {
        if parent_indices.encompasses_or_equal(indices) {
            if indices[parent_indices.len()] <= idx && !been_replaced {
                indices[parent_indices.len()] += 1;
            }
        }
        false
    });

    recache_along_indices(&parent_indices, root);

    let action = if let Some(old_value) = old_value {
        WorkbenchAction::Replace { indices, value: (None, old_value) }
    } else {
        WorkbenchAction::Add { indices }
    };

    Some(action)
}