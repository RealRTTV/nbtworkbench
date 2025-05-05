use crate::elements::{NbtElement, NbtElementAndKey};
use crate::tree::{MutableIndices, OwnedIndices, ParentNavigationInformationMut};
use crate::workbench::{MarkedLines, WorkbenchAction};

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
///     Indices::from_slice(&[0]).to_owned(),
///     &mut tab.bookmarks,
///     &mut self.subscription
/// )?;
/// tab.append_to_history(action);
/// ```
#[must_use]
pub fn add_element<'m1, 'm2: 'm1>(root: &mut NbtElement, kv: NbtElementAndKey, indices: OwnedIndices, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<AddElementResult> {
    let ParentNavigationInformationMut { true_line_number, parent, idx, parent_indices, .. } = root.navigate_parent_mut(&indices)?;
    let (old_parent_height, old_parent_true_height) = (parent.height(), parent.true_height());
    // SAFETY: we have updated all the relevant data
    let old_value = match unsafe { parent.insert(idx, kv) } {
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

    mutable_indices.apply(|indices, _ci| {
        if parent_indices.encompasses_or_equal(indices) {
            if indices[parent_indices.len()] <= idx && !been_replaced {
                indices[parent_indices.len()] += 1;
            }
        }
    });

    root.recache_along_indices(&parent_indices);

    Some(AddElementResult {
        indices,
        old_value,
    })
}

pub struct AddElementResult {
    pub indices: OwnedIndices,
    pub old_value: Option<NbtElement>
}

impl AddElementResult {
    pub fn into_action(self) -> WorkbenchAction {
        let Self { indices, old_value } = self;
        if let Some(old_value) = old_value {
            WorkbenchAction::Replace { indices, value: (None, old_value) }
        } else {
            WorkbenchAction::Add { indices }
        }
    }
}
