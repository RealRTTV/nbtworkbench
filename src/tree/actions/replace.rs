use super::MutableIndices;
use crate::elements::{NbtElement, NbtElementAndKey};
use crate::tree::{OwnedIndices, ParentNavigationInformationMut};
use crate::workbench::{MarkedLines, WorkbenchAction};

#[must_use]
pub fn replace_element<'m1, 'm2: 'm1>(root: &mut NbtElement, value: NbtElementAndKey, indices: OwnedIndices, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<ReplaceElementResult> {
	let Some(ParentNavigationInformationMut { parent, true_line_number, idx, .. }) = root.navigate_parent_mut(&indices) else {
		return if root.id() == value.1.id() {
			bookmarks.remove(..);

			Some(ReplaceElementResult {
				indices,
				kv: (None, core::mem::replace(root, value.1)),
			})
		} else {
			None
		}
	};

	let (old_parent_height, old_parent_true_height) = parent.heights();
	// SAFETY: we have updated all the relevant data
	let (old_key, old_value) = unsafe { parent.replace_key_value(idx, value) }.ok()??;
	let (_old_height, old_true_height) = old_value.heights();
	let (parent_height, parent_true_height) = parent.heights();
	let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
	bookmarks.remove(true_line_number..true_line_number + old_true_height);
	bookmarks[true_line_number..].increment(diff, true_diff);

	mutable_indices.apply(|mutable_indices, ci| {
		if indices.encompasses_or_equal(mutable_indices) {
			ci.remove();
		}
	});

	root.recache_along_indices(&indices);

	Some(ReplaceElementResult { indices, kv: (old_key, old_value) })
}

#[derive(Clone)]
pub struct ReplaceElementResult {
	pub indices: OwnedIndices,
	pub kv: NbtElementAndKey,
}

impl ReplaceElementResult {
	#[must_use]
	pub fn into_action(self) -> WorkbenchAction { WorkbenchAction::Replace { indices: self.indices, kv: self.kv } }
}
