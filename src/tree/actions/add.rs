use crate::elements::{ComplexNbtElementVariant, NbtElement, NbtElementAndKey};
use crate::tree::{MutableIndices, OwnedIndices, ParentNavigationInformationMut};
use crate::workbench::{MarkedLines, WorkbenchAction};

#[must_use]
pub fn add_element<'m1, 'm2: 'm1>(root: &mut NbtElement, kv: NbtElementAndKey, indices: OwnedIndices, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<AddElementResult> {
	let ParentNavigationInformationMut {
		true_line_number, parent, idx, parent_indices, ..
	} = root.navigate_parent_mut(&indices)?;
	let (old_parent_height, old_parent_true_height) = parent.heights();
	// SAFETY: we have updated all the relevant data
	let old_value = match unsafe { parent.insert(idx, kv) } {
		Ok(Some(old)) => Some(old),
		Ok(None) => None,
		Err(_) => return None,
	};
	let (parent_height, parent_true_height) = parent.heights();
	let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
	let (_old_height, old_true_height) = match old_value.as_ref().map(|kv| kv.1.heights()) {
		Some((a, b)) => (Some(a), Some(b)),
		None => (None, None),
	};
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

	Some(AddElementResult { indices, old_kv: old_value })
}

pub struct AddElementResult {
	pub indices: OwnedIndices,
	pub old_kv: Option<NbtElementAndKey>,
}

impl AddElementResult {
	pub fn into_action(self) -> WorkbenchAction {
		let Self { indices, old_kv } = self;
		if let Some(kv) = old_kv { WorkbenchAction::Replace { indices, kv } } else { WorkbenchAction::Add { indices } }
	}
}
