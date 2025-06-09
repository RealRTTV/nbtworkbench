use thiserror::Error;
use crate::elements::{NbtElement, NbtElementAndKey};
use crate::tree::{MutableIndices, OwnedIndices, ParentNavigationError, ParentNavigationInformationMut};
use crate::workbench::{MarkedLines, WorkbenchAction};

pub fn remove_element<'m1, 'm2: 'm1>(root: &mut NbtElement, indices: OwnedIndices, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Result<RemoveElementResult, RemoveElementError> {
	let ParentNavigationInformationMut {
		true_line_number, parent, idx, parent_indices, ..
	} = root.navigate_parent_mut(&indices)?;
	let (old_parent_height, old_parent_true_height) = parent.heights();
	// SAFETY: we have updated all the relevant data
	let (key, value) = match unsafe { parent.remove(idx) } {
		Some(x) => x,
		None => return Err(RemoveElementError::FailedRemoval { idx, parent: parent.display_name(), indices })
	};
	let (height, true_height) = value.heights();
	let (parent_height, parent_true_height) = parent.heights();
	let (diff, true_diff) = (old_parent_height.wrapping_sub(parent_height), old_parent_true_height.wrapping_sub(parent_true_height));
	// exists because of regions
	let been_replaced = !(height == diff && true_height == true_diff);
	bookmarks.remove(true_line_number..true_line_number + true_height);
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

	Ok(RemoveElementResult {
		indices,
		kv: (key, value),
		replaces: been_replaced,
	})
}

#[must_use]
#[derive(Clone)]
pub struct RemoveElementResult {
	pub indices: OwnedIndices,
	pub kv: NbtElementAndKey,
	pub replaces: bool,
}

impl RemoveElementResult {
	pub fn into_action(self) -> WorkbenchAction {
		if self.replaces {
			WorkbenchAction::Replace { indices: self.indices, kv: self.kv }
		} else {
			WorkbenchAction::Remove { kv: self.kv, indices: self.indices }
		}
	}
}

#[derive(Error, Debug)]
pub enum RemoveElementError {
	#[error(transparent)]
	Navigation(#[from] ParentNavigationError),
	#[error("Could not remove {nth} element of {parent} @ {indices}", nth = crate::util::nth(.idx + 1))]
	FailedRemoval { idx: usize, parent: &'static str, indices: OwnedIndices }
}
