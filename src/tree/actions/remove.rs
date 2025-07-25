use thiserror::Error;

use crate::elements::{NbtElementAndKey, NbtElementAndKeyRef};
use crate::elements::element::NbtElement;
use crate::history::WorkbenchAction;
use crate::tree::MutableIndices;
use crate::tree::indices::OwnedIndices;
use crate::tree::navigate::{ParentNavigationError, ParentNavigationInformationMut};

#[rustfmt::skip]
pub fn remove_element<'m1, 'm2: 'm1>(
	root: &mut NbtElement,
	indices: OwnedIndices,
	mi: &'m1 mut MutableIndices<'m2>
) -> Result<RemoveElementResult, RemoveElementError> {
	let ParentNavigationInformationMut {
		true_line_number, parent, idx, parent_indices, ..
	} = root.navigate_parent_mut(&indices)?;
	let replaces = parent.replaces();
	// SAFETY: we have updated all the relevant data
	let (key, value) = match unsafe { parent.remove(idx) } {
		Some(x) => x,
		None => return Err(RemoveElementError::FailedRemoval { idx, parent: parent.display_name(), indices }),
	};
	let (height, true_height) = value.heights();
	let (new_height, new_true_height) = if replaces { match parent.get(idx) { None => return Err(RemoveElementError::FailedReplacementIndexing { idx, parent: parent.display_name(), indices }), Some((_, v)) => v.heights() } } else { (0, 0) };
	let (height_increment, true_height_increment) = (height as isize - new_height as isize, true_height as isize - new_true_height as isize);
	mi.bookmarks.remove(true_line_number..true_line_number + true_height);
	mi.bookmarks[true_line_number..].offset(height_increment, true_height_increment);

	mi.apply(|mutable_indices, ci| {
		if indices.encompasses_or_equal(mutable_indices) {
			ci.remove();
		} else if parent_indices.encompasses(mutable_indices) {
			if mutable_indices[parent_indices.len()] >= idx && !replaces {
				mutable_indices[parent_indices.len()] -= 1;
			}
		}
	});

	root.recache_along_indices(&parent_indices);

	Ok(RemoveElementResult {
		indices,
		kv: (key, value),
		replaces,
	})
}

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
			WorkbenchAction::Remove { indices: self.indices, kv: self.kv }
		}
	}
}

#[derive(Debug, Error)]
pub enum RemoveElementError {
	#[error(transparent)]
	Navigation(#[from] ParentNavigationError),
	#[error("Could not remove {nth} element of {parent} @ {indices}", nth = crate::util::nth(.idx + 1))]
	FailedRemoval { idx: usize, parent: &'static str, indices: OwnedIndices },
	#[error("Could not re-index {nth} element of {parent} @ {indices} after removal.", nth = crate::util::nth(.idx + 1))]
	FailedReplacementIndexing { idx: usize, parent: &'static str, indices: OwnedIndices },
}
