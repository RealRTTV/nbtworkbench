use thiserror::Error;

use crate::elements::NbtElementAndKey;
use crate::elements::element::NbtElement;
use crate::history::WorkbenchAction;
use crate::tree::MutableIndices;
use crate::tree::indices::OwnedIndices;
use crate::tree::navigate::{ParentNavigationError, ParentNavigationInformationMut};

#[rustfmt::skip]
pub fn replace_element<'m1, 'm2: 'm1>(
	root: &mut NbtElement,
	value: NbtElementAndKey,
	indices: OwnedIndices,
	mi: &'m1 mut MutableIndices<'m2>
) -> Result<ReplaceElementResult, ReplaceElementError> {
	match root.navigate_parent_mut(&indices) {
		Ok(ParentNavigationInformationMut { parent, true_line_number, idx, .. }) => {
			let (old_parent_height, old_parent_true_height) = parent.heights();
			// SAFETY: we have updated all the relevant data
			let (old_key, old_value) = unsafe { parent.replace_key_value(idx, value) }
				.map_err(|value| ReplaceElementError::FailedReplacement {
					parent: parent.display_name(),
					idx,
					value: value.1.display_name(),
				})?
				.ok_or_else(|| ReplaceElementError::NoOldValue { parent: parent.display_name(), idx })?;
			let (_old_height, old_true_height) = old_value.heights();
			let (parent_height, parent_true_height) = parent.heights();
			let (diff, true_diff) = (parent_height.wrapping_sub(old_parent_height), parent_true_height.wrapping_sub(old_parent_true_height));
			mi.bookmarks.remove(true_line_number..true_line_number + old_true_height);
			mi.bookmarks[true_line_number..].increment(diff, true_diff);

			mi.apply(|mutable_indices, ci| {
				if indices.encompasses_or_equal(mutable_indices) {
					ci.remove();
				}
			});

			root.recache_along_indices(&indices);

			Ok(ReplaceElementResult { indices, kv: (old_key, old_value) })
		}
		Err(ParentNavigationError::EmptyIndices) =>
			if root.id() == value.1.id() {
				mi.bookmarks.remove(..);

				Ok(ReplaceElementResult {
					indices,
					kv: (None, core::mem::replace(root, value.1)),
				})
			} else {
				Err(ReplaceElementError::DifferentRootVariants {
					old: root.display_name(),
					new: value.1.display_name(),
				})
			},
		Err(e) => Err(e.into()),
	}
}

#[derive(Clone)]
pub struct ReplaceElementResult {
	pub indices: OwnedIndices,
	pub kv: NbtElementAndKey,
}

impl ReplaceElementResult {
	pub fn into_action(self) -> WorkbenchAction { WorkbenchAction::Replace { indices: self.indices, kv: self.kv } }
}

#[derive(Debug, Error)]
pub enum ReplaceElementError {
	#[error(transparent)]
	Navigation(#[from] ParentNavigationError),
	#[error("Cannot replace root with different element type ({old} -> {new}).")]
	DifferentRootVariants { old: &'static str, new: &'static str },
	#[error("Failed to replace {parent} @ index {idx} with value {value}.")]
	FailedReplacement { parent: &'static str, idx: usize, value: &'static str },
	#[error("Replacement of {parent}'s child had no old value for child at index {idx}.")]
	NoOldValue { parent: &'static str, idx: usize },
}
