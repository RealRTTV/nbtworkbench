use compact_str::CompactString;
use thiserror::Error;

use crate::elements::NbtElementAndKey;
use crate::elements::element::NbtElement;
use crate::tree::MutableIndices;
use crate::tree::actions::add::{AddElementError, AddElementResult, add_element};
use crate::tree::actions::remove::{RemoveElementError, RemoveElementResult, remove_element};
use crate::tree::actions::rename::{RenameElementError, rename_element};
use crate::tree::actions::reorder::{ReorderElementError, reorder_element};
use crate::tree::actions::replace::{ReplaceElementError, ReplaceElementResult, replace_element};
use crate::tree::actions::swap::{SwapElementErrorSameDepth, swap_element_same_depth};
use crate::tree::indices::OwnedIndices;
use crate::util::LinkedQueue;
use crate::workbench::HeldEntry;
use crate::workbench::tab::FilePath;

pub mod manager;

#[derive(Debug)]
#[must_use = "Should be added to history immedietly"]
pub enum WorkbenchAction {
	Add {
		indices: OwnedIndices,
	},
	Remove {
		kv: NbtElementAndKey,
		indices: OwnedIndices,
	},
	Rename {
		indices: OwnedIndices,
		key: Option<CompactString>,
		value: Option<String>,
	},
	Swap {
		parent: OwnedIndices,
		a: usize,
		b: usize,
	},
	Replace {
		indices: OwnedIndices,
		kv: NbtElementAndKey,
	},
	Reorder {
		indices: OwnedIndices,
		mapping: Box<[usize]>,
	},
	AddFromHeldEntry {
		/// The [Indices](OwnedIndices) for the addition to the [tab](super::Tab)'s value
		indices: OwnedIndices,

		/// The held entry's indices history
		indices_history: LinkedQueue<OwnedIndices>,

		/// The value of the [held entry](HeldEntry) before it was added to the [tab](super::Tab)'s value
		old_kv: Option<NbtElementAndKey>,
	},
	/// Uses the [held entry](HeldEntry)'s history to get the indices to insert at
	RemoveToHeldEntry,
	DiscardHeldEntry {
		held_entry: HeldEntry,
	},
	CreateHeldEntry,
	Bulk {
		actions: Box<[Self]>,
	},
}

impl WorkbenchAction {
	pub fn shrink_to_fit(&mut self) {
		match self {
			Self::Remove { indices, .. } => indices.shrink_to_fit(),
			Self::Add { indices, .. } => indices.shrink_to_fit(),
			Self::Rename { indices, .. } => indices.shrink_to_fit(),
			Self::Swap { parent, .. } => parent.shrink_to_fit(),
			Self::Replace { indices, .. } => indices.shrink_to_fit(),
			Self::Reorder { indices, .. } => indices.shrink_to_fit(),
			Self::AddFromHeldEntry { indices, .. } => indices.shrink_to_fit(),
			Self::RemoveToHeldEntry => (),
			Self::DiscardHeldEntry { .. } => (),
			Self::CreateHeldEntry => (),
			Self::Bulk { actions } =>
				for action in actions {
					action.shrink_to_fit();
				},
		}
	}

	pub fn undo<'m1, 'm2: 'm1>(self, root: &mut NbtElement, mi: &'m1 mut MutableIndices<'m2>, path: &mut FilePath, held_entry: &mut Option<HeldEntry>) -> Result<Self, UndoWorkbenchActionError> {
		Ok(match self {
			Self::Add { indices } => remove_element(root, indices, mi)?.into_action(),
			Self::Remove { kv, indices } => add_element(root, kv, indices, mi)?.into_action(),
			Self::Replace { indices, kv: value } => replace_element(root, value, indices, mi)?.into_action(),
			Self::Rename { indices, key, value } => rename_element(root, indices, key, value, path)
				.map_failure(UndoWorkbenchActionError::from)
				.flatten_pass(Err(UndoWorkbenchActionError::Passed))?
				.into_action(),
			Self::Swap { parent, a, b } => swap_element_same_depth(root, parent, a, b, mi)?.into_action(),
			Self::Reorder { indices, mapping } => reorder_element(root, indices, mapping, mi)?.into_action(),
			Self::AddFromHeldEntry { indices, mut indices_history, old_kv } => {
				if held_entry.is_some() {
					return Err(UndoWorkbenchActionError::AddFromHeldEntry(AddFromHeldEntryError::HasHeldEntry))
				}

				let (indices, kv) = if let Some(old_kv) = old_kv {
					let ReplaceElementResult { indices, kv } = replace_element(root, old_kv, indices, mi)?;
					(indices, kv)
				} else {
					let RemoveElementResult { indices, kv, replaces } = remove_element(root, indices, mi)?;
					if replaces {
						return Err(UndoWorkbenchActionError::AddFromHeldEntry(AddFromHeldEntryError::ExpectedOldKVPair))
					}
					(indices, kv)
				};
				indices_history.push(indices);
				*held_entry = Some(HeldEntry { kv, indices_history });
				Self::RemoveToHeldEntry
			}
			Self::RemoveToHeldEntry => {
				let HeldEntry { kv, mut indices_history } = held_entry.take().ok_or(UndoWorkbenchActionError::RemoveToHeldEntry(RemoveToHeldEntryError::ExpectedHeldEntry))?;
				if let Some(indices) = indices_history.pop() {
					let AddElementResult { indices, old_kv } = add_element(root, kv, indices, mi)?;
					Self::AddFromHeldEntry { indices, indices_history, old_kv }
				} else {
					Self::DiscardHeldEntry { held_entry: HeldEntry::from_aether(kv) }
				}
			}
			Self::DiscardHeldEntry { held_entry: new_held_entry } => {
				if held_entry.is_some() {
					return Err(UndoWorkbenchActionError::DiscardHeldEntry(DiscardHeldEntryError::HasHeldEntry))
				}
				*held_entry = Some(new_held_entry);
				Self::RemoveToHeldEntry
			}
			Self::CreateHeldEntry => {
				let held_entry = held_entry.take().ok_or(UndoWorkbenchActionError::CreateHeldEntry(CreateHeldEntryError::ExpectedHeldEntry))?;
				Self::DiscardHeldEntry { held_entry }
			}
			Self::Bulk { actions } => Self::Bulk {
				actions: actions
					.into_vec()
					.into_iter()
					.rev()
					.map(|action| action.undo(root, mi, path, held_entry))
					.collect::<Result<Vec<_>, UndoWorkbenchActionError>>()?
					.into_boxed_slice(),
			},
		})
	}

	#[must_use]
	pub fn bulk(actions: impl Into<Box<[WorkbenchAction]>>) -> Option<Self> {
		let actions = actions.into();
		if actions.is_empty() {
			return None
		}
		if actions.len() == 1 {
			return Some(actions.into_vec().remove(0))
		}

		Some(Self::Bulk { actions })
	}
}

#[derive(Error, Debug)]
pub enum UndoWorkbenchActionError {
	#[error(transparent)]
	Add(#[from] AddElementError),
	#[error(transparent)]
	Remove(#[from] RemoveElementError),
	#[error(transparent)]
	Replace(#[from] ReplaceElementError),
	#[error(transparent)]
	Rename(#[from] RenameElementError),
	#[error(transparent)]
	Swap(#[from] SwapElementErrorSameDepth),
	#[error(transparent)]
	Reorder(#[from] ReorderElementError),
	#[error(transparent)]
	AddFromHeldEntry(#[from] AddFromHeldEntryError),
	#[error(transparent)]
	RemoveToHeldEntry(#[from] RemoveToHeldEntryError),
	#[error(transparent)]
	DiscardHeldEntry(#[from] DiscardHeldEntryError),
	#[error(transparent)]
	CreateHeldEntry(#[from] CreateHeldEntryError),
	#[error("Workbench Action was passed rather than successful.")]
	Passed,
}

#[derive(Error, Debug)]
pub enum AddFromHeldEntryError {
	#[error("To remove an element and make a held entry out of it, one cannot have an pre-existing held entry.")]
	HasHeldEntry,
	#[error("If the indices had replacement for this value, then we would have the old value to swap it out with.")]
	ExpectedOldKVPair,
}

#[derive(Error, Debug)]
pub enum RemoveToHeldEntryError {
	#[error("To remove an element to the held entry. There must not be a pre-existing held entry.")]
	ExpectedHeldEntry,
}

#[derive(Error, Debug)]
pub enum DiscardHeldEntryError {
	#[error("Could not undo discard of held entry if there was already an entry held.")]
	HasHeldEntry,
}

#[derive(Error, Debug)]
pub enum CreateHeldEntryError {
	#[error("Expected a held entry to remove.")]
	ExpectedHeldEntry,
}
