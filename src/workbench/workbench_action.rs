use std::path::PathBuf;

use anyhow::{Context, Result, ensure};
use compact_str::CompactString;

use crate::elements::{NbtElement, NbtElementAndKey};
use crate::render::WindowProperties;
use crate::tree::{AddElementResult, MutableIndices, OwnedIndices, RemoveElementResult, ReplaceElementResult, add_element, remove_element, rename_element, reorder_element, replace_element, swap_element_same_depth};
use crate::util::LinkedQueue;
use crate::workbench::{HeldEntry, MarkedLines};

#[derive(Debug)]
#[must_use]
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

	pub fn undo<'m1, 'm2: 'm1>(
		self,
		root: &mut NbtElement,
		bookmarks: &mut MarkedLines,
		mutable_indices: &'m1 mut MutableIndices<'m2>,
		path: &mut Option<PathBuf>,
		name: &mut Box<str>,
		held_entry: &mut Option<HeldEntry>,
		window_properties: &mut WindowProperties,
	) -> Result<Self> {
		Ok(match self {
			Self::Add { indices } => remove_element(root, indices, bookmarks, mutable_indices)
				.context("Could remove element")?
				.into_action(),
			Self::Remove { kv, indices } => add_element(root, kv, indices, bookmarks, mutable_indices)
				.context("Couldn't add element")?
				.into_action(),
			Self::Replace { indices, kv: value } => replace_element(root, value, indices, bookmarks, mutable_indices)
				.context("Could not replace element")?
				.into_action(),
			Self::Rename { indices, key, value } => rename_element(root, indices, key, value, path, name, window_properties)
				.context("Could not rename element")?
				.into_action(),
			Self::Swap { parent, a, b } => swap_element_same_depth(root, parent, a, b, bookmarks, mutable_indices)
				.context("Could not swap elements")?
				.into_action(),
			Self::Reorder { indices, mapping } => reorder_element(root, indices, mapping, bookmarks, mutable_indices)
				.context("Could not reorder element")?
				.into_action(),
			Self::AddFromHeldEntry { indices, mut indices_history, old_kv } => {
				ensure!(held_entry.is_none(), "To remove an element and make a held entry out of it, one cannot have an pre-existing held entry.");

				let (indices, kv) = if let Some(old_kv) = old_kv {
					let ReplaceElementResult { indices, kv } = replace_element(root, old_kv, indices, bookmarks, mutable_indices).context("Could not remove element")?;
					(indices, kv)
				} else {
					let RemoveElementResult { indices, kv, replaces } = remove_element(root, indices, bookmarks, mutable_indices).context("Could not remove element")?;
					ensure!(!replaces, "If the indices had replacement for this value, then we would have the old value to swap it out with");
					(indices, kv)
				};
				indices_history.push(indices);
				*held_entry = Some(HeldEntry { kv, indices_history });
				Self::RemoveToHeldEntry
			}
			Self::RemoveToHeldEntry => {
				let HeldEntry { kv, mut indices_history } = held_entry
					.take()
					.context("Expected a held entry to add to the tab")?;
				if let Some(indices) = indices_history.pop() {
					let AddElementResult { indices, old_kv } = add_element(root, kv, indices, bookmarks, mutable_indices).context("Couldn't add element from held entry")?;
					Self::AddFromHeldEntry { indices, indices_history, old_kv }
				} else {
					Self::DiscardHeldEntry { held_entry: HeldEntry::from_aether(kv) }
				}
			}
			Self::DiscardHeldEntry { held_entry: new_held_entry } => {
				ensure!(held_entry.is_none(), "To create a held entry out of the aether, one cannot have an pre-existing held entry.");
				*held_entry = Some(new_held_entry);
				Self::RemoveToHeldEntry
			}
			Self::CreateHeldEntry => {
				let held_entry = held_entry
					.take()
					.context("Expected a held entry that was created")?;
				Self::DiscardHeldEntry { held_entry }
			}
			Self::Bulk { actions } => Self::Bulk {
				actions: actions
					.into_vec()
					.into_iter()
					.rev()
					.map(|action| action.undo(root, bookmarks, mutable_indices, path, name, held_entry, window_properties))
					.collect::<Result<Vec<_>>>()?
					.into_boxed_slice(),
			},
		})
	}
}
