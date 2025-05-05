use crate::elements::{NbtElement, NbtElementAndKey};
use crate::render::WindowProperties;
use crate::tree::{add_element, remove_element, rename_element, reorder_element, replace_element, swap_element_same_depth, AddElementResult, MutableIndices, OwnedIndices, RemoveElementResult, ReplaceElementResult};
use crate::workbench::{HeldEntry, MarkedLines};
use std::path::PathBuf;

use crate::util::LinkedQueue;
use anyhow::{ensure, Context, Result};
use compact_str::{CompactString, ToCompactString};

#[derive(Debug)]
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
		value: Option<CompactString>,
	},
	Swap {
		parent: OwnedIndices,
		a: usize,
		b: usize,
	},
	Replace {
		indices: OwnedIndices,
		value: NbtElementAndKey,
	},
	Reorder {
		indices: OwnedIndices,
		mapping: Box<[usize]>,
	},
	RemoveToHeldEntry {
		/// The [Indices](OwnedIndices) for the addition to the [tab](super::Tab)'s value
		indices: OwnedIndices,

		/// The held entry's indices history
		indices_history: LinkedQueue<OwnedIndices>,

		/// The key of the [held entry](HeldEntry) before it was added to the [tab](super::Tab)'s value
		old_key: Option<CompactString>,

		/// The value of the 
		old_value: Option<NbtElement>,
	},
	/// Uses the [held entry](HeldEntry)'s history to get the indices to insert at
	AddFromHeldEntry,
	DiscardHeldEntry {
		held_entry: HeldEntry
	},
	CreateHeldEntry,
	Bulk {
		actions: Box<[Self]>,
	}
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
			Self::RemoveToHeldEntry { indices, .. } => indices.shrink_to_fit(),
			Self::AddFromHeldEntry => (),
			Self::DiscardHeldEntry { .. } => (),
			Self::CreateHeldEntry => (),
			Self::Bulk { actions } => for action in actions { action.shrink_to_fit(); },
		}
	}

	#[cfg_attr(debug_assertions, inline(never))]
	#[allow(
		clippy::collapsible_else_if,
		clippy::too_many_lines,
		clippy::cognitive_complexity
	)]
	pub fn undo<'m1, 'm2: 'm1>(self, root: &mut NbtElement, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>, path: &mut Option<PathBuf>, name: &mut Box<str>, held_entry: &mut Option<HeldEntry>, window_properties: &mut WindowProperties) -> Result<Self> {
		Ok(match self {
			Self::Add { indices } => remove_element(root, indices, bookmarks, mutable_indices).context("Could remove element")?.into_action(),
			Self::Remove { kv, indices, } => add_element(root, kv, indices, bookmarks, mutable_indices).context("Couldn't add element")?.into_action(),
			Self::Replace { indices, value, } => replace_element(root, value, indices, bookmarks, mutable_indices).context("Could not replace element")?.into_action(),
			Self::Rename { indices, key, value } => rename_element(root, indices, key, value, path, name, window_properties).context("Could not rename element")?.into_action(),
			Self::Swap { parent, a, b, } => swap_element_same_depth(root, parent, a, b, bookmarks, mutable_indices).context("Could not swap elements")?.into_action(),
			Self::Reorder { indices, mapping } => reorder_element(root, indices, mapping, bookmarks, mutable_indices).context("Could not reorder element")?.into_action(),
			Self::RemoveToHeldEntry { indices, mut indices_history, old_key, old_value } => {
				ensure!(held_entry.is_none(), "To remove an element and make a held entry out of it, one cannot have an pre-existing held entry.");

				let (indices, kv) = if let Some(old_value) = old_value {
					let ReplaceElementResult { indices, kv } = replace_element(root, (old_key, old_value), indices, bookmarks, mutable_indices).context("Could not remove element")?;
					(indices, kv)

				} else {
					let RemoveElementResult { indices, kv, replaces } = remove_element(root, indices, bookmarks, mutable_indices).context("Could not remove element")?;
					ensure!(!replaces, "If the indices had replacement for this value, then we would have the old value to swap it out with");
					(indices, kv)
				};
				indices_history.push(indices);
				*held_entry = Some(HeldEntry { kv, indices_history });
				Self::AddFromHeldEntry
			}
			Self::AddFromHeldEntry => {
				let HeldEntry { kv, mut indices_history } = held_entry.take().context("Expected a held entry to add to the tab")?;
				if let Some(indices) = indices_history.pop() {
					let AddElementResult { indices, old_value } = add_element(root, kv, indices, bookmarks, mutable_indices).context("Couldn't add element from held entry")?;
					let old_kv = root.key_value_at(&indices).context("We just added an element to these indices, it for sure is valid")?;
					let old_key = old_kv.0.map(|key| key.to_compact_string());
					Self::RemoveToHeldEntry { indices, indices_history, old_key, old_value }
				} else {
					Self::DiscardHeldEntry { held_entry: HeldEntry::from_aether(kv) }
				}
			}
			Self::DiscardHeldEntry { held_entry: new_held_entry } => {
				ensure!(held_entry.is_none(), "To create a held entry out of the aether, one cannot have an pre-existing held entry.");
				*held_entry = Some(new_held_entry);
				Self::AddFromHeldEntry
			}
			Self::CreateHeldEntry => {
				let held_entry = held_entry.take().context("Expected a held entry that was created")?;
				Self::DiscardHeldEntry { held_entry }
			}
			Self::Bulk { actions } => {
				Self::Bulk {
					actions: actions
						.into_vec()
						.into_iter()
						.rev()
						.map(|action| action.undo(root, bookmarks, mutable_indices, path, name, held_entry, window_properties))
						.collect::<Result<Vec<_>>>()?
						.into_boxed_slice()
				}
			}
		})
	}
}
