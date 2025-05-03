use std::cell::SyncUnsafeCell;
use std::path::PathBuf;
use std::sync::Arc;

use crate::elements::{NbtElement, NbtElementAndKey};
use crate::render::WindowProperties;
use crate::tree::{add_element, remove_element, rename_element, reorder_element, replace_element, swap_element_same_depth, MutableIndices, OwnedIndices};
use crate::workbench::{HeldEntry, MarkedLines};

use anyhow::{anyhow, ensure, Context, Result};
use compact_str::CompactString;

#[derive(Debug)]
pub enum WorkbenchAction {
	Remove {
		element: NbtElementAndKey,
		indices: OwnedIndices,
	},
	Add {
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
	HeldEntrySwap {
		indices: OwnedIndices,
		original_key: Option<CompactString>,
	},
	HeldEntryDrop {
		from_indices: Option<Arc<SyncUnsafeCell<OwnedIndices>>>,
		indices: OwnedIndices,
		original_key: Option<CompactString>,
		original_value: Option<NbtElement>,
	},
	HeldEntrySteal {
		from_indices: Option<Arc<SyncUnsafeCell<OwnedIndices>>>,
		original_key: Option<CompactString>,
	},
	HeldEntryStealFromAether {
		indices: OwnedIndices,
		original_key: Option<CompactString>,
	},
	CreateHeldEntry,
	DeleteHeldEntry {
		held_entry: HeldEntry,
	},
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
			Self::HeldEntrySwap { indices, .. } => indices.shrink_to_fit(),
			Self::HeldEntryDrop { indices, .. } => indices.shrink_to_fit(),
			Self::HeldEntrySteal { .. } => (),
			Self::HeldEntryStealFromAether { indices, .. } => indices.shrink_to_fit(),
			Self::CreateHeldEntry => (),
			Self::DeleteHeldEntry { .. } => (),
			Self::Bulk { actions } => for action in actions { action.shrink_to_fit(); },
		}
	}

	#[cfg_attr(debug_assertions, inline(never))]
	#[allow(
		clippy::collapsible_else_if,
		clippy::too_many_lines,
		clippy::cognitive_complexity
	)]
	fn undo<'m1, 'm2: 'm1>(self, root: &mut NbtElement, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>, path: &mut Option<PathBuf>, name: &mut Box<str>, held_entry: &mut HeldEntry, window_properties: &mut WindowProperties) -> Result<Self> {
		Ok(match self {
			Self::Remove { element: (key, value), indices, } => add_element(root, key, value, indices, bookmarks, mutable_indices).context("Couldn't add element")?,
			Self::Add { indices } => remove_element(root, indices, bookmarks, mutable_indices).context("Could remove element")?.into_action(),
			Self::Replace { indices, value, } => replace_element(root, value, indices, bookmarks, mutable_indices).context("Could not replace element")?.into_action(),
			Self::Rename { indices, key, value } => rename_element(root, indices, key, value, path, name, window_properties).context("Could not rename element")?.into_action(),
			Self::Swap { parent, a, b, } => swap_element_same_depth(root, parent, a, b, bookmarks, mutable_indices).context("Could not swap elements")?.into_action(),
			Self::Reorder { indices, mapping } => reorder_element(root, indices, mapping, bookmarks, mutable_indices).context("Could not reorder element")?.into_action(),
			Self::HeldEntrySwap { indices, original_key } => {
				let ((new_key, value), known_data) = match held_entry.take() {
					HeldEntry::Empty => return Err(anyhow!("this shouldnt.. happen")),
					HeldEntry::FromAether(value) => (value, None),
					HeldEntry::FromKnown(value, indices, is_swap) => (value, Some((indices, is_swap))),
				};
				let (indices, kv, _) = replace_element(root, (original_key, value), indices, bookmarks, mutable_indices).expect("Could replace element").into_raw();
				*held_entry = if let Some((held_entry_indices, is_swap)) = known_data { HeldEntry::FromKnown(kv, held_entry_indices, is_swap) } else { HeldEntry::FromAether(kv) };
				Self::HeldEntrySwap { indices, original_key: new_key }
			},
			Self::HeldEntryDrop { mut from_indices, indices, original_key, original_value: original_value } => {
				let (indices, (new_key, value), _) = if let Some(original_value) = original_value {
					replace_element(root, (original_key, original_value), indices, bookmarks, mutable_indices).context("Able to replace element")?.into_raw()
				} else {
					remove_element(root, indices, bookmarks, mutable_indices).context("Able to remove element")?.into_raw()
				};
				if let Some(from_indices_arc) = &mut from_indices {
					*held_entry = HeldEntry::FromKnown((original_key, value), OwnedIndices::clone(from_indices_arc.get_mut()), false);
					Self::HeldEntrySteal { from_indices, original_key: new_key }
				} else {
					*held_entry = HeldEntry::FromAether((original_key, value));
					Self::HeldEntryStealFromAether { indices, original_key: new_key }
				}
			}
			Self::HeldEntrySteal { from_indices, original_key } => {
				let HeldEntry::FromKnown((new_key, value), indices, _) = held_entry.take() else { return Err(anyhow!("impossible!")) };
				let (indices, old_value) = add_element(root, original_key, value, indices, bookmarks, mutable_indices).context("Expected ability to add element")?.into_raw() else { return Err(anyhow!("impossible!")) };
				Self::HeldEntryDrop { from_indices, indices, original_key: new_key, original_value: old_value }

			},
			Self::HeldEntryStealFromAether { indices, original_key } => {
				let HeldEntry::FromAether((new_key, value)) = held_entry.take() else { return Err(anyhow!("Had no held entry to steal from")) };
				let Self::Add { indices } = add_element(root, original_key, value, indices, bookmarks, mutable_indices).context("Expected ability to add element")? else { return Err(anyhow!("impossible!")) };
				Self::HeldEntryDrop {
					from_indices: None,
					indices,
					original_key: new_key,
				}
			}
			Self::CreateHeldEntry => {
				Self::DeleteHeldEntry { held_entry: held_entry.take() }
			},
			Self::DeleteHeldEntry { held_entry: new_held_entry } => {
				*held_entry = new_held_entry;
				Self::CreateHeldEntry
			},
			Self::Bulk { actions } => {
				let mut new_actions = Vec::with_capacity(actions.len());

				for action in actions.into_vec().into_iter().rev() {
					new_actions.push(action.undo(root, bookmarks, mutable_indices, path, name, held_entry, window_properties)?);
				}

				Self::Bulk {
					actions: new_actions.into_boxed_slice()
				}
			}
		})
	}
}
