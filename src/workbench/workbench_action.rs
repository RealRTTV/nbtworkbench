use compact_str::{CompactString, ToCompactString};
use std::cell::SyncUnsafeCell;
use std::ffi::OsStr;
use std::mem::MaybeUninit;
use std::path::PathBuf;
use std::sync::Arc;

use crate::elements::{CompoundMap, Entry, NbtElement, NbtElementAndKey};
use crate::hash;
use crate::workbench::{add_element, recache_along_indices, remove_element, replace_element, sum_indices, swap_elements, FileUpdateSubscription, HeldEntry, MarkedLines, Navigate};

#[derive(Debug)]
pub enum WorkbenchAction {
	Remove {
		element: NbtElementAndKey,
		indices: Box<[usize]>,
	},
	Add {
		indices: Box<[usize]>,
	},
	Rename {
		indices: Box<[usize]>,
		key: Option<CompactString>,
		value: Option<CompactString>,
	},
	Swap {
		parent: Box<[usize]>,
		a: usize,
		b: usize,
	},
	Replace {
		indices: Box<[usize]>,
		value: NbtElementAndKey,
	},
	ReorderCompound {
		indices: Box<[usize]>,
		reordering_indices: Box<[usize]>,
	},
	HeldEntrySwap {
		indices: Box<[usize]>,
		original_key: Option<CompactString>,
	},
	HeldEntryDrop {
		from_indices: Option<Arc<SyncUnsafeCell<Box<[usize]>>>>,
		indices: Box<[usize]>,
		original_key: Option<CompactString>,
	},
	HeldEntrySteal {
		from_indices: Option<Arc<SyncUnsafeCell<Box<[usize]>>>>,
		original_key: Option<CompactString>,
	},
	HeldEntryStealFromAether {
		indices: Box<[usize]>,
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
	#[cfg_attr(debug_assertions, inline(never))]
	pub fn undo(self, root: &mut NbtElement, bookmarks: &mut MarkedLines, subscription: &mut Option<FileUpdateSubscription>, path: &mut Option<PathBuf>, name: &mut Box<str>, held_entry: &mut HeldEntry) -> Self {
		unsafe {
			self.undo0(root, bookmarks, subscription, path, name, held_entry)
				.expect("Failed to undo action")
		}
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	#[cfg_attr(debug_assertions, inline(never))]
	#[allow(
		clippy::collapsible_else_if,
		clippy::too_many_lines,
		clippy::cognitive_complexity
	)]
	unsafe fn undo0(self, root: &mut NbtElement, bookmarks: &mut MarkedLines, subscription: &mut Option<FileUpdateSubscription>, path: &mut Option<PathBuf>, name: &mut Box<str>, held_entry: &mut HeldEntry) -> Option<Self> {
		Some(match self {
			Self::Remove { element: (key, value), indices, } => add_element(root, key, value, indices, bookmarks, subscription).expect("Couldn't add element"),
			Self::Add { indices } => remove_element(root, indices, bookmarks, subscription).expect("Could remove element").into_action(),
			Self::Replace { indices, value, } => replace_element(root, value, indices, bookmarks, subscription).expect("Could not replace element").into_action(),
			Self::Rename { indices, key, value } => {
				if let Some((&last, rem)) = indices.split_last() {
					let key = if let Some(key) = key {
						let parent = Navigate::new(rem.iter().copied(), root).last().2;
						Some(if let Some(compound) = parent.as_compound_mut() {
							compound.entries.update_key(last, key)?
						} else if let Some(chunk) = parent.as_chunk_mut() {
							chunk.entries.update_key(last, key)?
						} else {
							key
						})
					} else {
						None
					};
					let value = value.and_then(|value| {
						Navigate::new(indices.iter().copied(), root)
							.last()
							.2
							.set_value(value)
							.map(|x| x.0)
					});
					if key.is_some() || value.is_some() {
						recache_along_indices(rem, root);
					}
					Self::Rename {
						indices,
						key,
						value,
					}
				} else {
					let new = PathBuf::from(value?.into_string());
					let new_name = new
						.file_name()
						.and_then(OsStr::to_str)?
						.to_owned()
						.into_boxed_str();
					let old = path
						.replace(new)
						.and_then(|new| new.to_str().map(|s| s.to_compact_string()))
						.unwrap_or_else(|| name.to_compact_string());
					*name = new_name;
					Self::Rename { indices, key, value: Some(old) }
				}
			}
			Self::Swap { parent, a, b, } => swap_elements(root, parent, a, b, bookmarks, subscription).expect("Couldn't swap element").into_action(),
			Self::ReorderCompound { indices: traversal_indices, reordering_indices } => {
				let line_number = sum_indices(traversal_indices.iter().copied(), root);
				let (_, _, element, true_line_number) = Navigate::new(traversal_indices.iter().copied(), root).last();
				let open = element.open();
				let true_height = element.true_height();
				let CompoundMap { indices, entries } = if let Some(compound) = element.as_compound_mut() {
					 &mut *compound.entries
				} else if let Some(chunk) = element.as_chunk_mut() {
					&mut *chunk.entries
				} else {
					panic!("invalid type for compound reordering")
				};
				let line_numbers = {
					let mut current_line_number = line_number + 1;
					entries.iter().map(|entry| { let new_line_number = current_line_number; current_line_number += entry.value.height(); new_line_number }).collect::<Vec<_>>()
				};
				let true_line_numbers = {
					let mut current_line_number = true_line_number + 1;
					entries.iter().map(|entry| { let new_line_number = current_line_number; current_line_number += entry.value.true_height(); new_line_number }).collect::<Vec<_>>()
				};
				let mut new_bookmarks = Vec::with_capacity(bookmarks[true_line_number..true_line_number + true_height].len());
				let mut previous_entries = core::mem::transmute::<_, Box<[MaybeUninit<Entry>]>>(core::mem::take(entries).into_boxed_slice());
				let mut new_entries = Vec::with_capacity(previous_entries.len());
				let mut inverted_indices = vec![0; previous_entries.len()];
				let mut current_line_number = line_number + 1;
				let mut current_true_line_number = true_line_number + 1;
				for (idx, &new_idx) in reordering_indices.iter().enumerate() {
					let entry = core::mem::replace(previous_entries.get_unchecked_mut(new_idx), MaybeUninit::uninit()).assume_init();
					*indices.find_mut(hash!(entry.key), |&x| x == new_idx).expect("index obviously exists") = idx;
					let line_number = *line_numbers.get_unchecked(new_idx);
					let true_line_number = *true_line_numbers.get_unchecked(new_idx);
					let height = entry.value.height();
					let true_height = entry.value.true_height();
					let offset = if open { current_line_number as isize - line_number as isize } else { 0 };
					let true_offset = current_true_line_number as isize - true_line_number as isize;
					for bookmark in bookmarks[true_line_number..true_line_number + true_height].iter() {
						new_bookmarks.push(bookmark.offset(offset as usize, true_offset as usize));
					}

					new_entries.push(entry);
					// swapped because that swaps the elements in the iterator
					inverted_indices[new_idx] = idx;
					current_true_line_number += true_height;
					current_line_number += height;
				}
				let bookmark_slice = &mut bookmarks[true_line_number..true_line_number + true_height];
				bookmark_slice.copy_from_slice(&new_bookmarks);
				*entries = new_entries;
				return Some(Self::ReorderCompound {
					indices: traversal_indices,
					reordering_indices: inverted_indices.into_boxed_slice(),
				})
			},
			Self::HeldEntrySwap { indices, original_key } => {
				let ((new_key, value), known_data) = match held_entry.take() {
					HeldEntry::Empty => panic!("this shouldnt.. happen"),
					HeldEntry::FromAether(value) => (value, None),
					HeldEntry::FromKnown(value, indices, is_swap) => (value, Some((indices, is_swap))),
				};
				let (indices, kv, _) = replace_element(root, (original_key, value), indices, bookmarks, subscription).expect("Could replace element").into_raw();
				*held_entry = if let Some((held_entry_indices, is_swap)) = known_data { HeldEntry::FromKnown(kv, held_entry_indices, is_swap) } else { HeldEntry::FromAether(kv) };
				Self::HeldEntrySwap { indices, original_key: new_key }
			},
			Self::HeldEntryDrop { from_indices, indices, original_key } => {
				let (indices, (new_key, value), _) = remove_element(root, indices, bookmarks, subscription).expect("Able to remove element").into_raw();
				*held_entry = if let Some(from_indices) = &from_indices { HeldEntry::FromKnown((original_key, value), Box::clone(&*from_indices.get()), false) } else { HeldEntry::FromAether((original_key, value)) };
				if matches!(held_entry, HeldEntry::FromAether(_)) {
					Self::HeldEntryStealFromAether { indices, original_key: new_key }
				} else {
					Self::HeldEntrySteal { from_indices, original_key: new_key }
				}
			}
			Self::HeldEntrySteal { from_indices, original_key } => {
				let HeldEntry::FromKnown((new_key, value), indices, _) = held_entry.take() else { return None };
				let Self::Add { indices } = add_element(root, original_key, value, indices, bookmarks, subscription).expect("Expected ability to add element") else { return None };
				Self::HeldEntryDrop { from_indices, indices, original_key: new_key }

			},
			Self::HeldEntryStealFromAether { indices, original_key } => {
				let HeldEntry::FromAether((new_key, value)) = held_entry.take() else { return None };
				let Self::Add { indices } = add_element(root, original_key, value, indices, bookmarks, subscription).expect("Expected ability to add element") else { return None };
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
					new_actions.push(action.undo(root, bookmarks, subscription, path, name, held_entry));
				}

				Self::Bulk {
					actions: new_actions.into_boxed_slice()
				}
			}
		})
	}
}
