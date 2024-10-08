use std::cell::SyncUnsafeCell;
use std::mem::MaybeUninit;
use std::path::PathBuf;
use std::sync::Arc;
use compact_str::{CompactString, ToCompactString};

use crate::{encompasses, encompasses_or_equal, FileUpdateSubscription, hash, HeldEntry, add_element, remove_element};
use crate::{Position, sum_indices};
use crate::Navigate;
use crate::elements::compound::{CompoundMap, Entry};
use crate::elements::element::NbtElement;
use crate::marked_line::MarkedLines;

#[derive(Debug)]
pub enum WorkbenchAction {
	Remove {
		element: (Option<CompactString>, NbtElement),
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
	Move {
		from: Box<[usize]>,
		to: Box<[usize]>,
		original_key: Option<CompactString>,
	},
	Replace {
		indices: Box<[usize]>,
		value: (Option<CompactString>, NbtElement),
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
		actions: Box<[WorkbenchAction]>,
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
			Self::Remove {
				element: (key, value),
				indices,
			} => {
				add_element(root, key, value, indices.as_ref(), bookmarks, subscription).expect("Could add element");
				Self::Add { indices }
			}
			Self::Add { indices } => {
				let (key, value) = remove_element(root, &indices, bookmarks, subscription).expect("Could remove element");
				Self::Remove {
					element: (key, value),
					indices,
				}
			}
			Self::Rename {
				indices,
				key,
				value,
			} => {
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
						crate::recache_along_indices(rem, root);
					}
					Self::Rename {
						indices,
						key,
						value,
					}
				} else {
					let old = PathBuf::from(value?.into_string());
					let old_name = old
						.file_name()
						.and_then(|str| str.to_str())?
						.to_owned()
						.into_boxed_str();
					let value = if let Some(new) = path.replace(old) {
						new.to_str()?.to_compact_string()
					} else {
						name.to_compact_string()
					};
					*name = old_name;
					Self::Rename {
						indices,
						key,
						value: Some(value),
					}
				}
			}
			Self::Move {
				mut from,
				to,
				original_key,
			} => {
				let mut changed_subscription_indices = false;

				let (key, mov) = {
					let (&last, rem) = to.split_last()?;
					let (_, _, parent, mut line_number) = Navigate::new(rem.iter().copied(), root).last();
					for n in 0..last {
						line_number += parent.get(n)?.true_height();
					}
					line_number += 1;
					let (key, element) = parent.remove(last)?;
					let (height, true_height) = (element.height(), element.true_height());
					let mut iter = Navigate::new(rem.iter().copied(), root);
					while let Some((_, _, _, element, _)) = iter.next() {
						element.decrement(height, true_height);
					}
					let _ = bookmarks.remove(line_number..line_number + true_height);
					bookmarks[line_number..].decrement(height, true_height);
					if let Some(subscription) = subscription {
						if to == subscription.indices {
							subscription.indices = from.clone();
							changed_subscription_indices = true;
						} else if encompasses(rem, &subscription.indices) {
							if subscription.indices[rem.len()] >= last {
								subscription.indices[rem.len()] -= 1;
							}
						}
					}
					crate::recache_along_indices(rem, root);
					(key, element)
				};

				{
					let line_number = {
						let last = from.last().copied()?;
						if last == 0 {
							Navigate::new(from.iter().copied().take(from.len() - 1), root)
								.last()
								.3 + 1
						} else {
							*from.last_mut()? -= 1;
							let x = Navigate::new(from.iter().copied(), root).last().3 + 1;
							*from.last_mut()? += 1;
							x
						}
					};
					let (&last, rem) = from.split_last()?;
					let (height, true_height) = (mov.height(), mov.true_height());
					let mut iter = Navigate::new(rem.iter().copied(), root);
					while let Some((position, _, _, element, _)) = iter.next() {
						match position {
							Position::First | Position::Middle => {
								element.increment(height, true_height);
							}
							Position::Last | Position::Only => {
								if let Some(compound) = element.as_compound_mut() {
									compound.insert(last, original_key?, mov);
								} else if let Some(chunk) = element.as_chunk_mut() {
									chunk.insert(last, original_key?, mov);
								} else {
									if element.insert(last, mov).is_err() { return None }
								}
								break;
							}
						}
					}
					bookmarks[line_number..].increment(height, true_height);
					if let Some(subscription) = subscription
						&& !changed_subscription_indices
						&& encompasses_or_equal(rem, &subscription.indices)
					{
						if subscription.indices[rem.len()] <= last {
							subscription.indices[rem.len()] += 1;
						}
					}
					crate::recache_along_indices(rem, root);
				}

				Self::Move {
					from: to,
					to: from,
					original_key: key,
				}
			}
			Self::Replace {
				indices,
				value: (key, value),
			} => {
				let element = Navigate::new(indices.iter().copied(), root).last().2;
				let (diff, true_diff) = (
					value.height().wrapping_sub(element.height()),
					value.true_height().wrapping_sub(element.true_height()),
				);
				if let Some((&last, rest)) = indices.split_last() {
					let mut iter = Navigate::new(rest.iter().copied(), root);
					while let Some((position, _, _, element, line_number)) = iter.next() {
						if let Position::Last | Position::Only = position {
							let (old_key, old_value) = element
								.remove(last)
								.expect("index is always valid");
							let old_true_height = old_value.true_height();
							element.decrement(old_value.height(), old_value.true_height());
							if let Some(compound) = element.as_compound_mut() {
								compound.insert(last, key?, value);
							} else if let Some(chunk) = element.as_chunk_mut() {
								chunk.insert(last, key?, value);
							} else {
								if element.insert(last, value).is_err() {
									panic!("oh crap");
								}
							}
							let _ = bookmarks.remove(line_number..line_number + old_true_height);
							bookmarks[line_number..].increment(true_diff, diff);

							crate::recache_along_indices(rest, root);
							if let Some(inner_subscription) = subscription
								&& encompasses(&indices, &inner_subscription.indices)
							{
								*subscription = None;
							}

							return Some(Self::Replace {
								indices,
								value: (old_key, old_value),
							});
						} else {
							element.increment(diff, true_diff);
						}
					}
					panic!("always will have a value")
				} else {
					let old_root = core::mem::replace(root, value);
					bookmarks.clear();
					return Some(Self::Replace {
						indices,
						value: (None, old_root),
					});
				}
			}
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
					*indices.find(hash!(entry.key), |&x| x == new_idx).expect("index obviously exists").as_mut() = idx;
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
				let Self::Replace { value, indices } = Self::Replace { indices, value: (original_key, value) }.undo0(root, bookmarks, subscription, path, name, held_entry)? else { return None };
				*held_entry = if let Some((held_entry_indices, is_swap)) = known_data { HeldEntry::FromKnown(value, held_entry_indices, is_swap) } else { HeldEntry::FromAether(value) };
				Self::HeldEntrySwap { indices, original_key: new_key }
			},
			Self::HeldEntryDrop { from_indices, indices, original_key } => {
				let (new_key, value) = remove_element(root, &indices, bookmarks, subscription).expect("Able to remove element");
				*held_entry = if let Some(from_indices) = &from_indices { HeldEntry::FromKnown((original_key, value), Box::clone(&*from_indices.get()), false) } else { HeldEntry::FromAether((original_key, value)) };
				if matches!(held_entry, HeldEntry::FromAether(_)) {
					Self::HeldEntryStealFromAether { indices, original_key: new_key }
				} else {
					Self::HeldEntrySteal { from_indices, original_key: new_key }
				}
			}
			Self::HeldEntrySteal { from_indices, original_key } => {
				let HeldEntry::FromKnown((new_key, value), indices, _) = held_entry.take() else { return None };
				add_element(root, original_key, value, &indices, bookmarks, subscription).expect("Able to add element");
				Self::HeldEntryDrop { from_indices, indices, original_key: new_key }

			},
			Self::HeldEntryStealFromAether { indices, original_key } => {
				let HeldEntry::FromAether((new_key, value)) = held_entry.take() else { return None };
				add_element(root, original_key, value, &indices, bookmarks, subscription).expect("Able to add element");
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
