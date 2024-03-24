use std::mem::MaybeUninit;
use compact_str::{CompactString, ToCompactString};
use std::path::PathBuf;

use crate::assets::{ADD_TAIL_UV, ADD_UV, MOVE_TAIL_UV, MOVE_UV, REMOVE_TAIL_UV, REMOVE_UV, RENAME_TAIL_UV, RENAME_UV, REORDER_TAIL_UV, REORDER_UV, REPLACE_TAIL_UV, REPLACE_UV};
use crate::elements::element::NbtElement;
use crate::vertex_buffer_builder::VertexBufferBuilder;
use crate::{encompasses, encompasses_or_equal, FileUpdateSubscription};
use crate::{panic_unchecked, Position, sum_indices};
use crate::{Navigate, OptionExt};
use crate::elements::compound::{CompoundMap, Entry};
use crate::bookmark::{Bookmark, Bookmarks};

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
	}
}

impl WorkbenchAction {
	#[cfg_attr(debug_assertions, inline(never))]
	pub fn undo(self, root: &mut NbtElement, bookmarks: &mut Bookmarks, subscription: &mut Option<FileUpdateSubscription>, path: &mut Option<PathBuf>, name: &mut Box<str>) -> Self {
		unsafe {
			self.undo0(root, bookmarks, subscription, path, name)
				.panic_unchecked("Failed to undo action")
		}
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	#[cfg_attr(debug_assertions, inline(never))]
	#[allow(
		clippy::collapsible_else_if,
		clippy::too_many_lines,
		clippy::cognitive_complexity
	)]
	unsafe fn undo0(self, root: &mut NbtElement, bookmarks: &mut Bookmarks, subscription: &mut Option<FileUpdateSubscription>, path: &mut Option<PathBuf>, name: &mut Box<str>) -> Option<Self> {
		Some(match self {
			Self::Remove {
				element: (key, value),
				indices,
			} => {
				let (&last, rem) = indices.split_last()?;
				let (height, true_height) = (value.height(), value.true_height());

				let mut iter = Navigate::new(rem.iter().copied(), root);
				while let Some((position, _, _, element, mut line_number)) = iter.next() {
					match position {
						Position::First | Position::Middle => {
							element.increment(height, true_height);
						}
						Position::Last | Position::Only => {
							if let Some(compound) = element.as_compound_mut() {
								compound.insert(last, key?, value);
							} else if let Some(chunk) = element.as_chunk_mut() {
								chunk.insert(last, key?, value);
							} else {
								let _ = element.insert(last, value);
							}
							for n in 0..last {
								line_number += element.get(n)?.true_height();
							}
							line_number += 1;

							bookmarks[line_number..].increment(height, true_height);
							break;
						}
					}
				}

				if let Some(subscription) = subscription
					&& encompasses_or_equal(rem, &subscription.indices)
				{
					if subscription.indices[rem.len()] <= last {
						subscription.indices[rem.len()] += 1;
					}
				}
				crate::recache_along_indices(rem, root);

				Self::Add { indices }
			}
			Self::Add { indices } => {
				let (&last, rem) = indices.split_last()?;
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

				bookmarks[line_number..].decrement(height, true_height);

				if let Some(inner_subscription) = subscription {
					if *rem == *inner_subscription.indices {
						*subscription = None;
					} else if encompasses(rem, &inner_subscription.indices) {
						if inner_subscription.indices[rem.len()] >= last {
							inner_subscription.indices[rem.len()] -= 1;
						}
					}
				}
				crate::recache_along_indices(rem, root);

				Self::Remove {
					element: (key, element),
					indices,
				}
			}
			Self::Rename {
				indices,
				key,
				value,
			} => {
				if let Some((&last, rem)) = indices.split_last() {
					let mut override_value = None;
					let key = if let Some(key) = key {
						let parent = Navigate::new(rem.iter().copied(), root).last().2;
						Some(if let Some(region) = parent.as_region_mut() {
							let (map, chunks) = &mut *region.chunks;
							let from = map.get(last).copied()? as usize;
							let to = ((key.parse::<u8>().ok()? as usize) << 5) | (value.as_ref()?.parse::<u8>().ok()? as usize);
							chunks.swap(from, to);
							override_value = Some((from & 31).to_compact_string());
							(from >> 5).to_compact_string()
						} else if let Some(compound) = parent.as_compound_mut() {
							compound.update_key(last, key)?
						} else if let Some(chunk) = parent.as_chunk_mut() {
							chunk.update_key(last, key)?
						} else {
							key
						})
					} else {
						None
					};
					let value = override_value.or_else(|| {
						value.and_then(|value| {
							Navigate::new(indices.iter().copied(), root)
								.last()
								.2
								.set_value(value)
								.map(|x| x.0)
						})
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
								.panic_unchecked("index is always valid");
							let old_true_height = old_value.true_height();
							element.decrement(old_value.height(), old_value.true_height());
							if let Some(compound) = element.as_compound_mut() {
								compound.insert(last, key?, value);
							} else if let Some(chunk) = element.as_chunk_mut() {
								chunk.insert(last, key?, value);
							} else {
								if element.insert(last, value).is_err() {
									panic_unchecked("oh crap");
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
					panic_unchecked("always will have a value")
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
					panic_unchecked("invalid type for compound reordering")
				};
				let line_numbers = {
					let mut current_line_number = line_number + 1;
					entries.iter().map(|entry| { let new_line_number = current_line_number; current_line_number += entry.value.height(); new_line_number }).collect::<Vec<_>>()
				};
				let true_line_numbers = {
					let mut current_line_number = true_line_number + 1;
					entries.iter().map(|entry| { let new_line_number = current_line_number; current_line_number += entry.value.true_height(); new_line_number }).collect::<Vec<_>>()
				};
				let mut new_bookmarks = Box::<[Bookmark]>::new_uninit_slice(bookmarks[true_line_number..true_line_number + true_height].len());
				let mut new_bookmarks_len = 0;
				let mut previous_entries = core::mem::transmute::<_, Box<[MaybeUninit<Entry>]>>(core::mem::take(entries).into_boxed_slice());
				let mut new_entries = Box::<[Entry]>::new_uninit_slice(previous_entries.len());
				let mut inverted_indices = Box::<[usize]>::new_uninit_slice(previous_entries.len());
				let mut current_line_number = line_number + 1;
				let mut current_true_line_number = true_line_number + 1;
				for (idx, &new_idx) in reordering_indices.into_iter().enumerate() {
					let entry = core::mem::replace(previous_entries.get_unchecked_mut(new_idx), MaybeUninit::uninit()).assume_init();
					*indices.find(entry.hash, |&x| x == new_idx).panic_unchecked("index obviously exists").as_mut() = idx;
					let line_number = *line_numbers.get_unchecked(new_idx);
					let true_line_number = *true_line_numbers.get_unchecked(new_idx);
					let height = entry.value.height();
					let true_height = entry.value.true_height();
					let offset = if open { current_line_number as isize - line_number as isize } else { 0 };
					let true_offset = current_true_line_number as isize - true_line_number as isize;
					for bookmark in bookmarks[true_line_number..true_line_number + true_height].iter() {
						new_bookmarks[new_bookmarks_len].write(bookmark.offset(offset as usize, true_offset as usize));
						new_bookmarks_len += 1;
					}

					new_entries.get_unchecked_mut(idx).write(entry);
					// swapped because that swaps the elements in the iterator
					inverted_indices[new_idx].write(idx);
					current_true_line_number += true_height;
					current_line_number += height;
				}
				let bookmark_slice = &mut bookmarks[true_line_number..true_line_number + true_height];
				unsafe { core::ptr::copy_nonoverlapping(new_bookmarks.as_ptr().cast::<Bookmark>(), bookmark_slice.as_mut_ptr(), bookmark_slice.len()); }
				*entries = new_entries.assume_init().into_vec();
				return Some(Self::ReorderCompound {
					indices: traversal_indices,
					reordering_indices: inverted_indices.assume_init(),
				})
			}
		})
	}

	#[inline]
	pub fn render(&self, pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder, tail: bool) {
		match self {
			Self::Remove { .. } => builder.draw_texture(pos, if tail { REMOVE_TAIL_UV } else { REMOVE_UV }, (16, 16)),
			Self::Add { .. } => builder.draw_texture(pos, if tail { ADD_TAIL_UV } else { ADD_UV }, (16, 16)),
			Self::Rename { .. } => builder.draw_texture(pos, if tail { RENAME_TAIL_UV } else { RENAME_UV }, (16, 16)),
			Self::Move { .. } => builder.draw_texture(pos, if tail { MOVE_TAIL_UV } else { MOVE_UV }, (16, 16)),
			Self::Replace { .. } => builder.draw_texture(
				pos,
				if tail { REPLACE_TAIL_UV } else { REPLACE_UV },
				(16, 16),
			),
			Self::ReorderCompound { .. } => builder.draw_texture(pos, if tail { REORDER_TAIL_UV } else { REORDER_UV }, (16, 16)),
		}
	}
}
