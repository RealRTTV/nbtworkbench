use std::path::PathBuf;
use compact_str::{CompactString, ToCompactString};

use crate::assets::{ADD_TAIL_UV, ADD_UV, MOVE_TAIL_UV, MOVE_UV, REMOVE_TAIL_UV, REMOVE_UV, RENAME_TAIL_UV, RENAME_UV, REPLACE_TAIL_UV, REPLACE_UV};
use crate::elements::element::NbtElement;
use crate::vertex_buffer_builder::VertexBufferBuilder;
use crate::{encompasses, encompasses_or_equal, panic_unchecked, FileUpdateSubscription, Position, Bookmark};
use crate::{Navigate, OptionExt};

pub enum WorkbenchAction {
	Remove {
		element: (Option<CompactString>, NbtElement),
		indices: Box<[usize]>
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
}

impl WorkbenchAction {
	#[cfg_attr(debug_assertions, inline(never))]
	pub fn undo(self, root: &mut NbtElement, bookmarks: &mut Vec<Bookmark>, subscription: &mut Option<FileUpdateSubscription>, path: &mut Option<PathBuf>, name: &mut Box<str>) -> Self {
		unsafe { self.undo0(root, bookmarks, subscription, path, name).panic_unchecked("Failed to undo action") }
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	#[cfg_attr(debug_assertions, inline(never))]
	#[allow(clippy::collapsible_else_if, clippy::too_many_lines, clippy::cognitive_complexity)]
	unsafe fn undo0(self, root: &mut NbtElement, bookmarks: &mut Vec<Bookmark>, subscription: &mut Option<FileUpdateSubscription>, path: &mut Option<PathBuf>, name: &mut Box<str>) -> Option<Self> {
		Some(match self {
			Self::Remove { element: (key, value), indices } => {
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
							let start = bookmarks.binary_search(&Bookmark::new(line_number, 0)).map_or_else(|x| x + 1, std::convert::identity);
							for bookmark in bookmarks.iter_mut().skip(start) {
								bookmark.true_line_number += true_height;
								bookmark.line_number += height;
							}
							break;
						}
					}
				}

				if let Some(subscription) = subscription && encompasses_or_equal(rem, &subscription.indices) {
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

				let mut idx = bookmarks.binary_search(&Bookmark::new(line_number, 0)).unwrap_or_else(std::convert::identity);
				while let Some(bookmark) = bookmarks.get_mut(idx) {
					if bookmark.true_line_number - line_number < true_height {
						let _ = bookmarks.remove(idx);
					} else {
						bookmark.true_line_number -= true_height;
						bookmark.line_number -= height;
						idx += 1;
					}
				}

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

				Self::Remove { element: (key, element), indices }
			}
			Self::Rename { indices, key, value } => {
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
					let value = override_value.or_else(|| value.and_then(|value| Navigate::new(indices.iter().copied(), root).last().2.set_value(value).map(|x| x.0)));
					if key.is_some() || value.is_some() {
						crate::recache_along_indices(rem, root);
					}
					Self::Rename { indices, key, value }
				} else {
					let old = PathBuf::from(value?.into_string());
					let old_name = old.file_name().and_then(|str| str.to_str())?.to_owned().into_boxed_str();
					let value = if let Some(new) = path.replace(old) {
						new.to_str()?.to_compact_string()
					} else {
						name.to_compact_string()
					};
					*name = old_name;
					Self::Rename { indices, key, value: Some(value) }
				}
			}
			Self::Move { mut from, to, original_key } => {
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
					let mut idx = bookmarks.binary_search(&Bookmark::new(line_number, 0)).unwrap_or_else(std::convert::identity);
					while let Some(bookmark) = bookmarks.get_mut(idx) {
						if bookmark.true_line_number - line_number < true_height {
							let _ = bookmarks.remove(idx);
						} else {
							bookmark.true_line_number -= true_height;
							bookmark.line_number -= height;
							idx += 1;
						}
					}
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
							Navigate::new(from.iter().copied().take(from.len() - 1), root).last().3 + 1
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
									if element.insert(last, mov).is_err() {
										return None;
									}
								}
								break;
							}
						}
					}
					let start = bookmarks.binary_search(&Bookmark::new(line_number, 0)).map_or_else(|x| x + 1, std::convert::identity);
					for bookmark in bookmarks.iter_mut().skip(start) {
						bookmark.true_line_number += true_height;
						bookmark.line_number += height;
					}
					if let Some(subscription) = subscription && !changed_subscription_indices && encompasses_or_equal(rem, &subscription.indices) {
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
			Self::Replace { indices, value: (key, value) } => {
				let element = Navigate::new(indices.iter().copied(), root).last().2;
				let (diff, true_diff) = (value.height().wrapping_sub(element.height()), value.true_height().wrapping_sub(element.true_height()));
				if let Some((&last, rest)) = indices.split_last() {
					let mut iter = Navigate::new(rest.iter().copied(), root);
					while let Some((position, _, _, element, line_number)) = iter.next() {
						if let Position::Last | Position::Only = position {
							let (old_key, old_value) = element.remove(last).panic_unchecked("index is always valid");
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

							let mut idx = bookmarks.binary_search(&Bookmark::new(line_number, 0)).unwrap_or_else(std::convert::identity);
							while let Some(bookmark) = bookmarks.get_mut(idx) {
								if bookmark.line_number < old_true_height + line_number {
									bookmarks.remove(idx);
								} else {
									bookmark.true_line_number = bookmark.true_line_number.wrapping_add(true_diff);
									bookmark.line_number = bookmark.line_number.wrapping_add(diff);
									idx += 1;
								}
							}

							crate::recache_along_indices(rest, root);
							if let Some(inner_subscription) = subscription && encompasses(&indices, &inner_subscription.indices) {
								*subscription = None;
							}

							return Some(Self::Replace { indices, value: (old_key, old_value) });
						} else {
							element.increment(diff, true_diff);
						}
					}
					panic_unchecked("always will have a value")
				} else {
					let old_root = core::mem::replace(root, value);
					bookmarks.clear();
					return Some(Self::Replace { indices, value: (None, old_root) });
				}
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
			Self::Replace { .. } => builder.draw_texture(pos, if tail { REPLACE_TAIL_UV } else { REPLACE_UV }, (16, 16)),
		}
	}
}
