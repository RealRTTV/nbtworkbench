use compact_str::{CompactString, ToCompactString};

use crate::assets::*;
use crate::elements::chunk::{NbtChunk, NbtRegion};
use crate::elements::compound::NbtCompound;
use crate::elements::element_type::NbtElement;
use crate::vertex_buffer_builder::VertexBufferBuilder;
use crate::{encompasses, encompasses_or_equal, panic_unchecked, FileUpdateSubscription, Position};
use crate::{Navigate, OptionExt};

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
}

impl WorkbenchAction {
	#[cfg_attr(debug_assertions, inline(never))]
	pub fn undo(self, root: &mut NbtElement, bookmarks: &mut Vec<usize>, subscription: &mut Option<FileUpdateSubscription>) -> Self {
		unsafe { self.undo0(root, bookmarks, subscription).panic_unchecked("Failed to undo action") }
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	#[cfg_attr(debug_assertions, inline(never))]
	#[allow(clippy::collapsible_else_if, clippy::too_many_lines, clippy::cognitive_complexity)]
	unsafe fn undo0(self, root: &mut NbtElement, bookmarks: &mut Vec<usize>, subscription: &mut Option<FileUpdateSubscription>) -> Option<Self> {
		Some(match self {
			Self::Remove { element: (key, value), indices } => {
				let (&last, rem) = indices.split_last()?;
				let (height, true_height) = (value.height(), value.true_height());

				let mut iter = Navigate::new(rem.iter().copied(), root);
				while let Some((position, _, _, element, line_number)) = iter.next() {
					match position {
						Position::First | Position::Middle => {
							element.increment(height, true_height);
						}
						Position::Last | Position::Only => {
							match element.id() {
								NbtCompound::ID => {
									element.data.compound.insert(last, key?, value);
								}
								_ => element.insert(last, value).ok().panic_unchecked("Should've been able to add to element"),
							}
							let start = bookmarks.binary_search(&line_number).unwrap_or_else(std::convert::identity);
							for bookmark in bookmarks.iter_mut().skip(start) {
								if *bookmark - line_number < true_height {
									// do nothing, since the bookmark is within the tail node
								} else {
									*bookmark += true_height;
								}
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
				let line_number = Navigate::new(indices.iter().copied(), root).last().3;
				let (&last, rem) = indices.split_last()?;
				let element = Navigate::new(rem.iter().copied(), root).last().2.remove(last)?;
				let (height, true_height) = (element.1.height(), element.1.true_height());
				let mut iter = Navigate::new(rem.iter().copied(), root);
				while let Some((_, _, _, element, _)) = iter.next() {
					element.decrement(height, true_height);
				}

				let start = bookmarks.binary_search(&line_number).unwrap_or_else(std::convert::identity);
				for bookmark in bookmarks.iter_mut().skip(start) {
					*bookmark -= true_height;
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

				Self::Remove { element, indices }
			}
			Self::Rename { indices, key, value } => {
				let (&last, rem) = indices.split_last()?;
				let mut override_value = None;
				let key = if let Some(key) = key {
					let parent = Navigate::new(rem.iter().copied(), root).last().2;
					Some(if parent.id() == NbtRegion::ID {
						let (map, chunks) = &mut *parent.data.region.chunks;
						let from = map.get(last).copied()? as usize;
						let to = ((key.parse::<u8>().ok()? as usize) << 5) | (value.as_ref()?.parse::<u8>().ok()? as usize);
						chunks.swap(from, to);
						override_value = Some((from & 31).to_compact_string());
						(from >> 5).to_compact_string()
					} else if parent.id() == NbtCompound::ID {
						parent.data.compound.update_key(last, key)?
					} else {
						parent.data.chunk.inner.update_key(last, key)?
					})
				} else {
					None
				};
				let value = override_value.or_else(|| value.and_then(|value| Navigate::new(indices.iter().copied(), root).last().2.set_value(value)));
				if key.is_some() || value.is_some() {
					crate::recache_along_indices(rem, root);
				}
				Self::Rename { indices, key, value }
			}
			Self::Move { mut from, to, original_key } => {
				let mut changed_subscription_indices = false;

				let (key, mov) = {
					let line_number = Navigate::new(to.iter().copied(), root).last().3;
					let (&last, rem) = to.split_last()?;
					let element = Navigate::new(rem.iter().copied(), root).last().2.remove(last)?;
					let (height, true_height) = (element.1.height(), element.1.true_height());
					let mut iter = Navigate::new(rem.iter().copied(), root);
					while let Some((_, _, _, element, _)) = iter.next() {
						element.decrement(height, true_height);
					}
					let start = bookmarks.binary_search(&line_number).unwrap_or_else(std::convert::identity);
					for bookmark in bookmarks.iter_mut().skip(start) {
						*bookmark -= true_height;
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
					element
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
								if element.id() == NbtCompound::ID {
									element.data.compound.insert(last, original_key?, mov);
								} else {
									if element.insert(last, mov).is_err() {
										return None;
									}
								}
								break;
							}
						}
					}
					let start = bookmarks.binary_search(&line_number).unwrap_or_else(std::convert::identity);
					for bookmark in bookmarks.iter_mut().skip(start) {
						*bookmark += true_height;
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
							if element.id() == NbtCompound::ID {
								element.data.compound.insert(last, key?, value);
							} else if element.id() == NbtChunk::ID {
								element.data.chunk.inner.insert(last, key?, value);
							} else {
								if element.insert(last, value).is_err() {
									panic_unchecked("oh crap");
								}
							}

							let mut idx = bookmarks.binary_search(&line_number).unwrap_or_else(std::convert::identity);
							while let Some(bookmark) = bookmarks.get_mut(idx) {
								if *bookmark < old_true_height + line_number {
									bookmarks.remove(idx);
								} else {
									*bookmark = (*bookmark).wrapping_add(true_diff);
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
