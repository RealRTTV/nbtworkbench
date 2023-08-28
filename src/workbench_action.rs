use crate::Position;

use crate::elements::compound::NbtCompound;
use crate::elements::element_type::NbtElement;
use crate::vertex_buffer_builder::VertexBufferBuilder;
use crate::{panic_unchecked, Navigate, OptionExt};

pub enum WorkbenchAction {
	Remove { element: (Option<Box<str>>, NbtElement), indices: Box<[usize]> },
	Add { indices: Box<[usize]> },
	Rename { indices: Box<[usize]>, previous: Box<str>, key: bool },
	Move { from: Box<[usize]>, to: Box<[usize]>, original_key: Option<Box<str>> },
}

impl WorkbenchAction {
	#[cfg_attr(debug_assertions, inline(never))]
	pub fn undo(self, root: &mut NbtElement, bookmarks: &mut Vec<usize>) -> Self {
		unsafe { self.undo0(root, bookmarks).panic_unchecked("Failed to undo action") }
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	#[cfg_attr(debug_assertions, inline(never))]
	#[allow(clippy::collapsible_else_if, clippy::too_many_lines)]
	unsafe fn undo0(self, root: &mut NbtElement, bookmarks: &mut Vec<usize>) -> Option<Self> {
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
									element.data.compound.insert(last, key?.into_string(), value);
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

				crate::recache_along_indices(rem, root);

				Self::Remove { element, indices }
			}
			Self::Rename { indices, previous, key } => {
				if key {
					let (&last, rem) = indices.split_last()?;
					crate::recache_along_indices(rem, root);
					Self::Rename {
						previous: {
							let end = Navigate::new(rem.iter().copied(), root).last().2;
							match end.id() {
								NbtCompound::ID => end.data.compound.update_key(last, previous)?,
								_ => panic_unchecked("Key was given for non-compound tail parent"),
							}
						},
						key,
						indices,
					}
				} else {
					crate::recache_along_indices(&indices, root);
					Self::Rename {
						previous: Navigate::new(indices.iter().copied(), root).last().2.set_value(previous)?,
						key,
						indices,
					}
				}
			}
			Self::Move { from, to, original_key } => {
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
					crate::recache_along_indices(rem, root);
					element
				};

				{
					let line_number = Navigate::new(from.iter().copied(), root).last().3;
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
									element.data.compound.insert(last, original_key?.into_string(), mov);
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
					crate::recache_along_indices(rem, root);
				}

				Self::Move {
					from: to,
					to: from,
					original_key: key,
				}
			}
		})
	}

	#[inline]
	pub fn render(&self, x: usize, y: usize, builder: &mut VertexBufferBuilder, tail: bool) {
		use crate::assets::{ADD_TAIL_UV, ADD_UV, MOVE_TAIL_UV, MOVE_UV, REMOVE_TAIL_UV, REMOVE_UV, RENAME_TAIL_UV, RENAME_UV};

		match self {
			Self::Remove { .. } => builder.draw_texture((x, y), if tail { REMOVE_TAIL_UV } else { REMOVE_UV }, (16, 16)),
			Self::Add { .. } => builder.draw_texture((x, y), if tail { ADD_TAIL_UV } else { ADD_UV }, (16, 16)),
			Self::Rename { .. } => builder.draw_texture((x, y), if tail { RENAME_TAIL_UV } else { RENAME_UV }, (16, 16)),
			Self::Move { .. } => builder.draw_texture((x, y), if tail { MOVE_TAIL_UV } else { MOVE_UV }, (16, 16)),
		}
	}
}
