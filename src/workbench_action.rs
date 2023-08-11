use crate::Position;

use crate::elements::element_type::NbtElement;
use crate::{panic_unchecked, Navigate, OptionExt};

pub enum WorkbenchAction {
	Remove {
		element: Result<(Box<str>, NbtElement), NbtElement>,
		indices: Box<[usize]>,
	},
	Add {
		indices: Box<[usize]>,
	},
	Rename {
		indices: Box<[usize]>,
		previous: Box<str>,
		key: bool,
	},
	Move {
		from: Box<[usize]>,
		to: Box<[usize]>,
		original_key: Option<Box<str>>,
	},
}

impl WorkbenchAction {
	#[cfg_attr(debug_assertions, inline(never))]
	pub fn undo(self, root: &mut NbtElement) -> Self {
		unsafe { self.undo0(root).panic_unchecked("Failed to undo action") }
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	#[cfg_attr(debug_assertions, inline(never))]
	#[allow(clippy::collapsible_else_if, clippy::too_many_lines)]
	unsafe fn undo0(self, root: &mut NbtElement) -> Option<Self> {
		Some(match self {
			Self::Remove { element, indices } => {
				let (key, value) = match element {
					Ok((key, value)) => (Some(key), value),
					Err(value) => (None, value),
				};

				let (&last, rem) = indices.split_last()?;
				let (height, true_height) = (value.height(), value.true_height());

				let mut iter = Navigate::new(rem.iter().copied(), root);
				while let Some((position, _, _, element)) = iter.next() {
					match position {
						Position::First | Position::Middle => {
							element.increment(height, true_height);
						}
						Position::Last | Position::Only => {
							match element {
								NbtElement::Compound(compound) => {
									compound.insert_full(last, key?.into_string(), value);
								}
								element => element.insert(last, value).ok().panic_unchecked("Should've been able to add to element"),
							}
							break;
						}
					}
				}

				Self::Add { indices }
			}
			Self::Add { indices } => {
				let (&last, rem) = indices.split_last()?;
				let element = Navigate::new(rem.iter().copied(), root).last().2.remove(last)?;
				let (height, true_height) = match &element {
					Ok((_, x)) | Err(x) => (x.height(), x.true_height()),
				};
				let mut iter = Navigate::new(rem.iter().copied(), root);
				while let Some((_, _, _, element)) = iter.next() {
					element.decrement(height, true_height);
				}

				Self::Remove { element, indices }
			}
			Self::Rename { indices, previous, key } => {
				if key {
					let (&last, rem) = indices.split_last()?;
					Self::Rename {
						previous: {
							match Navigate::new(rem.iter().copied(), root).last() {
								(_, _, NbtElement::Compound(compound)) => compound.update_key(last, previous)?,
								_ => panic_unchecked("Key was given for non-compound tail parent"),
							}
						},
						key,
						indices,
					}
				} else {
					Self::Rename {
						previous: Navigate::new(indices.iter().copied(), root).last().2.set_value(previous)?,
						key,
						indices,
					}
				}
			}
			Self::Move { from, to, original_key } => {
				let (key, mov) = {
					let (&last, rem) = to.split_last()?;
					let element = Navigate::new(rem.iter().copied(), root).last().2.remove(last)?;
					let (height, true_height) = match &element {
						Ok((_, x)) | Err(x) => (x.height(), x.true_height()),
					};
					let mut iter = Navigate::new(rem.iter().copied(), root);
					while let Some((_, _, _, element)) = iter.next() {
						element.decrement(height, true_height);
					}
					match element {
						Ok((k, v)) => (Some(k), v),
						Err(v) => (None, v),
					}
				};

				{
					let (&last, rem) = from.split_last()?;
					let (height, true_height) = (mov.height(), mov.true_height());
					let mut iter = Navigate::new(rem.iter().copied(), root);
					while let Some((position, _, _, element)) = iter.next() {
						match position {
							Position::First | Position::Middle => {
								element.increment(height, true_height);
							}
							Position::Last | Position::Only => {
								if let NbtElement::Compound(compound) = element {
									compound.insert_full(last, original_key?.into_string(), mov);
								} else {
									if element.insert(last, mov).is_err() {
										return None;
									}
								}
								break;
							}
						}
					}
				}

				Self::Move {
					from: to,
					to: from,
					original_key: key,
				}
			}
		})
	}
}
