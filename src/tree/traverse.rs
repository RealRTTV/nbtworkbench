use itertools::Itertools;
use thiserror::Error;
use crate::elements::{ComplexNbtElementVariant, NbtElement};
use crate::tree::{Indices, OwnedIndices};

fn find_traversal_child_idx(element: &NbtElement, indices: &Indices, y: &mut usize, line_number: &mut usize, true_line_number: &mut usize) -> Result<usize, TraversalError> {
	let initial_remaining_y = *y;
	(0..element.len().ok_or_else(|| TraversalError::ParentWasPrimitive { indices: indices.to_owned(), parent: element.display_name() })?)
		.map(|idx| &element[idx])
		.find_position(|child| {
			let (height, true_height) = child.heights();
			if *y > height {
				*y -= height;
				*line_number += height;
				*true_line_number += true_height;
				false
			} else {
				true
			}
		})
		.map(|(idx, _)| idx)
		.ok_or_else(|| TraversalError::BeyondParentHeight { indices: indices.to_owned(), remaining_y: initial_remaining_y })
}

pub struct TraversalInformation<'a> {
	pub indices: OwnedIndices,
	pub line_number: usize,
	pub true_line_number: usize,
	pub depth: usize,
	pub key: Option<&'a str>,
	pub element: &'a NbtElement,
}

impl<'a> TraversalInformation<'a> {
	pub fn from(mut element: &'a NbtElement, mut y: usize, mut x: Option<usize>) -> Result<Self, TraversalError> {
		let mut indices = OwnedIndices::new();
		let mut depth = 0;
		let mut line_number = 0;
		let mut true_line_number = 1;
		let mut key = None;

		{
			let height = element.height();

			if y > height {
				return Err(TraversalError::BeyondFullHeight { y, height });
			}
		}

		while y > 0 {
			let display_name = element.display_name();
			if let Some(region) = element.as_region()
				&& region.is_grid_layout()
				&& let Some(x) = &mut x
			{
				if (2..=31 + 2).contains(x) {
					*x -= 2;
					y -= 1;
					let idx = y * 16 + *x;
					depth += *x;
					for child in (0..idx).filter_map(|idx| region.get(idx)) {
						let (height, true_height) = child.heights();
						line_number += height;
						true_line_number += true_height;
					}
					let child = match region.get(idx) {
						Some(x) => x,
						None => return Err(TraversalError::IndexOutOfBounds { indices, idx, parent: display_name }),
					};
					line_number += 1;
					true_line_number += 1;
					indices.push(idx);
					element = child;
					break;
				} else {
					return Err(TraversalError::OutsideDepthRegion { depth: *x });
				}
			} else {
				let idx = find_traversal_child_idx(element, &indices, &mut y, &mut line_number, &mut true_line_number)?;
				y -= 1;
				line_number += 1;
				true_line_number += 1;
				indices.push(idx);
				depth += 1;
				let kv = match element.get(idx) {
					Some(x) => x,
					None => return Err(TraversalError::IndexOutOfBounds { indices, idx, parent: display_name })
				};
				key = kv.0;
				element = kv.1;
			}
		}

		Ok(Self {
			indices,
			line_number,
			true_line_number,
			depth,
			key,
			element,
		})
	}
}

pub struct TraversalInformationMut<'a> {
	pub indices: OwnedIndices,
	pub line_number: usize,
	pub true_line_number: usize,
	pub depth: usize,
	pub key: Option<&'a str>,
	pub element: &'a mut NbtElement,
}

impl<'a> TraversalInformationMut<'a> {
	pub fn from(mut element: &'a mut NbtElement, mut y: usize, mut x: Option<usize>) -> Result<Self, TraversalError> {
		let mut indices = OwnedIndices::new();
		let mut depth = 0;
		let mut line_number = 0;
		let mut true_line_number = 1;
		let mut key = None;

		{
			let height = element.height();

			if y > height {
				return Err(TraversalError::BeyondFullHeight { y, height });
			}
		}

		while y > 0 {
			let display_name = element.display_name();
			if let Some(region) = element.as_region()
				&& region.is_grid_layout()
				&& let Some(x) = &mut x
			{
				// SAFETY: confirmed above to be of correct variant
				let region = unsafe { element.as_region_unchecked_mut() };
				if (2..=31 + 2).contains(x) {
					*x -= 2;
					y -= 1;
					let idx = y * 16 + *x;
					depth += *x;
					for child in (0..idx).filter_map(|idx| region.get(idx)) {
						let (height, true_height) = child.heights();
						line_number += height;
						true_line_number += true_height;
					}
					let child = match region.get_mut(idx) {
						Some(x) => x,
						None => return Err(TraversalError::IndexOutOfBounds { indices, idx, parent: display_name }),
					};
					line_number += 1;
					true_line_number += 1;
					indices.push(idx);
					element = child;
					break;
				} else {
					return Err(TraversalError::OutsideDepthRegion { depth: *x });
				}
			} else {
				let idx = find_traversal_child_idx(element, &indices, &mut y, &mut line_number, &mut true_line_number)?;
				let kv = match element.get_mut(idx) {
					Some(x) => x,
					None => return Err(TraversalError::IndexOutOfBounds { indices, idx, parent: display_name })
				};
				y -= 1;
				line_number += 1;
				true_line_number += 1;
				indices.push(idx);
				depth += 1;
				key = kv.0;
				element = kv.1;
			}
		}

		Ok(Self {
			indices,
			line_number,
			true_line_number,
			depth,
			key,
			element,
		})
	}
}

#[derive(Error, Debug)]
pub enum TraversalError {
	#[error("Traversal at y = {y} was beyond tree full height ({height}). (You clicked outside the tree)")]
	BeyondFullHeight { y: usize, height: usize },
	#[error("Traversal through a grid-style region failed due to being invalid depth, expected 2..=33, found {depth}. (You clicked in empty space on a grid-style region)")]
	OutsideDepthRegion { depth: usize },
	#[error("Tried to index node @ {indices} but found out that it was a {parent}. (Your caches are likely bad)")]
	ParentWasPrimitive { indices: OwnedIndices, parent: &'static str },
	#[error("Tried to index {parent} @ {indices} @ index {idx} but was out of bounds. (Your caches are likely bad)")]
	IndexOutOfBounds { indices: OwnedIndices, idx: usize, parent: &'static str },
	#[error("Tried to find child within node @ {indices} with {remaining_y} y to go. (Your caches are likely bad)")]
	BeyondParentHeight { indices: OwnedIndices, remaining_y: usize },
}
