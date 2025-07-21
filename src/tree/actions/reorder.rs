use thiserror::Error;

use crate::elements::compound::CompoundMap;
use crate::elements::element::{NbtElement, NbtPatternMut};
use crate::hash;
use crate::history::WorkbenchAction;
use crate::tree::MutableIndices;
use crate::tree::indices::OwnedIndices;
use crate::tree::navigate::{NavigationError, NavigationInformationMut};
use crate::util::{self, InvertMappingError, ReorderMappingError, invert_mapping};
use crate::workbench::marked_line::MarkedLines;

#[rustfmt::skip]
#[allow(non_snake_case)]
pub fn reorder_element<'m1, 'm2: 'm1>(
	root: &mut NbtElement,
	indices: OwnedIndices,
	mapping: impl Into<Box<[usize]>>,
	mi: &'m1 mut MutableIndices<'m2>
) -> Result<ReorderElementResult, ReorderElementError> {
	let NavigationInformationMut { element, line_number, true_line_number, .. } = root.navigate_mut(&indices)?;
	let len = element
		.len()
		.ok_or_else(|| ReorderElementError::ElementWasPrimitive { element: element.display_name() })?;
	let mapping = mapping.into();
	if mapping.len() != len {
		return Err(ReorderElementError::InvalidMappingLength { mapping_len: mapping.len(), parent_len: len })
	}
	let is_parent_open = element.is_open();
	let parent_true_height = element.true_height();
	let CompoundMap { indices: map_indices, entries } = match element.as_pattern_mut() {
		NbtPatternMut::Compound(compound) => &mut *compound.map,
		NbtPatternMut::Chunk(chunk) => &mut *chunk.map,
		_ => return Err(ReorderElementError::ElementWasNotMap { element: element.display_name() }),
	};
	let inverted_mapping = invert_mapping(&mapping)?;
	// line numbers for the nth child under the new order
	let new_idx__line_numbers = {
		let mut new_idx_line_number = line_number + 1;
		let mut new_idx_true_line_number = true_line_number + 1;

		(0..len)
			.map(|idx| entries[mapping[idx]].value.heights())
			.map(|(height, true_height)| {
				let line_number = new_idx_line_number;
				let true_line_number = new_idx_true_line_number;
				new_idx_line_number += height;
				new_idx_true_line_number += true_height;
				(line_number, true_line_number)
			})
			.collect::<Vec<_>>()
	};

	let mut new_bookmarks = Vec::with_capacity(mi.bookmarks[true_line_number..true_line_number + parent_true_height].len());

	// line numbers for the current child under the old ordering
	let mut old_idx__line_number = line_number + 1;
	let mut old_idx__true_line_number = true_line_number + 1;

	for (((idx, &new_idx), entry), (new_idx__line_number, new_idx__true_line_number)) in mapping
		.iter()
		.enumerate()
		.zip(entries.iter())
		.zip(new_idx__line_numbers)
	{
		let child_height = entry.value.height();
		let child_true_height = entry.value.true_height();

		let offset = if is_parent_open { new_idx__line_number as isize - old_idx__line_number as isize } else { 0 };
		let true_offset = new_idx__true_line_number as isize - old_idx__true_line_number as isize;
		for bookmark in mi.bookmarks.for_element(&entry.value, old_idx__true_line_number) {
			new_bookmarks.push(bookmark.offset(offset, true_offset));
		}

		*map_indices
			.find_mut(hash!(entry.key), |&x| x == idx)
			.ok_or_else(|| ReorderElementError::NoEntryInIndices { idx })? = new_idx;

		old_idx__line_number += child_height;
		old_idx__true_line_number += child_true_height;
	}

	mi.apply(|mutable_indices, _ci| {
		if indices.encompasses(mutable_indices) {
			let idx = &mut mutable_indices[indices.len()];
			*idx = mapping[*idx];
		}
	});

	let bookmark_slice = &mut mi.bookmarks[true_line_number..true_line_number + parent_true_height];
	let new_bookmarks = MarkedLines::from(new_bookmarks);
	bookmark_slice.copy_from_slice(&new_bookmarks);

	util::reorder(entries, &*mapping)?;

	Ok(ReorderElementResult { indices, mapping: inverted_mapping })
}

pub struct ReorderElementResult {
	pub indices: OwnedIndices,
	pub mapping: Box<[usize]>,
}

impl ReorderElementResult {
	pub fn into_action(self) -> WorkbenchAction { WorkbenchAction::Reorder { indices: self.indices, mapping: self.mapping } }
}

#[derive(Error, Debug)]
pub enum ReorderElementError {
	#[error(transparent)]
	Navigation(#[from] NavigationError),
	#[error(transparent)]
	InvertMapping(#[from] InvertMappingError),
	#[error(transparent)]
	ReorderMapping(#[from] ReorderMappingError),
	#[error("{element} is primitive when was expected to be complex.")]
	ElementWasPrimitive { element: &'static str },
	#[error("Mapping was of length {mapping_len} while expecting element length {parent_len}.")]
	InvalidMappingLength { mapping_len: usize, parent_len: usize },
	#[error("Expected a map-based element to reorder, but found {element}")]
	ElementWasNotMap { element: &'static str },
	#[error("No entry was found at index {idx} in indices")]
	NoEntryInIndices { idx: usize },
}
