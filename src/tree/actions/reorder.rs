use crate::elements::{CompoundEntry, CompoundMap, NbtElement, NbtPatternMut};
use crate::hash;
use crate::tree::{MutableIndices, NavigationInformationMut, OwnedIndices};
use crate::workbench::{MarkedLines, WorkbenchAction};

#[must_use]
pub fn reorder_element<'m1, 'm2: 'm1>(root: &mut NbtElement, indices: OwnedIndices, mapping: impl Into<Box<[usize]>>, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<ReorderElementResult> {
	let NavigationInformationMut { element, line_number, true_line_number, .. } = root.navigate_mut(&indices)?;
	let len = element.len()?;
	let mapping = mapping.into();
	if mapping.len() != len {
		return None
	}
	let is_parent_open = element.is_open();
	let parent_true_height = element.true_height();
	let CompoundMap { indices: map_indices, entries } = match element.as_pattern_mut() {
		NbtPatternMut::Compound(compound) => &mut *compound.map,
		NbtPatternMut::Chunk(chunk) => &mut *chunk.map,
		_ => return None,
	};
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

	let mut new_bookmarks = Vec::with_capacity(bookmarks[true_line_number..true_line_number + parent_true_height].len());
	let previous_entries = core::mem::take(entries);
	let mut new_entries = vec![None; previous_entries.len()];
	let mut inverted_mapping = vec![0; previous_entries.len()];

	// line numbers for the current child under the old order
	let mut old_idx__line_number = line_number + 1;
	let mut old_idx__true_line_number = true_line_number + 1;

	for (((new_idx, &idx), entry), (new_idx__line_number, new_idx__true_line_number)) in mapping
		.iter()
		.enumerate()
		.zip(previous_entries.into_iter())
		.zip(new_idx__line_numbers)
	{
		let child_height = entry.value.height();
		let child_true_height = entry.value.true_height();

		let offset = if is_parent_open { new_idx__line_number as isize - old_idx__line_number as isize } else { 0 };
		let true_offset = new_idx__true_line_number as isize - old_idx__true_line_number as isize;
		for bookmark in bookmarks.for_element(&entry.value, old_idx__true_line_number) {
			new_bookmarks.push(bookmark.offset(offset, true_offset));
		}

		*map_indices.find_mut(hash!(entry.key), |&x| x == idx)? = new_idx;
		new_entries[new_idx] = Some(entry);
		inverted_mapping[new_idx] = idx;

		old_idx__line_number += child_height;
		old_idx__true_line_number += child_true_height;
	}

	mutable_indices.apply(|mutable_indices, _ci| {
		if indices.encompasses(mutable_indices) {
			let idx = &mut mutable_indices[indices.len()];
			*idx = mapping[*idx];
		}
	});

	let bookmark_slice = &mut bookmarks[true_line_number..true_line_number + parent_true_height];
	let new_bookmarks = MarkedLines::from(new_bookmarks);
	bookmark_slice.copy_from_slice(&new_bookmarks);
	*entries = new_entries
		.into_iter()
		.collect::<Option<Vec<CompoundEntry>>>()?;

	Some(ReorderElementResult {
		indices,
		mapping: inverted_mapping.into_boxed_slice(),
	})
}

pub struct ReorderElementResult {
	pub indices: OwnedIndices,
	pub mapping: Box<[usize]>,
}

impl ReorderElementResult {
	pub fn into_action(self) -> WorkbenchAction { WorkbenchAction::Reorder { indices: self.indices, mapping: self.mapping } }
}
