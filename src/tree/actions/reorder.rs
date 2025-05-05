use crate::elements::{CompoundMap, NbtElement, NbtPatternMut};
use crate::hash;
use crate::tree::{MutableIndices, NavigationInformationMut, OwnedIndices};
use crate::workbench::{MarkedLines, WorkbenchAction};

#[must_use]
pub fn reorder_element<'m1, 'm2: 'm1>(root: &mut NbtElement, indices: OwnedIndices, mapping: impl Into<Box<[usize]>>, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>) -> Option<ReorderElementResult> {
    let NavigationInformationMut { element, line_number, true_line_number, .. } = root.navigate_mut(&indices)?;
    let mapping = mapping.into();
    let is_parent_open = element.is_open();
    let parent_true_height = element.true_height();
    let CompoundMap { indices: map_indices, entries } = match element.as_pattern_mut() {
        NbtPatternMut::Compound(compound) => &mut *compound.entries,
        NbtPatternMut::Chunk(chunk) => &mut *chunk.entries,
        _ => return None
    };
    let line_numbers = {
        let mut current_line_number = line_number + 1;
        entries.iter().map(|entry| { let new_line_number = current_line_number; current_line_number += entry.value.height(); new_line_number }).collect::<Vec<_>>()
    };
    let true_line_numbers = {
        let mut current_line_number = true_line_number + 1;
        entries.iter().map(|entry| { let new_line_number = current_line_number; current_line_number += entry.value.true_height(); new_line_number }).collect::<Vec<_>>()
    };
    
    let mut new_bookmarks = Vec::with_capacity(bookmarks[true_line_number..true_line_number + parent_true_height].len());
    let mut previous_entries = core::mem::take(entries).into_iter().map(|x| Some(x)).collect::<Vec<_>>();
    let mut new_entries = Vec::with_capacity(previous_entries.len());
    let mut inverted_mapping = vec![0; previous_entries.len()];
    let mut new_child_line_number = line_number + 1;
    let mut new_child_true_line_number = true_line_number + 1;
    
    for (idx, &new_idx) in mapping.iter().enumerate() {
        let entry = previous_entries.get_mut(new_idx)?.take()?;
        
        *map_indices.find_mut(hash!(entry.key), |&x| x == new_idx)? = idx;
        
        let old_child_line_number = *line_numbers.get(new_idx)?;
        let old_child_true_line_number = *true_line_numbers.get(new_idx)?;
        let child_height = entry.value.height();
        let child_true_height = entry.value.true_height();
        
        let offset = if is_parent_open { new_child_line_number as isize - old_child_line_number as isize } else { 0 };
        let true_offset = new_child_true_line_number as isize - old_child_true_line_number as isize;
        
        for bookmark in bookmarks.for_element(&entry.value, old_child_true_line_number) {
            new_bookmarks.push(bookmark.offset(offset, true_offset));
        }

        new_entries.push(entry);
        inverted_mapping[new_idx] = idx;
        
        new_child_true_line_number += child_true_height;
        new_child_line_number += child_height;
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
    *entries = new_entries;
    
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
    pub fn into_action(self) -> WorkbenchAction {
        WorkbenchAction::Reorder {
            indices: self.indices,
            mapping: self.mapping,
        }
    }
}
