mod parents;

pub use parents::*;

use crate::elements::NbtElement;
use crate::tree::OwnedIndices;
use compact_str::ToCompactString;
use itertools::Itertools;

fn find_traversal_child_idx(element: &NbtElement, y: &mut usize, line_number: &mut usize, true_line_number: &mut usize) -> Option<usize> {
    (0..element.len()?).map(|idx| &element[idx]).find_position(|child| {
        let (height, true_height) = (child.height(), child.true_height());
        if *y > height {
            *y -= height;
            *line_number += height;
            *true_line_number += true_height;
            false
        } else {
            true
        }
    }).map(|(idx, _)| idx)
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
    #[must_use]
    pub fn from(mut element: &'a NbtElement, mut y: usize, mut x: Option<usize>) -> Option<Self> {
        let mut indices = OwnedIndices::new();
        let mut depth = 0;
        let mut line_number = 1;
        let mut true_line_number = 0;
        let mut key = None;
        
        while y > 0 {
            if let Some(region) = element.as_region() && region.is_grid_layout() && let Some(x) = &mut x {
                if (2..=31 + 2).contains(x) {
                    *x -= 2;
                    y -= 1;
                    let idx = y * 16 + *x;
                    depth += *x;
                    for child in (0..idx).filter_map(|idx| region.get(idx)) {
                        let (height, true_height) = (child.height(), child.true_height());
                        line_number += height;
                        true_line_number += true_height;
                    }
                    let child = region.get(idx)?;
                    line_number += 1;
                    true_line_number += 1;
                    indices.push(idx);
                    element = child;
                    break;
                } else {
                    return None;
                }
            } else {
                let idx = find_traversal_child_idx(element, &mut y, &mut line_number, &mut true_line_number)?;
                y -= 1;
                line_number += 1;
                true_line_number += 1;
                indices.push(idx);
                depth += 1;
                let kv = element.get(idx)?;
                key = kv.0;
                element = kv.1;
            }
        }
        
        Some(Self {
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
    pub element: &'a NbtElement,
}

impl<'a> TraversalInformationMut<'a> {
    #[must_use]
    pub fn from(mut element: &'a mut NbtElement, mut y: usize, mut x: Option<usize>) -> Option<Self> {
        let mut indices = OwnedIndices::new();
        let mut depth = 0;
        let mut line_number = 1;
        let mut true_line_number = 0;
        let mut key = None;

        while y > 0 {
            if let Some(region) = element.as_region_mut() && region.is_grid_layout() && let Some(x) = &mut x {
                if (2..=31 + 2).contains(x) {
                    *x -= 2;
                    y -= 1;
                    let idx = y * 16 + *x;
                    depth += *x;
                    for child in (0..idx).filter_map(|idx| region.get(idx)) {
                        let (height, true_height) = (child.height(), child.true_height());
                        line_number += height;
                        true_line_number += true_height;
                    }
                    let child = region.get_mut(idx)?;
                    line_number += 1;
                    true_line_number += 1;
                    indices.push(idx);
                    element = child;
                    break;
                } else {
                    return None;
                }
            } else {
                let idx = find_traversal_child_idx(element, &mut y, &mut line_number, &mut true_line_number)?;
                y -= 1;
                line_number += 1;
                true_line_number += 1;
                indices.push(idx);
                depth += 1;
                let kv = element.get_mut(idx)?;
                key = kv.0;
                element = kv.1;
            }
        }

        Some(Self {
            indices,
            line_number,
            true_line_number,
            depth,
            key,
            element,
        })
    }
}
