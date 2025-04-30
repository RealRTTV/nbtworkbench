mod parents;

pub use parents::*;

use crate::elements::{NbtElement, NbtPatternMut};
use crate::tree::OwnedIndices;
use compact_str::{CompactString, ToCompactString};
use itertools::Itertools;

/// Navigates through an [`NbtElement`] tree using a y value.
#[must_use]
#[deprecated]
pub struct Traverse<'a> {
    node: Option<(usize, Option<CompactString>, &'a mut NbtElement)>,
    x: usize,
    y: usize,
    killed: bool,
    line_number: usize,
    depth: usize,
    indices: Vec<usize>,
}

impl<'a> Traverse<'a> {
    #[deprecated]
    pub fn new(x: usize, y: usize, root: &'a mut NbtElement) -> Self {
        Self {
            killed: y >= root.height() || y == 0,
            node: Some((0, None, root)),
            x,
            y,
            line_number: 1,
            depth: 0,
            indices: Vec::new(),
        }
    }

    fn step(&mut self) -> Option<()> {
        let (_, _, node) = self.node.take()?;

        let (idx, key, new) = 'm: {
            match node.as_pattern_mut() {
                NbtPatternMut::Region(region) => {
                    self.y -= 1;
                    if region.is_grid_layout() {
                        if self.x < 2 {
                            self.killed = true;
                            self.node = None;
                            return None;
                        }
                        self.x -= 2;
                        if self.x >= 32 {
                            self.killed = true;
                            self.node = None;
                            return None;
                        }
                        let idx = self.y * 32 + self.x;
                        for chunk in region.children().take(idx) {
                            self.line_number += chunk.true_height();
                        }
                        self.line_number += 1;
                        self.y = 0;
                        self.killed = true;
                        break 'm (idx, None, &mut region.chunks[idx]);
                    } else {
                        for (idx, value) in region.children_mut().enumerate() {
                            let height = value.height();
                            if self.y >= height {
                                self.line_number += value.true_height();
                                self.y -= height;
                                continue;
                            } else {
                                self.killed = self.y == 0;
                                self.line_number += 1;
                                break 'm (idx, None, value);
                            }
                        }
                    }
                }
                NbtPatternMut::Chunk(chunk) => {
                    self.y -= 1;
                    for (idx, (key, value)) in chunk.children_mut().enumerate() {
                        let height = value.height();
                        if self.y >= height {
                            self.line_number += value.true_height();
                            self.y -= height;
                            continue;
                        } else {
                            self.killed = self.y == 0;
                            self.line_number += 1;
                            break 'm (idx, Some(key.to_compact_string()), value);
                        }
                    }
                }
                NbtPatternMut::Compound(compound) => {
                    self.y -= 1;
                    for (idx, (key, value)) in compound.children_mut().enumerate() {
                        let height = value.height();
                        if self.y >= height {
                            self.line_number += value.true_height();
                            self.y -= height;
                            continue;
                        } else {
                            self.killed = self.y == 0;
                            self.line_number += 1;
                            break 'm (idx, Some(key.to_compact_string()), value);
                        }
                    }
                }
                NbtPatternMut::List(list) => {
                    self.y -= 1;
                    for (idx, value) in list.children_mut().enumerate() {
                        let height = value.height();
                        if self.y >= height {
                            self.line_number += value.true_height();
                            self.y -= height;
                            continue;
                        } else {
                            self.killed = self.y == 0;
                            self.line_number += 1;
                            break 'm (idx, None, value);
                        }
                    }
                }
                NbtPatternMut::ByteArray(array) => {
                    self.killed = true;
                    let idx = core::mem::replace(&mut self.y, 0) - 1;
                    self.line_number += idx + 1;
                    break 'm (idx, None, array.get_mut(idx)?);
                }
                NbtPatternMut::IntArray(array) => {
                    self.killed = true;
                    let idx = core::mem::replace(&mut self.y, 0) - 1;
                    self.line_number += idx + 1;
                    break 'm (idx, None, array.get_mut(idx)?);
                }
                NbtPatternMut::LongArray(array) => {
                    self.killed = true;
                    let idx = core::mem::replace(&mut self.y, 0) - 1;
                    self.line_number += idx + 1;
                    break 'm (idx, None, array.get_mut(idx)?);
                }
                _ => return None,
            }

            self.killed = true;
            self.node = None;
            return None;
        };

        self.indices.push(idx);

        self.node = Some((idx, key, new));

        Some(())
    }

    #[must_use]
    #[allow(clippy::type_complexity)] // literally can't otherwise the compiler crashes... yeah...
    pub fn last(mut self) -> Option<(usize, (usize, Option<CompactString>, &'a mut NbtElement, usize, Box<[usize]>))> {
        while self.y > 0 && !self.killed {
            self.depth += 1;
            self.step();
        }
        self.node.map(|(a, b, c)| (self.depth, (a, b, c, self.line_number, self.indices.into_boxed_slice())))
    }
}

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
