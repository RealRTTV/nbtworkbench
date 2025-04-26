mod parents;
pub use parents::*;

use compact_str::{CompactString, ToCompactString};
use crate::elements::{NbtElement, NbtPatternMut};

/// Navigates through an [`NbtElement`] tree using a y value.
#[must_use]
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