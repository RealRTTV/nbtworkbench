use compact_str::{CompactString, ToCompactString};
use crate::elements::{NbtByteArray, NbtElement, NbtIntArray, NbtLongArray, NbtPatternMut};
use itertools::Position;

/// Navigates through an [`NbtElement`] tree using a y value.
/// This traversal will return parents with their child indices and keys with their parent's element.
#[must_use]
pub struct TraverseParents<'a> {
    node: Option<&'a mut NbtElement>,
    x: usize,
    y: usize,
    killed: bool,
    head: bool,
    line_number: usize,
}

impl<'a> TraverseParents<'a> {
    pub fn new(x: usize, y: usize, root: &'a mut NbtElement) -> Self {
        Self {
            killed: y >= root.height() || y == 0,
            node: Some(root),
            x,
            y,
            head: true,
            line_number: 1,
        }
    }

    fn step(&mut self) -> Option<()> {
        self.node = Some('m: {
            let node = self.node.take()?;
            match node.as_pattern_mut() {
                NbtPatternMut::Region(region) => {
                    self.y -= 1;
                    if region.is_grid_layout() {
                        return None;
                    } else {
                        for value in region.children_mut() {
                            let height = value.height();
                            if self.y >= height {
                                self.y -= height;
                                self.line_number += value.true_height();
                                continue;
                            } else {
                                self.line_number += 1;
                                break 'm value;
                            }
                        }
                    }
                }
                NbtPatternMut::List(list) => {
                    self.y -= 1;
                    for value in list.children_mut() {
                        let height = value.height();
                        if self.y >= height {
                            self.y -= height;
                            self.line_number += value.true_height();
                            continue;
                        } else {
                            self.line_number += 1;
                            break 'm value;
                        }
                    }
                }
                NbtPatternMut::Compound(compound) => {
                    self.y -= 1;
                    for (_, value) in compound.children_mut() {
                        let height = value.height();
                        if self.y >= height {
                            self.y -= height;
                            self.line_number += value.true_height();
                            continue;
                        } else {
                            self.line_number += 1;
                            break 'm value;
                        }
                    }
                }
                NbtPatternMut::Chunk(chunk) => {
                    self.y -= 1;
                    for (_, value) in chunk.children_mut() {
                        let height = value.height();
                        if self.y >= height {
                            self.y -= height;
                            self.line_number += value.true_height();
                            continue;
                        } else {
                            self.line_number += 1;
                            break 'm value;
                        }
                    }
                }
                NbtPatternMut::ByteArray(array) => {
                    self.line_number += self.y;
                    break 'm array.get_mut(core::mem::replace(&mut self.y, 0) - 1)?;
                }
                NbtPatternMut::IntArray(array) => {
                    self.line_number += self.y;
                    break 'm array.get_mut(core::mem::replace(&mut self.y, 0) - 1)?;
                }
                NbtPatternMut::LongArray(array) => {
                    self.line_number += self.y;
                    break 'm array.get_mut(core::mem::replace(&mut self.y, 0) - 1)?;
                }
                _ => return None,
            }
            self.killed = true;
            self.node = None;
            return None
        });

        Some(())
    }

    #[must_use]
    fn extras(&mut self) -> (usize, Option<CompactString>, bool) {
        let Some(node) = self.node.as_ref() else { self.killed = true; return (0, None, true) };
        if let Some(region) = node.as_region() {
            if region.is_grid_layout() {
                self.y -= 1;
                if self.x < 2 {
                    self.killed = true;
                    return (0, None, true)
                }
                self.x -= 2;
                if self.x >= 32 {
                    self.killed = true;
                    return (0, None, true)
                }
                let idx = self.y * 32 + self.x;
                self.y = 0;
                return (idx, None, true)
            } else {
                let mut remaining_y = self.y - 1;
                for (idx, element) in region.children().enumerate() {
                    if remaining_y >= element.height() {
                        remaining_y -= element.height();
                        continue;
                    } else {
                        return (idx, None, remaining_y == 0);
                    }
                }
            }
        } else if let Some(list) = node.as_list() {
            let mut remaining_y = self.y - 1;
            for (idx, element) in list.children().enumerate() {
                if remaining_y >= element.height() {
                    remaining_y -= element.height();
                    continue;
                } else {
                    return (idx, None, remaining_y == 0);
                }
            }
        } else if let Some(chunk) = node.as_chunk() {
            let mut remaining_y = self.y - 1;
            for (idx, (key, element)) in chunk.children().enumerate() {
                if remaining_y >= element.height() {
                    remaining_y -= element.height();
                    continue;
                } else {
                    return (idx, Some(key.to_compact_string()), remaining_y == 0);
                }
            }
        } else if let Some(compound) = node.as_compound() {
            let mut remaining_y = self.y - 1;
            for (idx, (key, element)) in compound.children().enumerate() {
                if remaining_y >= element.height() {
                    remaining_y -= element.height();
                    continue;
                } else {
                    return (idx, Some(key.to_compact_string()), remaining_y == 0);
                }
            }
        } else if let NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID = node.id() {
            return (self.y - 1, None, true);
        }
        panic!("Expected parent element to be complex")
    }

    #[must_use]
    #[allow(clippy::should_implement_trait)] // no
    pub fn next(
        &mut self,
    ) -> Option<(
        Position,
        usize,
        Option<CompactString>,
        &mut NbtElement,
        usize,
    )> {
        if self.killed { return None }

        let head = core::mem::replace(&mut self.head, false);

        if !head {
            self.step();
        }

        let (idx, key, is_last) = self.extras();
        if self.killed { return None }
        self.killed = is_last;
        let position = match (head, self.killed) {
            (true, false) => Position::First,
            (false, false) => Position::Middle,
            (false, true) => Position::Last,
            (true, true) => Position::Only,
        };

        Some((
            position,
            idx,
            key,
            &mut **self.node.as_mut()?,
            self.line_number,
        ))
    }
}