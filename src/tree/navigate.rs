#![allow(dead_code)]

use std::iter::Peekable;
use compact_str::{CompactString, ToCompactString};
use crate::elements::{NbtElement, NbtPatternMut};
use crate::tree::Indices;

/// Navigates a tree from the given route of an indices list, will cause UB on invalid lists. Typically used for operations which are saved and not direct clicking interaction.
#[must_use]
pub struct Navigate<'a, I: IntoIterator + ExactSizeIterator> {
    iter: Peekable<I::IntoIter>,
    at_head: bool,
    node: Option<(usize, Option<CompactString>, &'a mut NbtElement)>,
    line_number: usize,
}

impl<'a, I: Iterator<Item = usize> + ExactSizeIterator> Navigate<'a, I> {
    #[deprecated]
    pub fn new(iter: impl IntoIterator<Item = usize>, root: &'a mut NbtElement) -> Self {
        Self {
            iter: iter.into_iter().peekable(),
            at_head: true,
            node: Some((0, None, root)),
            line_number: 1,
        }
    }

    fn step(&mut self) -> Option<()> {
        let (_, _, node) = self.node.take()?;
        let idx = self.iter.next()?;
        let (key, node) = {
            match node.as_pattern_mut() {
                NbtPatternMut::Compound(compound) => {
                    self.line_number += 1 + compound
                        .children()
                        .take(idx)
                        .map(|(_, b)| b)
                        .map(NbtElement::true_height)
                        .sum::<usize>();
                    compound
                        .get_mut(idx)
                        .map(|(a, b)| (Some(a.to_compact_string()), b))
                }
                NbtPatternMut::List(list) => {
                    self.line_number += 1 + list
                        .children()
                        .take(idx)
                        .map(NbtElement::true_height)
                        .sum::<usize>();
                    list.get_mut(idx).map(|b| (None, b))
                }
                NbtPatternMut::ByteArray(array) => {
                    self.line_number += 1 + idx;
                    array.get_mut(idx).map(|b| (None, b))
                }
                NbtPatternMut::IntArray(array) => {
                    self.line_number += 1 + idx;
                    array.get_mut(idx).map(|b| (None, b))
                }
                NbtPatternMut::LongArray(array) => {
                    self.line_number += 1 + idx;
                    array.get_mut(idx).map(|b| (None, b))
                }
                NbtPatternMut::Region(region) => {
                    self.line_number += 1 + region
                        .children()
                        .take(idx)
                        .map(NbtElement::true_height)
                        .sum::<usize>();
                    region.get_mut(idx).map(|b| (None, b))
                }
                NbtPatternMut::Chunk(chunk) => {
                    self.line_number += 1 + chunk
                        .children()
                        .take(idx)
                        .map(|(_, b)| b)
                        .map(NbtElement::true_height)
                        .sum::<usize>();
                    chunk
                        .get_mut(idx)
                        .map(|(a, b)| (Some(a.to_compact_string()), b))
                }
                _ => None,
            }?
        };
        self.node = Some((idx, key, node));
        Some(())
    }

    #[must_use]
    pub fn last(mut self) -> (usize, Option<CompactString>, &'a mut NbtElement, usize) {
        while self.iter.peek().is_some() {
            self.step();
        }

        self.node
            .map(|(a, b, c)| (a, b, c, self.line_number))
            .expect("List cannot be empty")
    }
}

pub struct NavigationInformation<'a> {
    pub idx: Option<usize>,
    pub key: Option<&'a str>,
    pub element: &'a NbtElement,
    pub line_number: usize,
    pub true_line_number: usize,
}

impl<'a> NavigationInformation<'a> {
    #[must_use]
    pub fn from(mut element: &'a NbtElement, indices: &Indices) -> Option<Self> {
        let mut line_number = 1;
        let mut true_line_number = 0;
        let mut key = None;

        for idx in indices {
            line_number += 1;
            true_line_number += 1;
            for jdx in 0..idx {
                let sibling = &element[jdx];
                line_number += sibling.height();
                true_line_number += sibling.true_height();
            }

            let (k, v) = element.get(idx)?;
            key = k;
            element = v;
        }

        Some(Self {
            idx: indices.last(),
            key,
            element,
            line_number,
            true_line_number,
        })
    }
}

pub struct NavigationInformationMut<'a> {
    pub idx: Option<usize>,
    pub key: Option<&'a str>,
    pub element: &'a mut NbtElement,
    pub line_number: usize,
    pub true_line_number: usize,
}

impl<'a> NavigationInformationMut<'a> {
    #[must_use]
    pub fn from(mut element: &'a mut NbtElement, indices: &Indices) -> Option<Self> {
        let mut line_number = 1;
        let mut true_line_number = 0;
        let mut key = None;

        for idx in indices {
            line_number += 1;
            true_line_number += 1;
            for jdx in 0..idx {
                let sibling = &element[jdx];
                line_number += sibling.height();
                true_line_number += sibling.true_height();
            }

            let (k, v) = element.get_mut(idx)?;
            key = k;
            element = v;
        }

        Some(Self {
            idx: indices.last(),
            key,
            element,
            line_number,
            true_line_number,
        })
    }
}

pub struct ParentNavigationInformation<'nbt, 'indices> {
    pub idx: usize,
    pub key: Option<&'nbt str>,
    pub parent: &'nbt NbtElement,
    pub line_number: usize,
    pub true_line_number: usize,
    pub parent_indices: &'indices Indices,
}

impl<'nbt, 'indices> ParentNavigationInformation<'nbt, 'indices> {
    #[must_use]
    pub fn from(mut parent: &'nbt NbtElement, indices: &'indices Indices) -> Option<Self> {
        let (last, parent_indices) = indices.split_last()?;

        let mut line_number = 1;
        let mut true_line_number = 0;

        for idx in parent_indices {
            line_number += 1;
            true_line_number += 1;
            for jdx in 0..idx {
                let sibling = parent[jdx].as_nonnull()?;
                line_number += sibling.height();
                true_line_number += sibling.true_height();
            }

            parent = parent[idx].as_nonnull()?;
        }

        line_number += 1;
        true_line_number += 1;
        for jdx in 0..last {
            let sibling = parent[jdx].as_nonnull()?;
            line_number += sibling.height();
            true_line_number += sibling.true_height();
        }

        Some(Self {
            idx: last,
            key: parent.get(last).and_then(|(a, b)| a),
            parent,
            line_number,
            true_line_number,
            parent_indices,
        })
    }
}

pub struct ParentNavigationInformationMut<'nbt, 'indices> {
    pub idx: usize,
    pub key: Option<CompactString>,
    pub parent: &'nbt mut NbtElement,
    pub line_number: usize,
    pub true_line_number: usize,
    pub parent_indices: &'indices Indices,
}

impl<'nbt, 'indices> ParentNavigationInformationMut<'nbt, 'indices> {
    #[must_use]
    pub fn from(mut parent: &'nbt mut NbtElement, indices: &'indices Indices) -> Option<Self> {
        let (last, parent_indices) = indices.split_last()?;

        let mut line_number = 1;
        let mut true_line_number = 0;

        for idx in parent_indices {
            line_number += 1;
            true_line_number += 1;
            for jdx in 0..idx {
                let sibling = parent[jdx].as_nonnull()?;
                line_number += sibling.height();
                true_line_number += sibling.true_height();
            }

            parent = parent[idx].as_nonnull_mut()?;
        }

        line_number += 1;
        true_line_number += 1;
        for jdx in 0..last {
            let sibling = parent[jdx].as_nonnull()?;
            line_number += sibling.height();
            true_line_number += sibling.true_height();
        }

        Some(Self {
            idx: last,
            key: parent.get(last).and_then(|(a, b)| a.map(|x| x.to_compact_string())),
            parent,
            line_number,
            true_line_number,
            parent_indices,
        })
    }
}
