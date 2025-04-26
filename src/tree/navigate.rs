use std::iter::Peekable;
use compact_str::{CompactString, ToCompactString};
use crate::elements::{NbtElement, NbtPatternMut};
use itertools::Position;

/// Navigates a tree from the given route of an indices list, will cause UB on invalid lists. Typically used for operations which are saved and not direct clicking interaction.
#[must_use]
pub struct Navigate<'a, I: IntoIterator + ExactSizeIterator> {
    iter: Peekable<I::IntoIter>,
    at_head: bool,
    node: Option<(usize, Option<CompactString>, &'a mut NbtElement)>,
    line_number: usize,
}

impl<'a, I: Iterator<Item = usize> + ExactSizeIterator> Navigate<'a, I> {
    pub fn new(iter: impl IntoIterator<IntoIter = I> + ExactSizeIterator, root: &'a mut NbtElement) -> Self {
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
    #[allow(dead_code)]
    #[allow(clippy::should_implement_trait)] // no
    pub fn next(&mut self) -> Option<(Position, usize, Option<&str>, &mut NbtElement, usize)> {
        let head = core::mem::replace(&mut self.at_head, false);

        if !head {
            self.step();
        }

        let tail = self.iter.peek().is_none();

        let position = match (head, tail) {
            (true, false) => Position::First,
            (false, false) => Position::Middle,
            (false, true) => Position::Last,
            (true, true) => Position::Only,
        };

        let (idx, key, element) = self.node.as_mut()?;

        Some((position, *idx, key.as_deref(), *element, self.line_number))
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