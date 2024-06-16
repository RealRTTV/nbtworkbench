use std::cmp::Ordering;
use std::collections::Bound;
use std::convert::identity;
use std::ops::{Deref, DerefMut, Index, IndexMut, RangeBounds};

use crate::assets::{BOOKMARK_UV, HIDDEN_BOOKMARK_UV};
use crate::vertex_buffer_builder::Vec2u;

macro_rules! slice {
    ($($t:tt)*) => {
        unsafe { core::mem::transmute::<&[MarkedLine], &MarkedLineSlice>(&$($t)*) }
    };
}

macro_rules! slice_mut {
    ($($t:tt)*) => {
        unsafe { core::mem::transmute::<&mut [MarkedLine], &mut MarkedLineSlice>(&mut $($t)*) }
    };
}

#[derive(Copy, Clone, Debug)]
pub struct MarkedLine {
    true_line_number: usize,
    line_number: usize,
    uv: Vec2u,
}

impl MarkedLine {
    #[inline]
    pub const fn new(true_line_number: usize, line_number: usize) -> Self {
        Self {
            true_line_number,
            line_number,
            uv: BOOKMARK_UV,
        }
    }

    #[inline]
    pub const fn with_uv(true_line_number: usize, line_number: usize, uv: Vec2u) -> Self {
        Self {
            true_line_number,
            line_number,
            uv,
        }
    }

    #[inline]
    pub const fn true_line_number(self) -> usize { self.true_line_number }

    #[inline]
    pub const fn line_number(self) -> usize { self.line_number }

    #[inline]
    pub const fn uv(self) -> Vec2u { self.uv }

    #[inline]
    pub const fn hidden(self, line_number: usize) -> Self {
        Self {
            true_line_number: self.true_line_number,
            line_number,
            uv: HIDDEN_BOOKMARK_UV,
        }
    }

    #[inline]
    pub const fn open(self, line_number: usize) -> Self {
        Self {
            true_line_number: self.true_line_number,
            line_number,
            uv: BOOKMARK_UV,
        }
    }
    
    #[inline]
    pub const fn offset(self, offset: usize, true_offset: usize) -> Self {
        Self {
            true_line_number: self.true_line_number.wrapping_add(true_offset),
            line_number: self.line_number.wrapping_add(offset),
            uv: self.uv,
        }
    }
}

impl PartialEq for MarkedLine {
    fn eq(&self, other: &Self) -> bool { self.true_line_number == other.true_line_number }
}

impl Eq for MarkedLine {}

impl PartialOrd<Self> for MarkedLine {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}
impl Ord for MarkedLine {
    fn cmp(&self, other: &Self) -> Ordering { self.true_line_number.cmp(&other.true_line_number) }
}

pub struct MarkedLines {
    inner: Vec<MarkedLine>,
}

impl MarkedLines {
    #[inline]
    pub const fn new() -> Self {
        Self {
            inner: vec![]
        }
    }

    #[inline]
    pub fn toggle(&mut self, marked_line: MarkedLine) -> Result<(), MarkedLine> {
        match self.inner.binary_search(&marked_line) {
            Ok(idx) => Err(self.inner.remove(idx)),
            Err(idx) => {
                self.inner.insert(idx, marked_line);
                Ok(())
            }
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.inner.clear();
    }

    /// # Safety
    /// `inner` must be sorted least to greatest, i.e.; it is up to the caller to assure `inner.is_sorted()`
    #[inline]
    pub unsafe fn from_raw(inner: Vec<MarkedLine>) -> Self {
        Self {
            inner
        }
    }

    #[inline]
    pub fn into_raw(self) -> Box<[MarkedLine]> {
        self.inner.into_boxed_slice()
    }
    
    #[inline]
    pub fn remove<R: RangeBounds<usize>>(&mut self, range: R) -> Vec<MarkedLine> {
        match (range.start_bound().map(|&x| MarkedLine::new(x, 0)), range.end_bound().map(|&x| MarkedLine::new(x, 0))) {
            (Bound::Unbounded, Bound::Unbounded) => self.inner.drain(..),
            (Bound::Unbounded, Bound::Included(ref end)) => self.inner.drain(..=self.binary_search(end).unwrap_or_else(identity)),
            (Bound::Unbounded, Bound::Excluded(ref end)) => self.inner.drain(..self.binary_search(end).unwrap_or_else(identity)),
            (Bound::Included(ref start), Bound::Unbounded) => self.inner.drain(self.binary_search(start).unwrap_or_else(identity)..),
            (Bound::Included(ref start), Bound::Included(ref end)) => self.inner.drain(self.binary_search(start).unwrap_or_else(identity)..=self.binary_search(end).unwrap_or_else(identity)),
            (Bound::Included(ref start), Bound::Excluded(ref end)) => self.inner.drain(self.binary_search(start).unwrap_or_else(identity)..self.binary_search(end).unwrap_or_else(identity)),
            (Bound::Excluded(ref start), Bound::Unbounded) => self.inner.drain(self.binary_search(start).map_or_else(identity, |x| x + 1)..),
            (Bound::Excluded(ref start), Bound::Included(ref end)) => self.inner.drain(self.binary_search(start).map_or_else(identity, |x| x + 1)..=self.binary_search(end).unwrap_or_else(identity)),
            (Bound::Excluded(ref start), Bound::Excluded(ref end)) => self.inner.drain(self.binary_search(start).map_or_else(identity, |x| x + 1)..self.binary_search(end).unwrap_or_else(identity)),
        }.collect()
    }
}

#[repr(transparent)]
pub struct MarkedLineSlice([MarkedLine]);

impl MarkedLineSlice {
    #[inline]
    pub fn increment(&mut self, value: usize, true_value: usize) {
        for marked_line in &mut self.0 {
            marked_line.line_number = marked_line.line_number.wrapping_add(value);
            marked_line.true_line_number = marked_line.true_line_number.wrapping_add(true_value);
        }
    }

    #[inline]
    pub fn decrement(&mut self, value: usize, true_value: usize) {
        for marked_line in &mut self.0 {
            marked_line.line_number -= value;
            marked_line.true_line_number -= true_value;
        }
    }

    #[inline]
    pub fn split_first(&self) -> Option<(MarkedLine, &MarkedLineSlice)> {
        if let [head, rest @ ..] = &self.0 {
            Some((*head, slice!(rest)))
        } else {
            None
        }
    }

    #[inline]
    pub fn iter(&self) -> Iter<'_> {
        Iter(&self.0)
    }

    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        IterMut(&mut self.0)
    }
}

impl Deref for MarkedLines {
    type Target = MarkedLineSlice;

    fn deref(&self) -> &Self::Target {
        unsafe { core::mem::transmute(self.inner.as_slice()) }
    }
}

impl DerefMut for MarkedLines {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { core::mem::transmute(self.inner.as_mut_slice()) }
    }
}

impl Deref for MarkedLineSlice {
    type Target = [MarkedLine];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for MarkedLineSlice {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub struct Iter<'a>(&'a [MarkedLine]);

impl<'a> Iterator for Iter<'a> {
    type Item = MarkedLine;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((&item, rest)) = self.0.split_first() {
            self.0 = rest;
            Some(item)
        } else {
            None
        }
    }
}

pub struct IterMut<'a>(&'a mut [MarkedLine]);

impl<'a> Iterator for IterMut<'a> {
    type Item = &'a mut MarkedLine;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_empty() {
            None
        } else {
            unsafe {
                let ptr = self.0.as_mut_ptr();
                let len = self.0.len();
                self.0 = core::slice::from_raw_parts_mut(ptr.add(1), len - 1);
                Some(core::mem::transmute(ptr))
            }
        }
    }
}

impl<R: RangeBounds<usize>> Index<R> for MarkedLineSlice {
    type Output = MarkedLineSlice;

    fn index(&self, index: R) -> &Self::Output {
        match (index.start_bound().map(|&x| MarkedLine::new(x, 0)), index.end_bound().map(|&x| MarkedLine::new(x, 0))) {
            (Bound::Unbounded, Bound::Unbounded) => self,
            (Bound::Unbounded, Bound::Included(ref end)) => { let end = self.binary_search(end).unwrap_or_else(identity); if end >= self.len() { slice!([]) } else { slice!(self.0[..=end]) } },
            (Bound::Unbounded, Bound::Excluded(ref end)) => { let end = self.binary_search(end).unwrap_or_else(identity); slice!(self.0[..end]) },
            (Bound::Included(ref start), Bound::Unbounded) => { let start = self.binary_search(start).unwrap_or_else(identity); slice!(self.0[start..]) },
            (Bound::Included(ref start), Bound::Included(ref end)) => { let start = self.binary_search(start).unwrap_or_else(identity); let end = self.binary_search(end).unwrap_or_else(identity); if end >= self.len() { slice!([]) } else { slice!(self.0[start..=end]) } },
            (Bound::Included(ref start), Bound::Excluded(ref end)) => { let start = self.binary_search(start).unwrap_or_else(identity); let end = self.binary_search(end).unwrap_or_else(identity); slice!(self.0[start..end]) },
            (Bound::Excluded(ref start), Bound::Unbounded) => { let start = self.binary_search(start).map_or_else(identity, |x| x + 1); slice!(self.0[start..]) },
            (Bound::Excluded(ref start), Bound::Included(ref end)) => { let start = self.binary_search(start).map_or_else(identity, |x| x + 1); let end = self.binary_search(end).unwrap_or_else(identity); if end >= self.len() { slice!([]) } else { slice!(self.0[start..=end]) } },
            (Bound::Excluded(ref start), Bound::Excluded(ref end)) => { let start = self.binary_search(start).map_or_else(identity, |x| x + 1); let end = self.binary_search(end).unwrap_or_else(identity); slice!(self.0[start..end]) },
        }
    }
}

impl<R: RangeBounds<usize>> IndexMut<R> for MarkedLineSlice {
    fn index_mut(&mut self, index: R) -> &mut Self::Output {
        match (index.start_bound().map(|&x| MarkedLine::new(x, 0)), index.end_bound().map(|&x| MarkedLine::new(x, 0))) {
            (Bound::Unbounded, Bound::Unbounded) => self,
            (Bound::Unbounded, Bound::Included(ref end)) => { let end = self.binary_search(end).unwrap_or_else(identity); if end >= self.len() { slice_mut!([]) } else { slice_mut!(self.0[..=end]) } },
            (Bound::Unbounded, Bound::Excluded(ref end)) => { let end = self.binary_search(end).unwrap_or_else(identity); slice_mut!(self.0[..end]) },
            (Bound::Included(ref start), Bound::Unbounded) => { let start = self.binary_search(start).unwrap_or_else(identity); slice_mut!(self.0[start..]) },
            (Bound::Included(ref start), Bound::Included(ref end)) => { let start = self.binary_search(start).unwrap_or_else(identity); let end = self.binary_search(end).unwrap_or_else(identity); if end >= self.len() { slice_mut!([]) } else { slice_mut!(self.0[start..=end]) } },
            (Bound::Included(ref start), Bound::Excluded(ref end)) => { let start = self.binary_search(start).unwrap_or_else(identity); let end = self.binary_search(end).unwrap_or_else(identity); slice_mut!(self.0[start..end]) },
            (Bound::Excluded(ref start), Bound::Unbounded) => { let start = self.binary_search(start).map_or_else(identity, |x| x + 1); slice_mut!(self.0[start..]) },
            (Bound::Excluded(ref start), Bound::Included(ref end)) => { let start = self.binary_search(start).map_or_else(identity, |x| x + 1); let end = self.binary_search(end).unwrap_or_else(identity); if end >= self.len() { slice_mut!([]) } else { slice_mut!(self.0[start..=end]) } },
            (Bound::Excluded(ref start), Bound::Excluded(ref end)) => { let start = self.binary_search(start).map_or_else(identity, |x| x + 1); let end = self.binary_search(end).unwrap_or_else(identity); slice_mut!(self.0[start..end]) },
        }
    }
}
