use std::borrow::{Borrow, BorrowMut};
use std::fmt::{Debug, Formatter};
use std::iter::Copied;
use std::ops::{Deref, DerefMut, Index, IndexMut};
use std::slice;
use crate::elements::NbtElement;
use crate::util;

#[repr(transparent)]
pub struct Indices([usize]);

impl Indices {
    #[must_use]
    pub fn from_slice<'a>(slice: &'a [usize]) -> &'a Self {
        unsafe { core::mem::transmute::<&'a [usize], &'a Self>(slice) }
    }

    #[must_use]
    pub fn from_slice_mut<'a>(slice: &'a mut [usize]) -> &'a mut Self {
        unsafe { core::mem::transmute::<&'a mut [usize], &'a mut Self>(slice) }
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct OwnedIndices(Vec<usize>);

impl Debug for OwnedIndices {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "OwnedIndices {{ {:?} }}", &self.0)
    }
}

impl Debug for Indices {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Indices {{ {:?} }}", &self.0)
    }
}

impl OwnedIndices {
    #[must_use]
    pub const fn new() -> Self {
        Self(Vec::new())
    }
    
    pub fn push(&mut self, index: usize) -> &mut usize {
        self.0.push(index);
        // SAFETY: we just pushed an index to the indices
        unsafe { self.0.as_mut_ptr().add(self.0.len() - 1).as_mut_unchecked() }
    }
    
    pub fn pop(&mut self) -> Option<usize> {
        self.0.pop()
    }
    
    pub fn shrink_to_fit(&mut self) {
        self.0.shrink_to_fit();
    }
    
    pub unsafe fn as_inner(&self) -> &Vec<usize> {
        &self.0
    }
    
    pub unsafe fn as_inner_mut(&mut self) -> &mut Vec<usize> {
        &mut self.0
    }
}

impl Default for OwnedIndices {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl Indices {
    #[must_use]
    pub fn iter(&self) -> Copied<slice::Iter<usize>> {
        self.0.iter().copied()
    }

    #[must_use]
    pub fn iter_mut(&mut self) -> slice::IterMut<usize> {
        self.0.iter_mut()
    }

    #[must_use]
    pub fn split_first(&self) -> Option<(usize, &Self)> {
        self.0.split_first().map(|(tail, rest)| (*tail, Self::from_slice(rest)))
    }
    
    #[must_use]
    pub fn first(&self) -> Option<usize> {
        self.split_first().map(|x| x.0)
    }

    #[must_use]
    pub fn split_last(&self) -> Option<(usize, &Self)> {
        self.0.split_last().map(|(tail, rest)| (*tail, Self::from_slice(rest)))
    }

    #[must_use]
    pub fn last(&self) -> Option<usize> {
        self.split_last().map(|x| x.0)
    }

    #[must_use]
    pub fn split_first_mut(&mut self) -> Option<(&mut usize, &mut Self)> {
        self.0.split_first_mut().map(|(tail, rest)| (tail, Self::from_slice_mut(rest)))
    }

    #[must_use]
    pub fn first_mut(&mut self) -> Option<&mut usize> {
        self.split_first_mut().map(|x| x.0)
    }

    #[must_use]
    pub fn split_last_mut(&mut self) -> Option<(&mut usize, &mut Self)> {
        self.0.split_last_mut().map(|(tail, rest)| (tail, Self::from_slice_mut(rest)))
    }

    #[must_use]
    pub fn last_mut(&mut self) -> Option<&mut usize> {
        self.split_last_mut().map(|x| x.0)
    }

    #[must_use]
    pub fn encompasses_or_equal(&self, inner: &Self) -> bool {
        util::encompasses_or_equal(&self.0, &inner.0)
    }

    #[must_use]
    pub fn encompasses(&self, inner: &Self) -> bool {
        util::encompasses(&self.0, &inner.0)
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.0.len()
    }
    
    #[must_use]
    pub fn split_at(&self, idx: usize) -> (&Self, &Self) {
        let (left, right) = self.0.split_at(idx);
        (Self::from_slice(left), Self::from_slice(right))
    }

    #[must_use]
    pub fn split_at_mut(&mut self, idx: usize) -> (&mut Self, &mut Self) {
        let (left, right) = self.0.split_at_mut(idx);
        (Self::from_slice_mut(left), Self::from_slice_mut(right))
    }
    
    #[must_use]
    pub fn is_root(&self) -> bool {
        self.0.is_empty()
    }
}

impl<T: Into<Vec<usize>>> From<T> for OwnedIndices {
    #[inline]
    fn from(value: T) -> Self {
        let mut inner = value.into();
        inner.shrink_to_fit();
        Self(inner)
    }
}

impl<'a> IntoIterator for &'a Indices {
    type Item = usize;
    type IntoIter = Copied<slice::Iter<'a, usize>>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut Indices {
    type Item = &'a mut usize;
    type IntoIter = slice::IterMut<'a, usize>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<'a> Index<&'a Indices> for NbtElement {
    type Output = Self;

    #[inline]
    fn index(&self, indices: &'a Indices) -> &Self::Output {
        indices
            .iter()
            .fold(self, |nbt, idx| &nbt[idx])
    }
}

impl<'a> IndexMut<&'a Indices> for NbtElement {
    #[inline]
    fn index_mut(&mut self, indices: &'a Indices) -> &mut Self::Output {
        indices
            .iter()
            .fold(self, |nbt, idx| &mut nbt[idx])
    }
}

impl Index<usize> for Indices {
    type Output = usize;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl IndexMut<usize> for Indices {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl Borrow<Indices> for OwnedIndices {
    #[inline]
    fn borrow(&self) -> &Indices {
        Indices::from_slice(self.0.as_slice())
    }
}

impl BorrowMut<Indices> for OwnedIndices {
    fn borrow_mut(&mut self) -> &mut Indices {
        Indices::from_slice_mut(self.0.as_mut_slice())
    }
}

impl ToOwned for Indices {
    type Owned = OwnedIndices;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        OwnedIndices::from(&self.0)
    }
}

impl Deref for OwnedIndices {
    type Target = Indices;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.borrow()
    }
}

impl DerefMut for OwnedIndices {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.borrow_mut()
    }
}
