mod actions;
mod navigate;
mod traverse;

pub use actions::*;
pub use navigate::*;
pub use traverse::*;

use crate::elements::{NbtByteArray, NbtElement, NbtIntArray, NbtLongArray};
use crate::widget::SelectedText;
use crate::workbench::FileUpdateSubscription;

#[must_use]
pub fn sum_indices<I: Iterator<Item = usize>>(indices: I, mut root: &NbtElement) -> usize {
    let mut total = 0;
    let mut indices = indices.peekable();
    while let Some(idx) = indices.next() {
        root = if let NbtByteArray::ID | NbtIntArray::ID | NbtLongArray::ID = root.id() {
            total += 1 + idx;
            break;
        } else if let Some(list) = root.as_list() {
            total += 1 + list
                .children()
                .take(idx)
                .map(NbtElement::height)
                .sum::<usize>();
            if let Some(root) = list.get(idx) {
                root
            } else {
                break;
            }
        } else if let Some(compound) = root.as_compound() {
            total += 1 + compound
                .children()
                .take(idx)
                .map(|(_, b)| b)
                .map(NbtElement::height)
                .sum::<usize>();
            if let Some((_, root)) = compound.get(idx) {
                root
            } else {
                break;
            }
        } else if let Some(chunk) = root.as_chunk() {
            total += 1 + chunk
                .children()
                .take(idx)
                .map(|(_, b)| b)
                .map(NbtElement::height)
                .sum::<usize>();
            if let Some((_, root)) = chunk.get(idx) {
                root
            } else {
                break;
            }
        } else if let Some(region) = root.as_region() {
            total += 1 + region
                .children()
                .take(idx)
                .map(NbtElement::height)
                .sum::<usize>();
            if let Some(root) = region.get(idx) {
                root
            } else {
                break;
            }
        } else {
            total += root.height();
            if indices.peek().is_some() {
                panic!("tried to index non-indexable")
            } else {
                break;
            }
        };
    }
    total
}

pub fn recache_along_indices(indices: &[usize], parent: &mut NbtElement) {
    if let Some(region) = parent.as_region_mut() {
        if let Some((&idx, rest)) = indices.split_first() {
            recache_along_indices(rest, region.get_mut(idx).expect("expected valid index"));
        }
        region.recache();
    } else if let Some(array) = parent.as_byte_array_mut() {
        array.recache();
    } else if let Some(array) = parent.as_int_array_mut() {
        array.recache();
    } else if let Some(array) = parent.as_long_array_mut() {
        array.recache();
    } else if let Some(list) = parent.as_list_mut() {
        if let Some((&idx, rest)) = indices.split_first() {
            recache_along_indices(rest, list.get_mut(idx).expect("expected valid index"));
        }
        list.recache();
    } else if let Some(compound) = parent.as_compound_mut() {
        if let Some((&idx, rest)) = indices.split_first() {
            recache_along_indices(rest, compound.get_mut(idx).expect("expected valid index").1, );
        }
        compound.recache();
    } else if let Some(chunk) = parent.as_chunk_mut() {
        if let Some((&idx, rest)) = indices.split_first() {
            recache_along_indices(rest, chunk.get_mut(idx).expect("expected valid index").1, );
        }
        chunk.recache();
    }
}

pub struct MutableIndices<'m2> {
    is_empty: bool,
    subscription: &'m2 mut Option<FileUpdateSubscription>,
    selected_text: &'m2 mut Option<SelectedText>,
}

impl<'m1, 'm2: 'm1> MutableIndices<'m2> {
    #[must_use]
    pub fn new(subscription: &'m2 mut Option<FileUpdateSubscription>, selected_text: &'m2 mut Option<SelectedText>) -> Self {
        Self {
            is_empty: false,
            subscription,
            selected_text,
        }
    }

    #[must_use]
    pub fn empty() -> &'static mut Self {
        static mut EMPTY_SUBSCRIPTION: Option<FileUpdateSubscription> = None;
        static mut EMPTY_SELECTED_TEXT: Option<SelectedText> = None;
        static mut EMPTY: MutableIndices<'static> = MutableIndices {
            is_empty: true,
            subscription: unsafe { &mut EMPTY_SUBSCRIPTION },
            selected_text: unsafe { &mut EMPTY_SELECTED_TEXT },
        };

        unsafe { &mut EMPTY }
    }

    pub fn apply<F: FnMut(&mut Box<[usize]>) -> bool>(&mut self, mut f: F) {
        if self.is_empty {
            return;
        }

        if let Some(subscription) = self.subscription.as_mut() && f(&mut subscription.indices) {
            self.subscription.take();
        }

        if let Some(selected_text) = self.selected_text.as_mut() && f(&mut selected_text.indices) {
            self.subscription.take();
        }
    }

    /// required to be here because it is the only mutable reference to `FileUpdateSubscription` available when *_element-ing
    pub fn set_subscription(&mut self, subscription: Option<FileUpdateSubscription>) {
        if self.is_empty {
            return;
        }

        *self.subscription = subscription;
    }

    pub fn set_selected_text_indices(&mut self, indices: Box<[usize]>) {
        if self.is_empty {
            return;
        }

        if let Some(selected_text) = self.selected_text.as_mut() {
            selected_text.indices = indices;
        }
    }
}

