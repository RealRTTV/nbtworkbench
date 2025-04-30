mod actions;
mod navigate;
mod traverse;
mod indices;

pub use actions::*;
pub use navigate::*;
pub use traverse::*;
pub use indices::*;

use crate::elements::NbtElement;
use crate::widget::SelectedText;
use crate::workbench::FileUpdateSubscription;

#[must_use]
#[deprecated = "deprecated in favor of NbtElement::navigate"]
pub fn sum_indices(indices: impl IntoIterator<Item = usize>, mut root: &NbtElement) -> usize {
    let mut total = 0;
    for idx in indices.into_iter() {
        for jdx in 0..idx {
            total += root[jdx].height();
        }
        total += 1;
        root = &root[idx];
    }
    total
}

pub fn recache_along_indices(indices: impl IntoIterator<Item = usize>, mut root: &mut NbtElement) {
    root.recache();
    for idx in indices.into_iter() {
        root = &mut root[idx];
        root.recache();
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

    pub fn apply<F: FnMut(&mut OwnedIndices) -> bool>(&mut self, mut f: F) {
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

    pub fn set_selected_text_indices(&mut self, indices: OwnedIndices) {
        if self.is_empty {
            return;
        }

        if let Some(selected_text) = self.selected_text.as_mut() {
            selected_text.indices = indices;
        }
    }
}

