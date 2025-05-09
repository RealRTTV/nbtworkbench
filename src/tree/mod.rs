mod actions;
mod navigate;
mod traverse;
mod indices;

pub use actions::*;
pub use indices::*;
pub use navigate::*;
pub use traverse::*;

use crate::elements::NbtElement;
use crate::widget::SelectedText;
use crate::workbench::FileUpdateSubscription;

#[must_use]
pub fn line_number_at(indices: &Indices, mut root: &NbtElement) -> usize {
    let mut total = 0;
    for idx in indices {
        for jdx in 0..idx {
            total += root[jdx].height();
        }
        total += 1;
        root = &root[idx];
    }
    total
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
        #[allow(static_mut_refs)]
        static mut EMPTY: MutableIndices<'static> = MutableIndices {
            is_empty: true,
            subscription: unsafe { &mut EMPTY_SUBSCRIPTION },
            selected_text: unsafe { &mut EMPTY_SELECTED_TEXT },
        };

        #[allow(static_mut_refs)]
        unsafe { &mut EMPTY }
    }

    pub fn apply<F: FnMut(&mut OwnedIndices, &mut CallbackInfo)>(&mut self, mut f: F) {
        if self.is_empty {
            return;
        }

        if let Some(subscription) = self.subscription.as_mut() {
            let mut ci = CallbackInfo::new();
            f(&mut subscription.indices, &mut ci);
            if ci.removed() {
                self.subscription.take();
            }
        }

        if let Some(selected_text) = self.selected_text.as_mut() {
            let mut ci = CallbackInfo::new();
            f(&mut selected_text.indices, &mut ci);
            if ci.removed() {
                self.subscription.take();
            }
        }
    }

    /// required to be here because it is the only mutable reference to `FileUpdateSubscription` available when *_element-ing
    pub fn set_subscription(&mut self, subscription: Option<FileUpdateSubscription>) {
        if self.is_empty {
            return;
        }

        *self.subscription = subscription;
    }
}

mod callback_info {
    pub struct CallbackInfo {
        removed: bool,
    }

    impl Default for CallbackInfo {
        fn default() -> Self {
            Self::new()
        }
    }

    impl CallbackInfo {
        #[must_use]
        pub fn new() -> Self {
            Self {
                removed: false,
            }
        }

        pub fn remove(&mut self) {
            self.removed = true;
        }

        #[must_use]
        pub fn removed(&self) -> bool {
            self.removed
        }
    }
}

pub use callback_info::CallbackInfo;
