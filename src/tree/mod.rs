pub mod actions;
pub mod indices;
pub mod navigate;
pub mod traverse;

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

#[must_use]
pub fn indices_for_true(true_line_number: usize, mut root: &NbtElement) -> Option<OwnedIndices> {
	let mut true_line_number = true_line_number.checked_sub(1)?;
	if true_line_number > root.true_height() {
		return None
	}

	let mut indices = OwnedIndices::new();

	while true_line_number > 0 {
		let mut idx = 0;
		loop {
			// SAFETY: index is valid because of how the tree is structured
			let child = unsafe { root.get_unchecked(idx) };
			let true_height = child.true_height();
			if true_line_number > true_height {
				true_line_number -= true_height;
				idx += 1;
			} else {
				root = child;
				true_line_number -= 1;
				indices.push(idx);
				break
			}
		}
	}

	Some(indices)
}

/// # Shorthands
/// * `mi`
pub struct MutableIndices<'m2> {
	is_empty: bool,
	pub subscription: &'m2 mut Option<FileUpdateSubscription>,
	pub selected_text: &'m2 mut Option<SelectedText>,
	pub bookmarks: &'m2 mut MarkedLines,
	pub temp: Vec<&'m2 mut Option<OwnedIndices>>,
}

impl<'m1, 'm2: 'm1> MutableIndices<'m2> {
	#[must_use]
	pub fn new(subscription: &'m2 mut Option<FileUpdateSubscription>, selected_text: &'m2 mut Option<SelectedText>, bookmarks: &'m2 mut MarkedLines) -> Self {
		Self {
			is_empty: false,
			subscription,
			selected_text,
			bookmarks,
			temp: Vec::new(),
		}
	}

	pub fn apply<F: FnMut(&mut OwnedIndices, &mut CallbackInfo)>(&'m1 mut self, mut f: F) {
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

		for temp in &mut *self.temp {
			if let Some(temp_inner) = temp {
				let mut ci = CallbackInfo::new();
				f(temp_inner, &mut ci);
				if ci.removed() {
					**temp = None;
				}
			}
		}
	}

	pub fn recache_all_line_number_caches_from_indices(&'m1 mut self, root: &NbtElement) { self.selected_text.iter_mut().for_each(|text| text.recache_y(root)); }

	#[must_use]
	pub fn as_inner_mut(&'m1 mut self) -> (&'m1 mut &'m2 mut Option<FileUpdateSubscription>, &'m1 mut &'m2 mut Option<SelectedText>, &'m1 mut Vec<&'m2 mut Option<OwnedIndices>>) { (&mut self.subscription, &mut self.selected_text, &mut self.temp) }
}

mod callback_info {
	pub struct CallbackInfo {
		removed: bool,
	}

	impl Default for CallbackInfo {
		fn default() -> Self { Self::new() }
	}

	impl CallbackInfo {
		#[must_use]
		pub fn new() -> Self { Self { removed: false } }

		pub fn remove(&mut self) { self.removed = true; }

		#[must_use]
		pub fn removed(&self) -> bool { self.removed }
	}
}

pub use callback_info::CallbackInfo;

use crate::elements::element::NbtElement;
use crate::render::widget::selected_text::SelectedText;
use crate::tree::indices::{Indices, OwnedIndices};
use crate::workbench::FileUpdateSubscription;
use crate::workbench::marked_line::MarkedLines;
