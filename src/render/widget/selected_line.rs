use crate::tree::OwnedIndices;

pub struct SelectedLine {
	pub y: usize,
	pub indices: OwnedIndices,
}

impl SelectedLine {
	#[must_use]
	pub fn new(y: usize, indices: OwnedIndices) -> Self { Self { y, indices } }
}
