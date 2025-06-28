use std::fmt::{Debug, Formatter};
use anyhow::{Context, Result};

use crate::{
	elements::element::NbtElement,
	history::WorkbenchAction,
	tree::MutableIndices,
	util::LinkedQueue,
	workbench::{tab::FilePath, HeldEntry},
};

pub struct HistoryMananger {
	undos: LinkedQueue<WorkbenchAction>,
	redos: LinkedQueue<WorkbenchAction>,
	unsaved_changes: bool,
}

impl Debug for HistoryMananger {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "undos = {}, redos = {}, unsaved_changes = {}", self.undos.len(), self.redos.len(), self.unsaved_changes)
	}
}

impl HistoryMananger {
	#[must_use]
	pub const fn new() -> Self {
		Self {
			undos: LinkedQueue::new(),
			redos: LinkedQueue::new(),
			unsaved_changes: false,
		}
	}

	pub fn on_save(&mut self) { self.unsaved_changes = false; }

	pub fn append(&mut self, mut action: WorkbenchAction) {
		action.shrink_to_fit();
		self.undos.push(action);
		self.redos.clear();
		self.unsaved_changes = true;
	}

	pub fn undo<'m1, 'm2: 'm1>(&mut self, root: &mut NbtElement, mi: &'m1 mut MutableIndices<'m2>, path: &mut FilePath, held_entry: &mut Option<HeldEntry>) -> Result<()> {
		let action = self.undos.pop().context("No actions to undo")?;
		let undo_action = action.undo(root, mi, path, held_entry)?;
		self.redos.push(undo_action);
		Ok(())
	}

	pub fn redo<'m1, 'm2: 'm1>(&mut self, root: &mut NbtElement, mi: &'m1 mut MutableIndices<'m2>, path: &mut FilePath, held_entry: &mut Option<HeldEntry>) -> Result<()> {
		let action = self.redos.pop().context("No actions to undo")?;
		let undo_action = action.undo(root, mi, path, held_entry)?;
		self.undos.push(undo_action);
		Ok(())
	}

	#[must_use]
	pub fn has_unsaved_changes(&self) -> bool { self.unsaved_changes }
}
