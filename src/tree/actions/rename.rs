use std::ffi::OsStr;
use std::path::PathBuf;

use compact_str::{CompactString, ToCompactString};
use thiserror::Error;

use crate::action_result::{ActionResult, IntoFailingActionResult};
use crate::elements::element::NbtElement;
use crate::history::WorkbenchAction;
use crate::tree::indices::OwnedIndices;
use crate::tree::navigate::{ParentNavigationError, ParentNavigationInformationMut};
use crate::window_properties;
use crate::workbench::tab::{FilePath, FilePathError};

#[rustfmt::skip]
pub fn rename_element(
	root: &mut NbtElement,
	indices: OwnedIndices,
	key: Option<CompactString>,
	value: Option<String>,
	path: &mut FilePath
) -> ActionResult<RenameElementResult, RenameElementError> {
	if key.is_none() && value.is_none() {
		return ActionResult::Pass;
	}

	match root.navigate_parent_mut(&indices) {
		Ok(ParentNavigationInformationMut { parent, idx, .. }) => {
			let old_key = if let Some(key) = key {
				let new_key = key.clone();
				if let Some(result) = parent.update_key(idx, key.clone()) {
					match result {
						Some(old_key) if old_key == new_key => None,
						Some(old_key) => Some(old_key),
						None => return ActionResult::Failure(RenameElementError::DuplicateKey { idx, indices, key }),
					}
				} else {
					None
				}
			} else {
				None
			};

			let old_value = if let Some(value) = value {
				let new_value = value.clone();
				// no drops dw, well except for the value, but that's a simple thing dw
				let child = &mut parent[idx];
				match child.set_value(value) {
					Ok(old_value) if old_value == new_value => None,
					Ok(old_value) => Some(old_value),
					Err(value) => return ActionResult::Failure(RenameElementError::InvalidValue { value, child: child.display_name() }),
				}
			} else {
				None
			};

			if old_key.is_none() && old_value.is_none() {
				ActionResult::Pass
			} else {
				ActionResult::Success(RenameElementResult { indices, key: old_key, value: old_value })
			}
		}
		Err(ParentNavigationError::EmptyIndices) => {
			if let Some(key) = key.clone()
				&& value.is_none()
			{
				if path.path_str() == &key { return ActionResult::Pass }
				let old_path = path.set_path(key).map_err(RenameElementError::from).failure_on_err()?;
				window_properties().set_window_title(format!("{name} - NBT Workbench", name = path.name()).as_str());
				ActionResult::Success(RenameElementResult {
					indices,
					key: Some(old_path.to_string_lossy().into_owned().into()),
					value
				})
			} else {
				ActionResult::Failure(RenameElementError::InvalidRootRenaming { key, value })
			}
		}
		Err(e) => ActionResult::Failure(e.into()),
	}
}

#[derive(Clone)]
pub struct RenameElementResult {
	pub indices: OwnedIndices,
	pub key: Option<CompactString>,
	pub value: Option<String>,
}

impl RenameElementResult {
	pub fn into_action(self) -> WorkbenchAction {
		WorkbenchAction::Rename {
			indices: self.indices,
			key: self.key,
			value: self.value,
		}
	}
}

#[derive(Error, Debug)]
pub enum RenameElementError {
	#[error(transparent)]
	Navigation(#[from] ParentNavigationError),
	#[error(transparent)]
	FilePathError(#[from] FilePathError),
	#[error("Invalid value '{value}' for {child}.")]
	InvalidValue { value: String, child: &'static str },
	#[error("Duplicate key ({key}) @ {nth} child for {indices}", nth = crate::util::nth(idx + 1))]
	DuplicateKey { idx: usize, indices: OwnedIndices, key: CompactString },
	#[error("Tried to rename root with {key:?} and {value:?}; needs key only.")]
	InvalidRootRenaming { key: Option<CompactString>, value: Option<String> },
}
