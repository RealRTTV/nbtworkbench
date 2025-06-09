use std::ffi::OsStr;
use std::path::PathBuf;

use compact_str::{CompactString, ToCompactString};
use thiserror::Error;
use crate::elements::NbtElement;
use crate::render::WindowProperties;
use crate::tree::{OwnedIndices, ParentNavigationError, ParentNavigationInformationMut};
use crate::workbench::WorkbenchAction;

pub fn rename_element(root: &mut NbtElement, indices: OwnedIndices, key: Option<CompactString>, value: Option<String>, path: &mut Option<PathBuf>, name: &mut Box<str>, window_properties: &mut WindowProperties) -> Result<RenameElementResult, RenameElementError> {
	if key.is_none() && value.is_none() {
		return Ok(RenameElementResult { indices, key, value });
	}
	
	match root.navigate_parent_mut(&indices) {
		Ok(ParentNavigationInformationMut { parent, idx, .. }) => {
			let old_key = if let Some(key) = key {
				parent
					.update_key(idx, key.clone())
					.map(|x| x.unwrap_or(key))
			} else {
				None
			};

			let old_value = if let Some(value) = value {
				// no drops dw, well except for the value, but that's a simple thing dw
				let child = &mut parent[idx];
				match child.set_value(value) {
					Ok(old_value) => Some(old_value),
					Err(value) => return Err(RenameElementError::InvalidValue { value, child: child.display_name() })
				}
			} else {
				None
			};

			Ok(RenameElementResult { indices, key: old_key, value: old_value })
		}
		Err(ParentNavigationError::EmptyIndices) => {
			if let Some(key) = key.clone() && value.is_none() {
				if path
					.as_ref()
					.map(|path| path.as_os_str().to_string_lossy())
					.as_deref()
					.unwrap_or(name)
					== key
				{
					return Ok(RenameElementResult { indices, key: Some(key), value });
				}
				let buf = PathBuf::from(key);
				if let Some(new_name) = buf
					.file_name()
					.and_then(OsStr::to_str)
					.map(ToOwned::to_owned)
				{
					window_properties.set_window_title(&format!("{new_name} - NBT Workbench"));
					let old_name = core::mem::replace(name, new_name.into_boxed_str());
					Ok(RenameElementResult {
						indices: OwnedIndices::new(),
						key: None,
						value: Some(
							path.replace(buf)
								.as_deref()
								.and_then(|path| path.to_str())
								.map_or_else(|| old_name.into_string(), |str| str.to_owned()),
						),
					})
				} else {
					Err(RenameElementError::PathHasNoName { path: buf.to_string_lossy().into_owned() })
				}
			} else {
				Err(RenameElementError::InvalidRootRenaming { key, value })
			}
		}
		Err(e) => Err(e.into())
	}
}

#[must_use]
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
	#[error("Invalid value '{value}' for {child}.")]
	InvalidValue { value: String, child: &'static str },
	#[error("Path '{path}' has no file name.")]
	PathHasNoName { path: String },
	#[error("Tried to rename root with {key:?} and {value:?}; needs key only.")]
	InvalidRootRenaming { key: Option<CompactString>, value: Option<String> }
}
