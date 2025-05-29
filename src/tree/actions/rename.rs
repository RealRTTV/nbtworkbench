use std::ffi::OsStr;
use std::path::PathBuf;

use compact_str::{CompactString, ToCompactString};

use crate::elements::NbtElement;
use crate::render::WindowProperties;
use crate::tree::{OwnedIndices, ParentNavigationInformationMut};
use crate::workbench::{PathWithName, WorkbenchAction};

#[must_use]
pub fn rename_element(root: &mut NbtElement, indices: OwnedIndices, key: Option<CompactString>, value: Option<String>, path: &mut PathWithName, window_properties: &mut WindowProperties) -> Option<RenameElementResult> {
	if key.is_none() && value.is_none() {
		return None;
	}

	if let Some(ParentNavigationInformationMut { parent, idx, .. }) = root.navigate_parent_mut(&indices) {
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
			let (previous, success) = child
				.set_value(value)
				.expect("Type of indices tail can accept value writes");
			if !success || previous == child.value().0 {
				return None;
			}
			Some(previous)
		} else {
			None
		};

		Some(RenameElementResult { indices, key: old_key, value: old_value })
	} else if let Some(key) = key
		&& value.is_none()
		&& indices.is_root()
	{
		if path.path_str().unwrap_or(path.name()) == key {
			return None;
		}

		let old_path = path
			.set_path(PathBuf::from(key))
			.map(|s| s.to_string_lossy().into_owned())
			.unwrap_or_else(|| path.name().to_owned());
		window_properties.set_window_title(&format!("{} - NBT Workbench", path.name()));
		Some(RenameElementResult {
			indices: OwnedIndices::new(),
			key: None,
			value: Some(old_path),
		})
	} else {
		None
	}
}

// todo: add trait for these
#[derive(Clone)]
pub struct RenameElementResult {
	pub indices: OwnedIndices,
	pub key: Option<CompactString>,
	pub value: Option<String>,
}

#[allow(dead_code)]
impl RenameElementResult {
	pub fn into_action(self) -> WorkbenchAction {
		WorkbenchAction::Rename {
			indices: self.indices,
			key: self.key,
			value: self.value,
		}
	}
}
