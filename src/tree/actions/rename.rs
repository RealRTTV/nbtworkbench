use crate::elements::NbtElement;
use crate::render::WindowProperties;
use crate::tree::{OwnedIndices, ParentNavigationInformationMut};
use crate::workbench::WorkbenchAction;
use compact_str::{CompactString, ToCompactString};
use std::ffi::OsStr;
use std::path::PathBuf;

#[must_use]
pub fn rename_element(root: &mut NbtElement, indices: OwnedIndices, key: Option<CompactString>, value: Option<CompactString>, path: &mut Option<PathBuf>, name: &mut Box<str>, window_properties: &mut WindowProperties) -> Option<RenameElementResult> {
    if key.is_none() && value.is_none() {
        return None;
    }

    if let Some(ParentNavigationInformationMut { parent, idx, .. }) = root.navigate_parent_mut(&indices) {
        let old_key = if let Some(key) = key {
            parent.update_key(idx, key.clone()).map(|x| x.unwrap_or(key))
        } else {
            None
        };

        let old_value = if let Some(value) = value {
            // no drops dw, well except for the value, but that's a simple thing dw
            let child = &mut parent[idx];
            let (previous, success) = child
                .set_value(value)
                .expect("Type of indices tail can accept value writes");
            if !success || previous == child.value().0 { return None; }
            Some(previous)
        } else {
            None
        };

        Some(RenameElementResult {
            indices,
            key: old_key,
            value: old_value,
        })
    } else if let Some(key) = key && value.is_none() && indices.is_root() {
        if path.as_ref().map(|path| path.as_os_str().to_string_lossy()).as_deref().unwrap_or(&name) == key {
            return None;
        }
        let buf = PathBuf::from(key);
        if let Some(new_name) = buf
            .file_name()
            .and_then(OsStr::to_str)
            .map(ToOwned::to_owned)
        {
            window_properties.window_title(&format!("{new_name} - NBT Workbench"));
            let old_name = core::mem::replace(name, new_name.into_boxed_str());
            Some(RenameElementResult {
                indices: OwnedIndices::new(),
                key: None,
                value: Some(
                    path.replace(buf)
                        .as_deref()
                        .and_then(|path| path.to_str())
                        .map(|str| str.to_compact_string())
                        .unwrap_or_else(|| old_name.to_compact_string()),
                ),
            })
        } else {
            None
        }
    } else {
        None
    }
}

#[derive(Clone)]
pub struct RenameElementResult {
    pub indices: OwnedIndices,
    pub key: Option<CompactString>,
    pub value: Option<CompactString>
}

#[allow(dead_code)]
impl RenameElementResult {
    #[must_use]
    pub fn into_action(self) -> WorkbenchAction {
        WorkbenchAction::Rename {
            indices: self.indices,
            key: self.key,
            value: self.value,
        }
    }
}