use std::cmp::Ordering;
#[cfg(not(target_arch = "wasm32"))]
use std::{fs::OpenOptions, process::Command};

use compact_str::CompactString;
#[cfg(not(target_arch = "wasm32"))]
use notify::{EventKind, PollWatcher, RecursiveMode, Watcher};
use uuid::Uuid;

use crate::assets::{ACTION_WHEEL_Z, COPY_FORMATTED_UV, COPY_RAW_UV, INSERT_FROM_CLIPBOARD_UV, SORT_COMPOUND_BY_NAME_UV, SORT_COMPOUND_BY_TYPE_UV};
use crate::elements::{NbtByte, NbtByteArray, NbtChunk, NbtCompound, NbtDouble, NbtElement, NbtFloat, NbtInt, NbtIntArray, NbtList, NbtLong, NbtLongArray, NbtShort, NbtString};
use crate::render::{TextColor, VertexBufferBuilder};
use crate::util::{get_clipboard, now, set_clipboard, StrExt};
use crate::widget::Alert;
use crate::workbench::{FileUpdateSubscription, FileUpdateSubscriptionType, MarkedLines, WorkbenchAction};
use crate::tree::{add_element, MutableIndices, OwnedIndices};

#[cfg(not(target_arch = "wasm32"))]
use crate::assets::{OPEN_ARRAY_IN_HEX_UV, OPEN_IN_TXT_UV};

#[derive(Copy, Clone)]
pub enum ElementAction {
	CopyRaw,
	CopyFormatted,
	#[cfg(not(target_arch = "wasm32"))]
	OpenArrayInHex,
	#[cfg(not(target_arch = "wasm32"))]
	OpenInTxt,
	SortCompoundByName,
	SortCompoundByType,
	InsertFromClipboard,
}

impl ElementAction {
	pub fn render(self, builder: &mut VertexBufferBuilder, pos: impl Into<(usize, usize)>, hovered: bool) {
		let pos = pos.into();
		match self {
			Self::CopyRaw => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, COPY_RAW_UV, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Copy minified snbt to clipboard"], pos, false);
				}
			}
			Self::CopyFormatted => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, COPY_FORMATTED_UV, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Copy formatted snbt to clipboard"], pos, false);
				}
			}
			#[cfg(not(target_arch = "wasm32"))]
			Self::OpenArrayInHex => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, OPEN_ARRAY_IN_HEX_UV, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Open raw contents in hex editor"], pos, false);
				}
			}
			#[cfg(not(target_arch = "wasm32"))]
			Self::OpenInTxt => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, OPEN_IN_TXT_UV, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Open formatted snbt in text editor"], pos, false);
				}
			}
			Self::SortCompoundByName => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, SORT_COMPOUND_BY_NAME_UV, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Sort compound by name"], pos, false);
				}
			}
			Self::SortCompoundByType => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, SORT_COMPOUND_BY_TYPE_UV, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Sort compound by type"], pos, false);
				}
			}
			Self::InsertFromClipboard => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, INSERT_FROM_CLIPBOARD_UV, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Insert from clipboard"], pos, false);
				}
			}
		}
	}

	#[must_use]
	pub fn by_name(a: (&str, &NbtElement), b: (&str, &NbtElement)) -> Ordering {
		let (a_str, _) = a;
		let (b_str, _) = b;
		a_str.cmp(b_str)
	}

	#[must_use]
	pub fn by_type(a: (&str, &NbtElement), b: (&str, &NbtElement)) -> Ordering {
		const ORDERING: [usize; 256] = {
			let mut array = [usize::MAX; 256];
			array[NbtChunk::ID as usize] = 0;
			array[NbtCompound::ID as usize] = 1;
			array[NbtList::ID as usize] = 2;
			array[NbtLongArray::ID as usize] = 3;
			array[NbtIntArray::ID as usize] = 4;
			array[NbtByteArray::ID as usize] = 5;
			array[NbtString::ID as usize] = 6;
			array[NbtDouble::ID as usize] = 7;
			array[NbtFloat::ID as usize] = 8;
			array[NbtLong::ID as usize] = 9;
			array[NbtInt::ID as usize] = 10;
			array[NbtShort::ID as usize] = 11;
			array[NbtByte::ID as usize] = 12;
			array
		};

		let (a_str, a_nbt) = a;
		let (b_str, b_nbt) = b;
		ORDERING[a_nbt.id() as usize].cmp(&ORDERING[b_nbt.id() as usize]).then_with(|| a_str.cmp(b_str))
	}

	#[allow(clippy::too_many_lines)]
	pub fn apply<'a>(self, key: Option<CompactString>, mut indices: OwnedIndices, _tab_uuid: Uuid, true_line_number: usize, line_number: usize, element: &mut NbtElement, bookmarks: &mut MarkedLines, mutable_indices: &'a mut MutableIndices<'a>, alerts: &mut Vec<Alert>) -> Option<WorkbenchAction> {
		#[must_use]
		#[cfg(not(target_arch = "wasm32"))]
		fn open_file(str: &str) -> bool {
			'a: {
				#[cfg(target_os = "windows")]
				break 'a Command::new("cmd").args(["/c", "start", str]).status();
				#[cfg(target_os = "macos")]
				break 'a Command::new("open").arg(str).status();
				#[cfg(target_os = "linux")]
				break 'a Command::new("xdg-open").arg(str).status();
			}.is_ok()
		}

		'm: {
			match self {
				Self::CopyRaw => {
					use core::fmt::Write;

					let mut buffer = key.map_or(String::new(), |x| if StrExt::needs_escape(x.as_str()) { format!("{x:?}") } else { x.into_string() } + ":");
					if write!(&mut buffer, "{element}").is_err() { break 'm }
					set_clipboard(buffer);
				}
				Self::CopyFormatted => {
					use core::fmt::Write;

					let mut buffer = key.map_or(String::new(), |x| if StrExt::needs_escape(x.as_str()) { format!("{x:?}") } else { x.into_string() } + ":");
					if write!(&mut buffer, "{element:#?}").is_err() { break 'm }
					set_clipboard(buffer);
				}
				#[cfg(not(target_arch = "wasm32"))]
				Self::OpenArrayInHex => {
					use std::io::Write;

					let hash = (now().as_millis() as usize).wrapping_mul(element as *mut NbtElement as usize);
					let path = std::env::temp_dir().join(format!(
						"nbtworkbench-{hash:0width$x}.bin",
						width = usize::BITS as usize / 8
					));
					let (tx, rx) = std::sync::mpsc::channel();
					let Ok(mut watcher) = PollWatcher::new(
						move |event| {
							if let Ok(notify::Event {
										  kind: EventKind::Modify(_),
										  paths,
										  ..
									  }) = event
							{
								for path in paths {
									if let Ok(data) = std::fs::read(&path) {
										let _ = tx.send(data);
									}
								}
							}
						},
						notify::Config::default()
							.with_manual_polling()
							.with_compare_contents(true),
					) else {
						break 'm;
					};
					let subscription_type;
					if let Ok(mut file) = OpenOptions::new().write(true).create(true).open(&path) {
						if file.write_all(&unsafe {
								if let Some(array) = element.as_byte_array() {
									subscription_type = FileUpdateSubscriptionType::ByteArray;
									let mut vec = Vec::with_capacity(array.len());
									for child in array.children() {
										vec.push(child.as_byte_unchecked().value as u8);
									}
									vec
								} else if let Some(array) = element.as_int_array() {
									subscription_type = FileUpdateSubscriptionType::IntArray;
									let mut vec = Vec::with_capacity(array.len() * 4);
									for child in array.children() {
										vec.extend(child.as_int_unchecked().value.to_be_bytes());
									}
									vec
								} else if let Some(array) = element.as_long_array() {
									subscription_type = FileUpdateSubscriptionType::LongArray;
									let mut vec = Vec::with_capacity(array.len() * 8);
									for child in array.children() {
										vec.extend(child.as_long_unchecked().value.to_be_bytes());
									}
									vec
								} else if let Some(list) = element.as_list() {
									match list.id() {
										NbtByte::ID => {
											subscription_type = FileUpdateSubscriptionType::ByteList;
											let mut vec = Vec::with_capacity(list.len());
											for child in list.children() {
												vec.push(child.as_byte_unchecked().value as u8);
											}
											vec
										},
										NbtShort::ID => {
											subscription_type = FileUpdateSubscriptionType::ShortList;
											let mut vec = Vec::with_capacity(list.len() * 2);
											for child in list.children() {
												vec.extend(child.as_short_unchecked().value.to_be_bytes());
											}
											vec
										},
										NbtInt::ID => {
											subscription_type = FileUpdateSubscriptionType::IntList;
											let mut vec = Vec::with_capacity(list.len() * 4);
											for child in list.children() {
												vec.extend(child.as_int_unchecked().value.to_be_bytes());
											}
											vec
										},
										NbtLong::ID => {
											subscription_type = FileUpdateSubscriptionType::LongList;
											let mut vec = Vec::with_capacity(list.len() * 8);
											for child in list.children() {
												vec.extend(child.as_long_unchecked().value.to_be_bytes());
											}
											vec
										},
										_ => panic!("list was let through even thought it didn't have valid type"),
									}
								} else {
									break 'm;
								}
							})
							.is_err()
						{
							break 'm;
						}
						drop(file);
						if watcher.watch(&path, RecursiveMode::NonRecursive).is_err() { break 'm; };
						if !open_file(&path.display().to_string()) { break 'm; }
						mutable_indices.set_subscription(Some(FileUpdateSubscription {
							subscription_type,
							indices: indices.clone(),
							rx,
							watcher,
							tab_uuid: _tab_uuid,
						}));
					}
				}
				#[cfg(not(target_arch = "wasm32"))]
				Self::OpenInTxt => {
					use std::io::Write;

					let hash = (now().as_millis() as usize).wrapping_mul(element as *mut NbtElement as usize);
					let path = std::env::temp_dir().join(format!(
						"nbtworkbench-{hash:0width$x}.txt",
						width = usize::BITS as usize / 8
					));
					let (tx, rx) = std::sync::mpsc::channel();
					let Ok(mut watcher) = PollWatcher::new(
						move |event| {
							if let Ok(notify::Event {
										  kind: EventKind::Modify(_),
										  paths,
										  ..
									  }) = event
							{
								for path in paths {
									if let Ok(data) = std::fs::read(&path) {
										let _ = tx.send(data);
									}
								}
							}
						},
						notify::Config::default()
							.with_manual_polling()
							.with_compare_contents(true),
					) else {
						break 'm;
					};
					if let Ok(mut file) = OpenOptions::new().write(true).create(true).open(&path) {
						if let Some(key) = key.as_deref()
							&& element.id() != NbtChunk::ID
						{
							if write!(&mut file, "{k}: ", k = if key.needs_escape() { format!("{key:?}") } else { key.to_owned() }).is_err() { break 'm; }
						}
						if write!(&mut file, "{element:#?}").is_err() { break 'm; }
						drop(file);
						if watcher.watch(&path, RecursiveMode::NonRecursive).is_err() { break 'm; };
						if !open_file(&path.display().to_string()) { break 'm; }
						mutable_indices.set_subscription(Some(FileUpdateSubscription {
							subscription_type: FileUpdateSubscriptionType::Snbt,
							indices: indices.clone(),
							rx,
							watcher,
							tab_uuid: _tab_uuid,
						}));
					}
				}
				Self::SortCompoundByName => {
					let open = element.open();
					let true_height = element.true_height();
					let bookmark_slice = &mut bookmarks[true_line_number..true_line_number + element.true_height()];
					let reordering_indices = if let Some(compound) = element.as_compound_mut() {
						compound.entries.sort_by(Self::by_name, line_number, true_line_number, true_height, open, bookmark_slice)
					} else if let Some(chunk) = element.as_chunk_mut() {
						chunk.entries.sort_by(Self::by_name, line_number, true_line_number, true_height, open, bookmark_slice)
					} else {
						panic!("Unknown element kind for compound sorting")
					};

					return Some(WorkbenchAction::ReorderCompound { indices, reordering_indices });
				}
				Self::SortCompoundByType => {
					let open = element.open();
					let true_height = element.true_height();
					let bookmark_slice = &mut bookmarks[true_line_number..true_line_number + element.true_height()];
					let reordering_indices = if let Some(compound) = element.as_compound_mut() {
						compound.entries.sort_by(Self::by_type, line_number, true_line_number, true_height, open, bookmark_slice)
					} else if let Some(chunk) = element.as_chunk_mut() {
						chunk.entries.sort_by(Self::by_type, line_number, true_line_number, true_height, open, bookmark_slice)
					} else {
						panic!("Unknown element kind for compound sorting")
					};

					return Some(WorkbenchAction::ReorderCompound { indices, reordering_indices });
				}
				Self::InsertFromClipboard => {
					let Some(clipboard) = get_clipboard() else {
						alerts.push(Alert::new("Error!", TextColor::Red, "Failed to get clipboard"));
						return None;
					};
					let (key, value) = match NbtElement::from_str(&clipboard) {
						Ok((key, value)) => (key, value),
						Err(idx) => {
							alerts.push(Alert::new("Error!", TextColor::Red, format!("Could not parse clipboard as SNBT (failed at index {idx})")));
							return None;
						}
					};
					indices.push(0);
					if let None = add_element(element, key, value, OwnedIndices::from(&[0]), bookmarks, mutable_indices) {
						alerts.push(Alert::new("Error!", TextColor::Red, "Failed to insert from clipboard"));
						return None;
					}

					return Some(WorkbenchAction::Add { indices })
				}
			}
		}

		None
	}
}
