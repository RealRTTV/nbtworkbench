use std::cmp::Ordering;
#[cfg(not(target_arch = "wasm32"))]
use std::{fs::OpenOptions, process::Command};

#[cfg(not(target_arch = "wasm32"))]
use notify::{EventKind, PollWatcher, RecursiveMode, Watcher};
use uuid::Uuid;

use crate::assets::{ACTION_WHEEL_Z, COPY_FORMATTED_UV, COPY_RAW_UV, INSERT_FROM_CLIPBOARD_UV, INVERT_BOOKMARKS_UV, SORT_COMPOUND_BY_NAME_UV, SORT_COMPOUND_BY_TYPE_UV};
#[cfg(not(target_arch = "wasm32"))]
use crate::assets::{OPEN_ARRAY_IN_HEX_UV, OPEN_IN_TXT_UV};
use crate::elements::{NbtByte, NbtByteArray, NbtChunk, NbtCompound, NbtDouble, NbtElement, NbtFloat, NbtInt, NbtIntArray, NbtList, NbtLong, NbtLongArray, NbtPattern, NbtShort, NbtString};
use crate::render::VertexBufferBuilder;
use crate::serialization::UncheckedBufWriter;
use crate::tree::{MutableIndices, NavigationInformation, OwnedIndices, ReorderElementResult, add_element, reorder_element};
use crate::util::{StrExt, get_clipboard, now, set_clipboard};
use crate::widget::Alert;
use crate::workbench::{FileUpdateSubscription, FileUpdateSubscriptionType, MarkedLine, MarkedLines, WorkbenchAction};

#[derive(Copy, Clone, PartialEq, Eq)]
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
	InvertBookmarks,
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
			Self::InvertBookmarks => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, INVERT_BOOKMARKS_UV, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Invert bookmarks"], pos, false);
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
		ORDERING[a_nbt.id() as usize]
			.cmp(&ORDERING[b_nbt.id() as usize])
			.then_with(|| a_str.cmp(b_str))
	}

	// todo: add more alerts for errors (potentially have anyhow::Result)
	pub fn apply<'m1, 'm2: 'm1>(self, root: &mut NbtElement, mut indices: OwnedIndices, bookmarks: &mut MarkedLines, mutable_indices: &'m1 mut MutableIndices<'m2>, alerts: &mut Vec<Alert>, tab_uuid: Uuid) -> Option<WorkbenchAction> {
		#[must_use]
		#[cfg(not(target_arch = "wasm32"))]
		fn open_file(str: &str) -> bool {
			'a: {
				#[cfg(target_os = "windows")]
				break 'a Command::new("cmd")
					.args(["/c", "start", str])
					.status();
				#[cfg(target_os = "macos")]
				break 'a Command::new("open").arg(str).status();
				#[cfg(target_os = "linux")]
				break 'a Command::new("xdg-open").arg(str).status();
			}
			.is_ok()
		}

		match self {
			action @ (Self::CopyRaw | Self::CopyFormatted) => {
				use core::fmt::Write;

				let NavigationInformation { key, element, .. } = root.navigate(&indices)?;

				let mut buffer = String::new();
				if let Some(key) = key {
					if key.needs_escape() {
						let _ = write!(&mut buffer, "{key:?}");
					} else {
						let _ = write!(&mut buffer, "{key}");
					}
					let _ = write!(&mut buffer, ":");
				}

				if action == Self::CopyRaw {
					let _ = write!(&mut buffer, "{element}");
				} else {
					let _ = write!(&mut buffer, "{element:#?}");
				}

				set_clipboard(buffer);

				None
			}
			#[cfg(not(target_arch = "wasm32"))]
			action @ (Self::OpenArrayInHex | Self::OpenInTxt) => {
				use std::io::Write;

				let NavigationInformation { key, element, .. } = root.navigate(&indices)?;

				let hash = (now().as_millis() as usize).wrapping_mul(element as *const NbtElement as usize);
				let path = std::env::temp_dir().join(format!("nbtworkbench-{hash:0width$x}.{ext}", width = usize::BITS as usize / 8, ext = if action == Self::OpenArrayInHex { "bin" } else { "txt" }));
				let (tx, rx) = std::sync::mpsc::channel();
				let mut watcher = PollWatcher::new(
					move |event| {
						if let Ok(notify::Event { kind: EventKind::Modify(_), paths, .. }) = event {
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
				)
				.ok()?;
				let subscription_type = {
					let mut file = OpenOptions::new()
						.write(true)
						.create(true)
						.open(&path)
						.ok()?;
					if action == Self::OpenArrayInHex {
						let mut buffer = UncheckedBufWriter::new();
						element.to_le_bytes(&mut buffer);
						let contents = buffer.finish();
						let (subscription_type, bytes) = match (element.id(), contents.as_slice()) {
							(NbtByteArray::ID, [_, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::ByteArray, bytes),
							(NbtIntArray::ID, [_, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::IntArray, bytes),
							(NbtLongArray::ID, [_, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::LongArray, bytes),
							(NbtList::ID, [NbtByte::ID, _, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::ByteList, bytes),
							(NbtList::ID, [NbtInt::ID, _, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::IntList, bytes),
							(NbtList::ID, [NbtShort::ID, _, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::ShortList, bytes),
							(NbtList::ID, [NbtLong::ID, _, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::LongList, bytes),
							_ => return None,
						};
						file.write_all(bytes).ok()?;
						subscription_type
					} else {
						let mut buffer = UncheckedBufWriter::new();
						if let Some(key) = key {
							if key.needs_escape() {
								let _ = write!(&mut buffer, "{key:?}");
							} else {
								let _ = write!(&mut buffer, "{key}");
							}
							let _ = write!(&mut buffer, ": ");
						}

						if action == Self::CopyRaw {
							let _ = write!(&mut buffer, "{element}");
						} else {
							let _ = write!(&mut buffer, "{element:#?}");
						}

						let contents = buffer.finish();
						file.write_all(&contents).ok()?;
						FileUpdateSubscriptionType::Snbt
					}
				};
				watcher
					.watch(&path, RecursiveMode::NonRecursive)
					.ok()?;
				if !open_file(&path.display().to_string()) {
					return None
				}
				mutable_indices.set_subscription(Some(FileUpdateSubscription::new(subscription_type, indices, rx, watcher, tab_uuid)));
				None
			}
			action @ (Self::SortCompoundByName | Self::SortCompoundByType) => {
				let NavigationInformation { element, .. } = root.navigate(&indices)?;

				let mapping = match element.as_pattern() {
					NbtPattern::Compound(compound) => compound
						.entries
						.create_sort_mapping(if action == Self::SortCompoundByName { Self::by_name } else { Self::by_type }),
					NbtPattern::Chunk(chunk) => chunk
						.entries
						.create_sort_mapping(if action == Self::SortCompoundByName { Self::by_name } else { Self::by_type }),
					_ => return None,
				};

				reorder_element(root, indices, mapping, bookmarks, mutable_indices).map(ReorderElementResult::into_action)
			}
			Self::InsertFromClipboard => {
				let Some(clipboard) = get_clipboard() else {
					alerts.push(Alert::error("Failed to get clipboard"));
					return None;
				};
				let kv = match NbtElement::from_str(&clipboard) {
					Ok((key, value)) => (key, value),
					Err(idx) => {
						alerts.push(Alert::error(format!("Could not parse clipboard as SNBT (failed at index {idx})")));
						return None;
					}
				};
				indices.push(0);
				match add_element(root, kv, indices, bookmarks, mutable_indices) {
					Some(action) => Some(action.into_action()),
					None => {
						alerts.push(Alert::error("Failed to insert from clipboard"));
						None
					}
				}
			}
			Self::InvertBookmarks => {
				let NavigationInformation {
					element,
					mut line_number,
					mut true_line_number,
					..
				} = root.navigate(&indices)?;
				let mut queue = Vec::new();
				queue.push(element);
				while let Some(element) = queue.pop() {
					let _ = bookmarks.toggle(MarkedLine::new(true_line_number, line_number));

					if element.is_open() {
						if let Some(iter) = element.values() {
							for child in iter.rev() {
								queue.push(child);
							}
						}
						true_line_number += 1;
					} else {
						true_line_number += element.true_height();
					}
					line_number += element.height();
				}
				None
			}
		}
	}
}
