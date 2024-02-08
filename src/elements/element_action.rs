use std::cmp::Ordering;
use std::convert::identity;
use std::fs::OpenOptions;
use std::process::Command;

use compact_str::CompactString;
use notify::{EventKind, PollWatcher, RecursiveMode, Watcher};
use uuid::Uuid;

use crate::{Bookmark, FileUpdateSubscription, FileUpdateSubscriptionType, panic_unchecked};
use crate::assets::{ACTION_WHEEL_Z, COPY_FORMATTED_UV, COPY_RAW_UV, OPEN_ARRAY_IN_HEX_UV, OPEN_IN_TXT, SORT_COMPOUND_BY_NAME, SORT_COMPOUND_BY_TYPE};
use crate::elements::chunk::NbtChunk;
use crate::elements::compound::NbtCompound;
use crate::elements::element::{NbtByte, NbtByteArray, NbtDouble, NbtElement, NbtFloat, NbtInt, NbtIntArray, NbtLong, NbtLongArray, NbtShort};
use crate::elements::list::NbtList;
use crate::elements::string::NbtString;
use crate::vertex_buffer_builder::VertexBufferBuilder;
use crate::workbench_action::WorkbenchAction;

#[derive(Copy, Clone)]
pub enum ElementAction {
	CopyRaw,
	CopyFormatted,
	OpenArrayInHex,
	OpenInTxt,
	SortCompoundByName,
	SortCompoundByType,
}

impl ElementAction {
	pub fn render(self, builder: &mut VertexBufferBuilder, pos: impl Into<(usize, usize)>, hovered: bool) {
		let pos = pos.into();
		match self {
			Self::CopyRaw => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, COPY_RAW_UV, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Copy minified snbt to clipboard"], pos);
				}
			}
			Self::CopyFormatted => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, COPY_FORMATTED_UV, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Copy formatted snbt to clipboard"], pos);
				}
			}
			Self::OpenArrayInHex => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, OPEN_ARRAY_IN_HEX_UV, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Open raw contents in hex editor"], pos);
				}
			}
			Self::OpenInTxt => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, OPEN_IN_TXT, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Open formatted snbt in text editor"], pos);
				}
			}
			Self::SortCompoundByName => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, SORT_COMPOUND_BY_NAME, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Sort compound by name"], pos);
				}
			}
			Self::SortCompoundByType => {
				builder.draw_texture_z(pos, ACTION_WHEEL_Z, SORT_COMPOUND_BY_TYPE, (10, 10));
				if hovered {
					builder.draw_tooltip(&["Sort compound by type"], pos);
				}
			}
		}
	}

	#[allow(clippy::too_many_lines)]
	pub fn apply(self, key: Option<CompactString>, indices: Box<[usize]>, tab_uuid: Uuid, true_line_number: usize, line_number: usize, element: &mut NbtElement, bookmarks: &mut Vec<Bookmark>, subscription: &mut Option<FileUpdateSubscription>) -> Option<WorkbenchAction> {
		#[must_use]
		fn open_file(str: &str) -> bool {
			if cfg!(target_os = "windows") {
				Command::new("cmd").args(["/c", "start", str]).status()
			} else if cfg!(target_os = "macos") {
				Command::new("open").arg(str).status()
			} else {
				Command::new("xdg-open").arg(str).status()
			}.is_ok()
		}

		#[must_use]
		fn by_name(a: (&str, &NbtElement), b: (&str, &NbtElement)) -> Ordering {
			let (a_str, _) = a;
			let (b_str, _) = b;
			a_str.cmp(b_str)
		}

		#[must_use]
		fn by_type(a: (&str, &NbtElement), b: (&str, &NbtElement)) -> Ordering {
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

		'm: {
			match self {
				Self::CopyRaw => {
					use core::fmt::Write;

					let mut buffer = key.map_or(String::new(), |x| x.into_string() + ":");
					if write!(&mut buffer, "{element}").is_err() { break 'm }
					let _ = cli_clipboard::set_contents(buffer);
				}
				Self::CopyFormatted => {
					use core::fmt::Write;

					let mut buffer = key.map_or(String::new(), |x| x.into_string() + ": ");
					if write!(&mut buffer, "{element:#?}").is_err() { break 'm }
					let _ = cli_clipboard::set_contents(buffer);
				}
				Self::OpenArrayInHex => {
					use std::io::Write;

					let hash = (unsafe { core::arch::x86_64::_rdtsc() as usize }).wrapping_mul(element as *mut NbtElement as usize);
					let path = std::env::temp_dir().join(format!(
						"nbtworkbench-{hash:0width$x}.hex",
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
						if file
							.write_all(&unsafe {
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
						*subscription = Some(FileUpdateSubscription {
							subscription_type,
							indices,
							rx,
							watcher,
							tab_uuid,
						});
					}
				}
				Self::OpenInTxt => {
					use std::io::Write;

					let hash = (unsafe { core::arch::x86_64::_rdtsc() as usize }).wrapping_mul(element as *mut NbtElement as usize);
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
						if let Some(key) = key
							&& element.id() != NbtChunk::ID
						{
							if write!(&mut file, "{key}: ").is_err() { break 'm; }
						}
						if write!(&mut file, "{element:#?}").is_err() { break 'm; }
						drop(file);
						if watcher.watch(&path, RecursiveMode::NonRecursive).is_err() { break 'm; };
						if !open_file(&path.display().to_string()) { break 'm; }
						*subscription = Some(FileUpdateSubscription {
							subscription_type: FileUpdateSubscriptionType::Snbt,
							indices,
							rx,
							watcher,
							tab_uuid,
						});
					}
				}
				Self::SortCompoundByName => {
					let open = element.open();
					let true_height = element.true_height();
					let bookmark_start = bookmarks.binary_search(&Bookmark::new(true_line_number, 0)).map_or_else(identity, |x| x + 1);
					let bookmark_end = bookmarks.binary_search(&Bookmark::new(true_line_number + element.true_height() - 1, 0)).map_or_else(identity, |x| x + 1);
					let bookmark_slice = if bookmark_end > bookmark_start || bookmark_end > bookmarks.len() { &mut [] } else { &mut bookmarks[bookmark_start..bookmark_end] };
					let reordering_indices = if let Some(compound) = element.as_compound_mut() {
						compound.entries.sort_by(by_name, line_number, true_line_number, true_height, open, bookmark_slice)
					} else if let Some(chunk) = element.as_chunk_mut() {
						chunk.entries.sort_by(by_name, line_number, true_line_number, true_height, open, bookmark_slice)
					} else {
						unsafe { panic_unchecked("Unknown element kind for compound sorting") }
					};

					return Some(WorkbenchAction::ReorderCompound { indices, reordering_indices });
				}
				Self::SortCompoundByType => {
					let open = element.open();
					let true_height = element.true_height();
					let bookmark_start = bookmarks.binary_search(&Bookmark::new(true_line_number, 0)).map_or_else(identity, |x| x + 1);
					let bookmark_end = bookmarks.binary_search(&Bookmark::new(true_line_number + element.true_height() - 1, 0)).map_or_else(identity, |x| x + 1);
					let bookmark_slice = if bookmark_end < bookmark_start || bookmark_end > bookmarks.len() { &mut [] } else { &mut bookmarks[bookmark_start..bookmark_end] };
					let reordering_indices = if let Some(compound) = element.as_compound_mut() {
						compound.entries.sort_by(by_type, line_number, true_line_number, true_height, open, bookmark_slice)
					} else if let Some(chunk) = element.as_chunk_mut() {
						chunk.entries.sort_by(by_type, line_number, true_line_number, true_height, open, bookmark_slice)
					} else {
						unsafe { panic_unchecked("Unknown element kind for compound sorting") }
					};

					return Some(WorkbenchAction::ReorderCompound { indices, reordering_indices });
				}
			}
		}

		return None;
	}
}
