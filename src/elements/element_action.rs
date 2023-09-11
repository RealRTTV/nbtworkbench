use compact_str::CompactString;
use std::fs::OpenOptions;
use std::ops::Deref;
use std::process::Command;

use notify::{EventKind, PollWatcher, RecursiveMode, Watcher};
use uuid::Uuid;

use crate::assets::*;
use crate::elements::element_type::{NbtByteArray, NbtElement, NbtIntArray, NbtLongArray};
use crate::vertex_buffer_builder::VertexBufferBuilder;
use crate::{FileUpdateSubscription, FileUpdateSubscriptionType};

#[derive(Copy, Clone)]
pub enum ElementAction {
	CopyRaw,
	CopyFormatted,
	OpenArrayInHex,
	OpenInTxt,
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
		}
	}

	#[must_use]
	#[allow(clippy::too_many_lines)]
	pub fn apply(self, key: Option<CompactString>, indices: Box<[usize]>, tab_uuid: Uuid, element: &mut NbtElement) -> Option<FileUpdateSubscription> {
		#[must_use]
		fn open_file(str: &str) -> bool {
			if cfg!(target_os = "windows") {
				Command::new("cmd").args(["/c", "start", str]).status()
			} else if cfg!(target_os = "macos") {
				Command::new("open").arg(str).status()
			} else {
				Command::new("xdg-open").arg(str).status()
			}
			.is_ok()
		}

		match self {
			Self::CopyRaw => {
				use core::fmt::Write;

				let mut buffer = key.map_or(String::new(), |x| x.into_string() + ":");
				if write!(&mut buffer, "{element}").is_err() {
					return None;
				}
				let _ = cli_clipboard::set_contents(buffer);
			}
			Self::CopyFormatted => {
				use core::fmt::Write;

				let mut buffer = key.map_or(String::new(), |x| x.into_string() + ": ");
				if write!(&mut buffer, "{element:#?}").is_err() {
					return None;
				}
				let _ = cli_clipboard::set_contents(buffer);
			}
			Self::OpenArrayInHex => {
				use std::io::Write;

				let path = std::env::temp_dir().join(format!(
					"nbtworkbench-{:0width$x}.hex",
					(unsafe { core::arch::x86_64::_rdtsc() as usize }).wrapping_mul(element as *mut NbtElement as usize),
					width = usize::BITS as usize / 8
				));
				let (tx, rx) = std::sync::mpsc::channel();
				let Ok(mut watcher) = PollWatcher::new(
					move |event| {
						if let Ok(notify::Event {
							kind: EventKind::Modify(_), paths, ..
						}) = event
						{
							for path in paths {
								if let Ok(data) = std::fs::read(&path) {
									let _ = tx.send(data);
								}
							}
						}
					},
					notify::Config::default().with_manual_polling().with_compare_contents(true),
				) else {
					return None;
				};
				let subscription_type;
				if let Ok(mut file) = OpenOptions::new().write(true).create(true).open(&path) {
					if file
						.write_all(&unsafe {
							match element.id() {
								NbtByteArray::ID => {
									subscription_type = FileUpdateSubscriptionType::ByteArray;
									let mut vec = Vec::with_capacity(element.data.byte_array.len());
									for child in element.data.byte_array.children() {
										vec.push(child.data.byte.deref().value as u8);
									}
									vec
								}
								NbtIntArray::ID => {
									subscription_type = FileUpdateSubscriptionType::IntArray;
									let mut vec = Vec::with_capacity(element.data.int_array.len() * 4);
									for child in element.data.long_array.children() {
										vec.extend(child.data.int.deref().value.to_be_bytes());
									}
									vec
								}
								NbtLongArray::ID => {
									subscription_type = FileUpdateSubscriptionType::LongArray;
									let mut vec = Vec::with_capacity(element.data.long_array.len() * 8);
									for child in element.data.long_array.children() {
										vec.extend(child.data.long.deref().value.to_be_bytes());
									}
									vec
								}
								_ => return None,
							}
						})
						.is_err()
					{
						return None;
					}
					drop(file);
					if watcher.watch(&path, RecursiveMode::NonRecursive).is_err() {
						return None;
					};
					if !open_file(&path.display().to_string()) {
						return None;
					}
					return Some(FileUpdateSubscription {
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

				let path = std::env::temp_dir().join(format!(
					"nbtworkbench-{:0width$x}.txt",
					(unsafe { core::arch::x86_64::_rdtsc() as usize }).wrapping_mul(element as *mut NbtElement as usize),
					width = usize::BITS as usize / 8
				));
				let (tx, rx) = std::sync::mpsc::channel();
				let Ok(mut watcher) = PollWatcher::new(
					move |event| {
						if let Ok(notify::Event {
							kind: EventKind::Modify(_), paths, ..
						}) = event
						{
							for path in paths {
								if let Ok(data) = std::fs::read(&path) {
									let _ = tx.send(data);
								}
							}
						}
					},
					notify::Config::default().with_manual_polling().with_compare_contents(true),
				) else {
					return None;
				};
				if let Ok(mut file) = OpenOptions::new().write(true).create(true).open(&path) {
					if let Some(key) = key && write!(&mut file, "{key}: ").is_err() { return None };
					if write!(&mut file, "{element:#?}").is_err() {
						return None;
					}
					drop(file);
					if watcher.watch(&path, RecursiveMode::NonRecursive).is_err() {
						return None;
					};
					if !open_file(&path.display().to_string()) {
						return None;
					}
					return Some(FileUpdateSubscription {
						subscription_type: FileUpdateSubscriptionType::Snbt,
						indices,
						rx,
						watcher,
						tab_uuid,
					});
				}
			}
		}

		None
	}
}
