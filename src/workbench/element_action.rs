use std::cmp::Ordering;
#[cfg(not(target_arch = "wasm32"))] use std::fs::OpenOptions;

use anyhow::{Context, bail};
#[cfg(not(target_arch = "wasm32"))]
use notify::{EventKind, PollWatcher, RecursiveMode, Watcher};

use crate::elements::NbtElementVariant;
use crate::elements::array::{NbtByteArray, NbtIntArray, NbtLongArray};
use crate::elements::byte::NbtByte;
use crate::elements::chunk::NbtChunk;
use crate::elements::compound::{CompoundEntry, NbtCompound};
use crate::elements::double::NbtDouble;
use crate::elements::element::{NbtElement, NbtPattern};
use crate::elements::float::NbtFloat;
use crate::elements::int::NbtInt;
use crate::elements::list::NbtList;
use crate::elements::long::NbtLong;
use crate::elements::short::NbtShort;
use crate::elements::string::NbtString;
use crate::history::WorkbenchAction;
use crate::render::assets::{ACTION_WHEEL_Z, COPY_FORMATTED_UV, COPY_RAW_UV, INSERT_FROM_CLIPBOARD_UV, INVERT_BOOKMARKS_UV, SORT_COMPOUND_BY_NAME_UV, SORT_COMPOUND_BY_TYPE_UV};
#[cfg(not(target_arch = "wasm32"))]
use crate::render::assets::{OPEN_ARRAY_IN_HEX_UV, OPEN_IN_TXT_UV};
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::serialization::encoder::UncheckedBufWriter;
use crate::tree::MutableIndices;
use crate::tree::actions::add::add_element;
use crate::tree::actions::reorder::reorder_element;
use crate::tree::indices::OwnedIndices;
use crate::tree::navigate::NavigationInformation;
use crate::util::{StrExt, Timestamp, get_clipboard, set_clipboard};
use crate::workbench::marked_line::MarkedLine;
use crate::workbench::{FileUpdateSubscription, FileUpdateSubscriptionType};

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
	pub fn by_name(a: &CompoundEntry, b: &CompoundEntry) -> Ordering { a.key.cmp(&b.key) }

	#[must_use]
	pub fn by_type(a: &CompoundEntry, b: &CompoundEntry) -> Ordering {
		const ORDERING: [usize; 256] = {
			const PREFERENCE: [u8; 13] = [
				NbtChunk::ID,
				NbtCompound::ID,
				NbtList::ID,
				NbtLongArray::ID,
				NbtIntArray::ID,
				NbtByteArray::ID,
				NbtString::ID,
				NbtDouble::ID,
				NbtFloat::ID,
				NbtLong::ID,
				NbtInt::ID,
				NbtShort::ID,
				NbtByte::ID,
			];

			let mut array = [usize::MAX; 256];
			let mut idx = 0;
			while idx < PREFERENCE.len() {
				array[PREFERENCE[idx] as usize] = idx;
				idx += 1;
			}
			array
		};

		ORDERING[a.value.id() as usize].cmp(&ORDERING[b.value.id() as usize]).then_with(|| a.key.cmp(&b.key))
	}

	pub fn apply<'m1, 'm2: 'm1>(self, root: &mut NbtElement, mut indices: OwnedIndices, mi: &'m1 mut MutableIndices<'m2>) -> anyhow::Result<Option<WorkbenchAction>> {
		match self {
			action @ (Self::CopyRaw | Self::CopyFormatted) => {
				use core::fmt::Write;

				let NavigationInformation { key, element, .. } = root.navigate(&indices).context("Could not navigate indices")?;

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

				Ok(None)
			}
			#[cfg(not(target_arch = "wasm32"))]
			action @ (Self::OpenArrayInHex | Self::OpenInTxt) => {
				use std::io::Write;

				use NbtPattern as Nbt;

				let NavigationInformation { key, element, .. } = root.navigate(&indices).context("Could not navigate indices")?;

				let hash = (Timestamp::now().elapsed().as_millis() as usize).wrapping_mul(element as *const NbtElement as usize);
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
					notify::Config::default().with_manual_polling().with_compare_contents(true),
				)?;
				let subscription_type = {
					let mut file = OpenOptions::new().write(true).create(true).open(&path)?;
					if action == Self::OpenArrayInHex {
						let mut buffer = UncheckedBufWriter::new();
						element.to_le_bytes(&mut buffer);
						let contents = buffer.finish();
						let (subscription_type, bytes) = match (element.as_pattern(), contents.as_slice()) {
							(Nbt::ByteArray(_), [_, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::ByteArray, bytes),
							(Nbt::IntArray(_), [_, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::IntArray, bytes),
							(Nbt::LongArray(_), [_, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::LongArray, bytes),
							(Nbt::List(_), [NbtByte::ID, _, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::ByteList, bytes),
							(Nbt::List(_), [NbtInt::ID, _, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::IntList, bytes),
							(Nbt::List(_), [NbtShort::ID, _, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::ShortList, bytes),
							(Nbt::List(_), [NbtLong::ID, _, _, _, _, bytes @ ..]) => (FileUpdateSubscriptionType::LongList, bytes),
							_ => return Ok(None),
						};
						file.write_all(bytes)?;
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
						file.write_all(&contents)?;
						FileUpdateSubscriptionType::Snbt
					}
				};
				watcher.watch(&path, RecursiveMode::NonRecursive)?;
				crate::util::open_file(&path.display().to_string())?;
				*mi.subscription = Some(FileUpdateSubscription::new(subscription_type, indices, rx, watcher));
				Ok(None)
			}
			action @ (Self::SortCompoundByName | Self::SortCompoundByType) => {
				let NavigationInformation { element, .. } = root.navigate(&indices).context("Could not navigate indices")?;

				let mapping = match element.as_pattern() {
					NbtPattern::Compound(compound) => compound.map.create_sort_mapping(if action == Self::SortCompoundByName { Self::by_name } else { Self::by_type }),
					NbtPattern::Chunk(chunk) => chunk.map.create_sort_mapping(if action == Self::SortCompoundByName { Self::by_name } else { Self::by_type }),
					_ => bail!("Could not sort element"),
				};

				Ok(Some(reorder_element(root, indices, mapping, mi)?.into_action()))
			}
			Self::InsertFromClipboard => {
				let clipboard = get_clipboard().context("Could not get clipboard")?;
				let kv = NbtElement::from_str(&clipboard)?;
				indices.push(0);
				Ok(Some(add_element(root, kv, indices, mi).context("Failed to insert element")?.into_action()))
			}
			Self::InvertBookmarks => {
				let NavigationInformation {
					element,
					mut line_number,
					mut true_line_number,
					..
				} = root.navigate(&indices).context("Could not navigate indices")?;
				let mut queue = Vec::new();
				queue.push(element);
				while let Some(element) = queue.pop() {
					let _ = mi.bookmarks.toggle(MarkedLine::new(true_line_number, line_number));

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
				Ok(None)
			}
		}
	}
}
