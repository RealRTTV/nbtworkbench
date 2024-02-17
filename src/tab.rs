use std::ffi::OsStr;
use std::fs::write;
use std::io::Read;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::time::Duration;

use anyhow::{anyhow, Context, Result};
use compact_str::{CompactString, ToCompactString};
use flate2::Compression;
use uuid::Uuid;

use crate::{Bookmark, LinkedQueue, OptionExt, panic_unchecked, RenderContext, StrExt, WindowProperties};
use crate::assets::{BYTE_ARRAY_GHOST_UV, BYTE_ARRAY_UV, BYTE_GRAYSCALE_UV, BYTE_UV, CHUNK_GHOST_UV, CHUNK_UV, COMPOUND_GHOST_UV, COMPOUND_ROOT_UV, COMPOUND_UV, DOUBLE_GRAYSCALE_UV, DOUBLE_UV, ENABLED_FREEHAND_MODE_UV, FLOAT_GRAYSCALE_UV, FLOAT_UV, FREEHAND_MODE_UV, GZIP_FILE_TYPE_UV, HEADER_SIZE, HELD_SCROLLBAR_UV, INT_ARRAY_GHOST_UV, INT_ARRAY_UV, INT_GRAYSCALE_UV, INT_UV, JUST_OVERLAPPING_BASE_Z, LINE_NUMBER_SEPARATOR_UV, LIST_GHOST_UV, LIST_UV, LONG_ARRAY_GHOST_UV, LONG_ARRAY_UV, LONG_GRAYSCALE_UV, LONG_UV, MCA_FILE_TYPE_UV, NBT_FILE_TYPE_UV, REDO_UV, REGION_UV, SCROLLBAR_Z, SHORT_GRAYSCALE_UV, SHORT_UV, SNBT_FILE_TYPE_UV, STEAL_ANIMATION_OVERLAY, STRING_GHOST_UV, STRING_UV, UNDO_UV, UNHELD_SCROLLBAR_UV, UNKNOWN_NBT_GHOST_UV, UNKNOWN_NBT_UV, ZLIB_FILE_TYPE_UV};
use crate::color::TextColor;
use crate::elements::chunk::NbtRegion;
use crate::elements::compound::NbtCompound;
use crate::elements::element::NbtElement;
use crate::selected_text::SelectedText;
use crate::tree_travel::Navigate;
use crate::vertex_buffer_builder::{Vec2u, VertexBufferBuilder};
use crate::workbench_action::WorkbenchAction;

pub struct Tab {
	pub value: Box<NbtElement>,
	pub name: Box<str>,
	pub path: Option<PathBuf>,
	pub compression: FileFormat,
	pub undos: LinkedQueue<WorkbenchAction>,
	pub redos: LinkedQueue<WorkbenchAction>,
	pub history_changed: bool,
	pub scroll: usize,
	pub horizontal_scroll: usize,
	pub window_height: usize,
	pub window_width: usize,
	pub bookmarks: Vec<Bookmark>, // must be ordered least to greatest
	pub uuid: Uuid,
	pub freehand_mode: bool,
	pub selected_text: Option<SelectedText>,
	pub last_close_attempt: Duration,
}

impl Tab {
	#[must_use]
	pub fn new(nbt: NbtElement, path: &Path, compression: FileFormat, window_height: usize, window_width: usize) -> Result<Self> {
		if !(nbt.id() == NbtCompound::ID || nbt.id() == NbtRegion::ID) { return Err(anyhow!("Parsed NBT was not a Compound or Region")) }

		Ok(Self {
			value: Box::new(nbt),
			name: path.file_name().map(OsStr::to_string_lossy).context("Could not obtain path filename")?.into(),
			path: Some(path).filter(|path| path.is_absolute()).map(|path| path.to_path_buf()),
			compression,
			undos: LinkedQueue::new(),
			redos: LinkedQueue::new(),
			history_changed: false,
			scroll: 0,
			horizontal_scroll: 0,
			window_height,
			window_width,
			bookmarks: vec![],
			uuid: Uuid::new_v4(),
			freehand_mode: false,
			selected_text: None,
			last_close_attempt: Duration::ZERO,
		})
	}

	pub fn save(&mut self, force_dialog: bool) -> Result<()> {
		let path = self.path.as_deref().unwrap_or(self.name.as_ref().as_ref());
		#[cfg(target_os = "windows")] {
			if !path.exists() || force_dialog {
				let mut builder = native_dialog::FileDialog::new();
				if self.value.id() == NbtRegion::ID {
					builder = builder.add_filter("Region File", &["mca", "mcr"]);
				} else {
					builder = builder.add_filter("NBT File", &["nbt", "dat", "dat_old", "dat_mcr", "old"]);
				}
				let path = builder.show_save_single_file()?.ok_or_else(|| anyhow!("Save cancelled"))?;
				self.name = path.file_name().and_then(|x| x.to_str()).expect("Path has a filename").to_string().into_boxed_str();
				write(&path, self.compression.encode(&self.value))?;
				self.path = Some(path);
				self.history_changed = false;
				Ok(())
			} else {
				write(path, self.compression.encode(&self.value))?;
				self.history_changed = false;
				Ok(())
			}
		}
		#[cfg(target_arch = "wasm32")] {
			let bytes = self.compression.encode(&self.value);
			crate::save(self.name.as_ref(), bytes);
			Ok(())
		}
	}

	#[allow(clippy::too_many_lines)]
	pub fn render(&self, builder: &mut VertexBufferBuilder, ctx: &mut RenderContext, held: bool, held_entry: Option<&NbtElement>, skip_tooltips: bool, steal_delta: f32) {
		let mouse_x = ctx.mouse_x;
		let mouse_y = ctx.mouse_y;

		let horizontal_scroll_before = core::mem::replace(
			&mut builder.horizontal_scroll,
			self.horizontal_scroll(held_entry),
		);
		if let Some(compound) = self.value.as_compound() {
			compound.render_root(builder, &self.name, ctx);
		} else if let Some(region) = self.value.as_region() {
			region.render_root(builder, &self.name, ctx);
		}
		builder.color = TextColor::White.to_raw();
		ctx.render_line_numbers(builder, &self.bookmarks);
		ctx.render_key_value_errors(builder);
		builder.horizontal_scroll = horizontal_scroll_before;

		if builder.window_height() >= HEADER_SIZE {
			let height = self.value.height() * 16;
			let total = builder.window_height() - HEADER_SIZE;
			if height > total & !15 {
				let scrollbar_height = (total & !15) * total / height;
				let offset = total * self.scroll() / height + HEADER_SIZE;
				let held = ((builder.window_width() - 8)..builder.window_width()).contains(&mouse_x) && (offset..=(offset + scrollbar_height)).contains(&mouse_y) || held;
				let uv = if held {
					HELD_SCROLLBAR_UV
				} else {
					UNHELD_SCROLLBAR_UV
				};
				builder.draw_texture_z(
					(builder.window_width() - 7, offset),
					SCROLLBAR_Z,
					uv,
					(6, 1),
				);
				if scrollbar_height > 2 {
					builder.draw_texture_region_z(
						(builder.window_width() - 7, offset + 1),
						SCROLLBAR_Z,
						uv + (0, 5),
						(6, scrollbar_height.saturating_sub(1)),
						(6, 4),
					);
				}
				if scrollbar_height > 1 {
					builder.draw_texture_z(
						(builder.window_width() - 7, offset + scrollbar_height),
						SCROLLBAR_Z,
						uv + (0, 15),
						(6, 1),
					);
				}
			}
		}

		ctx.render_scrollbar_bookmarks(builder, &self.bookmarks, &self.value);

		{
			let mut tail = self.undos.tail.as_deref();
			builder.draw_texture(
				(builder.window_width() - 107, 26),
				LINE_NUMBER_SEPARATOR_UV,
				(2, 16),
			);
			builder.draw_texture(
				(builder.window_width() - 129, 26),
				LINE_NUMBER_SEPARATOR_UV,
				(2, 16),
			);
			builder.draw_texture((builder.window_width() - 125, 26), UNDO_UV, (16, 16));
			let mut x = builder.window_width() - 104;
			for _ in 0..5_usize {
				if let Some(t) = tail {
					t.value.render((x, 26), builder, t.prev.is_none());
					x += 16;
					tail = t.prev.as_deref();
				} else {
					break;
				}
			}
		}

		{
			let mut tail = self.redos.tail.as_deref();
			builder.draw_texture(
				(builder.window_width() - 213, 26),
				LINE_NUMBER_SEPARATOR_UV,
				(2, 16),
			);
			builder.draw_texture(
				(builder.window_width() - 235, 26),
				LINE_NUMBER_SEPARATOR_UV,
				(2, 16),
			);
			builder.draw_texture((builder.window_width() - 231, 26), REDO_UV, (16, 16));
			let mut x = builder.window_width() - 210;
			for _ in 0..5_usize {
				if let Some(t) = tail {
					t.value.render((x, 26), builder, t.prev.is_none());
					x += 16;
					tail = t.prev.as_deref();
				} else {
					break;
				}
			}
		}

		{
			builder.draw_texture(
				(builder.window_width() - 22, 26),
				LINE_NUMBER_SEPARATOR_UV,
				(2, 16),
			);
			let freehand_uv = {
				let hovering = (builder.window_width() - 16..builder.window_width()).contains(&mouse_x) && (26..42).contains(&mouse_y);
				if hovering {
					builder.draw_tooltip(&["Freehand Mode (Alt + F)"], (mouse_x, mouse_y));
				}

				if self.freehand_mode {
					ENABLED_FREEHAND_MODE_UV
				} else {
					if hovering {
						ENABLED_FREEHAND_MODE_UV
					} else {
						FREEHAND_MODE_UV
					}
				}
			};
			builder.draw_texture((builder.window_width() - 18, 26), freehand_uv, (16, 16));
		}

		{
			let mx = if (24..46).contains(&mouse_y) && mouse_x >= 16 + 4 {
				Some((mouse_x - (16 + 4)) & !15)
			} else {
				None
			};
			for (idx, (selected, unselected, name)) in [
				(BYTE_UV, BYTE_GRAYSCALE_UV, "Byte"),
				(SHORT_UV, SHORT_GRAYSCALE_UV, "Short"),
				(INT_UV, INT_GRAYSCALE_UV, "Int"),
				(LONG_UV, LONG_GRAYSCALE_UV, "Long"),
				(FLOAT_UV, FLOAT_GRAYSCALE_UV, "Float"),
				(DOUBLE_UV, DOUBLE_GRAYSCALE_UV, "Double"),
				(BYTE_ARRAY_UV, BYTE_ARRAY_GHOST_UV, "Byte Array"),
				(INT_ARRAY_UV, INT_ARRAY_GHOST_UV, "Int Array"),
				(LONG_ARRAY_UV, LONG_ARRAY_GHOST_UV, "Long Array"),
				(STRING_UV, STRING_GHOST_UV, "String"),
				(LIST_UV, LIST_GHOST_UV, "List"),
				(COMPOUND_UV, COMPOUND_GHOST_UV, "Compound"),
			]
			.into_iter()
			.enumerate()
			{
				let uv = if mx == Some(idx * 16) && !skip_tooltips {
					builder.draw_tooltip(&[name], (mouse_x, mouse_y));
					selected
				} else {
					unselected
				};

				builder.draw_texture((idx * 16 + 16 + 4, 26), uv, (16, 16));
			}

			{
				let uv = if mx == Some(192) && self.value.id() == NbtRegion::ID && !skip_tooltips {
					builder.draw_tooltip(&["Chunk"], (mouse_x, mouse_y));
					CHUNK_UV
				} else {
					CHUNK_GHOST_UV
				};
				builder.draw_texture((192 + 16 + 4, 26), uv, (16, 16));
			}

			{
				let uv = if mx == Some(208) && !skip_tooltips {
					builder.draw_tooltip(&["Clipboard"], (mouse_x, mouse_y));
					UNKNOWN_NBT_UV
				} else {
					UNKNOWN_NBT_GHOST_UV
				};
				builder.draw_texture((208 + 16 + 4, 26), uv, (16, 16));
			}
		}

		if steal_delta > 0.0 {
			let y = ((mouse_y - HEADER_SIZE) & !15) + HEADER_SIZE;
			let height = (16.0 * steal_delta).round() as usize;
			builder.draw_texture_region_z((ctx.left_margin - 2, y + (16 - height)), JUST_OVERLAPPING_BASE_Z, STEAL_ANIMATION_OVERLAY, (builder.window_width() + 2 - ctx.left_margin, height), (16, 16));
		}
	}

	pub fn draw_icon(&self, builder: &mut VertexBufferBuilder, pos: impl Into<(usize, usize)>, z: u8) {
		let id = self.value.id();
		if id == NbtCompound::ID {
			builder.draw_texture_z(pos, z, COMPOUND_ROOT_UV, (16, 16));
		} else if id == NbtRegion::ID {
			builder.draw_texture_z(pos, z, REGION_UV, (16, 16));
		}
	}

	pub fn append_to_history(&mut self, action: WorkbenchAction) {
		self.undos.push(action);
		self.redos.clear();
		self.history_changed = true;
	}

	#[must_use]
	pub fn scroll(&self) -> usize {
		let height = self.value.height() * 16 + 32 + 15;
		let scroll = self.scroll;
		let max = (height + HEADER_SIZE).saturating_sub(self.window_height);
		scroll.min(max) & !15
	}

	#[must_use]
	pub fn horizontal_scroll(&self, held: Option<&NbtElement>) -> usize {
		let left_margin = self.left_margin(held);
		let selected_text_width = if let Some(selected_text) = &self.selected_text {
			selected_text.indices.len() * 16 + 32 + 4 + selected_text.width()
		} else {
			0
		};
		let width = self
			.value
			.max_depth()
			.max(self.name.width())
			.max(selected_text_width)
			+ 32 + 48;
		let scroll = self.horizontal_scroll;
		let max = (width + left_margin).saturating_sub(self.window_width);
		scroll.min(max)
	}

	#[must_use]
	pub fn left_margin(&self, held: Option<&NbtElement>) -> usize { ((self.value.true_height() + held.map_or(0, NbtElement::true_height)).ilog10() as usize + 1) * 8 + 4 + 8 }

	pub fn set_scroll(&mut self, scroll: f32) {
		const SCROLL_MULTIPLIER: f32 = 48.0;

		if scroll.is_sign_negative() && self.scroll < (scroll * -SCROLL_MULTIPLIER) as usize {
			self.scroll = 0;
		} else if scroll.is_sign_negative() {
			self.scroll -= (scroll * -SCROLL_MULTIPLIER) as usize;
		} else {
			self.scroll += (scroll * SCROLL_MULTIPLIER) as usize;
		}
		self.scroll = self.scroll();
	}

	pub fn set_horizontal_scroll(&mut self, scroll: f32, held: Option<&NbtElement>) {
		const SCROLL_MULTIPLIER: f32 = 48.0;

		if scroll.is_sign_negative() && self.horizontal_scroll < (scroll * -SCROLL_MULTIPLIER) as usize {
			self.horizontal_scroll = 0;
		} else if scroll.is_sign_negative() {
			self.horizontal_scroll -= (scroll * -SCROLL_MULTIPLIER) as usize;
		} else {
			self.horizontal_scroll += (scroll * SCROLL_MULTIPLIER) as usize;
		}
		self.horizontal_scroll = self.horizontal_scroll(held);
	}

	#[inline]
	#[must_use]
	#[allow(clippy::too_many_lines)]
	pub fn close_selected_text(&mut self, ignore_invalid_format: bool, window_properties: &mut WindowProperties) -> bool {
		unsafe {
			if let Some(SelectedText {
				indices,
				value,
				prefix,
				suffix,
				keyfix,
				valuefix,
				editable: true,
				..
			}) = self.selected_text.clone()
			{
				if let Some((&last, rem)) = indices.split_last() {
					let value = CompactString::from(value);
					let key = prefix.0.is_empty() && !suffix.0.is_empty();
					let (key, value) = {
						let element = Navigate::new(rem.iter().copied(), &mut self.value).last().2;
						if key {
							if let Some(compound) = element.as_compound_mut() {
								let idx = compound.entries.idx_of(&value);
								if let Some(idx) = idx {
									return if idx == last {
										self.selected_text = None;
										true
									} else {
										ignore_invalid_format
									};
								}
								(
									Some(compound.update_key(last, value.clone()).unwrap_or(value)),
									None,
								)
							} else if let Some(chunk) = element.as_chunk_mut() {
								let idx = chunk.entries.idx_of(&value);
								if let Some(idx) = idx {
									return if idx == last {
										self.selected_text = None;
										true
									} else {
										ignore_invalid_format
									};
								}
								(
									Some(chunk.update_key(last, value.clone()).unwrap_or(value)),
									None,
								)
							} else if let Some(region) = element.as_region_mut() {
								let (Ok(x @ 0..=31), Ok(z @ 0..=31)) = (
									value.parse::<u8>(),
									valuefix
										.panic_unchecked("A chunk has a key and value")
										.0
										.parse::<u8>(),
								) else {
									return ignore_invalid_format;
								};
								let (old_x, old_z) = {
									let chunk = region
										.get_mut(last)
										.panic_unchecked("Last index was valid")
										.as_chunk_unchecked_mut();
									(
										core::mem::replace(&mut chunk.x, x),
										core::mem::replace(&mut chunk.z, z),
									)
								};
								let new_idx = ((x as usize) << 5) | (z as usize);
								if old_x == x && old_z == z {
									self.selected_text = None;
									return true;
								}
								if !region.chunks.deref().1[new_idx].is_null() {
									let chunk = region
										.get_mut(last)
										.panic_unchecked("Last index was valid")
										.as_chunk_unchecked_mut();
									chunk.x = old_x;
									chunk.z = old_z;
									return ignore_invalid_format;
								}
								let (map, chunks) = &mut *region.chunks;
								let from = core::mem::replace(&mut map[last], new_idx as u16) as usize;
								chunks.swap(from, new_idx);
								(
									Some(old_x.to_compact_string()),
									Some(old_z.to_compact_string()),
								)
							} else {
								panic_unchecked("Expected key-value indices chain tail to be of type compound")
							}
						} else {
							if let Some(region) = element.as_region_mut() {
								let (Ok(x @ 0..=31), Ok(z @ 0..=31)) = (
									keyfix
										.panic_unchecked("A chunk always has a key and value")
										.0
										.parse::<u8>(),
									value.parse::<u8>(),
								) else {
									return ignore_invalid_format;
								};
								let (old_x, old_z) = {
									let chunk = region
										.get_mut(last)
										.panic_unchecked("Last index was valid")
										.as_chunk_unchecked_mut();
									(
										core::mem::replace(&mut chunk.x, x),
										core::mem::replace(&mut chunk.z, z),
									)
								};
								let new_idx = ((x as usize) << 5) | (z as usize);
								if old_x == x && old_z == z {
									self.selected_text = None;
									return true;
								}
								if !region.chunks.deref().1[new_idx].is_null() {
									let chunk = region
										.get_mut(last)
										.panic_unchecked("Last index was valid")
										.as_chunk_unchecked_mut();
									chunk.x = old_x;
									chunk.z = old_z;
									return ignore_invalid_format;
								};
								let (map, chunks) = &mut *region.chunks;
								let from = core::mem::replace(&mut map[last], new_idx as u16) as usize;
								chunks.swap(from, new_idx);
								(
									Some(old_x.to_compact_string()),
									Some(old_z.to_compact_string()),
								)
							} else {
								// no drops dw, well except for the value, but that's a simple thing dw
								let child = element
									.get_mut(last)
									.panic_unchecked("Last index was valid");
								let (previous, success) = child
									.set_value(value)
									.panic_unchecked("Type of indices tail can accept value writes");
								if !success { return ignore_invalid_format }
								if previous == child.value().0 {
									self.selected_text = None;
									return true
								}
								(None, Some(previous))
							}
						}
					};

					self.append_to_history(WorkbenchAction::Rename {
						indices,
						key,
						value,
					});
					self.selected_text = None;
				} else {
					let buf = PathBuf::from(value);
					return if let Some(name) = buf
						.file_name()
						.and_then(OsStr::to_str)
						.map(ToOwned::to_owned)
					{
						window_properties.window_title(&format!("{name} - NBT Workbench"));
						let old_name = core::mem::replace(&mut self.name, name.into_boxed_str());
						let action = WorkbenchAction::Rename {
							indices: Box::new([]),
							key: None,
							value: Some(
								self.path
									.replace(buf)
									.as_deref()
									.and_then(|path| path.to_str())
									.map(|str| str.to_compact_string())
									.unwrap_or_else(|| old_name.to_compact_string()),
							),
						};
						self.append_to_history(action);
						self.selected_text = None;
						true
					} else {
						false
					};
				}
			}
		}
		true
	}
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum FileFormat {
	Nbt,
	Gzip,
	Zlib,
	ChunkLz4,
	Snbt,
	Mca,
}

impl FileFormat {
	#[must_use]
	pub const fn cycle(self) -> Self {
		match self {
			Self::Nbt => Self::Gzip,
			Self::Gzip => Self::Zlib,
			Self::Zlib => Self::Snbt,
			Self::Snbt => Self::Nbt,

			// has to be separate
			Self::Mca => Self::Mca,
			Self::ChunkLz4 => Self::ChunkLz4,
		}
	}

	#[must_use]
	pub const fn rev_cycle(self) -> Self {
		match self {
			Self::Nbt => Self::Snbt,
			Self::Gzip => Self::Nbt,
			Self::Zlib => Self::Gzip,
			Self::Snbt => Self::Zlib,

			// has to be separate
			Self::Mca => Self::Mca,
			Self::ChunkLz4 => Self::ChunkLz4,
		}
	}

	#[must_use]
	pub fn encode(self, data: &NbtElement) -> Vec<u8> {
		match self {
			Self::Nbt | Self::Mca => data.to_file(),
			Self::Gzip => {
				let mut vec = vec![];
				let _ = flate2::read::GzEncoder::new(&*data.to_file(), Compression::best()).read_to_end(&mut vec);
				vec
			}
			Self::Zlib => {
				let mut vec = vec![];
				let _ = flate2::read::ZlibEncoder::new(&*data.to_file(), Compression::best()).read_to_end(&mut vec);
				vec
			}
			Self::ChunkLz4 => {
				lz4_flex::compress(&*data.to_file())
			}
			Self::Snbt => data.to_string().into_bytes(),
		}
	}

	#[must_use]
	pub const fn uv(self) -> Vec2u {
		match self {
			Self::Nbt => NBT_FILE_TYPE_UV,
			Self::Gzip => GZIP_FILE_TYPE_UV,
			Self::Zlib => ZLIB_FILE_TYPE_UV,
			Self::Snbt => SNBT_FILE_TYPE_UV,
			Self::Mca => MCA_FILE_TYPE_UV,
			Self::ChunkLz4 => Vec2u::new(240, 240),
		}
	}

	#[must_use]
	pub const fn into_str(self) -> &'static str {
		match self {
			Self::Nbt => "Uncompressed",
			Self::Gzip => "GZip",
			Self::Zlib => "ZLib",
			Self::Snbt => "SNBT",
			Self::Mca => "MCA",
			Self::ChunkLz4 => "LZ4",
		}
	}
}

impl ToString for FileFormat {
	fn to_string(&self) -> String { self.into_str().to_owned() }
}
