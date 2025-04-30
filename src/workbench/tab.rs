use std::cell::SyncUnsafeCell;
use std::ffi::OsStr;
use std::fmt::Display;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use anyhow::{anyhow, Context, Result};
use flate2::Compression;
use uuid::Uuid;
use zune_inflate::DeflateDecoder;

use super::{HeldEntry, MarkedLines, WorkbenchAction};
use crate::tree::{rename_element, OwnedIndices};
use crate::assets::{ZOffset, BASE_Z, BYTE_ARRAY_GHOST_UV, BYTE_ARRAY_UV, BYTE_GRAYSCALE_UV, BYTE_UV, CHUNK_GHOST_UV, CHUNK_UV, COMPOUND_GHOST_UV, COMPOUND_ROOT_UV, COMPOUND_UV, DIM_LIGHTBULB_UV, DISABLED_REFRESH_UV, DOUBLE_GRAYSCALE_UV, DOUBLE_UV, ENABLED_FREEHAND_MODE_UV, FLOAT_GRAYSCALE_UV, FLOAT_UV, FREEHAND_MODE_UV, GZIP_FILE_TYPE_UV, HEADER_SIZE, HELD_SCROLLBAR_UV, HOVERED_WIDGET_UV, INT_ARRAY_GHOST_UV, INT_ARRAY_UV, INT_GRAYSCALE_UV, INT_UV, JUST_OVERLAPPING_BASE_Z, LIGHTBULB_UV, LINE_NUMBER_SEPARATOR_UV, LIST_GHOST_UV, LIST_UV, LITTLE_ENDIAN_HEADER_NBT_FILE_TYPE_UV, LITTLE_ENDIAN_NBT_FILE_TYPE_UV, LONG_ARRAY_GHOST_UV, LONG_ARRAY_UV, LONG_GRAYSCALE_UV, LONG_UV, MCA_FILE_TYPE_UV, NBT_FILE_TYPE_UV, REFRESH_UV, REGION_UV, SCROLLBAR_Z, SELECTED_WIDGET_UV, SHORT_GRAYSCALE_UV, SHORT_UV, SNBT_FILE_TYPE_UV, STEAL_ANIMATION_OVERLAY_UV, STRING_GHOST_UV, STRING_UV, UNHELD_SCROLLBAR_UV, UNKNOWN_NBT_GHOST_UV, UNKNOWN_NBT_UV, UNSELECTED_WIDGET_UV, ZLIB_FILE_TYPE_UV};
use crate::elements::{NbtCompound, NbtElement, NbtList, NbtRegion};
use crate::render::{RenderContext, TextColor, Vec2u, VertexBufferBuilder, WindowProperties};
use crate::util::{drop_on_separate_thread, now, LinkedQueue, StrExt};
use crate::widget::{get_cursor_left_jump_idx, get_cursor_right_jump_idx, SelectedText, SelectedTextAdditional, Text, TEXT_DOUBLE_CLICK_INTERVAL};

pub struct Tab {
	pub(in crate::workbench) value: Box<NbtElement>,
	pub(in crate::workbench) name: Box<str>,
	pub(in crate::workbench) path: Option<PathBuf>,
	pub(in crate::workbench) format: FileFormat,
	pub(in crate::workbench) undos: LinkedQueue<WorkbenchAction>,
	pub(in crate::workbench) redos: LinkedQueue<WorkbenchAction>,
	pub(in crate::workbench) unsaved_changes: bool,
	pub(in crate::workbench) scroll: usize,
	pub(in crate::workbench) horizontal_scroll: usize,
	pub(in crate::workbench) window_height: usize,
	pub(in crate::workbench) window_width: usize,
	pub(in crate::workbench) bookmarks: MarkedLines,
	pub(in crate::workbench) uuid: Uuid,
	pub(in crate::workbench) freehand_mode: bool,
	pub(in crate::workbench) selected_text: Option<SelectedText>,
	pub(in crate::workbench) last_close_attempt: Duration,
	pub(in crate::workbench) last_selected_text_interaction: (usize, usize, Duration),
	pub(in crate::workbench) last_interaction: Duration,
	pub(in crate::workbench) last_double_click_interaction: (usize, Duration),
	pub(in crate::workbench) held_entry: HeldEntry,
	// this took me two days
	pub(in crate::workbench) from_indices_arc: Option<Arc<SyncUnsafeCell<OwnedIndices>>>,
}

impl Tab {
	pub const FILE_TYPE_FILTERS: &'static [(&'static str, &'static [&'static str])] = &[
		("Uncompressed NBT File", &["nbt"]),
		("SNBT File", &["snbt"]),
		("Region File", &["mca", "mcr"]),
		("Compressed NBT File", &["dat", "dat_old", "dat_new", "dat_mcr", "old", "schem", "schematic", "litematic"]),
		("Little Endian NBT File", &["nbt", "mcstructure"]),
		("Little Endian NBT File (With Header)", &["dat"]),
	];
	pub const AUTOSAVE_INTERVAL: Duration = Duration::from_secs(30);
	pub const TAB_CLOSE_DOUBLE_CLICK_INTERVAL: Duration = Duration::from_millis(2_000);
	pub const AUTOSAVE_MAXIMUM_LINES: usize = 1_000_000;

	pub fn new(nbt: NbtElement, path: &Path, format: FileFormat, window_height: usize, window_width: usize) -> Result<Self> {
		if !(nbt.id() == NbtCompound::ID || nbt.id() == NbtRegion::ID || nbt.id() == NbtList::ID) { return Err(anyhow!("Parsed NBT was not a Compound, Region, or List")) }

		Ok(Self {
			value: Box::new(nbt),
			name: path.file_name().map(OsStr::to_string_lossy).context("Could not obtain path filename")?.into(),
			path: Some(path).filter(|path| path.is_absolute()).map(|path| path.to_path_buf()),
			format,
			undos: LinkedQueue::new(),
			redos: LinkedQueue::new(),
			unsaved_changes: false,
			scroll: 0,
			horizontal_scroll: 0,
			window_height,
			window_width,
			bookmarks: MarkedLines::new(),
			uuid: Uuid::new_v4(),
			freehand_mode: false,
			selected_text: None,
			last_close_attempt: Duration::ZERO,
			last_selected_text_interaction: (0, 0, Duration::ZERO),
			last_interaction: now(),
			last_double_click_interaction: (0, Duration::ZERO),
			held_entry: HeldEntry::Empty,
			from_indices_arc: None,
		})
	}

	#[cfg(any(target_os = "windows", target_os = "macos", target_os = "linux"))]
	pub fn save(&mut self, force_dialog: bool, window_properties: &mut WindowProperties) -> Result<()> {
		let _ = self.write_selected_text(true, window_properties, false);
		if let Some(path) = self.path.as_deref() && path.is_absolute() && !force_dialog {
			std::fs::write(path, self.format.encode(&self.value))?;
			self.unsaved_changes = false;
			Ok(())
		} else {
			let mut builder = native_dialog::FileDialog::new();
			let initial_index = match self.format {
				FileFormat::Nbt => 0,
				FileFormat::Snbt => 1,
				FileFormat::Lz4 | FileFormat::Mca => 2,
				FileFormat::Gzip | FileFormat::Zlib => 3,
				FileFormat::LittleEndianNbt => 4,
				FileFormat::LittleEndianHeaderNbt => 5,
			};
			builder = builder.add_filter(Self::FILE_TYPE_FILTERS[initial_index].0, Self::FILE_TYPE_FILTERS[initial_index].1);
			builder = Self::FILE_TYPE_FILTERS.iter().enumerate().filter_map(|(idx, value)| if idx == initial_index { None } else { Some(value) }).fold(builder, |builder, filter| builder.add_filter(filter.0, filter.1));
			let Some(path) = builder.show_save_single_file()? else { return Ok(()) };
			self.name = path.file_name().and_then(|x| x.to_str()).expect("Path has a filename").to_string().into_boxed_str();
			std::fs::write(&path, self.format.encode(&self.value))?;
			self.path = Some(path);
			self.unsaved_changes = false;
			Ok(())
		}
	}

	#[cfg(target_arch = "wasm32")]
	pub fn save(&mut self, _: bool, window_properties: &mut WindowProperties) -> Result<()> {
		let _ = self.write_selected_text(true, window_properties, false);
		let bytes = self.format.encode(&self.value);
		crate::wasm::save(self.name.as_ref(), bytes);
		self.unsaved_changes = false;
		Ok(())
	}

	#[allow(clippy::too_many_lines)]
	pub fn render(&self, builder: &mut VertexBufferBuilder, ctx: &mut RenderContext, held: bool, skip_tooltips: bool, steal_delta: f32) {
		let (mouse_x, mouse_y) = ctx.mouse_pos().into();

		let horizontal_scroll_before = core::mem::replace(
			&mut builder.horizontal_scroll,
			self.horizontal_scroll(),
		);
		// let start = std::time::Instant::now();
		if let Some(compound) = self.value.as_compound() {
			compound.render_root(builder, &self.name, ctx);
		} else if let Some(region) = self.value.as_region() {
			region.render_root(builder, &self.name, ctx);
		} else if let Some(list) = self.value.as_list() {
			list.render_root(builder, &self.name, ctx);
		}
		// println!("Tree Only: {}ms", start.elapsed().as_millis_f64());
		builder.color = TextColor::White.to_raw();
		if self.value.as_region().is_some_and(|region| region.is_grid_layout()) {
			ctx.render_grid_line_numbers(builder, &self.bookmarks);
		} else {
			ctx.render_line_numbers(builder, &self.bookmarks);
		}
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

		if self.value.as_region().is_none_or(|region| !region.is_grid_layout()) {
			ctx.render_scrollbar_bookmarks(builder, &self.bookmarks, &self.value);
		}

		{
			// shifted one left to center between clipboard and freehand
			builder.draw_texture_region_z(
				(260, 22),
				BASE_Z,
				LINE_NUMBER_SEPARATOR_UV,
				(2, 23),
				(2, 16),
			);
			let freehand_uv = {
				let hovering = (264..280).contains(&mouse_x) && (26..42).contains(&mouse_y);
				if hovering {
					builder.draw_tooltip(&["Freehand Mode (Ctrl + Shift + F)"], (mouse_x, mouse_y), false);
				}

				if self.freehand_mode || hovering {
					ENABLED_FREEHAND_MODE_UV
				} else {
					FREEHAND_MODE_UV
				}
			};
			builder.draw_texture((264, 26), freehand_uv, (16, 16));
		}

		{
			let enabled = self.path.as_deref().is_some_and(|path| path.exists()) && !cfg!(target_arch = "wasm32");
			let widget_uv = if enabled {
				let (mouse_x, mouse_y) = ctx.mouse_pos().into();
				if (296..312).contains(&mouse_x) && (26..42).contains(&mouse_y) {
					#[cfg(target_arch = "wasm32")]
					builder.draw_tooltip(&["Refresh Tab (Disabled on WebAssembly version)"], ctx.mouse_pos(), false);
					#[cfg(not(target_arch = "wasm32"))]
					builder.draw_tooltip(&["Refresh Tab (Ctrl + R)"], ctx.mouse_pos(), false);
					HOVERED_WIDGET_UV
				} else {
					SELECTED_WIDGET_UV
				}
			} else {
				UNSELECTED_WIDGET_UV
			};

			builder.draw_texture((296, 26), widget_uv, (16, 16));
			builder.draw_texture((296, 26), if enabled { REFRESH_UV } else { DISABLED_REFRESH_UV }, (16, 16));
		}
		{
			let hovering = (312..328).contains(&mouse_x) && (26..42).contains(&mouse_y);
			if hovering {
				builder.draw_tooltip(&["Change Theme (Ctrl + Alt + T)"], (mouse_x, mouse_y), false);
			}
			builder.draw_texture((312, 26), if hovering { DIM_LIGHTBULB_UV } else { LIGHTBULB_UV }, (16, 16));
		}

		{
			let mx = if (24..46).contains(&mouse_y) && mouse_x >= 16 + 16 + 4 {
				Some((mouse_x - (16 + 16 + 4)) & !15)
			} else {
				None
			};
			for (idx, (selected, unselected, name)) in [
				(BYTE_UV, BYTE_GRAYSCALE_UV, "Byte (1)"),
				(SHORT_UV, SHORT_GRAYSCALE_UV, "Short (2)"),
				(INT_UV, INT_GRAYSCALE_UV, "Int (3)"),
				(LONG_UV, LONG_GRAYSCALE_UV, "Long (4)"),
				(FLOAT_UV, FLOAT_GRAYSCALE_UV, "Float (5)"),
				(DOUBLE_UV, DOUBLE_GRAYSCALE_UV, "Double (6)"),
				(BYTE_ARRAY_UV, BYTE_ARRAY_GHOST_UV, "Byte Array (7)"),
				(INT_ARRAY_UV, INT_ARRAY_GHOST_UV, "Int Array (8)"),
				(LONG_ARRAY_UV, LONG_ARRAY_GHOST_UV, "Long Array (9)"),
				(STRING_UV, STRING_GHOST_UV, "String (0)"),
				(LIST_UV, LIST_GHOST_UV, "List (-)"),
				(COMPOUND_UV, COMPOUND_GHOST_UV, "Compound (=)"),
			]
				.into_iter()
				.enumerate()
			{
				let uv = if mx == Some(idx * 16) && !skip_tooltips {
					builder.draw_tooltip(&[name], (idx * 16 + 16 + 16 + 4, 26 + 16), false);
					selected
				} else {
					unselected
				};

				builder.draw_texture((idx * 16 + 16 + 16 + 4, 26), uv, (16, 16));
			}

			{
				let uv = if mx == Some(192) && self.value.id() == NbtRegion::ID && !skip_tooltips {
					builder.draw_tooltip(&["Chunk (`)"], (192, 26 + 16), false);
					CHUNK_UV
				} else {
					CHUNK_GHOST_UV
				};
				builder.draw_texture((192 + 16 + 16 + 4, 26), uv, (16, 16));
			}

			{
				let uv = if mx == Some(208) && !skip_tooltips {
					builder.draw_tooltip(&["Clipboard (V)"], (208, 26 + 16), false);
					UNKNOWN_NBT_UV
				} else {
					UNKNOWN_NBT_GHOST_UV
				};
				builder.draw_texture((208 + 16 + 16 + 4, 26), uv, (16, 16));
			}
		}

		if steal_delta > 0.0 {
			let y = ((mouse_y - HEADER_SIZE) & !15) + HEADER_SIZE;
			let height = (16.0 * steal_delta).round() as usize;
			builder.draw_texture_region_z((ctx.left_margin() - 2, y + (16 - height)), JUST_OVERLAPPING_BASE_Z, STEAL_ANIMATION_OVERLAY_UV, (builder.window_width() + 2 - ctx.left_margin(), height), (16, 16));
		}
	}

	pub fn draw_icon(&self, builder: &mut VertexBufferBuilder, pos: impl Into<(usize, usize)>, z: ZOffset) {
		let id = self.value.id();
		if id == NbtCompound::ID {
			builder.draw_texture_z(pos, z, COMPOUND_ROOT_UV, (16, 16));
		} else if id == NbtRegion::ID {
			builder.draw_texture_z(pos, z, REGION_UV, (16, 16));
		} else if id == NbtList::ID {
			builder.draw_texture_z(pos, z, LIST_UV, (16, 16));
		}
	}

	pub fn set_selected_text(&mut self, y: Option<usize>, selected_text: Option<SelectedText>) {
		self.selected_text = selected_text;
		let now = now();
		if let Some(selected_text) = self.selected_text.as_mut() && let Some(y) = y {
			let (old_y, times_clicked, timestamp) = core::mem::replace(&mut self.last_selected_text_interaction, (y, 0, now));
			if now - timestamp < TEXT_DOUBLE_CLICK_INTERVAL && old_y == y && !selected_text.value.is_empty() {
				self.last_selected_text_interaction = (y, times_clicked + 1, now);
				// the previous click count was divisible by 1
				let (left, right) = if times_clicked % 2 == 1 {
					(0, selected_text.value.len())
				} else {
					(get_cursor_left_jump_idx(selected_text.cursor, selected_text.value.as_bytes()), get_cursor_right_jump_idx(selected_text.cursor, selected_text.value.as_bytes()))
				};
				if right > left {
					selected_text.selection = Some(left);
				}
				selected_text.cursor = right;
			}
		} else {
			self.last_selected_text_interaction = (0, 0, Duration::ZERO);
		}
	}

	pub fn append_to_history(&mut self, mut action: WorkbenchAction) {
		action.shrink_to_fit();
		self.undos.push(action);
		self.redos.clear();
		self.unsaved_changes = true;
	}

	#[must_use]
	pub fn value(&self) -> &NbtElement {
		&self.value
	}

	#[must_use]
	pub fn value_mut(&mut self) -> &mut NbtElement {
		&mut self.value
	}

	#[must_use]
	pub fn format(&self) -> FileFormat {
		self.format
	}

	#[must_use]
	pub fn scroll(&self) -> usize {
		let height = self.value.height() * 16 + 32 + 15;
		let scroll = self.scroll;
		let max = (height + HEADER_SIZE).saturating_sub(self.window_height);
		scroll.min(max) & !15
	}

	#[must_use]
	pub fn horizontal_scroll(&self) -> usize {
		let left_margin = self.left_margin();
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

	pub fn refresh_scrolls(&mut self) {
		self.scroll = self.scroll();
		self.horizontal_scroll = self.horizontal_scroll();
	}

	pub fn update_window_dimensions(&mut self, window_width: usize, window_height: usize) {
		self.window_width = window_width;
		self.window_height = window_height;
		self.refresh_scrolls();
	}

	#[must_use]
	pub fn left_margin(&self) -> usize { ((self.value.true_height() + self.held_entry.element().map_or(0, NbtElement::true_height)).ilog10() as usize + 1) * 8 + 4 + 8 }

	pub fn set_scroll(&mut self, scroll: f32) {
		#[cfg(target_os = "macos")]
		const SCROLL_MULTIPLIER: f32 = 4.0;
		#[cfg(not(target_os = "macos"))]
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

	pub fn set_horizontal_scroll(&mut self, scroll: f32) {
		#[cfg(target_os = "macos")]
		const SCROLL_MULTIPLIER: f32 = 4.0;
		#[cfg(not(target_os = "macos"))]
		const SCROLL_MULTIPLIER: f32 = 48.0;

		if scroll.is_sign_negative() && self.horizontal_scroll < (scroll * -SCROLL_MULTIPLIER) as usize {
			self.horizontal_scroll = 0;
		} else if scroll.is_sign_negative() {
			self.horizontal_scroll -= (scroll * -SCROLL_MULTIPLIER) as usize;
		} else {
			self.horizontal_scroll += (scroll * SCROLL_MULTIPLIER) as usize;
		}
		self.horizontal_scroll = self.horizontal_scroll();
	}

	#[must_use]
	#[allow(clippy::too_many_lines)]
	pub fn write_selected_text(&mut self, on_invalid_format: bool, window_properties: &mut WindowProperties, close_selected_text: bool) -> bool {
		if let Some(SelectedText(Text { value, editable: true, additional: SelectedTextAdditional { indices, prefix, suffix, .. }, .. })) = self.selected_text.clone() {
			let key = prefix.0.is_empty() && !suffix.0.is_empty();
			let (key, value) = if key { (Some(value.into()), None) } else { (None, Some(value.into())) };
			return if let Some(result) = rename_element(&mut self.value, indices, key, value, &mut self.path, &mut self.name, window_properties) {
				if close_selected_text {
					self.selected_text = None;
				}
				self.append_to_history(result.into_action());
				true
			} else {
				on_invalid_format
			}
		}
		true
	}

	pub fn parse_raw(path: &Path, buf: Vec<u8>) -> Result<(NbtElement, FileFormat)> {
		Ok(if let Some("mca" | "mcr") = path.extension().and_then(OsStr::to_str) {
			(
				NbtElement::from_be_mca(buf.as_slice()).context("Failed to parse MCA file")?,
				FileFormat::Mca,
			)
		} else if let Some(0x1F8B) = buf.first_chunk::<2>().copied().map(u16::from_be_bytes) {
			(
				NbtElement::from_be_file(
					&DeflateDecoder::new(buf.as_slice())
						.decode_gzip()
						.context("Failed to decode gzip compressed NBT")?,
				)
					.context("Failed to parse NBT")?,
				FileFormat::Gzip,
			)
		} else if let Some(0x7801 | 0x789C | 0x78DA) = buf.first_chunk::<2>().copied().map(u16::from_be_bytes) {
			(
				NbtElement::from_be_file(
					&DeflateDecoder::new(buf.as_slice())
						.decode_zlib()
						.context("Failed to decode zlib compressed NBT")?,
				)
					.context("Failed to parse NBT")?,
				FileFormat::Zlib,
			)
		} else if let Some(nbt) = NbtElement::from_be_file(buf.as_slice()) {
			(nbt, FileFormat::Nbt)
		} else if let Some((nbt, header)) = NbtElement::from_le_file(buf.as_slice()) {
			(nbt, if header { FileFormat::LittleEndianHeaderNbt } else { FileFormat::LittleEndianNbt })
		} else {
			(
				core::str::from_utf8(&buf)
					.ok()
					.and_then(|s| NbtElement::from_str(s).ok())
					.context(anyhow!(
							"Failed to find file type for file {}",
							path.file_name()
								.unwrap_or(&OsStr::new(""))
								.to_string_lossy()
						))?
					.1,
				FileFormat::Snbt,
			)
		})
	}

	#[cfg(not(target_arch = "wasm32"))]
	pub fn refresh(&mut self) -> Result<()> {
		let Some(path) = self.path.as_deref() else { return Ok(()) };

		if self.unsaved_changes && (now() - core::mem::replace(&mut self.last_close_attempt, now())) > Self::TAB_CLOSE_DOUBLE_CLICK_INTERVAL {
			return Ok(());
		}

		let bytes = std::fs::read(path)?;
		let (value, format) = Tab::parse_raw(path, bytes)?;

		self.bookmarks.clear();
		self.scroll = 0;
		self.format = format;
		self.unsaved_changes = false;
		self.uuid = Uuid::new_v4();
		self.selected_text = None;
		self.last_close_attempt = Duration::ZERO;
		let old = (core::mem::replace(&mut self.value, Box::new(value)), core::mem::replace(&mut self.undos, LinkedQueue::new()), core::mem::replace(&mut self.redos, LinkedQueue::new()));
		drop_on_separate_thread(old);

		Ok(())
	}

	#[cfg(target_arch = "wasm32")]
	pub fn refresh(&mut self) -> Result<()> {
		Ok(())
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum FileFormat {
	Nbt,
	Gzip,
	Zlib,
	Snbt,
	LittleEndianNbt,
	LittleEndianHeaderNbt,

	Lz4,

	Mca,
}

impl FileFormat {
	#[must_use]
	pub const fn cycle(self) -> Self {
		match self {
			Self::Nbt => Self::Gzip,
			Self::Gzip => Self::Zlib,
			Self::Zlib => Self::LittleEndianNbt,
			Self::LittleEndianNbt => Self::LittleEndianHeaderNbt,
			Self::LittleEndianHeaderNbt => Self::Snbt,
			Self::Snbt => Self::Nbt,

			// has to be separate
			Self::Mca => Self::Mca,
			Self::Lz4 => Self::Lz4,
		}
	}

	#[must_use]
	pub const fn rev_cycle(self) -> Self {
		match self {
			Self::Nbt => Self::Snbt,
			Self::Gzip => Self::Nbt,
			Self::Zlib => Self::Gzip,
			Self::LittleEndianNbt => Self::Zlib,
			Self::LittleEndianHeaderNbt => Self::LittleEndianNbt,
			Self::Snbt => Self::LittleEndianHeaderNbt,

			// has to be separate
			Self::Mca => Self::Mca,
			Self::Lz4 => Self::Lz4,
		}
	}

	#[must_use]
	pub fn encode(self, data: &NbtElement) -> Vec<u8> {
		match self {
			Self::Nbt | Self::Mca => data.to_be_file(),
			Self::Gzip => {
				let mut vec = vec![];
				let _ = flate2::read::GzEncoder::new(&*data.to_be_file(), Compression::best()).read_to_end(&mut vec);
				vec
			}
			Self::Zlib => {
				let mut vec = vec![];
				let _ = flate2::read::ZlibEncoder::new(&*data.to_be_file(), Compression::best()).read_to_end(&mut vec);
				vec
			}
			Self::Lz4 => lz4_flex::compress(&*data.to_be_file()),
			Self::Snbt => data.to_string().into_bytes(),
			format @ (Self::LittleEndianNbt | Self::LittleEndianHeaderNbt) => data.to_le_file(format == Self::LittleEndianHeaderNbt),
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
			Self::LittleEndianNbt => LITTLE_ENDIAN_NBT_FILE_TYPE_UV,
			Self::LittleEndianHeaderNbt => LITTLE_ENDIAN_HEADER_NBT_FILE_TYPE_UV,
			Self::Lz4 => Vec2u::new(240, 240),
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
			Self::Lz4 => "LZ4",
			Self::LittleEndianNbt => "Little Endian NBT",
			Self::LittleEndianHeaderNbt => "Little Endian NBT (With Header)",
		}
	}
}

impl Display for FileFormat {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.into_str())
	}
}
