use std::ffi::OsStr;
use std::fmt::Display;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::Duration;

use anyhow::{Context, Result, anyhow, ensure};
use flate2::Compression;
use zune_inflate::DeflateDecoder;

use super::{FileUpdateSubscription, HeldEntry, MarkedLines, WorkbenchAction};
use crate::assets::{
	BASE_Z, FROM_CLIPBOARD_GHOST_UV, FROM_CLIPBOARD_UV, GZIP_FILE_TYPE_UV, HEADER_SIZE, HELD_SCROLLBAR_UV, JUST_OVERLAPPING_BASE_Z, LINE_NUMBER_SEPARATOR_UV, LITTLE_ENDIAN_HEADER_NBT_FILE_TYPE_UV, LITTLE_ENDIAN_NBT_FILE_TYPE_UV, MCA_FILE_TYPE_UV,
	NBT_FILE_TYPE_UV, SCROLLBAR_Z, SNBT_FILE_TYPE_UV, STEAL_ANIMATION_OVERLAY_UV, UNHELD_SCROLLBAR_UV, ZLIB_FILE_TYPE_UV, CONNECTION_UV, ZOffset,
};
use crate::elements::{ComplexNbtElementVariant, NbtByte, NbtByteArray, NbtChunk, NbtCompound, NbtDouble, NbtElement, NbtElementVariant, NbtFloat, NbtInt, NbtIntArray, NbtList, NbtLong, NbtLongArray, NbtRegion, NbtShort, NbtString};
use crate::render::{RenderContext, TextColor, VertexBufferBuilder, WindowProperties};
use crate::tab_mut;
use crate::util::{LinkedQueue, StrExt, Vec2u, drop_on_separate_thread, now};
use crate::widget::{SelectedText, SelectedTexts, TEXT_DOUBLE_CLICK_INTERVAL, get_cursor_left_jump_idx, get_cursor_right_jump_idx};

pub struct Tab {
	pub value: Box<NbtElement>,
	pub path: PathWithName,
	pub(super) format: FileFormat,
	pub(super) undos: LinkedQueue<WorkbenchAction>,
	pub(super) redos: LinkedQueue<WorkbenchAction>,
	pub(super) unsaved_changes: bool,
	pub(super) scroll: usize,
	pub(super) horizontal_scroll: usize,
	pub(super) window_height: usize,
	pub(super) window_width: usize,
	pub bookmarks: MarkedLines,
	pub freehand_mode: bool,
	pub subscription: Option<FileUpdateSubscription>,
	pub selected_texts: SelectedTexts,
	pub(super) last_close_attempt: Duration,
	pub(super) last_selected_text_interaction: (usize, usize, Duration),
	pub(super) last_interaction: Duration,
	pub(super) last_double_click_interaction: (usize, Duration),
	pub(super) held_entry: Option<HeldEntry>,
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
		ensure!(nbt.id() == NbtCompound::ID || nbt.id() == NbtRegion::ID || nbt.id() == NbtList::ID, "Parsed NBT was not a Compound, Region, or List");

		Ok(Self {
			value: Box::new(nbt),
			path: PathWithName::for_path(path.to_path_buf()),
			format,
			undos: LinkedQueue::new(),
			redos: LinkedQueue::new(),
			unsaved_changes: false,
			scroll: 0,
			horizontal_scroll: 0,
			window_height,
			window_width,
			bookmarks: MarkedLines::new(),
			freehand_mode: false,
			subscription: None,
			selected_texts: SelectedTexts::new(),
			last_close_attempt: Duration::ZERO,
			last_selected_text_interaction: (0, 0, Duration::ZERO),
			last_interaction: now(),
			last_double_click_interaction: (0, Duration::ZERO),
			held_entry: None,
		})
	}

	#[must_use]
	pub fn new_empty_tab(region: bool, window_dims: impl Into<Vec2u>) -> Self {
		let window_dims = window_dims.into();

		Self {
			value: Box::new(if region { NbtElement::Region(NbtRegion::default()) } else { NbtElement::Compound(NbtCompound::default()) }),
			path: PathWithName::without_path("new.nbt"),
			format: FileFormat::Nbt,
			undos: LinkedQueue::new(),
			redos: LinkedQueue::new(),
			unsaved_changes: false,
			scroll: 0,
			horizontal_scroll: 0,
			window_height: window_dims.x,
			window_width: window_dims.y,
			bookmarks: MarkedLines::new(),
			freehand_mode: false,
			subscription: None,
			selected_texts: SelectedTexts::new(),
			last_close_attempt: Duration::ZERO,
			last_selected_text_interaction: (0, 0, Duration::ZERO),
			last_interaction: now(),
			last_double_click_interaction: (0, Duration::ZERO),
			held_entry: None,
		}
	}

	#[cfg(any(target_os = "windows", target_os = "macos", target_os = "linux"))]
	pub fn save(&mut self, force_dialog: bool, window_properties: &mut WindowProperties) -> Result<()> {
		if let Some(bulk) = WorkbenchAction::bulk(
			self.selected_texts
				.iter()
				.filter_map(|selected_text| selected_text.write(window_properties, &mut self.value, &mut self.path))
				.collect::<Vec<_>>(),
		) {
			self.append_to_history(bulk);
		}
		if let Some(path) = self.path.path.as_deref()
			&& path.is_absolute()
			&& !force_dialog
		{
			std::fs::write(path, self.format.encode(&self.value))?;
			self.unsaved_changes = false;
			Ok(())
		} else {
			let initial_index = match self.format {
				FileFormat::Nbt => 0,
				FileFormat::Snbt => 1,
				FileFormat::Lz4 | FileFormat::Mca => 2,
				FileFormat::Gzip | FileFormat::Zlib => 3,
				FileFormat::LittleEndianNbt => 4,
				FileFormat::LittleEndianHeaderNbt => 5,
			};
			let dialog = native_dialog::FileDialogBuilder::default()
				.add_filter(Self::FILE_TYPE_FILTERS[initial_index].0, Self::FILE_TYPE_FILTERS[initial_index].1)
				.add_filters(
					Self::FILE_TYPE_FILTERS
						.iter()
						.copied()
						.map(|(a, b)| {
							(
								a.to_owned(),
								b.iter()
									.map(|x| x.to_string())
									.collect::<Vec<_>>(),
							)
						})
						.enumerate()
						.filter(|(idx, _)| *idx != initial_index)
						.map(|(_, x)| x),
				)
				.save_single_file();
			let Ok(Some(path)) = dialog.show() else { return Ok(()) };
			let result = std::fs::write(&path, self.format.encode(&self.value));
			self.path.set_path(path);
			result?;
			self.unsaved_changes = false;
			Ok(())
		}
	}

	#[cfg(target_arch = "wasm32")]
	pub fn save(&mut self, _: bool, window_properties: &mut WindowProperties) -> Result<()> {
		self.append_to_history(WorkbenchAction::bulk(
			self.selected_texts
				.iter()
				.filter_map(|selected_text| selected_text.write(window_properties, &mut self.value, &mut self.path, &mut self.name))
				.collect::<Vec<_>>(),
		));
		let bytes = self.format.encode(&self.value);
		crate::wasm::save(self.name.as_ref(), bytes);
		self.unsaved_changes = false;
		Ok(())
	}

	pub fn render(&self, builder: &mut VertexBufferBuilder, ctx: &mut RenderContext, held: bool, skip_tooltips: bool, steal_delta: f32) {
		let (mouse_x, mouse_y) = ctx.mouse_pos().into();

		let horizontal_scroll_before = core::mem::replace(&mut builder.horizontal_scroll, self.horizontal_scroll());
		// let start = std::time::Instant::now();
        {
            let mut remaining_scroll = builder.scroll() / 16;
            if remaining_scroll == 0 {
                builder.draw_texture(ctx.pos() - (16, 0), CONNECTION_UV, (16, 9));
            }
            if let Some(compound) = self.value.as_compound() {
                compound.render(builder, Some(&self.path.name()), &mut (builder.scroll() / 16), true, ctx);
            } else if let Some(region) = self.value.as_region() {
                region.render(builder, Some(&self.path.name()), &mut (builder.scroll() / 16), true, ctx);
            } else if let Some(list) = self.value.as_list() {
                list.render(builder, Some(&self.path.name()), &mut (builder.scroll() / 16), true, ctx);
            }
        }
		// println!("Tree Only: {}ms", start.elapsed().as_millis_f64());
		builder.color = TextColor::White.to_raw();
		if self
			.value
			.as_region()
			.is_some_and(|region| region.is_grid_layout())
		{
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
				let uv = if held { HELD_SCROLLBAR_UV } else { UNHELD_SCROLLBAR_UV };
				builder.draw_texture_z((builder.window_width() - 7, offset), SCROLLBAR_Z, uv, (6, 1));
				if scrollbar_height > 2 {
					builder.draw_texture_region_z((builder.window_width() - 7, offset + 1), SCROLLBAR_Z, uv + (0, 5), (6, scrollbar_height.saturating_sub(1)), (6, 4));
				}
				if scrollbar_height > 1 {
					builder.draw_texture_z((builder.window_width() - 7, offset + scrollbar_height), SCROLLBAR_Z, uv + (0, 15), (6, 1));
				}
			}
		}

		if self
			.value
			.as_region()
			.is_none_or(|region| !region.is_grid_layout())
		{
			ctx.render_scrollbar_bookmarks(builder, &self.bookmarks, &self.value);
		}

		// shifted one left to center between clipboard and freehand
		builder.draw_texture_region_z((260, 22), BASE_Z, LINE_NUMBER_SEPARATOR_UV, (2, 23), (2, 16));

		{
			let mx = ((24..46).contains(&mouse_y) && mouse_x >= 16 + 16 + 4).then(|| (mouse_x - (16 + 16 + 4)) & !15);
			for (idx, (selected, unselected, name)) in [
				(NbtByte::UV, NbtByte::GHOST_UV, "Byte (1)"),
				(NbtShort::UV, NbtShort::GHOST_UV, "Short (2)"),
				(NbtInt::UV, NbtInt::GHOST_UV, "Int (3)"),
				(NbtLong::UV, NbtLong::GHOST_UV, "Long (4)"),
				(NbtFloat::UV, NbtFloat::GHOST_UV, "Float (5)"),
				(NbtDouble::UV, NbtDouble::GHOST_UV, "Double (6)"),
				(NbtByteArray::UV, NbtByteArray::GHOST_UV, "Byte Array (7)"),
				(NbtIntArray::UV, NbtIntArray::GHOST_UV, "Int Array (8)"),
				(NbtLongArray::UV, NbtLongArray::GHOST_UV, "Long Array (9)"),
				(NbtString::UV, NbtString::GHOST_UV, "String (0)"),
				(NbtList::UV, NbtList::GHOST_UV, "List (-)"),
				(NbtCompound::UV, NbtCompound::GHOST_UV, "Compound (=)"),
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
					NbtChunk::UV
				} else {
					NbtChunk::GHOST_UV
				};
				builder.draw_texture((192 + 16 + 16 + 4, 26), uv, (16, 16));
			}

			{
				let uv = if mx == Some(208) && !skip_tooltips {
					builder.draw_tooltip(&["Clipboard (V)"], (208, 26 + 16), false);
					FROM_CLIPBOARD_UV
				} else {
					FROM_CLIPBOARD_GHOST_UV
				};
				builder.draw_texture((208 + 16 + 16 + 4, 26), uv, (16, 16));
			}
		}

		if steal_delta > 0.0 {
			let y = ((mouse_y - HEADER_SIZE) & !15) + HEADER_SIZE;
			let height = (16.0 * steal_delta).round() as usize;
			builder.draw_texture_region_z(
				(ctx.left_margin() - 2, y + (16 - height)),
				JUST_OVERLAPPING_BASE_Z,
				STEAL_ANIMATION_OVERLAY_UV,
				(builder.window_width() + 2 - ctx.left_margin(), height),
				(16, 16),
			);
		}
	}

	pub fn draw_icon(&self, builder: &mut VertexBufferBuilder, pos: impl Into<(usize, usize)>, z: ZOffset) {
		let id = self.value.id();
		if id == NbtCompound::ID {
			builder.draw_texture_z(pos, z, NbtCompound::ROOT_UV, (16, 16));
		} else if id == NbtRegion::ID {
			builder.draw_texture_z(pos, z, NbtRegion::UV, (16, 16));
		} else if id == NbtList::ID {
			builder.draw_texture_z(pos, z, NbtList::UV, (16, 16));
		}
	}

	pub fn append_to_history(&mut self, mut action: WorkbenchAction) {
		action.shrink_to_fit();
		self.undos.push(action);
		self.redos.clear();
		self.unsaved_changes = true;
	}

	#[must_use]
	pub fn format(&self) -> FileFormat { self.format }

	pub fn modify_scroll(&mut self, f: impl FnOnce(usize) -> usize) {
		self.scroll = f(self.scroll);
		self.scroll = self.scroll();
	}

	#[must_use]
	pub fn scroll(&self) -> usize {
		let height = self.value.height() * 16 + 32 + 15;
		let scroll = self.scroll;
		let max = (height + HEADER_SIZE).saturating_sub(self.window_height);
		scroll.min(max) & !15
	}

	pub fn modify_horizontal_scroll(&mut self, f: impl FnOnce(usize) -> usize) {
		self.horizontal_scroll = f(self.horizontal_scroll);
		self.horizontal_scroll = self.horizontal_scroll();
	}

	#[must_use]
	pub fn horizontal_scroll(&self) -> usize {
		let left_margin = self.left_margin();
		let max_selected_text_width = self
			.selected_texts
			.iter()
			.map(|selected_text| selected_text.indices.len() * 16 + 32 + 4 + selected_text.width())
			.max()
			.unwrap_or(0);
		let width = self
			.value
			.max_depth()
			.max(self.path.name().width())
			.max(max_selected_text_width)
			+ 32 + 48;
		let scroll = self.horizontal_scroll;
		let max = (width + left_margin).saturating_sub(self.window_width);
		scroll.min(max)
	}

	pub fn refresh_selected_text_horizontal_scroll(&mut self) {
		let left_margin = self.left_margin();
		let horizontal_scroll = self.horizontal_scroll();

		let free_space = 48 + left_margin;
		for selected_text in self.selected_texts.iter() {
			let pos = selected_text.cursor_x(left_margin);
			if pos + free_space < self.window_width {
				self.horizontal_scroll = 0;
				self.horizontal_scroll = self.horizontal_scroll();
			} else if pos + free_space >= self.window_width + horizontal_scroll {
				self.horizontal_scroll = pos + free_space - self.window_width;
				self.horizontal_scroll = self.horizontal_scroll();
			} else if pos < horizontal_scroll + free_space {
				self.horizontal_scroll = pos.saturating_sub(free_space);
				self.horizontal_scroll = self.horizontal_scroll();
			}
		}
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
	pub fn left_margin(&self) -> usize {
		((self.value.true_height()
			+ self
				.held_entry
				.as_ref()
				.map_or(0, |held_entry| held_entry.kv.1.true_height()))
		.ilog10() as usize
			+ 1) * 8 + 4
			+ 8
	}

	#[must_use]
	pub fn window_dims(&self) -> Vec2u { Vec2u::new(self.window_width, self.window_height) }

	pub fn on_scroll(&mut self, scroll: f32) {
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

	pub fn on_horizontal_scroll(&mut self, scroll: f32) {
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

	pub fn parse_raw(path: &Path, buf: Vec<u8>) -> Result<(NbtElement, FileFormat)> {
		Ok(if let Some("mca" | "mcr") = path.extension().and_then(OsStr::to_str) {
			(NbtElement::from_be_mca(buf.as_slice()).context("Failed to parse MCA file")?, FileFormat::Mca)
		} else if let Some(0x1F8B) = buf
			.first_chunk::<2>()
			.copied()
			.map(u16::from_be_bytes)
		{
			(
				NbtElement::from_be_file(
					&DeflateDecoder::new(buf.as_slice())
						.decode_gzip()
						.context("Failed to decode gzip compressed NBT")?,
				)
				.context("Failed to parse NBT")?,
				FileFormat::Gzip,
			)
		} else if let Some(0x7801 | 0x789C | 0x78DA) = buf
			.first_chunk::<2>()
			.copied()
			.map(u16::from_be_bytes)
		{
			(
				NbtElement::from_be_file(
					&DeflateDecoder::new(buf.as_slice())
						.decode_zlib()
						.context("Failed to decode zlib compressed NBT")?,
				)
				.context("Failed to parse NBT")?,
				FileFormat::Zlib,
			)
		} else if let result = NbtElement::from_be_file(buf.as_slice()).context("Tried to parse uncompressed NBT")
			&& {
				#[cfg(debug_assertions)]
				if result.is_err() {
					crate::error!("{result:?}");
				}
				true
			} && let Ok(nbt) = result
		{
			(nbt, FileFormat::Nbt)
		} else if let result = NbtElement::from_le_file(buf.as_slice()).context("Tried to parse uncompressed little-endian NBT")
			&& {
				#[cfg(debug_assertions)]
				if result.is_err() {
					crate::error!("{result:?}");
				}
				true
			} && let Ok((nbt, header)) = result
		{
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
		let Some(path) = self.path.path.as_deref() else { return Ok(()) };

		if self.unsaved_changes && (now() - core::mem::replace(&mut self.last_close_attempt, now())) > Self::TAB_CLOSE_DOUBLE_CLICK_INTERVAL {
			return Ok(());
		}

		let bytes = std::fs::read(path)?;
		let (value, format) = Tab::parse_raw(path, bytes)?;

		self.bookmarks.clear();
		self.scroll = 0;
		self.format = format;
		self.unsaved_changes = false;
		self.selected_texts.clear();
		self.last_close_attempt = Duration::ZERO;
		let old = (
			core::mem::replace(&mut self.value, Box::new(value)),
			core::mem::replace(&mut self.undos, LinkedQueue::new()),
			core::mem::replace(&mut self.redos, LinkedQueue::new()),
		);
		drop_on_separate_thread(old);

		Ok(())
	}

	#[cfg(target_arch = "wasm32")]
	pub fn refresh(&mut self) -> Result<()> { Ok(()) }
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
				let _ = flate2::read::GzEncoder::new(data.to_be_file().as_slice(), Compression::best()).read_to_end(&mut vec);
				vec
			}
			Self::Zlib => {
				let mut vec = vec![];
				let _ = flate2::read::ZlibEncoder::new(data.to_be_file().as_slice(), Compression::best()).read_to_end(&mut vec);
				vec
			}
			Self::Lz4 => lz4_flex::compress(&data.to_be_file()),
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
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.into_str()) }
}

pub struct PathWithName {
	path: Option<PathBuf>,
	cached_name: String,
}

impl PathWithName {
	#[must_use]
	fn name_for_path(path: &Path) -> String {
		path.file_name()
			.map(|s| s.to_string_lossy().into_owned())
			.unwrap_or_default()
	}

	#[must_use]
	pub fn for_path(path: PathBuf) -> Self {
		Self {
			cached_name: Self::name_for_path(&path),
			path: Some(path),
		}
	}

	#[must_use]
	pub fn without_path(name: impl Into<String>) -> Self { Self { path: None, cached_name: name.into() } }

	#[must_use]
	pub fn path(&self) -> Option<&Path> { self.path.as_deref() }

	#[must_use]
	pub fn path_str(&self) -> Option<&str> {
		self.path
			.as_deref()
			.and_then(|p| p.as_os_str().to_str())
	}

	pub fn set_path(&mut self, path: impl Into<PathBuf>) -> Option<PathBuf> {
		let path = path.into();
		self.cached_name = Self::name_for_path(&path);
		core::mem::replace(&mut self.path, Some(path))
	}

	#[must_use]
	pub fn name(&self) -> &str { &self.cached_name }
}
