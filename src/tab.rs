use std::fs::write;
use std::io::Read;
use std::path::{Path, PathBuf};

use flate2::Compression;
use uuid::Uuid;

use crate::assets::{
	BYTE_ARRAY_GHOST_UV, BYTE_ARRAY_UV, BYTE_GHOST_UV, BYTE_UV, CHUNK_GHOST_UV, CHUNK_UV, COMPOUND_GHOST_UV, COMPOUND_ROOT_UV, COMPOUND_UV, DOUBLE_GHOST_UV, DOUBLE_UV, ENABLED_FREEHAND_MODE_UV,
	FLOAT_GHOST_UV, FLOAT_UV, FREEHAND_MODE_UV, GZIP_FILE_TYPE_UV, HEADER_SIZE, HELD_SCROLLBAR_UV, INT_ARRAY_GHOST_UV, INT_ARRAY_UV, INT_GHOST_UV, INT_UV, LINE_NUMBER_SEPARATOR_UV, LIST_GHOST_UV,
	LIST_UV, LONG_ARRAY_GHOST_UV, LONG_ARRAY_UV, LONG_GHOST_UV, LONG_UV, MCA_FILE_TYPE_UV, NBT_FILE_TYPE_UV, REDO_UV, REGION_UV, SCROLLBAR_Z, SHORT_GHOST_UV, SHORT_UV, SNBT_FILE_TYPE_UV,
	STRING_GHOST_UV, STRING_UV, UNDO_UV, UNHELD_SCROLLBAR_UV, UNKNOWN_NBT_GHOST_UV, UNKNOWN_NBT_UV, ZLIB_FILE_TYPE_UV,
};
use crate::elements::chunk::NbtRegion;
use crate::elements::compound::NbtCompound;
use crate::elements::element::NbtElement;
use crate::vertex_buffer_builder::{Vec2u, VertexBufferBuilder};
use crate::workbench_action::WorkbenchAction;
use crate::{LinkedQueue, RenderContext, StrExt};

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
	// must be ordered least to greatest
	pub bookmarks: Vec<usize>,
	pub uuid: Uuid,
	pub freehand_mode: bool,
}

impl Tab {
	#[must_use]
	pub fn new(nbt: NbtElement, path: &Path, compression: FileFormat, window_height: usize, window_width: usize) -> Option<Self> {
		if !(nbt.id() == NbtCompound::ID || nbt.id() == NbtRegion::ID) {
			return None;
		}

		Some(Self {
			value: Box::new(nbt),
			name: path.file_name()?.to_str()?.into(),
			path: Some(path.to_path_buf()),
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
		})
	}

	pub fn save(&mut self) -> bool {
		if write(self.path.as_deref().unwrap_or_else(|| self.name.as_ref().as_ref()), self.compression.encode(&self.value)).is_err() {
			return false;
		};
		self.history_changed = false;
		true
	}

	#[allow(clippy::too_many_lines)]
	pub fn render(&self, builder: &mut VertexBufferBuilder, ctx: &mut RenderContext, held: bool, held_entry: Option<&NbtElement>) {
		let mouse_x = ctx.mouse_x;
		let mouse_y = ctx.mouse_y;

		let horizontal_scroll_before = core::mem::replace(&mut builder.horizontal_scroll, self.horizontal_scroll(held_entry));
		if let Some(compound) = self.value.as_compound() {
			compound.render_root(builder, &self.name, ctx);
		} else if let Some(region) = self.value.as_region() {
			region.render_root(builder, &self.name, ctx);
		}
		ctx.render_line_numbers(builder, &self.bookmarks);
		builder.horizontal_scroll = horizontal_scroll_before;

		if builder.window_height() >= HEADER_SIZE {
			let height = self.value.height() * 16 + 48;
			let total = builder.window_height() - HEADER_SIZE;
			if height > total {
				let offset = total * self.scroll() / height + HEADER_SIZE;
				let height = (total * total) / height;
				let held = ((builder.window_width() - 8)..(builder.window_width())).contains(&mouse_x) && (offset..=(offset + height)).contains(&mouse_y) || held;
				let uv = if held { HELD_SCROLLBAR_UV } else { UNHELD_SCROLLBAR_UV };
				builder.draw_texture_z((builder.window_width() - 7, offset), SCROLLBAR_Z, uv, (6, 1));
				if height > 2 {
					builder.draw_texture_region_z((builder.window_width() - 7, offset + 1), SCROLLBAR_Z, uv + (0, 5), (6, height.saturating_sub(1)), (6, 4));
				}
				if height > 1 {
					builder.draw_texture_z((builder.window_width() - 7, offset + height), SCROLLBAR_Z, uv + (0, 15), (6, 1));
				}
			}
		}

		{
			let mut tail = self.undos.tail.as_deref();
			builder.draw_texture((builder.window_width() - 107, 27), LINE_NUMBER_SEPARATOR_UV + (0, 1), (2, 14));
			builder.draw_texture((builder.window_width() - 129, 26), LINE_NUMBER_SEPARATOR_UV, (2, 16));
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
			builder.draw_texture((builder.window_width() - 213, 27), LINE_NUMBER_SEPARATOR_UV + (0, 1), (2, 14));
			builder.draw_texture((builder.window_width() - 235, 26), LINE_NUMBER_SEPARATOR_UV, (2, 16));
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
			builder.draw_texture((builder.window_width() - 22, 26), LINE_NUMBER_SEPARATOR_UV, (2, 16));
			let freehand_uv = {
				let hovering = (builder.window_width() - 16..builder.window_width()).contains(&mouse_x) && (26..42).contains(&mouse_y);
				if hovering {
					builder.draw_tooltip(&["Freehand Mode (alt + f)"], (mouse_x, mouse_y));
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
			let mx = if (24..46).contains(&mouse_y) { Some(mouse_x & !0b1111) } else { None };
			builder.draw_texture((0, 26), if mx == Some(0) { BYTE_UV } else { BYTE_GHOST_UV }, (16, 16));
			builder.draw_texture((16, 26), if mx == Some(16) { SHORT_UV } else { SHORT_GHOST_UV }, (16, 16));
			builder.draw_texture((32, 26), if mx == Some(32) { INT_UV } else { INT_GHOST_UV }, (16, 16));
			builder.draw_texture((48, 26), if mx == Some(48) { LONG_UV } else { LONG_GHOST_UV }, (16, 16));
			builder.draw_texture((64, 26), if mx == Some(64) { FLOAT_UV } else { FLOAT_GHOST_UV }, (16, 16));
			builder.draw_texture((80, 26), if mx == Some(80) { DOUBLE_UV } else { DOUBLE_GHOST_UV }, (16, 16));
			builder.draw_texture((96, 26), if mx == Some(96) { BYTE_ARRAY_UV } else { BYTE_ARRAY_GHOST_UV }, (16, 16));
			builder.draw_texture((112, 26), if mx == Some(112) { INT_ARRAY_UV } else { INT_ARRAY_GHOST_UV }, (16, 16));
			builder.draw_texture((128, 26), if mx == Some(128) { LONG_ARRAY_UV } else { LONG_ARRAY_GHOST_UV }, (16, 16));
			builder.draw_texture((144, 26), if mx == Some(144) { STRING_UV } else { STRING_GHOST_UV }, (16, 16));
			builder.draw_texture((160, 26), if mx == Some(160) { LIST_UV } else { LIST_GHOST_UV }, (16, 16));
			builder.draw_texture((176, 26), if mx == Some(176) { COMPOUND_UV } else { COMPOUND_GHOST_UV }, (16, 16));
			builder.draw_texture((192, 26), if mx == Some(192) && self.value.id() == NbtRegion::ID { CHUNK_UV } else { CHUNK_GHOST_UV }, (16, 16));
			builder.draw_texture((208, 26), if mx == Some(208) { UNKNOWN_NBT_UV } else { UNKNOWN_NBT_GHOST_UV }, (16, 16));
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

	#[must_use]
	pub fn scroll(&self) -> usize {
		let height = self.value.height() * 16 + 48;
		let scroll = self.scroll;
		let max = (height + HEADER_SIZE).saturating_sub(self.window_height);
		scroll.min(max) & !0b1111
	}

	#[must_use]
	pub fn horizontal_scroll(&self, held: Option<&NbtElement>) -> usize {
		let left_margin = self.left_margin(held);
		let width = self.value.max_depth().max(self.name.width()) + 32 + 48;
		let scroll = self.horizontal_scroll;
		let max = (width + left_margin).saturating_sub(self.window_width);
		scroll.min(max)
	}

	#[must_use]
	pub fn left_margin(&self, held: Option<&NbtElement>) -> usize {
		((self.value.true_height() + held.map_or(0, NbtElement::true_height)).ilog10() as usize + 1) * 8 + 4 + 8
	}

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
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum FileFormat {
	Nbt,
	Gzip,
	Zlib,
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
		}
	}

	#[must_use]
	pub fn encode(self, data: &NbtElement) -> Vec<u8> {
		match self {
			Self::Nbt | Self::Mca => data.to_file(),
			Self::Gzip => {
				dbg!();
				let mut vec = vec![];
				dbg!();
				let _ = flate2::read::GzEncoder::new(&*data.to_file(), Compression::best()).read_to_end(&mut vec);
				dbg!();
				vec
			}
			Self::Zlib => {
				let mut vec = vec![];
				let _ = flate2::read::ZlibEncoder::new(&*data.to_file(), Compression::best()).read_to_end(&mut vec);
				vec
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
		}
	}
}

impl ToString for FileFormat {
	fn to_string(&self) -> String {
		self.into_str().to_owned()
	}
}