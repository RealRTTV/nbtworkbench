use crate::assets::{GZIP_FILE_TYPE_UV, HEADER_SIZE, HELD_SCROLLBAR_UV, MCA_FILE_TYPE_UV, NBT_FILE_TYPE_UV, SNBT_FILE_TYPE_UV, UNHELD_SCROLLBAR_UV, ZLIB_FILE_TYPE_UV};
use crate::elements::chunk::NbtRegion;
use crate::elements::compound::NbtCompound;
use crate::elements::element_type::NbtElement;
use crate::vertex_buffer_builder::{Vec2u, VertexBufferBuilder};
use crate::workbench_action::WorkbenchAction;
use crate::{LinkedQueue, RenderContext, StrExt};
use flate2::Compression;
use std::fs::write;
use std::io::Read;
use std::path::{Path, PathBuf};

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
	pub depth: usize,
}

impl Tab {
	#[must_use]
	pub fn new(nbt: NbtElement, path: &Path, compression: FileFormat, window_height: usize, window_width: usize) -> Option<Self> {
		if !(nbt.id() == NbtCompound::ID || nbt.id() == NbtRegion::ID) {
			return None;
		}

		let name: Box<str> = path.file_name()?.to_str()?.into();

		Some(Self {
			value: Box::new(nbt),
			depth: 32 + name.width(),
			name,
			path: Some(path.to_path_buf()),
			compression,
			undos: LinkedQueue::new(),
			redos: LinkedQueue::new(),
			history_changed: false,
			scroll: 0,
			horizontal_scroll: 0,
			window_height,
			window_width,
		})
	}

	pub fn save(&mut self) -> bool {
		if let Some(dir) = &self.path {
			let _ = write(dir, self.compression.encode(&self.value));
			self.history_changed = false;
			true
		} else {
			false
		}
	}

	pub fn recache_depth(&mut self) {
		self.depth = self.value.depth(Some(self.name.as_ref())) + 16;
	}

	pub fn render(&self, builder: &mut VertexBufferBuilder, ctx: &mut RenderContext, held: bool) {
		let mouse_x = ctx.mouse_x;
		let mouse_y = ctx.mouse_y;

		let horizontal_scroll_before = core::mem::replace(&mut builder.horizontal_scroll, self.horizontal_scroll());
		if let box NbtElement::Compound(compound) = &self.value {
			compound.render_root(builder, &self.name, ctx);
		} else if let box NbtElement::Region(region) = &self.value {
			region.render_root(builder, &self.name, ctx);
		}
		builder.horizontal_scroll = horizontal_scroll_before;

		if builder.window_height() >= HEADER_SIZE {
			let height = self.value.height() * 16 + 48;
			let total = builder.window_height() - HEADER_SIZE;
			if height > total {
				let offset = total * self.scroll() / height + HEADER_SIZE;
				let height = (total * total) / height;
				let held = ((builder.window_width() - 4)..(builder.window_width())).contains(&mouse_x) && (offset..=(offset + height)).contains(&mouse_y) || held;
				let uv = if held { HELD_SCROLLBAR_UV } else { UNHELD_SCROLLBAR_UV };
				builder.draw_texture((builder.window_width() - 7, offset), uv, (6, 1));
				if height > 2 {
					builder.draw_texture_region_z((builder.window_width() - 7, offset + 1), 0.0, uv + Vec2u::new(0, 5), (6, height.saturating_sub(1)), (6, 4));
				}
				if height > 1 {
					builder.draw_texture((builder.window_width() - 7, offset + height), uv + Vec2u::new(0, 15), (6, 1));
				}
			}
		}

		/*let mut tail = self.undos.tail.as_ref();
		let mut x = builder.window_width() - 19;
		for _ in 0..5 {
			if let Some(t) = tail {
				t.value.render(x, 16, builder);
				x -= 16;
				tail = t.prev.as_ref();
			} else {
				break;
			}
		}*/
	}

	#[must_use]
	pub fn scroll(&self) -> usize {
		let height = self.value.height() * 16 + 48;
		let scroll = self.scroll;
		let max = (height + HEADER_SIZE).saturating_sub(self.window_height);
		scroll.min(max) & !0b1111
	}

	#[must_use]
	pub fn horizontal_scroll(&self) -> usize {
		let left_margin = self.left_margin();
		let width = self.depth + 48;
		let scroll = self.horizontal_scroll;
		let max = (dbg!(width) + left_margin).saturating_sub(self.window_width);
		scroll.min(max)
	}

	#[must_use]
	pub fn left_margin(&self) -> usize {
		(self.value.true_height().ilog10() as usize + 1) * 8 + 4 + 8
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

	pub fn set_horizontal_scroll(&mut self, scroll: f32) {
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
}

#[derive(Copy, Clone)]
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
	pub fn encode(self, data: &NbtElement) -> Vec<u8> {
		match self {
			Self::Nbt => data.to_file().unwrap_or_default(),
			Self::Gzip => {
				let mut vec = vec![];
				let _ = flate2::read::GzEncoder::new(&*data.to_file().unwrap_or_default(), Compression::best()).read_to_end(&mut vec);
				vec
			}
			Self::Zlib => {
				let mut vec = vec![];
				let _ = flate2::read::ZlibEncoder::new(&*data.to_file().unwrap_or_default(), Compression::best()).read_to_end(&mut vec);
				vec
			}
			Self::Snbt => data.to_string().into_bytes(),
			Self::Mca => data.to_file().unwrap_or_default(),
		}
	}

	#[must_use]
	pub const fn uv(&self) -> Vec2u {
		match self {
			Self::Nbt => NBT_FILE_TYPE_UV,
			Self::Gzip => GZIP_FILE_TYPE_UV,
			Self::Zlib => ZLIB_FILE_TYPE_UV,
			Self::Snbt => SNBT_FILE_TYPE_UV,
			Self::Mca => MCA_FILE_TYPE_UV,
		}
	}
}
