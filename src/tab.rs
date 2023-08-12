use std::fs::write;
use std::io::Read;
use std::path::{Path, PathBuf};
use flate2::Compression;
use crate::assets::HEADER_SIZE;
use crate::elements::element_type::NbtElement;
use crate::{LinkedQueue, RenderContext};
use crate::elements::compound::NbtCompound;
use crate::vertex_buffer_builder::VertexBufferBuilder;
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
}

impl Tab {
	#[must_use]
	pub fn new(nbt: NbtElement, path: &Path, compression: FileFormat, window_height: usize) -> Option<Self> {
		if nbt.id() != NbtCompound::ID {
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

	pub fn render(&self, builder: &mut VertexBufferBuilder, ctx: &mut RenderContext) {
		if let box NbtElement::Compound(compound) = &self.value {
			builder.horizontal_scroll = self.horizontal_scroll;
			compound.render_root(builder, &self.name, ctx);
			builder.horizontal_scroll = 0;
			if builder.window_height() >= HEADER_SIZE {
				let height = compound.height() * 16 + 48;
				let total = builder.window_height() - HEADER_SIZE;
				if height > total {
					let offset = total * self.scroll() / height + HEADER_SIZE;
					let height = (total * total) / height;
					builder.draw_texture((builder.window_width() - 7, offset), (112, 32), (6, 1));
					builder.draw_texture_region_z((builder.window_width() - 4, offset + 1), 0.0, (112, 38), (6, height.saturating_sub(1)), (3, 4));
					builder.draw_texture((builder.window_width() - 7, offset + height), (112, 47), (6, 1));
				}
			}
		}
	}

	#[must_use]
	pub fn scroll(&self) -> usize {
		let height = self.value.height() * 16 + 48;
		let scroll = self.scroll;
		let max = (height + HEADER_SIZE).saturating_sub(self.window_height);
		scroll.min(max) & !0b1111
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
	}
}

#[derive(Copy, Clone)]
pub enum FileFormat {
	Nbt = 0,
	Gzip = 1,
	Zlib = 2,
	Snbt = 3,
}

impl FileFormat {
	#[must_use]
	pub const fn cycle(self) -> Self {
		match self {
			Self::Nbt => Self::Gzip,
			Self::Gzip => Self::Zlib,
			Self::Zlib => Self::Snbt,
			Self::Snbt => Self::Nbt,
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
		}
	}
}
