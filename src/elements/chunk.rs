use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::hint::likely;
use std::io::Read;
use std::ops::{Deref, DerefMut};

use zune_inflate::{DeflateDecoder, DeflateOptions};

use crate::elements::compound::{CompoundEntry, NbtCompound};
use crate::elements::result::{NbtParseResult, err, from_opt, from_result, ok};
use crate::elements::{ComplexNbtElementVariant, Matches, NbtElement, NbtElementVariant};
use crate::render::TreeRenderContext;
use crate::render::assets::{CHUNK_GHOST_UV, CHUNK_UV, CONNECTION_UV, HEADER_SIZE, JUST_OVERLAPPING_BASE_TEXT_Z};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::serialization::decoder::Decoder;
use crate::serialization::encoder::UncheckedBufWriter;
use crate::serialization::formatter::{PrettyDisplay, PrettyFormatter};
use crate::util::{StrExt, Timestamp, Vec2u};
#[cfg(target_arch = "wasm32")]
use crate::wasm::{FakeScope as Scope, fake_scope as scope};
use crate::workbench::tab::ChunkFileFormat;

#[repr(C)]
pub struct NbtChunk {
	inner: Box<NbtCompound>,
	pub last_modified: u32,
	// need to restrict this file format to only use GZIP, ZLIB, Uncompressed, and LZ4
	format: ChunkFileFormat,
	pub x: u8,
	pub z: u8,
	_id: u8,
}

impl Matches for NbtChunk {
	fn matches(&self, other: &Self) -> bool { self.inner.matches(&other.inner) }
}

impl PartialEq for NbtChunk {
	fn eq(&self, other: &Self) -> bool { self.inner.eq(&other.inner) }
}

impl Default for NbtChunk {
	fn default() -> Self {
		Self {
			inner: Box::new(NbtCompound::default()),
			last_modified: Timestamp::now().elapsed().as_secs() as u32,
			format: ChunkFileFormat::default(),
			x: 0,
			z: 0,
			_id: Self::ID,
		}
	}
}

impl Clone for NbtChunk {
	fn clone(&self) -> Self {
		Self {
			inner: unsafe { Box::try_new(self.inner.deref().clone()).unwrap_unchecked() },
			last_modified: self.last_modified,
			format: self.format,
			x: self.x,
			z: self.z,
			_id: Self::ID,
		}
	}
}

impl Display for NbtChunk {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}|{}{{", self.x, self.z)?;
		for (idx, CompoundEntry { key, value }) in self.children().enumerate() {
			if key.needs_escape() {
				write!(f, "{key:?}")?;
			} else {
				write!(f, "{key}")?;
			}
			write!(f, ":{value}")?;
			if likely(idx < self.len() - 1) {
				write!(f, ",")?;
			}
		}
		write!(f, "}}")
	}
}

impl PrettyDisplay for NbtChunk {
	fn pretty_fmt(&self, f: &mut PrettyFormatter) {
		f.write_str(&format!("{} | {} ", self.x, self.z));
		self.inner.pretty_fmt(f)
	}
}

impl Deref for NbtChunk {
	type Target = NbtCompound;

	fn deref(&self) -> &Self::Target { &self.inner }
}

impl DerefMut for NbtChunk {
	fn deref_mut(&mut self) -> &mut Self::Target { &mut self.inner }
}

impl NbtElementVariant for NbtChunk {
	type ExtraParseInfo = usize;

	const ID: u8 = 64 | 1;
	const UV: Vec2u = CHUNK_UV;
	const GHOST_UV: Vec2u = CHUNK_GHOST_UV;

	fn from_str0(mut s: &str) -> Result<(&str, Self), usize>
	where Self: Sized {
		let digits = s.bytes().take_while(|b| b.is_ascii_digit()).count();
		let (digits, s2) = s.split_at(digits);
		let Ok(x @ 0..=31) = digits.parse::<u8>() else { return Err(s.len()) };
		s = s2.trim_start();

		s = s.strip_prefix('|').ok_or(s.len())?.trim_start();

		let digits = s.bytes().take_while(|b| b.is_ascii_digit()).count();
		let (digits, s2) = s.split_at(digits);
		let Ok(z @ 0..=31) = digits.parse::<u8>() else { return Err(s.len()) };
		s = s2.trim_start();

		let (s, compound) = NbtCompound::from_str0(s)?;
		Ok((s, Self::new(compound, (x, z), ChunkFileFormat::Zlib, Timestamp::now().elapsed().as_secs() as u32)))
	}

	fn from_bytes<'a, D: Decoder<'a>>(decoder: &mut D, idx: usize) -> NbtParseResult<Self>
	where Self: Sized {
		let bytes = decoder.rest();
		let (offsets, bytes) = from_opt(bytes.split_first_chunk::<4096>(), "header wasn't big enough")?;
		let (last_modifieds, bytes) = from_opt(bytes.split_first_chunk::<4096>(), "header wasn't big enough")?;
		let last_modified = u32::from_be_bytes(unsafe { last_modifieds[idx * 4..=idx * 4 + 3].as_ptr().cast::<[u8; 4]>().read() });
		let offset = u32::from_be_bytes(unsafe { offsets[idx * 4..=idx * 4 + 3].as_ptr().cast::<[u8; 4]>().read() });
		if offset < 512 {
			return ok(NbtChunk::unloaded_from_pos(idx));
		}
		let pos = ((idx % 16) as u8, (idx / 16) as u8);
		let len = (offset as usize & 0xFF) * 4096;
		// value does include header so we must offset against that
		let offset = ((offset >> 8) - 2) as usize * 4096;
		if bytes.len() < offset + len {
			return err("Offset goes outside bytes");
		}
		let data = &bytes[offset..offset + len];

		if let &[a, b, c, d, compression, ref data @ ..] = data {
			let chunk_len = from_opt((u32::from_be_bytes([a, b, c, d]) as usize).checked_sub(1), "Chunk was inside header.")?;
			if data.len() < chunk_len {
				return err("Offset is invalid");
			}
			let data = &data[..chunk_len];
			let (compression, element) = match compression {
				1 => (
					ChunkFileFormat::Gzip,
					NbtElement::from_be_file(&from_result(DeflateDecoder::new_with_options(data, DeflateOptions::default().set_confirm_checksum(false)).decode_gzip())?)?,
				),
				2 => (
					ChunkFileFormat::Zlib,
					NbtElement::from_be_file(&from_result(DeflateDecoder::new_with_options(data, DeflateOptions::default().set_confirm_checksum(false)).decode_zlib())?)?,
				),
				3 => (ChunkFileFormat::Nbt, NbtElement::from_be_file(data)?),
				4 => (
					ChunkFileFormat::Lz4,
					NbtElement::from_be_file(&{
						let mut vec = vec![];
						from_result(lz4_java_wrc::Lz4BlockInput::new(data).read_to_end(&mut vec))?;
						vec
					})?,
				),
				_ => return err("Unknown compression format"),
			};
			return ok(NbtChunk::new(from_opt(element.into_compound(), "Chunk was not of type compound")?, pos, compression, last_modified));
		}
		err("Invalid chunk data")
	}

	fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) {
		// todo, mcc files
		let encoded = self.format.encode(unsafe { (self.inner.as_ref() as *const NbtCompound).cast::<NbtElement>().as_ref_unchecked() });
		let len = encoded.len() + 1;
		// plus four for the len field writing, and + 1 for the compression
		let pad_len = (4096 - (len + 4) % 4096) % 4096;
		writer.write(&(len as u32).to_be_bytes());
		writer.write(
			&match self.format {
				ChunkFileFormat::Gzip => 1_u8,
				ChunkFileFormat::Zlib => 2_u8,
				ChunkFileFormat::Nbt => 3_u8,
				ChunkFileFormat::Lz4 => 4_u8,
			}
			.to_be_bytes(),
		);
		writer.write(&encoded);
		drop(encoded);
		let pad = vec![0; pad_len];
		writer.write(&pad);
	}

	fn to_le_bytes(&self, _writer: &mut UncheckedBufWriter) {}

	fn render(&self, builder: &mut VertexBufferBuilder, _name: Option<&str>, remaining_scroll: &mut usize, tail: bool, ctx: &mut TreeRenderContext) {
		use std::fmt::Write as _;

		let mut y_before = ctx.pos().y;

		'head: {
			if *remaining_scroll > 0 {
				*remaining_scroll -= 1;
				ctx.skip_line_numbers(1);
				break 'head;
			}

			let pos = ctx.pos();

			ctx.line_number();
			builder.draw_texture(pos, self.uv(), (16, 16));
			if !self.is_empty() {
				ctx.draw_toggle(pos - (16, 0), self.is_open(), builder);
			}
			ctx.check_for_invalid_key(|key| !key.parse::<usize>().is_ok_and(|x| (0..=31).contains(&x)));
			ctx.check_for_invalid_value(|value| !value.parse::<usize>().is_ok_and(|z| (0..=31).contains(&z)));
			ctx.render_errors(pos, builder);
			if ctx.forbid(pos) {
				builder.settings(pos + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "{}, {}", self.x, self.z);
			}

			ctx.offset_pos(0, 16);
			y_before += 16;
		}

		let x_before = ctx.pos().x - 16;

		if self.is_open() {
			ctx.offset_pos(16, 0);

			{
				let children_contains_forbidden = 'f: {
					let mut y = ctx.pos().y;
					for CompoundEntry { key: _, value } in self.children() {
						if ctx.selected_text_y() == Some(y.saturating_sub(*remaining_scroll * 16)) && ctx.selected_text_y().is_some_and(|y| y >= HEADER_SIZE) {
							break 'f true;
						}
						y += value.height() * 16;
					}
					false
				};
				if children_contains_forbidden {
					let mut y = ctx.pos().y;
					for CompoundEntry { key: name, value } in self.children() {
						ctx.check_for_key_duplicate(|text, _| text == name, false);
						if ctx.selected_text_y() == Some(y.saturating_sub(*remaining_scroll * 16)) && y.saturating_sub(*remaining_scroll * 16) >= HEADER_SIZE && ctx.has_duplicate_key_error() {
							ctx.set_red_line_number(y.saturating_sub(*remaining_scroll * 16), 1);
							ctx.draw_error_underline(ctx.pos().x, y.saturating_sub(*remaining_scroll * 16), builder);
							break;
						}
						y += value.height() * 16;
					}
				}
			}

			for (idx, CompoundEntry { key, value }) in self.children().enumerate() {
				let pos = ctx.pos();
				if pos.y > builder.window_height() {
					break;
				}

				let height = value.height();
				if *remaining_scroll >= height {
					*remaining_scroll -= height;
					ctx.skip_line_numbers(value.true_height());
					continue;
				}

				if *remaining_scroll == 0 {
					builder.draw_texture(pos - (16, 0), CONNECTION_UV, (16, (idx != self.len() - 1) as usize * 7 + 9));
				}
				ctx.check_for_key_duplicate(|text, _| self.inner.map.has(text) && key != text, false);
				if ctx.has_duplicate_key_error() && Some(pos.y) == ctx.selected_text_y() {
					ctx.set_red_line_number(pos.y, 0);
				}
				value.render(remaining_scroll, builder, Some(key), tail && idx == self.len() - 1, ctx);
			}

			if !tail {
				let len = (ctx.pos().y - y_before) / 16;
				for i in 0..len {
					builder.draw_texture((x_before, y_before + i * 16), CONNECTION_UV, (8, 16));
				}
			}

			ctx.offset_pos(-16, 0);
		} else {
			ctx.skip_line_numbers(self.true_height() - 1);
		}
	}

	fn value(&self) -> Cow<'_, str> { Cow::Owned(format!("{}, {}", self.x, self.z)) }
}

impl NbtChunk {
	#[must_use]
	pub fn new(inner: NbtCompound, pos: (u8, u8), compression: ChunkFileFormat, last_modified: u32) -> Self {
		Self {
			x: pos.0,
			z: pos.1,
			inner: Box::new(inner),
			format: compression,
			last_modified,
			_id: Self::ID,
		}
	}

	#[must_use]
	pub fn unloaded_from_pos(pos: usize) -> Self { Self::new(NbtCompound::default(), ((pos / 32) as u8, (pos % 32) as u8), ChunkFileFormat::default(), 0) }

	#[must_use]
	pub fn pos(&self) -> usize { self.x as usize * 32 + self.z as usize }

	pub fn set_pos(&mut self, pos: usize) {
		self.x = (pos / 32) as u8;
		self.z = (pos % 32) as u8;
	}

	#[must_use]
	pub fn is_unloaded(&self) -> bool { self.inner.is_empty() && self.last_modified == 0 }

	#[must_use]
	pub fn is_loaded(&self) -> bool { !self.is_unloaded() }

	#[must_use]
	pub fn uv(&self) -> Vec2u { if self.is_unloaded() { Self::GHOST_UV } else { Self::UV } }
}
