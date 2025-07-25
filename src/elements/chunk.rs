use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::hint::likely;
use std::io::Read;
use std::ops::{Deref, DerefMut};
use std::slice::{Iter, IterMut};

use zune_inflate::{DeflateDecoder, DeflateOptions};

use crate::elements::compound::{CompoundEntry, NbtCompound};
use crate::elements::result::{NbtParseResult, err, from_opt, from_result, ok};
use crate::elements::{ComplexNbtElementVariant, Matches, NbtElement, NbtElementVariant};
use crate::render::TreeRenderContext;
use crate::render::assets::{CHUNK_GHOST_UV, CHUNK_UV};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::serialization::decoder::Decoder;
use crate::serialization::encoder::UncheckedBufWriter;
use crate::serialization::formatter::{PrettyDisplay, PrettyFormatter};
use crate::util::{StrExt, Timestamp, Vec2u};
#[cfg(target_arch = "wasm32")]
use crate::wasm::{FakeScope as Scope, fake_scope as scope};
use crate::workbench::marked_line::MarkedLines;
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
	const VALUE_COLOR: TextColor = TextColor::TreePrimitive;
	const SEPERATOR_COLOR: TextColor = TextColor::TreeValueDesc;

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

	fn render(&self, builder: &mut VertexBufferBuilder, key: Option<&str>, tail: bool, ctx: &mut TreeRenderContext) {
		'head: {
			if ctx.remaining_scroll > 0 {
				ctx.remaining_scroll -= 1;
				ctx.skip_line_numbers(1);
				break 'head;
			}

			let pos = ctx.pos;

			ctx.line_number();
			builder.draw_texture(pos, self.uv(), (16, 16));
			if !self.is_empty() {
				ctx.draw_toggle(pos - (16, 0), self.is_open(), builder);
			}
			ctx.mark_possible_invalid_key(builder, |key| key.parse::<usize>().is_ok_and(|x| (0..=31).contains(&x)));
			ctx.mark_possible_invalid_value(builder, |value| value.parse::<usize>().is_ok_and(|x| (0..=31).contains(&x)));
			ctx.try_render_text::<Self>(key, self.value(), builder);

			ctx.pos += (0, 16);
		}

		ctx.render_complex_body_kv(self, builder, tail, TreeRenderContext::draw_held_entry_bar, TreeRenderContext::draw_held_entry_bar);
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

impl ComplexNbtElementVariant for NbtChunk {
	type Entry = <NbtCompound as ComplexNbtElementVariant>::Entry;
	const ROOT_UV: Vec2u = NbtChunk::UV;

	fn new(entries: Vec<Self::Entry>) -> Self
	where Self: Sized {
		Self::new(NbtCompound::new(entries), (0, 0), ChunkFileFormat::default(), 0)
	}

	fn height(&self) -> usize { <NbtCompound as ComplexNbtElementVariant>::height(&self.inner) }

	fn true_height(&self) -> usize { <NbtCompound as ComplexNbtElementVariant>::true_height(&self.inner) }

	fn len(&self) -> usize { <NbtCompound as ComplexNbtElementVariant>::len(&self.inner) }

	fn is_empty(&self) -> bool { <NbtCompound as ComplexNbtElementVariant>::is_empty(&self.inner) }

	fn can_insert(&self, value: &NbtElement) -> bool { <NbtCompound as ComplexNbtElementVariant>::can_insert(&self.inner, value) }

	fn is_open(&self) -> bool { <NbtCompound as ComplexNbtElementVariant>::is_open(&self.inner) }

	fn end_x(&self) -> usize { <NbtCompound as ComplexNbtElementVariant>::end_x(&self.inner) }

	unsafe fn toggle(&mut self) { unsafe { <NbtCompound as ComplexNbtElementVariant>::toggle(&mut self.inner) } }

	unsafe fn insert(&mut self, idx: usize, entry: Self::Entry) -> Result<Option<Self::Entry>, Self::Entry> { unsafe { <NbtCompound as ComplexNbtElementVariant>::insert(&mut self.inner, idx, entry) } }

	unsafe fn remove(&mut self, idx: usize) -> Option<Self::Entry> { unsafe { <NbtCompound as ComplexNbtElementVariant>::remove(&mut self.inner, idx) } }

	unsafe fn replace(&mut self, idx: usize, entry: Self::Entry) -> Result<Option<Self::Entry>, Self::Entry> { unsafe { <NbtCompound as ComplexNbtElementVariant>::replace(&mut self.inner, idx, entry) } }

	unsafe fn swap(&mut self, a: usize, b: usize) { unsafe { <NbtCompound as ComplexNbtElementVariant>::swap(&mut self.inner, a, b) } }

	unsafe fn shut<'a, 'b>(&'b mut self, scope: &'a std::thread::Scope<'a, 'b>) { unsafe { <NbtCompound as ComplexNbtElementVariant>::shut(&mut self.inner, scope) } }

	unsafe fn expand<'a, 'b>(&'b mut self, scope: &'a std::thread::Scope<'a, 'b>) { unsafe { <NbtCompound as ComplexNbtElementVariant>::expand(&mut self.inner, scope) } }

	fn recache(&mut self) { <NbtCompound as ComplexNbtElementVariant>::recache(&mut self.inner) }

	fn on_style_change(&mut self, bookmarks: &mut MarkedLines) -> bool { <NbtCompound as ComplexNbtElementVariant>::on_style_change(&mut self.inner, bookmarks) }

	fn get(&self, idx: usize) -> Option<&Self::Entry> { <NbtCompound as ComplexNbtElementVariant>::get(&self.inner, idx) }

	fn get_mut(&mut self, idx: usize) -> Option<&mut Self::Entry> { <NbtCompound as ComplexNbtElementVariant>::get_mut(&mut self.inner, idx) }

	unsafe fn get_unchecked(&self, idx: usize) -> &Self::Entry { unsafe { <NbtCompound as ComplexNbtElementVariant>::get_unchecked(&self.inner, idx) } }

	unsafe fn get_unchecked_mut(&mut self, idx: usize) -> &mut Self::Entry { unsafe { <NbtCompound as ComplexNbtElementVariant>::get_unchecked_mut(&mut self.inner, idx) } }

	fn children(&self) -> Iter<'_, Self::Entry> { <NbtCompound as ComplexNbtElementVariant>::children(&self.inner) }

	fn children_mut(&mut self) -> IterMut<'_, Self::Entry> { <NbtCompound as ComplexNbtElementVariant>::children_mut(&mut self.inner) }
}
