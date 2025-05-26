use std::array;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::hint::likely;
use std::mem::MaybeUninit;
use std::slice::{Iter, IterMut};
#[cfg(not(target_arch = "wasm32"))] use std::thread::{Scope, scope};

use crate::assets::{CONNECTION_UV, HEADER_SIZE, JUST_OVERLAPPING_BASE_TEXT_Z, JUST_OVERLAPPING_BOOKMARK_Z, LINE_NUMBER_CONNECTOR_Z, LINE_NUMBER_SEPARATOR_UV, REGION_GRID_UV, REGION_UV};
use crate::elements::result::NbtParseResult;
use crate::elements::{ComplexNbtElementVariant, Matches, NbtChunk, NbtElement, NbtElementVariant};
use crate::render::{RenderContext, TextColor, VertexBufferBuilder};
use crate::serialization::{Decoder, PrettyDisplay, PrettyFormatter, UncheckedBufWriter};
use crate::util::Vec2u;
#[cfg(target_arch = "wasm32")]
use crate::wasm::{FakeScope as Scope, fake_scope as scope};
use crate::workbench::MarkedLines;

#[repr(C)]
pub struct NbtRegion {
	pub chunks: Box<[NbtElement; 32 * 32]>,
	height: u32,
	true_height: u32,
	max_depth: u32,
	loaded_chunks: u16,
	flags: u8,
}

impl Matches for NbtRegion {
	fn matches(&self, other: &Self) -> bool {
		for (a, b) in self.chunks.iter().zip(other.chunks.iter()) {
			if !a.matches(b) {
				return false
			}
		}
		true
	}
}

impl PartialEq for NbtRegion {
	fn eq(&self, other: &Self) -> bool { self.chunks.eq(&other.chunks) }
}

impl Clone for NbtRegion {
	fn clone(&self) -> Self {
		let mut chunks = [const { MaybeUninit::uninit() }; 32 * 32];
		for (src, dst) in self.chunks.iter().zip(chunks.iter_mut()) {
			dst.write(src.clone());
		}

		Self {
			chunks: unsafe { Box::try_new(MaybeUninit::array_assume_init(chunks)).unwrap_unchecked() },
			height: self.height,
			true_height: self.true_height,
			max_depth: self.max_depth,
			loaded_chunks: self.loaded_chunks,
			flags: self.flags,
		}
	}
}

impl Default for NbtRegion {
	fn default() -> Self {
		Self {
			chunks: Box::new(array::from_fn(|pos| NbtElement::Chunk(NbtChunk::unloaded_from_pos(pos)))),
			height: 1024 + 1,
			true_height: 1024 + 1,
			flags: 0b00,
			loaded_chunks: 0,
			max_depth: 0,
		}
	}
}

impl Display for NbtRegion {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "Region{{")?;
		for (idx, chunk) in self
			.children()
			.filter_map(NbtElement::as_chunk)
			.filter(|chunk| chunk.is_loaded())
			.enumerate()
		{
			write!(f, "{chunk}")?;
			if likely(idx + 1 < self.loaded_chunks()) {
				write!(f, ",")?;
			}
		}
		write!(f, "}}")?;
		Ok(())
	}
}

impl PrettyDisplay for NbtRegion {
	fn pretty_fmt(&self, f: &mut PrettyFormatter) {
		if self.is_empty() {
			f.write_str("Region {}")
		} else {
			f.write_str("Region {\n");
			f.increase();
			let len = self.loaded_chunks();
			for (idx, chunk) in self
				.children()
				.filter_map(NbtElement::as_chunk)
				.filter(|chunk| chunk.is_loaded())
				.enumerate()
			{
				f.indent();
				chunk.pretty_fmt(f);
				if idx + 1 < len {
					f.write_str(",\n");
				} else {
					f.write_str("\n");
				}
			}
			f.decrease();
			f.indent();
			f.write_str("}");
		}
	}
}

impl NbtRegion {
	pub const CHUNK_BANDWIDTH: usize = 32;
	pub const GRID_UV: Vec2u = REGION_GRID_UV;

	pub fn set_open(&mut self, open: bool) { self.flags = (self.flags & !0b1) | open as u8 }

	#[must_use]
	pub fn is_grid_layout(&self) -> bool { (self.flags & 0b10) > 0 }

	#[must_use]
	pub fn loaded_chunks(&self) -> usize { self.loaded_chunks as usize }
}

impl NbtElementVariant for NbtRegion {
	const ID: u8 = 64 | 0;
	const UV: Vec2u = REGION_UV;
	const GHOST_UV: Vec2u = REGION_UV;

	fn from_str0(mut s: &str) -> Result<(&str, Self), usize>
	where Self: Sized {
		s = s
			.strip_prefix("Region")
			.ok_or(s.len())?
			.trim_start();
		s = s.strip_prefix('{').ok_or(s.len())?.trim_start();
		let mut region = Self::default();
		while !s.starts_with('}') {
			let (s2, child) = NbtElement::from_str0(s, NbtElement::parse_int)?;
			// SAFETY: no caches to outdate
			if let Some(chunk) = child.as_chunk()
				&& let Ok(_) = unsafe { region.insert(chunk.pos(), child) }
			{
				s = s2.trim_start();
				if let Some(s2) = s.strip_prefix(',') {
					s = s2.trim_start();
				} else if s.starts_with('}') {
					break;
				} else {
					return Err(s.len())
				}
			} else {
				// invalid insertion
				return Err(s.len())
			}
		}
		s = s.strip_prefix('}').ok_or(s.len())?.trim_start();
		region.recache();
		Ok((s, region))
	}

	fn from_bytes<'a, D: Decoder<'a>>(decoder: &mut D) -> NbtParseResult<Self>
	where Self: Sized {
		use super::result::*;

		decoder.assert_len(8192)?;

		scope(|s| {
			let mut region = Self::default();

			let bytes = decoder.rest();
			let mut threads = Vec::with_capacity(1024);

			for idx in 0..1024 {
				while decoder.extra_data().read().is_some() {
					core::hint::spin_loop();
				}
				*decoder.extra_data().write() = Some(idx);

				let d2: &mut D = unsafe {
					(decoder as *const D)
						.cast_mut()
						.as_mut_unchecked()
				};
				threads.push(s.spawn(move || NbtChunk::from_bytes(d2)));
			}

			for (idx, thread) in threads.into_iter().enumerate() {
				let child = from_opt(thread.join().ok(), "Thread panicked")??;
				region.chunks[idx] = NbtElement::Chunk(child);
			}

			decoder.skip(bytes.len());
			region.recache();

			ok(region)
		})
	}

	fn to_be_bytes(&self, writer: &mut UncheckedBufWriter) {
		scope(move |s| {
			let mut chunks = Vec::with_capacity(1024);
			for chunk in self.chunks.iter() {
				let chunk = unsafe { chunk.as_chunk_unchecked() };
				chunks.push(s.spawn(move || {
					if chunk.is_unloaded() {
						(vec![], 0)
					} else {
						let mut writer = UncheckedBufWriter::new();
						chunk.to_be_bytes(&mut writer);
						(writer.finish(), chunk.last_modified)
					}
				}));
			}
			let mut o = 2_u32;
			let mut offsets = [0; 1024];
			let mut timestamps = [0; 1024];
			let mut new_chunks = Vec::with_capacity(chunks.len());
			for (chunk, (offset, timestamp)) in chunks
				.into_iter()
				.zip(offsets.iter_mut().zip(timestamps.iter_mut()))
			{
				let Ok((chunk, last_modified)) = chunk.join() else {
					return;
				};
				let sectors = (chunk.len() / 4096) as u32;
				if sectors > 0 {
					*offset = (o.to_be() >> 8) | (sectors << 24);
					o += sectors;
					*timestamp = last_modified;
					new_chunks.push(chunk);
				} else {
					*offset = 0;
					*timestamp = 0;
				}
			}
			writer.write(unsafe { core::slice::from_raw_parts(offsets.as_ptr().cast::<u8>(), 4096) });
			writer.write(unsafe { core::slice::from_raw_parts(timestamps.as_ptr().cast::<u8>(), 4096) });
			for chunk in new_chunks {
				writer.write(&chunk);
			}
		});
	}

	fn to_le_bytes(&self, writer: &mut UncheckedBufWriter) {
		self.to_be_bytes(writer); // todo, better way?
	}

	fn render(&self, builder: &mut VertexBufferBuilder, name: Option<&str>, remaining_scroll: &mut usize, tail: bool, ctx: &mut RenderContext) {
		use std::fmt::Write as _;

		builder.draw_texture_z(ctx.pos() - (20, 2), LINE_NUMBER_CONNECTOR_Z, LINE_NUMBER_SEPARATOR_UV, (2, 2));

		'head: {
			if *remaining_scroll > 0 {
				*remaining_scroll -= 1;
				ctx.skip_line_numbers(1);
				break 'head;
			}

			let pos = ctx.pos();

			// not used on the grid layout
			if !self.is_grid_layout() {
				ctx.line_number();
			} else {
				ctx.skip_line_numbers(1);
			}
			// fun hack for connection
			builder.draw_texture(pos, self.uv(), (16, 16));
			builder.draw_texture(pos - (16, 0), CONNECTION_UV, (16, 9));
			if !self.is_empty() {
				ctx.draw_toggle(pos - (16, 0), self.is_open(), builder);
			}
			ctx.render_errors(pos, builder);
			if ctx.forbid(pos) {
				builder.settings(pos + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
				if let Some(key) = name {
					builder.color = TextColor::TreeKey.to_raw();
					let _ = write!(builder, "{key}: [{}]", self.value());
				}
			}

			ctx.offset_pos(0, 16);
		}

		ctx.offset_pos(16, 0);

		if self.is_open() {
			if self.is_grid_layout() {
				let initial_x_offset = ctx.pos().x;
				for z in 0..32 {
					if ctx.pos().y > builder.window_height() {
						break;
					}

					if *remaining_scroll >= 1 {
						*remaining_scroll -= 1;
						for x in 0..32 {
							ctx.line_number();
							ctx.skip_line_numbers(self.chunks[z * 32 + x].true_height() - 1);
						}
						continue;
					}

					if *remaining_scroll == 0 {
						builder.draw_texture(ctx.pos() - (16, 0), CONNECTION_UV, (16, (z != 32 - 1 && tail) as usize * 7 + 9));
					}

					for x in 0..32 {
						let chunk = self.chunks[z * 32 + x]
							.as_chunk()
							.expect("All region children are chunks");

						ctx.line_number();
						ctx.skip_line_numbers(chunk.true_height() - 1);

						builder.draw_texture_z(ctx.pos(), JUST_OVERLAPPING_BOOKMARK_Z, chunk.uv(), (16, 16));

						if ctx.mouse_pos().x > ctx.left_margin() && ctx.mouse_pos().y > HEADER_SIZE {
							let mx = ((ctx.mouse_pos().x - ctx.left_margin()) & !15) + ctx.left_margin();
							let my = ((ctx.mouse_pos().y - HEADER_SIZE) & !15) + HEADER_SIZE;
							if ctx.pos() == (mx, my) {
								let text = chunk.value();
								builder.color = TextColor::White.to_raw();
								builder.draw_tooltip(&[&text], ctx.pos(), false);
							}
						}

						let pos = ctx.pos();
						ctx.draw_held_entry_grid_chunk(pos, builder, |x, y| pos == (x, y) || pos == (x, y - 8), |x| self.can_insert(x));

						ctx.offset_pos(16, 0);
					}

					ctx.offset_pos(0, 16);
					let x = ctx.pos().x;
					ctx.offset_pos(initial_x_offset as isize - x as isize, 0);
				}
			} else {
				for (idx, value) in self.children().enumerate() {
					if ctx.pos().y > builder.window_height() {
						break;
					}

					let height = value.height();
					if *remaining_scroll >= height {
						*remaining_scroll -= height;
						ctx.skip_line_numbers(value.true_height());
						continue;
					}

					if *remaining_scroll == 0 {
						builder.draw_texture(ctx.pos() - (16, 0), CONNECTION_UV, (16, (idx != self.len() - 1 && tail) as usize * 7 + 9));
					}

					let pos = ctx.pos();
					ctx.draw_held_entry_chunk(pos, builder, |x, y| pos == (x, y) || pos == (x, y - 8), |x| self.can_insert(x));

					value.render(&mut *remaining_scroll, builder, None, idx == self.len() - 1 && tail, ctx);
				}
			}
		}
	}

	fn value(&self) -> Cow<'_, str> { Cow::Owned(format!("{} chunk{}", self.loaded_chunks, if self.loaded_chunks == 1 { "" } else { "s" })) }

	fn uv(&self) -> Vec2u { if self.is_grid_layout() { Self::GRID_UV } else { Self::UV } }
}

impl ComplexNbtElementVariant for NbtRegion {
	type Entry = NbtElement;

	fn new(entries: Vec<Self::Entry>) -> Self
	where Self: Sized {
		let mut this = Self::default();
		for (idx, entry) in entries.into_iter().enumerate() {
			let _ = unsafe { this.insert(idx, entry) };
		}
		this.recache();
		this
	}

	fn height(&self) -> usize { self.height as usize }

	fn true_height(&self) -> usize { self.true_height as usize }

	fn len(&self) -> usize { 1024 }

	fn can_insert(&self, value: &NbtElement) -> bool { value.is_chunk() }

	fn is_open(&self) -> bool { (self.flags & 0b1) > 0 }

	fn max_depth(&self) -> usize { self.max_depth as usize }

	unsafe fn toggle(&mut self) {
		let open = !self.is_open() && !self.is_empty();
		self.set_open(open);
		if !open && !self.is_empty() {
			scope(|scope| unsafe { self.shut(scope) });
		}
	}

	unsafe fn insert(&mut self, idx: usize, entry: Self::Entry) -> Result<Option<Self::Entry>, Self::Entry> { unsafe { self.replace(idx, entry) } }

	unsafe fn remove(&mut self, pos: usize) -> Option<Self::Entry> { Some(core::mem::replace(self.chunks.get_mut(pos)?, NbtElement::Chunk(NbtChunk::unloaded_from_pos(pos)))) }

	unsafe fn replace(&mut self, idx: usize, mut entry: Self::Entry) -> Result<Option<Self::Entry>, Self::Entry> {
		let Some(chunk) = entry.as_chunk_mut() else { return Err(entry) };
		chunk.set_pos(idx);
		let result = match self.chunks.get_mut(idx) {
			Some(slot) => core::mem::replace(slot, entry),
			None => return Err(entry),
		};
		Ok(Some(result))
	}

	unsafe fn swap(&mut self, a: usize, b: usize) {
		let Some(a_value) = (unsafe { self.remove(a) }) else { return };
		let Some(b_value) = (unsafe { self.remove(b) }) else { return };
		let _ = unsafe { self.replace(a, b_value) };
		let _ = unsafe { self.replace(b, a_value) };
	}

	unsafe fn shut<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		self.set_open(false);

		let mut iter = self
			.children_mut()
			.array_chunks::<{ Self::CHUNK_BANDWIDTH }>();
		for elements in iter.by_ref() {
			scope.spawn(|| {
				for element in elements {
					if element.is_open() {
						unsafe { element.shut(scope) };
					}
				}
			});
		}
		if let Some(rem) = iter.into_remainder() {
			scope.spawn(|| {
				for element in rem {
					if element.is_open() {
						unsafe { element.shut(scope) };
					}
				}
			});
		}
	}

	unsafe fn expand<'a, 'b>(&'b mut self, scope: &'a Scope<'a, 'b>) {
		self.set_open(!self.is_empty());
		if !self.is_grid_layout() {
			let mut iter = self
				.children_mut()
				.array_chunks::<{ Self::CHUNK_BANDWIDTH }>();
			for elements in iter.by_ref() {
				scope.spawn(|| {
					for element in elements {
						unsafe { element.expand(scope) };
					}
				});
			}
			if let Some(rem) = iter.into_remainder() {
				scope.spawn(|| {
					for element in rem {
						unsafe { element.expand(scope) };
					}
				});
			}
		}
	}

	fn recache(&mut self) {
		let mut true_height = 1;
		let mut height = 1;
		let mut loaded_chunks = 0_usize;
		for child in self.children() {
			if child
				.as_chunk()
				.is_some_and(|chunk| chunk.is_loaded())
			{
				loaded_chunks += 1;
			}
			true_height += child.true_height() as u32;
			height += child.height() as u32;
		}
		self.true_height = true_height;
		self.height = height;
		self.loaded_chunks = loaded_chunks as u16;

		let mut max_depth = 0;
		if self.is_open() {
			if self.is_grid_layout() {
				max_depth = 16 + 32 * 16;
				self.height = 33;
			} else {
				for child in self.children() {
					max_depth = usize::max(max_depth, 16 + 4 + child.value_width());
					max_depth = usize::max(max_depth, 16 + child.max_depth());
				}
			}
		}
		self.max_depth = max_depth as u32;
	}

	fn on_style_change(&mut self, bookmarks: &mut MarkedLines) -> bool {
		scope(|scope| {
			self.flags ^= 0b10;
			if self.is_grid_layout() {
				// one for the region + one for the chunk head
				let mut true_line_number = 2_usize;
				for (idx, chunk) in self.children_mut().enumerate() {
					let true_height = chunk.true_height();
					// SAFETY: the bookmarks are updated
					unsafe { chunk.shut(scope) };
					// skip the head because that shouldn't be hidden
					for bookmark in &mut bookmarks[true_line_number + 1..=true_line_number + true_height] {
						*bookmark = bookmark.hidden(idx + 1);
					}
					true_line_number += true_height;
				}
			}
		});
		true
	}

	fn get(&self, idx: usize) -> Option<&Self::Entry> { self.chunks.get(idx) }

	fn get_mut(&mut self, idx: usize) -> Option<&mut Self::Entry> { self.chunks.get_mut(idx) }

	unsafe fn get_unchecked(&self, idx: usize) -> &Self::Entry { unsafe { self.chunks.get_unchecked(idx) } }

	unsafe fn get_unchecked_mut(&mut self, idx: usize) -> &mut Self::Entry { unsafe { self.chunks.get_unchecked_mut(idx) } }

	fn children(&self) -> Iter<'_, Self::Entry> { self.chunks.iter() }

	fn children_mut(&mut self) -> IterMut<'_, Self::Entry> { self.chunks.iter_mut() }
}
