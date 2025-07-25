pub mod assets;
pub mod color;
pub mod shader;
pub mod text_shader;
pub mod vertex_buffer_builder;
pub mod widget;
pub mod window;

use std::borrow::Cow;
use std::error::Error;
use std::ops::Range;

use winit::dpi::PhysicalSize;

use crate::elements::compound::CompoundEntry;
use crate::elements::element::NbtElement;
use crate::elements::{ComplexNbtElementVariant, NbtElementVariant};
use crate::render::assets::{
	BASE_TEXT_Z, BASE_Z, BOOKMARK_UV, BOOKMARK_Z, CONNECTION_UV, END_LINE_NUMBER_SEPARATOR_UV, HEADER_SIZE, HIDDEN_BOOKMARK_UV, INSERTION_CHUNK_UV, INSERTION_UV, INVALID_STRIPE_UV, JUST_OVERLAPPING_BASE_TEXT_Z, LINE_NUMBER_SEPARATOR_UV,
	LINE_NUMBER_Z, SCROLLBAR_BOOKMARK_Z, SELECTED_TOGGLE_OFF_UV, SELECTED_TOGGLE_ON_UV, TEXT_UNDERLINE_UV, TEXT_UNDERLINE_Z, TOGGLE_Z, UNSELECTED_TOGGLE_OFF_UV, UNSELECTED_TOGGLE_ON_UV,
};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::selected_text::{SelectedText, SelectedTextKeyValueError};
use crate::tree::indices::OwnedIndices;
use crate::util::{AABB, StrExt, Vec2u};
use crate::workbench::marked_line::MarkedLineSlice;

pub struct TreeRenderContext<'w> {
	ghost: Option<(&'w NbtElement, Vec2u)>,
	left_margin: usize,
	pub mouse: Vec2u,
	line_number: usize,
	pub pos: Vec2u,
	total_scroll_px: usize,
	pub remaining_scroll: usize,
	freehand: bool,
	indices: OwnedIndices,
	selected_texts: Vec<&'w SelectedText>,
	selected_texts_for_errors: Vec<&'w SelectedText>,

	// sorted least to greatest
	red_line_numbers_y: Vec<usize>,
	line_numbers: Vec<usize>,
}

impl<'w> TreeRenderContext<'w> {
	#[must_use]
	#[allow(clippy::type_complexity)] // forbidden is fine to be like that, c'mon
	pub fn new(ghost: Option<(&'w NbtElement, Vec2u)>, left_margin: usize, mouse: Vec2u, freehand: bool, total_scroll_px: usize, selected_texts: impl IntoIterator<Item = &'w SelectedText>) -> Self {
		let selected_texts = selected_texts.into_iter().collect::<Vec<_>>();
		Self {
			ghost,
			left_margin,
			mouse,
			line_number: 1,
			pos: (16 + left_margin, HEADER_SIZE).into(),
			freehand,
			total_scroll_px,
			remaining_scroll: total_scroll_px / 16,
			indices: OwnedIndices::new(),

			selected_texts_for_errors: selected_texts.clone(),
			selected_texts,
			red_line_numbers_y: vec![],
			line_numbers: vec![],
		}
	}

	#[must_use]
	pub const fn mouse(&self) -> Vec2u { self.mouse }

	#[must_use]
	pub const fn left_margin(&self) -> usize { self.left_margin }

	#[must_use]
	pub const fn total_scroll(&self) -> usize { self.total_scroll_px }

	pub fn set_last_index(&mut self, idx: usize) {
		if let Some(last) = self.indices.last_mut() {
			*last = idx;
		}
	}

	pub fn push_index(&mut self) { self.indices.push(0); }

	pub fn pop_index(&mut self) { self.indices.pop(); }

	pub fn take_child_selected_texts(&mut self) -> Vec<&'w SelectedText> { self.selected_texts_for_errors.extract_if(.., |text| self.indices.is_parent_for(&text.indices)).collect() }

	pub fn mark_line_with_error(&mut self, line_y: usize, error_underline_span: Range<usize>, error: impl Error, builder: &mut VertexBufferBuilder) {
		self.red_line_numbers_y.push(line_y);
		self.draw_error_underline_width(line_y, error_underline_span.clone(), builder);
		builder.color = TextColor::Red.to_raw();
		if AABB::new(error_underline_span.start, error_underline_span.end, line_y, line_y + 16).contains(self.mouse) {
			builder.draw_tooltip(&[&error.to_string()], self.mouse, false);
		}
	}

	pub fn mark_possible_duplicate_keys(&mut self, pos: Vec2u, key: &str, child_selected_texts: &[&'w SelectedText], builder: &mut VertexBufferBuilder) {
		let scroll = self.total_scroll();
		let left_margin = self.left_margin();

		for text in child_selected_texts {
			let is_text_of_current_child = text.indices == self.indices;
			if text.is_editing_key() && text.value.as_str() == key && !is_text_of_current_child {
				if HEADER_SIZE <= pos.y && pos.y < builder.window_height() {
					let start = pos.x + NbtElement::DEPTH_INCREMENT_WIDTH + SelectedText::PREFIXING_SPACE_WIDTH;
					let width = key.width();
					self.mark_line_with_error(pos.y, start..start + width, SelectedTextKeyValueError::DuplicateKey, builder);
				}

				if let Some(text_key_span) = text.key_span(left_margin)
					&& let Some(selected_text_y) = text.y.checked_sub(scroll).filter(|&x| HEADER_SIZE <= x && x < builder.window_height())
				{
					self.mark_line_with_error(selected_text_y, text_key_span, SelectedTextKeyValueError::DuplicateKey, builder);
				}
			}
		}
	}

	pub fn mark_possible_invalid_key(&mut self, builder: &mut VertexBufferBuilder, is_valid: impl FnOnce(&str) -> bool) {
		let pos = self.pos;
		let left_margin = self.left_margin();
		let selected_text = self.selected_texts.iter().find(|text| text.indices == self.indices);
		if let Some(text) = selected_text
			&& text.is_editing_key()
			&& let Some(key_span) = text.key_span(left_margin)
			&& !is_valid(&text.value)
		{
			self.mark_line_with_error(pos.y, key_span, SelectedTextKeyValueError::InvalidKey, builder);
		}
	}

	pub fn mark_possible_invalid_value(&mut self, builder: &mut VertexBufferBuilder, is_valid: impl FnOnce(&str) -> bool) {
		let pos = self.pos;
		let left_margin = self.left_margin();
		let selected_text = self.selected_texts.iter().find(|text| text.indices == self.indices);
		if let Some(text) = selected_text
			&& text.is_editing_value()
			&& let Some(value_span) = text.value_span(left_margin)
			&& !is_valid(&text.value)
		{
			self.mark_line_with_error(pos.y, value_span, SelectedTextKeyValueError::InvalidValue, builder);
		}
	}

	#[must_use]
	pub fn can_render_text(&self) -> bool { !self.selected_texts.iter().any(|text| text.indices == self.indices) }

	pub fn try_render_text<Nbt: NbtElementVariant>(&self, key: Option<&str>, value: Cow<'_, str>, builder: &mut VertexBufferBuilder) {
		use core::fmt::Write;

		let pos = self.pos;
		if self.can_render_text() {
			builder.settings(pos + (20, 0), false, JUST_OVERLAPPING_BASE_TEXT_Z);
			if let Some(key) = key {
				builder.color = TextColor::TreeKey.to_raw();
				let _ = write!(builder, "{key}");
				builder.color = Nbt::SEPERATOR_COLOR.to_raw();
				let _ = write!(builder, ": ");
			}

			builder.color = Nbt::VALUE_COLOR.to_raw();
			let _ = write!(builder, "{value}");
		}
	}

	pub fn draw_error_underline_width(&self, y: usize, underline_span: Range<usize>, builder: &mut VertexBufferBuilder) {
		let horizontal_scroll_before = core::mem::replace(&mut builder.horizontal_scroll, 0);
		builder.draw_texture_region_z((0, y), BASE_Z, INVALID_STRIPE_UV + (1, 1), (builder.window_width(), 16), (14, 14));
		builder.horizontal_scroll = horizontal_scroll_before;
		builder.draw_texture_region_z((underline_span.start, y + 14), TEXT_UNDERLINE_Z, TEXT_UNDERLINE_UV, (underline_span.end - underline_span.start, 2), (16, 2));
	}

	pub fn draw_toggle(&self, pos: impl Into<(usize, usize)>, open: bool, builder: &mut VertexBufferBuilder) {
		let pos = pos.into();
		let x = (pos.0 - self.left_margin) / 16;
		let y = (pos.1 - HEADER_SIZE) / 16;
		let hovered = if (self.mouse.x >= self.left_margin) & (self.mouse.y >= HEADER_SIZE) {
			((x >= (self.mouse.x - self.left_margin) / 16) || self.freehand) & (y == (self.mouse.y - HEADER_SIZE) / 16)
		} else {
			false
		};
		let uv = match (open, hovered) {
			(false, false) => UNSELECTED_TOGGLE_OFF_UV,
			(false, true) => SELECTED_TOGGLE_OFF_UV,
			(true, false) => UNSELECTED_TOGGLE_ON_UV,
			(true, true) => SELECTED_TOGGLE_ON_UV,
		};
		builder.draw_texture_z(Vec2u::from(pos) + (3, 5), TOGGLE_Z, uv, (8, 8));
	}

	pub fn render_complex_head<Nbt: ComplexNbtElementVariant>(&mut self, element: &Nbt, builder: &mut VertexBufferBuilder, key: Option<&str>, mut draw_held_entry: impl FnMut(&Self, Vec2u, &mut VertexBufferBuilder, AABB, &Nbt) -> bool) {
		if self.remaining_scroll > 0 {
			self.remaining_scroll -= 1;
			self.skip_line_numbers(1);
			return;
		}

		self.line_number();
		builder.draw_texture(self.pos, if self.pos == Vec2u::new(self.left_margin(), HEADER_SIZE) { Nbt::ROOT_UV } else { Nbt::UV }, (16, 16));
		if !element.is_empty() {
			self.draw_toggle(self.pos - (16, 0), element.is_open(), builder);
		}
		self.try_render_text::<Nbt>(key, element.value(), builder);

		let pos = self.pos;
		let _ = draw_held_entry(self, pos + (16, 16), builder, AABB::from_pos_and_dims(pos + (16, 8), PhysicalSize::new(16, 8)), element)
			|| (element.height() == 1 && draw_held_entry(self, pos + (16, 16), builder, AABB::from_pos_and_dims(pos + (16, 16), PhysicalSize::new(16, 8)), element));

		self.pos += (0, 16);
	}

	pub fn render_complex_body_kv<Nbt: ComplexNbtElementVariant<Entry = CompoundEntry>>(
		&mut self,
		element: &Nbt,
		builder: &mut VertexBufferBuilder,
		tail: bool,
		mut pre_render_draw_held_entry: impl FnMut(&Self, Vec2u, &mut VertexBufferBuilder, AABB, &Nbt) -> bool,
		mut post_render_draw_held_entry: impl FnMut(&Self, Vec2u, &mut VertexBufferBuilder, AABB, &Nbt) -> bool,
	) {
		if element.is_open() {
			let pos_before = self.pos;
			self.pos += (16, 0);
			self.push_index();

			let child_selected_texts = self.take_child_selected_texts();

			for (idx, CompoundEntry { key, value }) in element.children().enumerate() {
				let pos = self.pos;
				self.set_last_index(idx);

				self.mark_possible_duplicate_keys(pos, key, &child_selected_texts, builder);

				if pos.y > builder.window_height() {
					break;
				}

				let height = value.height();
				if self.remaining_scroll >= height {
					self.remaining_scroll -= height;
					self.skip_line_numbers(value.true_height());
					continue;
				}

				pre_render_draw_held_entry(self, pos, builder, AABB::from_pos_and_dims(pos, PhysicalSize::new(16, 8)), element);

				if self.remaining_scroll == 0 {
					builder.draw_texture(pos - (16, 0), CONNECTION_UV, (16, (idx + 1 != element.len()) as usize * 7 + 9));
				}
				value.render(builder, Some(key), idx + 1 == element.len(), self);

				let pos = self.pos;
				post_render_draw_held_entry(self, pos, builder, AABB::from_pos_and_dims(pos - (0, 8), PhysicalSize::new(16, 8)), element);
			}

			self.pop_index();
			self.pos -= (16, 0);

			if !tail {
				for i in 0..(self.pos.y - pos_before.y) / 16 {
					builder.draw_texture(pos_before - (16, 0) + (0, i * 16), CONNECTION_UV, (8, 16));
				}
			}
		} else {
			self.skip_line_numbers(element.true_height() - 1);
		}
	}

	pub fn render_complex_body<Nbt: ComplexNbtElementVariant<Entry = NbtElement>>(
		&mut self,
		element: &Nbt,
		builder: &mut VertexBufferBuilder,
		tail: bool,
		mut pre_render_draw_held_entry: impl FnMut(&Self, Vec2u, &mut VertexBufferBuilder, AABB, &Nbt) -> bool,
		mut post_render_draw_held_entry: impl FnMut(&Self, Vec2u, &mut VertexBufferBuilder, AABB, &Nbt) -> bool,
	) {
		if element.is_open() {
			let pos_before = self.pos;
			self.pos += (16, 0);
			self.push_index();

			for (idx, value) in element.children().enumerate() {
				let pos = self.pos;
				self.set_last_index(idx);

				if pos.y > builder.window_height() {
					break;
				}

				let height = value.height();
				if self.remaining_scroll >= height {
					self.remaining_scroll -= height;
					self.skip_line_numbers(value.true_height());
					continue;
				}

				pre_render_draw_held_entry(self, pos, builder, AABB::from_pos_and_dims(pos, PhysicalSize::new(16, 8)), element);

				if self.remaining_scroll == 0 {
					builder.draw_texture(self.pos - (16, 0), CONNECTION_UV, (16, (idx != element.len() - 1) as usize * 7 + 9));
				}
				value.render(builder, None, idx == element.len() - 1, self);

				let pos = self.pos;
				post_render_draw_held_entry(self, pos, builder, AABB::from_pos_and_dims(pos - (0, 8), PhysicalSize::new(16, 8)), element);
			}

			self.pop_index();
			self.pos -= (16, 0);

			if !tail {
				for i in 0..(self.pos.y - pos_before.y) / 16 {
					builder.draw_texture(pos_before - (16, 0) + (0, i * 16), CONNECTION_UV, (8, 16));
				}
			}
		} else {
			self.skip_line_numbers(element.true_height() - 1);
		}
	}

	pub fn skip_line_numbers(&mut self, n: usize) { self.line_number = self.line_number.wrapping_add(n); }

	pub fn line_number(&mut self) {
		self.line_numbers.push(self.line_number);
		self.line_number += 1;
	}

	pub fn render_line_numbers(&self, builder: &mut VertexBufferBuilder, mut bookmarks: &MarkedLineSlice) {
		use std::fmt::Write as _;

		let start = self.line_numbers.first();
		while let Some((head, rest)) = bookmarks.split_first() {
			if start.is_some_and(|&start| start > head.true_line_number()) {
				bookmarks = rest;
			} else {
				break;
			}
		}
		let mut y = HEADER_SIZE;
		for (idx, &line_number) in self.line_numbers.iter().enumerate() {
			let next_line_number = self.line_numbers.get(idx + 1).copied();

			let color = if self.red_line_numbers_y.contains(&y) {
				if idx % 2 == 0 { 0xC33C3C } else { TextColor::Red.to_raw() }
			} else {
				if idx % 2 == 0 { 0x777777 } else { TextColor::Gray.to_raw() }
			};
			let color = core::mem::replace(&mut builder.color, color);
			builder.settings((self.left_margin - line_number.ilog10() as usize * 8 - 16, y), false, BASE_TEXT_Z);
			let _ = write!(builder, "{line_number}");
			builder.color = color;

			if let Some((first, rest)) = bookmarks.split_first()
				&& line_number == first.true_line_number()
			{
				bookmarks = rest;
				builder.draw_texture_region_z((1, y + 2), BOOKMARK_Z, first.uv(), (builder.text_coords.0 + 1, 12), (16, 16));
			}
			let mut hidden_bookmarks = 0_usize;
			while let Some((first, rest)) = bookmarks.split_first()
				&& next_line_number.is_none_or(|next_line_number| line_number <= first.true_line_number() && first.true_line_number() < next_line_number)
			{
				bookmarks = rest;
				if hidden_bookmarks < 5 {
					builder.draw_texture_region_z((1, y + 15), BOOKMARK_Z, first.uv(), (builder.text_coords.0 + 1, 2), (16, 16));
				}
				hidden_bookmarks += 1;
			}

			let uv = if idx + 1 == self.line_numbers.len() { END_LINE_NUMBER_SEPARATOR_UV } else { LINE_NUMBER_SEPARATOR_UV };
			builder.draw_texture_z((builder.text_coords.0 + 4, y), LINE_NUMBER_Z, uv, (2, 16));
			y += 16;
		}
	}

	pub fn render_grid_line_numbers(&self, builder: &mut VertexBufferBuilder, mut bookmarks: &MarkedLineSlice) {
		use std::fmt::Write as _;

		let scroll = builder.scroll();

		let last_line_number = (self.line_numbers.len() > 1) as usize * 32 + 1;
		for line_number in 1..=last_line_number {
			let uv = if line_number == last_line_number { END_LINE_NUMBER_SEPARATOR_UV } else { LINE_NUMBER_SEPARATOR_UV };
			if 16 * line_number >= scroll + 16 {
				let color = if line_number % 2 == 1 { 0x777777 } else { TextColor::Gray.to_raw() };
				let color = core::mem::replace(&mut builder.color, color);
				builder.settings((self.left_margin - line_number.ilog10() as usize * 8 - 16, HEADER_SIZE + 16 * line_number - 16 - scroll), false, BASE_TEXT_Z);
				let _ = write!(builder, "{line_number}");
				builder.color = color;
				builder.draw_texture_z((builder.text_coords.0 + 4, HEADER_SIZE + 16 * line_number - 16 - scroll), LINE_NUMBER_Z, uv, (2, 16));
			}
		}

		if let Some((first, rest)) = bookmarks.split_first()
			&& first.true_line_number() == 1
		{
			bookmarks = rest;
			if scroll < 16 {
				builder.draw_texture_region_z((1, HEADER_SIZE + 2), BOOKMARK_Z, first.uv(), (self.left_margin - 7, 12), (16, 16));
			}
		}

		for (idx, &line_number) in self.line_numbers.iter().enumerate() {
			let next_line_number = self.line_numbers.get(idx + 1).copied();
			let x = idx % 32;
			let z = idx / 32;
			let pos = Vec2u::new(self.left_margin + 16 + 16 + x * 16, HEADER_SIZE + 16 + z * 16);
			if let Some((first, rest)) = bookmarks.split_first()
				&& first.true_line_number() == line_number
			{
				bookmarks = rest;
				if pos.y >= scroll + HEADER_SIZE {
					builder.draw_texture_region_z(pos - (0, scroll), BOOKMARK_Z, first.uv(), (16, 16), (16, 16));
				}
			}
			let mut hidden_bookmarks = 0_usize;
			while let Some((first, rest)) = bookmarks.split_first()
				&& next_line_number.is_none_or(|next_line_number| line_number <= first.true_line_number() && first.true_line_number() < next_line_number)
			{
				bookmarks = rest;
				if hidden_bookmarks < 5 {
					if pos.y >= scroll + HEADER_SIZE {
						builder.draw_texture_region_z(pos + (0, 14) - (0, scroll), BOOKMARK_Z, first.uv(), (16, 2), (16, 16));
					}
				}
				hidden_bookmarks += 1;
			}
		}
	}

	pub fn render_scrollbar_bookmarks(&self, builder: &mut VertexBufferBuilder, bookmarks: &MarkedLineSlice, root: &NbtElement) {
		let height = root.height();
		let mut hidden_bookmarks_at_y = 0_usize;
		let mut hidden_bookmark_y = 0;
		let mut bookmarks_at_y = 0_usize;
		let mut bookmark_y = 0;
		for bookmark in bookmarks {
			let y = HEADER_SIZE + (bookmark.line_number() * (builder.window_height() - HEADER_SIZE)) / height;
			if bookmark.uv() == BOOKMARK_UV {
				if bookmarks_at_y < 5 {
					builder.draw_texture_z((builder.window_width() - 8, y), SCROLLBAR_BOOKMARK_Z, BOOKMARK_UV, (8, 2));
				}

				if y == bookmark_y {
					bookmarks_at_y += 1;
				} else {
					bookmark_y = y;
					bookmarks_at_y = 1;
				}
			} else {
				if hidden_bookmarks_at_y < 5 {
					builder.draw_texture_z((builder.window_width() - 8, y), SCROLLBAR_BOOKMARK_Z, HIDDEN_BOOKMARK_UV, (8, 2));
				}

				if y == hidden_bookmark_y {
					hidden_bookmarks_at_y += 1;
				} else {
					hidden_bookmark_y = y;
					hidden_bookmarks_at_y = 1;
				}
			}
		}
	}

	pub fn draw_held_entry_bar<Nbt: ComplexNbtElementVariant>(&self, offset: Vec2u, builder: &mut VertexBufferBuilder, aabb: AABB, parent: &Nbt) -> bool {
		if let Some((element, pos)) = self.ghost
			&& aabb.contains(pos)
			&& parent.can_insert(element)
		{
			builder.draw_texture_region_z((self.left_margin - 2, offset.y - 1), BASE_Z, INSERTION_UV, (offset.x + 18 - self.left_margin, 2), (16, 2));
			true
		} else {
			false
		}
	}

	pub fn draw_held_entry_chunk<Nbt: ComplexNbtElementVariant>(&self, offset: Vec2u, builder: &mut VertexBufferBuilder, aabb: AABB, parent: &Nbt) -> bool {
		if let Some((element, pos)) = self.ghost
			&& aabb.contains(pos)
			&& parent.can_insert(element)
		{
			builder.draw_texture_region_z((self.left_margin - 2, offset.y), BASE_Z, INSERTION_CHUNK_UV, (offset.x + 18 - self.left_margin, 16), (16, 16));
			true
		} else {
			false
		}
	}

	pub fn draw_held_entry_grid_chunk<Nbt: ComplexNbtElementVariant>(&self, offset: Vec2u, builder: &mut VertexBufferBuilder, aabb: AABB, parent: &Nbt) -> bool {
		if let Some((element, pos)) = self.ghost
			&& aabb.contains(pos)
			&& parent.can_insert(element)
		{
			builder.draw_texture_region_z(offset, BASE_Z, INSERTION_CHUNK_UV, (16, 16), (16, 16));
			true
		} else {
			false
		}
	}
}
