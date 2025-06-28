pub mod assets;
pub mod color;
pub mod shader;
pub mod text_shader;
pub mod vertex_buffer_builder;
pub mod widget;
pub mod window;

use crate::{
	elements::element::NbtElement,
	render::{
		assets::{
			BASE_TEXT_Z, BASE_Z, BOOKMARK_UV, BOOKMARK_Z, END_LINE_NUMBER_SEPARATOR_UV, HEADER_SIZE, HIDDEN_BOOKMARK_UV, INSERTION_CHUNK_UV, INSERTION_UV, INVALID_STRIPE_UV, LINE_NUMBER_SEPARATOR_UV, LINE_NUMBER_Z, SCROLLBAR_BOOKMARK_Z,
			SELECTED_TOGGLE_OFF_UV, SELECTED_TOGGLE_ON_UV, TEXT_UNDERLINE_UV, TOGGLE_Z, UNSELECTED_TOGGLE_OFF_UV, UNSELECTED_TOGGLE_ON_UV,
		},
		color::TextColor,
		vertex_buffer_builder::VertexBufferBuilder,
	},
	util::{StrExt, Vec2u},
	workbench::marked_line::MarkedLineSlice,
};

pub struct RenderContext<'a> {
	selecting_key: bool,
	selected_text_y: Option<usize>,
	selected_key: Option<Box<str>>,
	selected_value: Option<Box<str>>,
	extend_error: bool,
	invalid_key_error: bool,
	invalid_value_error: bool,
	key_duplicate_error: bool,
	ghost: Option<(&'a NbtElement, Vec2u)>,
	left_margin: usize,
	pub mouse: Vec2u,
	line_number: usize,
	// the most errors from invalid selected text can be 2 lines (duplicate key)
	red_line_numbers: [usize; 2],
	x_offset: usize,
	y_offset: usize,
	// sorted least to greatest
	line_numbers: Vec<usize>,
	freehand: bool,
}

impl<'a> RenderContext<'a> {
	#[must_use]
	#[allow(clippy::type_complexity)] // forbidden is fine to be like that, c'mon
	pub fn new(selected_text_y: Option<usize>, selected_key: Option<Box<str>>, selected_value: Option<Box<str>>, selecting_key: bool, ghost: Option<(&'a NbtElement, Vec2u)>, left_margin: usize, mouse: Vec2u, freehand: bool) -> Self {
		Self {
			selecting_key,
			selected_text_y,
			selected_key,
			selected_value,
			extend_error: false,
			invalid_key_error: false,
			invalid_value_error: false,
			key_duplicate_error: false,
			ghost,
			left_margin,
			mouse,
			line_number: 1,
			red_line_numbers: [0, 0],
			x_offset: 16 + left_margin,
			y_offset: HEADER_SIZE,
			line_numbers: vec![],
			freehand,
		}
	}

	#[must_use]
	pub const fn pos(&self) -> Vec2u { Vec2u::new(self.x_offset, self.y_offset) }

	pub fn offset_pos(&mut self, x: isize, y: isize) {
		self.x_offset = self.x_offset.wrapping_add(x as usize);
		self.y_offset = self.y_offset.wrapping_add(y as usize);
	}

	#[must_use]
	pub fn selected_text_y(&self) -> Option<usize> { self.selected_text_y }

	#[must_use]
	pub const fn mouse(&self) -> Vec2u { self.mouse }

	#[must_use]
	pub const fn left_margin(&self) -> usize { self.left_margin }

	#[must_use]
	pub const fn has_invalid_key_error(&self) -> bool { self.invalid_key_error }

	#[must_use]
	pub const fn has_invalid_value_error(&self) -> bool { self.invalid_value_error }

	#[must_use]
	pub const fn has_duplicate_key_error(&self) -> bool { self.key_duplicate_error }

	pub const fn set_red_line_number(&mut self, y: usize, idx: usize) { self.red_line_numbers[idx] = y; }

	pub fn check_for_key_duplicate<F: FnOnce(&str, Option<&str>) -> bool>(&mut self, f: F, extend: bool) {
		if let Some(selected_key) = self.selected_key.as_ref()
			&& self.selecting_key
		{
			self.key_duplicate_error = f(selected_key, self.selected_value.as_ref().map(Box::as_ref));
			self.extend_error = extend;
		}
	}

	pub fn check_for_invalid_key<F: FnOnce(&str) -> bool>(&mut self, f: F) {
		let (_, y) = self.pos().into();
		if let Some(selected_key) = self.selected_key.as_ref()
			&& Some(y) == self.selected_text_y
			&& self.selecting_key
		{
			self.invalid_key_error = f(selected_key);
		}
	}

	pub fn check_for_invalid_value<F: FnOnce(&str) -> bool>(&mut self, f: F) {
		let (_, y) = self.pos().into();
		if let Some(selected_value) = self.selected_value.as_ref()
			&& Some(y) == self.selected_text_y
			&& !self.selecting_key
		{
			self.invalid_value_error = f(selected_value);
		}
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

	#[must_use]
	pub fn forbid(&self, pos: impl Into<(usize, usize)>) -> bool {
		let (_, y) = pos.into();
		if self.selected_text_y == Some(y) { false } else { true }
	}

	pub fn render_errors(&mut self, pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder) {
		let (x, y) = pos.into();
		if let Some(selected_text_y) = self.selected_text_y
			&& (self.key_duplicate_error | self.invalid_key_error | self.invalid_value_error)
			&& y == selected_text_y
		{
			self.red_line_numbers[0] = selected_text_y;
			self.draw_error_underline(x, y, builder);
		}
	}

	pub fn draw_error_underline_width(&self, x: usize, y: usize, overridden_width: usize, builder: &mut VertexBufferBuilder) {
		let horizontal_scroll_before = core::mem::replace(&mut builder.horizontal_scroll, 0);
		builder.draw_texture_region_z((0, y), BASE_Z, INVALID_STRIPE_UV + (1, 1), (builder.window_width(), 16), (14, 14));
		builder.horizontal_scroll = horizontal_scroll_before;
		builder.draw_texture_region_z((x + 20, y + 14), BASE_Z, TEXT_UNDERLINE_UV, (overridden_width, 2), (16, 2));
	}

	pub fn draw_error_underline(&self, x: usize, y: usize, builder: &mut VertexBufferBuilder) {
		let key_width = self.selected_key.as_deref().map(str::width).unwrap_or(0);
		let value_width = self.selected_value.as_deref().map(str::width).unwrap_or(0);
		let (overridden_width, x_shift) = if self.selected_key.is_some() {
			if self.extend_error {
				(key_width + value_width + ": ".width(), 0)
			} else if self.selecting_key {
				(key_width, 0)
			} else {
				(value_width, key_width + ": ".width())
			}
		} else {
			(value_width, 0)
		};
		self.draw_error_underline_width(x + x_shift, y, overridden_width, builder);
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

			let color = if (self.red_line_numbers[0] == y) | (self.red_line_numbers[1] == y) {
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

	pub fn render_key_value_errors(&mut self, builder: &mut VertexBufferBuilder) {
		if self.mouse.y < HEADER_SIZE {
			return
		}
		let y = ((self.mouse.y - HEADER_SIZE) & !15) + HEADER_SIZE;
		if self.red_line_numbers.into_iter().any(|red_line_number| red_line_number == y) {
			let mut errors = vec![];
			if self.invalid_value_error {
				errors.push("Error! The currently entered value is not valid for this type.");
			}
			if self.invalid_key_error {
				errors.push("Error! The currently entered key is not valid for this type.");
			}
			if self.key_duplicate_error {
				errors.push("Error! The current key is a duplicate of another one.");
			}
			let color_before = core::mem::replace(&mut builder.color, TextColor::Red.to_raw());
			builder.draw_tooltip(&errors, self.mouse, false);
			builder.color = color_before;
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

	pub fn draw_held_entry_bar<F: FnOnce(usize, usize) -> bool, G: FnOnce(&NbtElement) -> bool>(&mut self, pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder, f: F, g: G) -> bool {
		let (x_offset, y_offset) = pos.into();
		if let Some((element, pos)) = self.ghost
			&& f(pos.x, pos.y)
			&& g(element)
		{
			builder.draw_texture_region_z((self.left_margin - 2, y_offset - 1), BASE_Z, INSERTION_UV, (x_offset + 18 - self.left_margin, 2), (16, 2));
			true
		} else {
			false
		}
	}

	pub fn draw_held_entry_chunk<F: FnOnce(usize, usize) -> bool, G: FnOnce(&NbtElement) -> bool>(&mut self, pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder, f: F, g: G) -> bool {
		let (x_offset, y_offset) = pos.into();
		if let Some((element, pos)) = self.ghost
			&& f(pos.x, pos.y)
			&& g(element)
		{
			builder.draw_texture_region_z((self.left_margin - 2, y_offset), BASE_Z, INSERTION_CHUNK_UV, (x_offset + 18 - self.left_margin, 16), (16, 16));
			true
		} else {
			false
		}
	}

	pub fn draw_held_entry_grid_chunk<F: FnOnce(usize, usize) -> bool, G: FnOnce(&NbtElement) -> bool>(&mut self, pos: impl Into<(usize, usize)>, builder: &mut VertexBufferBuilder, f: F, g: G) -> bool {
		let (x_offset, y_offset) = pos.into();
		if let Some((element, pos)) = self.ghost
			&& f(pos.x, pos.y)
			&& g(element)
		{
			builder.draw_texture_region_z((x_offset, y_offset), BASE_Z, INSERTION_CHUNK_UV, (16, 16), (16, 16));
			true
		} else {
			false
		}
	}
}
