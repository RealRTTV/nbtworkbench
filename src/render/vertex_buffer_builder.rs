use std::fmt::{Debug, Formatter};
use std::intrinsics::unlikely;
use std::ops::BitAnd;

use winit::dpi::PhysicalSize;

use crate::assets::{ZOffset, BASE_TEXT_Z, BASE_Z, TOOLTIP_UV, TOOLTIP_Z};
use crate::render::TextColor;
use crate::util::StrExt;

pub struct VertexBufferBuilder {
	vertices: Vec<f32>,
	indices: Vec<u16>,
	text_vertices: Vec<f32>,
	text_indices: Vec<u32>,
	vertices_len: u32,
	text_vertices_len: u32,
	window_width: f32,
	window_height: f32,
	scroll: usize,
	pub horizontal_scroll: usize,
	pub text_coords: (usize, usize),
	dropshadow: bool,
	text_z: ZOffset,
	pub color: u32,
	two_over_width: f32,
	negative_two_over_height: f32,
	tooltips: Vec<(Box<[String]>, Vec2u, bool, u32)>,
	scale: f32,
}

impl core::fmt::Write for VertexBufferBuilder {
	fn write_str(&mut self, text: &str) -> std::fmt::Result {
		let (mut x, y) = self.text_coords;
		x += text.chars().fold(0, |offset, char| {
			let char = if (char as u32) < 56832 {
				char as u16
			} else {
				56829
			};
			offset + self.draw_char(char, x + offset, y, self.text_z)
		});
		self.text_coords = (x, y);
		Ok(())
	}

	fn write_char(&mut self, c: char) -> std::fmt::Result {
		let c = if (c as u32) < 56832 { c as u16 } else { 56829 };
		self.text_coords.0 += self.draw_char(c, self.text_coords.0, self.text_coords.1, self.text_z);
		Ok(())
	}
}

impl VertexBufferBuilder {
	pub const CHAR_WIDTH: &'static [u8] = include_bytes!("../assets/char_widths.hex");
	pub const CHAR_HEIGHT: usize = 16;

	pub fn new(size: PhysicalSize<u32>, scroll: usize, scale: f32) -> Self {
		Self {
			vertices: Vec::with_capacity(98304),
			indices: Vec::with_capacity(65536),
			text_vertices: Vec::with_capacity(98304),
			text_indices: Vec::with_capacity(65536),
			vertices_len: 0,
			text_vertices_len: 0,
			window_width: size.width as f32,
			window_height: size.height as f32,
			scroll,
			horizontal_scroll: 0,
			text_coords: (0, 0),
			dropshadow: false,
			text_z: BASE_TEXT_Z,
			color: TextColor::White.to_raw(),
			two_over_width: 2.0 / size.width as f32,
			negative_two_over_height: -2.0 / size.height as f32,
			tooltips: vec![],
			scale
		}
	}

	#[inline]
	pub const fn scroll(&self) -> usize { self.scroll }

	#[inline]
	pub fn settings(&mut self, pos: impl Into<(usize, usize)>, dropshadow: bool, z: ZOffset) {
		self.text_coords = pos.into();
		self.dropshadow = dropshadow;
		self.text_z = z;
	}

	#[inline]
	#[rustfmt::skip]
	fn draw_char(&mut self, c: u16, x: usize, y: usize, z: ZOffset) -> usize {
		if self.dropshadow {
			self.draw_unicode_z_color(x + 1, y + 1, z, c, {
				self.color
					.wrapping_shr(16)
					.bitand(0xFF)
					.wrapping_mul(21)
					.wrapping_div(85)
					.wrapping_shl(16)
			  | self.color
					.wrapping_shr(8)
					.bitand(0xFF)
					.wrapping_mul(21)
					.wrapping_div(85)
					.wrapping_shl(8)
			  | self.color
					.wrapping_shr(0)
					.bitand(0xFF)
					.wrapping_mul(21)
					.wrapping_div(85)
					.wrapping_shl(0)
			});
		}
		self.draw_unicode_z_color(x, y, z, c, self.color & 0xFFFFFF);
		Self::CHAR_WIDTH[c as usize] as usize
	}

	#[inline]
	pub fn draw_tooltip(&mut self, text: &[&str], pos: impl Into<(usize, usize)>, force_draw_right: bool) {
		let color = self.color;
		self.tooltips.push((text.iter().map(|s| s.to_string()).collect::<Vec<_>>().into_boxed_slice(), Vec2u::from(pos.into()), force_draw_right, color));
	}

	pub fn reset(&mut self) {
		self.vertices.clear();
		self.indices.clear();
		self.text_vertices.clear();
		self.text_indices.clear();
		self.vertices_len = 0;
		self.text_vertices_len = 0;
		self.horizontal_scroll = 0;
		self.text_coords = (0, 0);
		self.dropshadow = false;
		self.text_z = BASE_TEXT_Z;
		self.color = TextColor::White.to_raw();
		self.tooltips = vec![];
	}

	#[inline]
	pub fn draw_tooltips(&mut self) {
		use core::fmt::Write;

		for (text, pos, no_tooltip_repositioning, color) in core::mem::replace(&mut self.tooltips, vec![]) {
			self.color = color;

			let (mut x, y) = pos.into();
			let mut y = y + Self::CHAR_HEIGHT;
			let text_width = text.iter().map(|x| x.width()).max().unwrap_or(0);
			if !no_tooltip_repositioning && x + text_width + 6 > self.window_width() {
				x = usize::max(x.saturating_sub(text_width + 30), 4)
			}
			if !no_tooltip_repositioning && y + text.len() * 16 + 9 > self.window_height() {
				y = self.window_height().saturating_sub(text.len() * 16 + 9);
			}
			self.text_z = TOOLTIP_Z;
			self.text_coords = (x + 3, y + 3);
			self.draw_texture_z((x, y), TOOLTIP_Z, TOOLTIP_UV, (3, 3));
			let mut max = x + 3;
			for line in text.iter() {
				let _ = write!(self, "{line}");
				max = max.max(self.text_coords.0);
				self.text_coords.0 = x + 3;
				self.text_coords.1 += Self::CHAR_HEIGHT;
			}
			let width = max - 3 - x;
			let height = self.text_coords.1 - 3 - y;
			self.draw_texture_region_z(
				(x + 3, y),
				TOOLTIP_Z,
				TOOLTIP_UV + (3, 0),
				(width, 3),
				(10, 3),
			);
			self.draw_texture_z((x + width + 3, y), TOOLTIP_Z, TOOLTIP_UV + (13, 0), (3, 3));

			self.draw_texture_z((x, y + height + 3), TOOLTIP_Z, TOOLTIP_UV + (0, 13), (3, 3));
			self.draw_texture_region_z(
				(x + 3, y + height + 3),
				TOOLTIP_Z,
				TOOLTIP_UV + (3, 13),
				(width, 3),
				(10, 3),
			);
			self.draw_texture_z(
				(x + width + 3, y + height + 3),
				TOOLTIP_Z,
				TOOLTIP_UV + (13, 13),
				(3, 3),
			);
			self.draw_texture_region_z(
				(x, y + 3),
				TOOLTIP_Z,
				TOOLTIP_UV + (0, 3),
				(3, height),
				(3, 10),
			);
			self.draw_texture_region_z(
				(x + width + 3, y + 3),
				TOOLTIP_Z,
				TOOLTIP_UV + (13, 3),
				(3, height),
				(3, 10),
			);

			self.draw_texture_region_z(
				(x + 3, y + 3),
				TOOLTIP_Z,
				TOOLTIP_UV + (3, 3),
				(width, height),
				(10, 10),
			);
		}
	}

	#[inline]
	pub fn draw_unicode_z_color(&mut self, x: usize, y: usize, z: ZOffset, char: u16, color: u32) {
		unsafe {
			if unlikely(self.text_vertices.capacity() - self.text_vertices.len() < 16) {
				self.extend_text_buffers();
			}
			let x = (x as isize - self.horizontal_scroll as isize) as f32 * self.scale;
			let y = y as f32 * self.scale;
			let z_and_color = f32::from_bits(((255 - z as u8) as u32) | (color << 8));
			let char = f32::from_bits(char as u32);

			let x0 = x.mul_add(self.two_over_width, -1.0);
			let x1 = self.two_over_width.mul_add(Self::CHAR_HEIGHT as f32 * self.scale, x0);
			let y1 = y.mul_add(self.negative_two_over_height, 1.0);
			let y0 = self.negative_two_over_height.mul_add(Self::CHAR_HEIGHT as f32 * self.scale, y1);

			let len = self.text_vertices_len;
			let vec = &mut self.text_vertices;

			let vertices_len = vec.len();
			let ptr = vec.as_mut_ptr().add(vertices_len);
			// top left, 0 -> 1.0, 0.0
			*ptr = x1;
			*(ptr.add(1)) = y1;
			*(ptr.add(2)) = z_and_color;
			*(ptr.add(3)) = char;

			// top right, 1 -> 0.0, 0.0
			*(ptr.add(4)) = x0;
			*(ptr.add(5)) = y1;
			*(ptr.add(6)) = z_and_color;
			*(ptr.add(7)) = char;

			// bottom left, 2 -> 0.0, 1.0
			*(ptr.add(8)) = x0;
			*(ptr.add(9)) = y0;
			*(ptr.add(10)) = z_and_color;
			*(ptr.add(11)) = char;

			// bottom right, 3 -> 1.0, 1.0
			*(ptr.add(12)) = x1;
			*(ptr.add(13)) = y0;
			*(ptr.add(14)) = z_and_color;
			*(ptr.add(15)) = char;

			vec.set_len(vertices_len + 16);

			let indices_len = self.text_indices.len();
			let ptr = self.text_indices.as_mut_ptr().add(indices_len);

			*ptr = len;
			*(ptr.add(1)) = len + 1;
			*(ptr.add(2)) = len + 2;
			*(ptr.add(3)) = len;
			*(ptr.add(4)) = len + 2;
			*(ptr.add(5)) = len + 3;

			self.text_indices.set_len(indices_len + 6);

			self.text_vertices_len += 4;
		}
	}

	#[cold]
	pub fn extend_text_buffers(&mut self) {
		self.text_vertices.reserve_exact(98304);
		self.text_indices.reserve_exact(36864);
	}

	#[inline]
	pub fn window_height(&self) -> usize { (self.window_height / self.scale) as usize }

	#[inline]
	pub fn window_width(&self) -> usize { (self.window_width / self.scale) as usize }

	#[inline]
	pub fn vertices(&self) -> &[u8] { unsafe { core::slice::from_raw_parts(self.vertices.as_ptr().cast::<u8>(), self.vertices.len() * 4) } }

	#[inline]
	pub fn indices(&self) -> &[u8] { unsafe { core::slice::from_raw_parts(self.indices.as_ptr().cast::<u8>(), self.indices.len() * 4) } }

	#[inline]
	pub fn text_vertices(&self) -> &[u8] {
		unsafe {
			core::slice::from_raw_parts(
				self.text_vertices.as_ptr().cast::<u8>(),
				self.text_vertices.len() * 4,
			)
		}
	}

	#[inline]
	pub fn text_indices(&self) -> &[u8] {
		unsafe {
			core::slice::from_raw_parts(
				self.text_indices.as_ptr().cast::<u8>(),
				self.text_indices.len() * 4,
			)
		}
	}

	#[inline]
	pub fn indices_len(&self) -> u32 { self.indices.len() as u32 }

	#[inline]
	pub fn text_indices_len(&self) -> u32 { self.text_indices.len() as u32 }

	#[inline]
	pub fn draw_texture(&mut self, pos: impl Into<(usize, usize)>, uv: impl Into<(usize, usize)>, dims: impl Into<(usize, usize)>) { self.draw_texture_z(pos, BASE_Z, uv, dims); }

	#[inline]
	pub fn draw_texture_z(&mut self, pos: impl Into<(usize, usize)>, z: ZOffset, uv: impl Into<(usize, usize)>, dims: impl Into<(usize, usize)>) {
		let dims = dims.into();
		self.draw_texture_region_z(pos, z, uv, dims, dims);
	}

	#[allow(clippy::many_single_char_names)]
	pub fn draw_texture_region_z(&mut self, pos: impl Into<(usize, usize)>, z: ZOffset, uv: impl Into<(usize, usize)>, dims: impl Into<(usize, usize)>, uv_dims: impl Into<(usize, usize)>) {
		unsafe {
			let pos = pos.into();
			let uv = uv.into();
			let dims = dims.into();
			let uv_dims = uv_dims.into();
			let x = (pos.0 as isize - self.horizontal_scroll as isize) as f32 * self.scale;
			let y = pos.1 as f32 * self.scale;
			let z = 1.0 - z as u8 as f32 / 256.0;
			let u = uv.0 as f32;
			let v = uv.1 as f32;
			let width = dims.0 as f32 * self.scale;
			let height = dims.1 as f32 * self.scale;
			let uv_width = uv_dims.0 as f32;
			let uv_height = uv_dims.1 as f32;

			let x0 = self.two_over_width.mul_add(x, -1.0);
			let y1 = self.negative_two_over_height.mul_add(y, 1.0);
			let u0 = u.next_up();
			let v0 = v.next_up();
			let x1 = self.two_over_width.mul_add(width, x0);
			let y0 = self.negative_two_over_height.mul_add(height, y1);
			let u1 = u + uv_width.next_down();
			let v1 = v + uv_height.next_down();

			let len = self.vertices_len;
			let vec = &mut self.vertices;

			let vertices_len = vec.len();
			let ptr = vec.as_mut_ptr().add(vertices_len);
			// top left
			*ptr = x1;
			*(ptr.add(1)) = y1;
			*(ptr.add(2)) = z;
			*(ptr.add(3)) = u1;
			*(ptr.add(4)) = v0;
			// top right
			*(ptr.add(5)) = x0;
			*(ptr.add(6)) = y1;
			*(ptr.add(7)) = z;
			*(ptr.add(8)) = u0;
			*(ptr.add(9)) = v0;
			// bottom left
			*(ptr.add(10)) = x0;
			*(ptr.add(11)) = y0;
			*(ptr.add(12)) = z;
			*(ptr.add(13)) = u0;
			*(ptr.add(14)) = v1;
			// bottom right
			*(ptr.add(15)) = x1;
			*(ptr.add(16)) = y0;
			*(ptr.add(17)) = z;
			*(ptr.add(18)) = u1;
			*(ptr.add(19)) = v1;

			vec.set_len(vertices_len + 20);

			let indices_len = self.indices.len();
			let ptr = self.indices.as_mut_ptr().add(indices_len).cast::<u8>();

			*ptr = len as u8;
			*(ptr.add(1)) = (len >> 8) as u8;
			*(ptr.add(2)) = (len + 1) as u8;
			*(ptr.add(3)) = ((len + 1) >> 8) as u8;
			*(ptr.add(4)) = (len + 2) as u8;
			*(ptr.add(5)) = ((len + 2) >> 8) as u8;
			*(ptr.add(6)) = *ptr;
			*(ptr.add(7)) = *(ptr.add(1));
			*(ptr.add(8)) = *(ptr.add(4));
			*(ptr.add(9)) = *(ptr.add(5));
			*(ptr.add(10)) = (len + 3) as u8;
			*(ptr.add(11)) = ((len + 3) >> 8) as u8;

			self.indices.set_len(indices_len + 6);

			self.vertices_len += 4;
		}
	}
}

#[derive(Copy, Clone, Eq)]
pub struct Vec2u {
	pub x: usize,
	pub y: usize,
}

impl Debug for Vec2u {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "({x},{y})", x = self.x, y = self.y)
	}
}

impl Vec2u {
	pub const fn new(x: usize, y: usize) -> Self { Self { x, y } }

	pub const fn wrapping_sub(self, rhs: Self) -> Self {
		Self {
			x: self.x.wrapping_sub(rhs.x),
			y: self.y.wrapping_sub(rhs.y),
		}
	}

	pub const fn saturating_sub(self, rhs: Self) -> Self {
		Self {
			x: self.x.saturating_sub(rhs.x),
			y: self.y.saturating_sub(rhs.y),
		}
	}
}

impl<T: Into<(usize, usize)> + Clone> PartialEq<T> for Vec2u {
	fn eq(&self, other: &T) -> bool {
		let (x, y) = other.clone().into();
		(self.x == x) & (self.y == y)
	}
}

impl From<(usize, usize)> for Vec2u {
	fn from(value: (usize, usize)) -> Self {
		let (x, y) = value;
		Self::new(x, y)
	}
}

impl From<Vec2u> for (usize, usize) {
	fn from(val: Vec2u) -> Self { (val.x, val.y) }
}

impl<T: Into<(usize, usize)>> std::ops::Add<T> for Vec2u {
	type Output = Self;

	fn add(self, rhs: T) -> Self::Output {
		let (x, y) = rhs.into();
		Self {
			x: self.x + x,
			y: self.y + y,
		}
	}
}

impl<T: Into<(usize, usize)>> std::ops::AddAssign<T> for Vec2u {
	fn add_assign(&mut self, rhs: T) {
		let (x, y) = rhs.into();
		self.x += x;
		self.y += y;
	}
}

impl<T: Into<(usize, usize)>> std::ops::Sub<T> for Vec2u {
	type Output = Self;

	fn sub(self, rhs: T) -> Self::Output {
		let (x, y) = rhs.into();
		Self {
			x: self.x - x,
			y: self.y - y,
		}
	}
}

impl<T: Into<(usize, usize)>> std::ops::SubAssign<T> for Vec2u {
	fn sub_assign(&mut self, rhs: T) {
		let (x, y) = rhs.into();
		self.x = self.x.wrapping_sub(x);
		self.y = self.x.wrapping_sub(y);
	}
}
