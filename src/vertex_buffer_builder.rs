use std::ops::BitAnd;
use winit::dpi::PhysicalSize;
use crate::assets::TOOLTIP_UV;

pub struct VertexBufferBuilder {
	vertices: Vec<f32>,
	indices: Vec<u16>,
	text_vertices: Vec<f32>,
	text_indices: Vec<u32>,
	vertices_len: u32,
	text_vertices_len: u32,
	window_width: f32,
	window_height: f32,
	recip_texture_width: f32,
	recip_texture_height: f32,
	scroll: usize,
	pub horizontal_scroll: usize,
	pub text_coords: (usize, usize),
	dropshadow: bool,
	text_z: u8,
	pub color: u32,
	two_over_width: f32,
	negative_two_over_height: f32,
	drew_tooltip: bool,
}

impl core::fmt::Write for VertexBufferBuilder {
	fn write_str(&mut self, text: &str) -> std::fmt::Result {
		let (mut x, y) = self.text_coords;
		x += text
			.chars()
			.fold(0, |offset, char| offset + if (char as u32) < 56832 { self.draw_char(char as u16, x + offset, y, self.text_z) } else { 0 });
		self.text_coords = (x, y);
		Ok(())
	}

	fn write_char(&mut self, c: char) -> std::fmt::Result {
		if (c as u32) < 56832 {
			self.text_coords.0 += self.draw_char(c as u16, self.text_coords.0, self.text_coords.1, self.text_z);
		}
		Ok(())
	}
}

impl VertexBufferBuilder {
	pub const CHAR_WIDTH: &'static [u8] = include_bytes!("assets/char_widths.hex");

	pub fn new(size: PhysicalSize<u32>, texture_width: usize, texture_height: usize, scroll: usize) -> Self {
		Self {
			vertices: Vec::with_capacity(98304),
			indices: Vec::with_capacity(65536),
			text_vertices: Vec::with_capacity(98304),
			text_indices: Vec::with_capacity(65536),
			vertices_len: 0,
			text_vertices_len: 0,
			window_width: size.width as f32,
			window_height: size.height as f32,
			recip_texture_width: (texture_width as f32).recip(),
			recip_texture_height: (texture_height as f32).recip(),
			scroll,
			horizontal_scroll: 0,
			text_coords: (0, 0),
			dropshadow: false,
			text_z: 1,
			color: 0xFFFFFF,
			two_over_width: 2.0 / size.width as f32,
			negative_two_over_height: -2.0 / size.height as f32,
			drew_tooltip: false,
		}
	}

	#[inline]
	pub const fn scroll(&self) -> usize {
		self.scroll
	}
	
	#[inline]
	pub const fn drew_tooltip(&self) -> bool { self.drew_tooltip }

	#[inline]
	pub fn settings(&mut self, pos: impl Into<(usize, usize)>, dropshadow: bool, z: u8) {
		self.text_coords = pos.into();
		self.dropshadow = dropshadow;
		self.text_z = z;
	}

	#[inline]
	fn draw_char(&mut self, c: u16, x: usize, y: usize, z: u8) -> usize {
		if self.dropshadow {
			self.draw_unicode_z_color(x + 1, y + 1, z, c, {
				self.color.wrapping_shr(16).bitand(0xFF).wrapping_mul(21).wrapping_div(85).wrapping_shl(16)
					| self.color.wrapping_shr(8).bitand(0xFF).wrapping_mul(21).wrapping_div(85).wrapping_shl(8)
					| self.color.wrapping_shr(0).bitand(0xFF).wrapping_mul(21).wrapping_div(85).wrapping_shl(0)
			});
		}
		self.draw_unicode_z_color(x, y, z, c, self.color & 0xFFFFFF);
		Self::CHAR_WIDTH[c as usize] as usize
	}

	#[inline]
	pub fn draw_tooltip(&mut self, text: &[&str], x: usize, y: usize) {
		use core::fmt::Write;

		let old_text_z = core::mem::replace(&mut self.text_z, 192);
		let old_text_coords = core::mem::replace(&mut self.text_coords, (x + 3, y + 3));
		self.draw_texture_z((x, y), 0.6, TOOLTIP_UV, (3, 3));
		let mut max = x + 3;
		for &line in text {
			let _ = write!(self, "{line}");
			max = max.max(self.text_coords.0);
			self.text_coords.0 = x + 3;
			self.text_coords.1 += 16;
		}
		let width = max - 3 - x;
		let height = self.text_coords.1 - 3 - y;
		self.draw_texture_region_z((x + 3, y), 0.6, TOOLTIP_UV + (3, 0), (width, 3), (10, 3));
		self.draw_texture_z((x + width + 3, y), 0.6, TOOLTIP_UV + (13, 0), (3, 3));

		self.draw_texture_z((x, y + height + 3), 0.6, TOOLTIP_UV + (0, 13), (3, 3));
		self.draw_texture_region_z((x + 3, y + height + 3), 0.6, TOOLTIP_UV + (3, 13), (width, 3), (10, 3));
		self.draw_texture_z((x + width + 3, y + height + 3), 0.6, TOOLTIP_UV + (13, 13), (3, 3));

		self.draw_texture_region_z((x, y + 3), 0.6, TOOLTIP_UV + (0, 3), (3, height), (3, 10));
		self.draw_texture_region_z((x + width + 3, y + 3), 0.6, TOOLTIP_UV + (13, 3), (3, height), (3, 10));

		self.draw_texture_region_z((x + 3, y + 3), 0.6, TOOLTIP_UV + (3, 3), (width, height), (10, 10));

		self.text_z = old_text_z;
		self.text_coords = old_text_coords;
		self.drew_tooltip = true;
	}

	// todo, handwritten simd
	#[inline]
	pub fn draw_unicode_z_color(&mut self, x: usize, y: usize, z: u8, char: u16, color: u32) {
		unsafe {
			if self.text_vertices.capacity() - self.text_vertices.len() < 16 {
				self.text_vertices.reserve_exact(98304);
				self.text_indices.reserve_exact(36864);
			}
			let x = (x as isize - self.horizontal_scroll as isize) as f32;
			let y = y as f32;
			let z_and_color = f32::from_bits(((255 - z) as u32) | (color << 8));
			let char = f32::from_bits(char as u32);

			let x0 = x.mul_add(self.two_over_width, -1.0);
			let x1 = self.two_over_width.mul_add(16.0, x0);
			let y1 = y.mul_add(self.negative_two_over_height, 1.0);
			let y0 = self.negative_two_over_height.mul_add(16.0, y1);

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

	#[inline]
	pub const fn window_height(&self) -> usize {
		self.window_height as usize
	}

	#[inline]
	pub const fn window_width(&self) -> usize {
		self.window_width as usize
	}

	#[inline]
	pub fn vertices(&self) -> &[u8] {
		unsafe { core::slice::from_raw_parts(self.vertices.as_ptr().cast::<u8>(), self.vertices.len() * 4) }
	}

	#[inline]
	pub fn indices(&self) -> &[u8] {
		unsafe { core::slice::from_raw_parts(self.indices.as_ptr().cast::<u8>(), self.indices.len() * 4) }
	}

	#[inline]
	pub fn text_vertices(&self) -> &[u8] {
		unsafe { core::slice::from_raw_parts(self.text_vertices.as_ptr().cast::<u8>(), self.text_vertices.len() * 4) }
	}

	#[inline]
	pub fn text_indices(&self) -> &[u8] {
		unsafe { core::slice::from_raw_parts(self.text_indices.as_ptr().cast::<u8>(), self.text_indices.len() * 4) }
	}

	#[inline]
	pub fn indices_len(&self) -> u32 {
		self.indices.len() as u32
	}

	#[inline]
	pub fn text_indices_len(&self) -> u32 {
		self.text_indices.len() as u32
	}

	#[inline]
	pub fn draw_texture(&mut self, pos: impl Into<(usize, usize)>, uv: impl Into<(usize, usize)>, dims: impl Into<(usize, usize)>) {
		self.draw_texture_z(pos, 0.0, uv, dims);
	}

	#[inline]
	pub fn draw_texture_z(&mut self, pos: impl Into<(usize, usize)>, z: f32, uv: impl Into<(usize, usize)>, dims: impl Into<(usize, usize)>) {
		let dims = dims.into();
		self.draw_texture_region_z(pos, z, uv, dims, dims);
	}

	// todo, handwritten simd
	#[inline]
	#[allow(clippy::many_single_char_names)]
	pub fn draw_texture_region_z(&mut self, pos: impl Into<(usize, usize)>, z: f32, uv: impl Into<(usize, usize)>, dims: impl Into<(usize, usize)>, uv_dims: impl Into<(usize, usize)>) {
		unsafe {
			let pos = pos.into();
			let uv = uv.into();
			let dims = dims.into();
			let uv_dims = uv_dims.into();
			let x = (pos.0 as isize - self.horizontal_scroll as isize) as f32;
			let y = pos.1 as f32;
			let z = 1.0 - z;
			let u = uv.0 as f32;
			let v = uv.1 as f32;
			let width = dims.0 as f32;
			let height = dims.1 as f32;
			let uv_width = uv_dims.0 as f32;
			let uv_height = uv_dims.1 as f32;

			let x0 = self.two_over_width.mul_add(x, -1.0);
			let y1 = self.negative_two_over_height.mul_add(y, 1.0);
			let u0 = self.recip_texture_width * u;
			let v0 = self.recip_texture_height * v;
			let x1 = self.two_over_width.mul_add(width, x0);
			let y0 = self.negative_two_over_height.mul_add(height, y1);
			let u1 = self.recip_texture_width.mul_add(uv_width, u0);
			let v1 = self.recip_texture_height.mul_add(uv_height, v0);

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

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Vec2u {
	pub x: usize,
	pub y: usize,
}

impl Vec2u {
	pub const fn new(x: usize, y: usize) -> Self {
		Self { x, y }
	}

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

impl PartialEq<(usize, usize)> for Vec2u {
	fn eq(&self, other: &(usize, usize)) -> bool {
		let other = *other;
		(self.x == other.0) & (self.y == other.1)
	}
}

impl From<(usize, usize)> for Vec2u {
	fn from(value: (usize, usize)) -> Self {
		Self::new(value.0, value.1)
	}
}

impl From<Vec2u> for (usize, usize) {
	fn from(val: Vec2u) -> Self {
		(val.x, val.y)
	}
}

impl std::ops::Add<Self> for Vec2u {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		Self { x: self.x + rhs.x, y: self.y + rhs.y }
	}
}

impl std::ops::Add<(usize, usize)> for Vec2u {
	type Output = Self;

	fn add(self, rhs: (usize, usize)) -> Self::Output {
		Self { x: self.x + rhs.0, y: self.y + rhs.1 }
	}
}

impl std::ops::AddAssign<Self> for Vec2u {
	fn add_assign(&mut self, rhs: Self) {
		self.x += rhs.x;
		self.y += rhs.y;
	}
}

impl std::ops::Sub<Self> for Vec2u {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output {
		Self { x: self.x - rhs.x, y: self.y - rhs.y }
	}
}

impl std::ops::Sub<(usize, usize)> for Vec2u {
	type Output = Self;

	fn sub(self, rhs: (usize, usize)) -> Self::Output {
		Self { x: self.x - rhs.0, y: self.y - rhs.1 }
	}
}

impl std::ops::SubAssign<Self> for Vec2u {
	fn sub_assign(&mut self, rhs: Self) {
		self.x -= rhs.x;
		self.y -= rhs.y;
	}
}
