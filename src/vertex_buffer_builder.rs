use winit::dpi::PhysicalSize;
use crate::assets;

pub struct VertexBufferBuilder {
    vertices: Vec<u8>,
    indices: Vec<u8>,
    vertices_len: u32,
    window_width: f32,
    window_height: f32,
    texture_width: f32,
    texture_height: f32,
    scroll: u32
}

impl VertexBufferBuilder {
    pub fn new(size: &PhysicalSize<u32>, texture_width: u32, texture_height: u32, scroll: u32) -> VertexBufferBuilder {
        VertexBufferBuilder {
            vertices: Vec::with_capacity(393216),
            indices: Vec::with_capacity(131072),
            vertices_len: 0,
            window_width: size.width as f32,
            window_height: size.height as f32,
            texture_width: texture_width as f32,
            texture_height: texture_height as f32,
            scroll
        }
    }

    pub fn width(str: &str) -> u32 {
        str.bytes().map(|x| {
            let file_x = (x as u32 % 16) * 8 + 128;
            let file_y = (x as u32 / 16) * 8 + 128;
            VertexBufferBuilder::furthest_pixel(file_x, file_y)
        }).sum()
    }

    #[inline]
    pub fn scroll(&self) -> u32 {
        self.scroll
    }

    #[inline]
    fn furthest_pixel(file_x: u32, file_y: u32) -> u32 {
        for x_pixel in (0..8u32).rev() {
            for y_pixel in (0..8u32).rev() {
                if assets::ATLAS[((file_x + x_pixel) + (file_y + y_pixel) * assets::ATLAS_WIDTH) as usize * 4 + 3] > 0 {
                    return x_pixel + 2;
                }
            }
        }
        5 // space
    }

    #[inline]
    pub fn draw_text(&mut self, x: u32, y: u32, text: &str, dropshadow: bool) -> u32 {
        self.draw_text_z(x, y, 0.0, text, dropshadow)
    }

    #[inline]
    pub fn draw_text_color(&mut self, x: u32, y: u32, text: &str, dropshadow: bool, color: u32) -> u32 {
        self.draw_text_z_color(x, y, 0.0, text, dropshadow, color)
    }

    #[inline]
    pub fn draw_text_z(&mut self, x: u32, y: u32, z: f32, text: &str, dropshadow: bool) -> u32 {
        self.draw_text_z_color(x, y, z, text, dropshadow, 0xFFFFFF_FF)
    }

    pub fn draw_text_z_color(&mut self, x: u32, y: u32, z: f32, text: &str, dropshadow: bool, color: u32) -> u32 {
        let mut offset = 0;
        let bytes = text.as_bytes();
        for byte in bytes {
            let byte = *byte;
            offset += self.draw_char(byte, x + offset, y, z, dropshadow, color);
        }
        offset
    }

    fn draw_char(&mut self, c: u8, x: u32, y: u32, z: f32, dropshadow: bool, color: u32) -> u32 {
        let file_x = (c as u32 % 16) * 8 + 128;
        let file_y = (c as u32 / 16) * 8 + 128;
        if dropshadow {
            self.draw_texture_z_color(x + 1, y + 1, z, file_x, file_y, 8, 8, VertexBufferBuilder::dropshadow(color));
        }
        self.draw_texture_z_color(x, y, z, file_x, file_y, 8, 8, color);
        VertexBufferBuilder::furthest_pixel(file_x, file_y)
    }

    fn dropshadow(color: u32) -> u32 {
        const MULTIPLIER: f32 = 0.247f32;
          (((((color >> 24) & 0xFF) as f32 * MULTIPLIER + 0.5f32) as u32) << 24)
        | (((((color >> 16) & 0xFF) as f32 * MULTIPLIER + 0.5f32) as u32) << 16)
        | (((((color >>  8) & 0xFF) as f32 * MULTIPLIER + 0.5f32) as u32) <<  8)
        | (   color        & 0xFF)
    }

    #[inline]
    pub fn window_height(&self) -> u32 {
        self.window_height as u32
    }

    #[inline]
    pub fn window_width(&self) -> u32 {
        self.window_width as u32
    }

    #[inline]
    pub fn vertices(&self) -> &[u8] {
        &self.vertices
    }

    #[inline]
    pub fn indices(&self) -> &[u8] {
        &self.indices
    }

    #[inline]
    pub fn indices_len(&self) -> u32 {
        (self.indices.len() >> 1) as u32
    }

    #[inline]
    pub fn draw_texture(&mut self, x: u32, y: u32, u: u32, v: u32, width: u32, height: u32) {
        self.draw_texture_stretched(x, y, u, v, width, height, 1f32, 1f32);
    }

    #[inline]
    pub fn draw_texture_z(&mut self, x: u32, y: u32, z: f32, u: u32, v: u32, width: u32, height: u32) {
        self.draw_texture_stretched_z(x, y, z, u, v, width, height, 1f32, 1f32);
    }
    #[inline]
    pub fn draw_texture_z_color(&mut self, x: u32, y: u32, z: f32, u: u32, v: u32, width: u32, height: u32, color: u32) {
        self.draw_texture_stretched_z_color(x, y, z, u, v, width, height, 1f32, 1f32, color);
    }

    #[inline]
    pub fn draw_texture_stretched(&mut self, x: u32, y: u32, u: u32, v: u32, width: u32, height: u32, x_scale: f32, y_scale: f32) {
        self.draw_texture_stretched_z(x, y, 0.0, u, v, width, height, x_scale, y_scale);
    }

    #[inline]
    pub fn draw_texture_stretched_z(&mut self, x: u32, y: u32, z: f32, u: u32, v: u32, width: u32, height: u32, x_scale: f32, y_scale: f32) {
        self.draw_texture_stretched_z_color(x, y, z, u, v, width, height, x_scale, y_scale, 0xFFFFFFFF);
    }

    pub fn draw_texture_stretched_z_color(&mut self, x: u32, y: u32, z: f32, u: u32, v: u32, width: u32, height: u32, x_scale: f32, y_scale: f32, color: u32) {
        unsafe {
            let x = x as f32;
            let y = y as f32;
            let u = u as f32;
            let v = v as f32;
            let width = width as f32;
            let height = height as f32;
            let c = *(&color as *const u32 as *const f32);

            let x0 = ((x / self.window_width) * 2.0f32) - 1.0f32;
            let x1 = (((x + (width * x_scale)) / self.window_width) * 2.0f32) - 1.0;
            let y0 = -((((y + (height * y_scale)) / self.window_height) * 2.0) - 1.0);
            let y1 = -(((y / self.window_height) * 2.0) - 1.0);
            let u0 = u / self.texture_width;
            let u1 = (u + width) / self.texture_width;
            let v0 = v / self.texture_height;
            let v1 = (v + height) / self.texture_height;
            let z = z;

            let len = self.vertices_len;
            let vec = &mut self.vertices;

            let vertices_len = vec.len();
            let ptr = vec.as_mut_ptr().add(vertices_len) as *mut f32;
            // top left
            *(ptr)              = x1;
            *(ptr.add(1)) = y1;
            *(ptr.add(2)) = z;
            *(ptr.add(3)) = u1;
            *(ptr.add(4)) = v0;
            *(ptr.add(5)) = c;
            // top right
            *(ptr.add(6)) = x0;
            *(ptr.add(7)) = y1;
            *(ptr.add(8)) = z;
            *(ptr.add(9)) = u0;
            *(ptr.add(10)) = v0;
            *(ptr.add(11)) = c;
            // bottom left
            *(ptr.add(12)) = x0;
            *(ptr.add(13)) = y0;
            *(ptr.add(14)) = z;
            *(ptr.add(15)) = u0;
            *(ptr.add(16)) = v1;
            *(ptr.add(17)) = c;
            // bottom right
            *(ptr.add(18)) = x1;
            *(ptr.add(19)) = y0;
            *(ptr.add(20)) = z;
            *(ptr.add(21)) = u1;
            *(ptr.add(22)) = v1;
            *(ptr.add(23)) = c;

            vec.set_len(vertices_len + 96);

            let indices_len = self.indices.len();
            let ptr = self.indices.as_mut_ptr().add(indices_len);

            *(ptr)               =   len            as u8;
            *(ptr.add(1))  =  (len >> 8)      as u8;
            *(ptr.add(2))  =  (len + 1)       as u8;
            *(ptr.add(3))  = ((len + 1) >> 8) as u8;
            *(ptr.add(4))  =  (len + 2)       as u8;
            *(ptr.add(5))  = ((len + 2) >> 8) as u8;
            *(ptr.add(6))  = *ptr;
            *(ptr.add(7))  = *(ptr.add(1));
            *(ptr.add(8))  = *(ptr.add(4));
            *(ptr.add(9))  = *(ptr.add(5));
            *(ptr.add(10)) =  (len + 3)       as u8;
            *(ptr.add(11)) = ((len + 3) >> 8) as u8;

            self.indices.set_len(indices_len + 12);

            self.vertices_len += 4;
        }
    }
}
