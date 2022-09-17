use winit::dpi::PhysicalSize;

use crate::assets;

pub struct VertexBufferBuilder {
    vertices: Vec<u8>,
    indices: Vec<u8>,
    text_vertices: Vec<u8>,
    text_indices: Vec<u8>,
    vertices_len: u32,
    text_vertices_len: u32,
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
            text_vertices: Vec::with_capacity(393216),
            text_indices: Vec::with_capacity(131072),
            vertices_len: 0,
            text_vertices_len: 0,
            window_width: size.width as f32,
            window_height: size.height as f32,
            texture_width: texture_width as f32,
            texture_height: texture_height as f32,
            scroll
        }
    }

    pub fn width(str: &str) -> u32 {
        str.chars().map(|x| if x as u32 <= 0xFFFF {
            VertexBufferBuilder::furthest_pixel(x as u16)
        } else { panic!() }).sum()
    }

    #[inline]
    pub fn scroll(&self) -> u32 {
        self.scroll
    }

    #[inline]
    fn furthest_pixel(char: u16) -> u32 {
        for x_pixel in (0..16u32).rev() {
            for y_pixel in (0..16u32).rev() {
                if ((assets::UNICODE[(char as u32 * 32 + y_pixel * 2 + x_pixel / 8) as usize] >> (7 - x_pixel % 8)) & 1) == 1 {
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

    pub fn draw_text_z(&mut self, x: u32, y: u32, z: f32, text: &str, dropshadow: bool) -> u32 {
        text.chars().fold(0, |offset, char| offset + if char as u32 <= 0xFFFF { self.draw_char(char as u16, x + offset, y, z, dropshadow) } else { panic!() })
    }

    #[inline]
    fn draw_char(&mut self, c: u16, x: u32, y: u32, z: f32, dropshadow: bool) -> u32 {
        if dropshadow {
            self.draw_unicode_z_color(x + 1, y + 1, z, c, 0xD);
        }
        self.draw_unicode_z_color(x, y, z, c, 0xFF);
        VertexBufferBuilder::furthest_pixel(c)
    }

    pub fn draw_unicode_z_color(&mut self, x: u32, y: u32, z: f32, char: u16, color: u8) {
        unsafe {
            let x = x as f32;
            let y = y as f32;
            let z = z;
            let char = *(&((char as u32) | ((color as u32) << 24)) as *const u32 as *const f32);

            let x0 = ((x / self.window_width) * 2.0f32) - 1.0f32;
            let x1 = (((x + 16.0) / self.window_width) * 2.0f32) - 1.0; // todo, optimize-able
            let y0 = -((((y + 16.0) / self.window_height) * 2.0) - 1.0);
            let y1 = -(((y / self.window_height) * 2.0) - 1.0); // todo, optimize-able

            let len = self.text_vertices_len;
            let vec = &mut self.text_vertices;

            let vertices_len = vec.len();
            let ptr = vec.as_mut_ptr().add(vertices_len) as *mut f32;

            *ptr                = x1;
            *(ptr.add(1)) = y1;
            *(ptr.add(2)) = z;
            *(ptr.add(3)) = char;
            *(ptr.add(4)) = 1.0;
            *(ptr.add(5)) = 0.0;
            // top right
            *(ptr.add(6)) = x0;
            *(ptr.add(7)) = y1;
            *(ptr.add(8)) = z;
            *(ptr.add(9)) = char;
            *(ptr.add(10)) = 0.0;
            *(ptr.add(11)) = 0.0;
            // bottom left
            *(ptr.add(12)) = x0;
            *(ptr.add(13)) = y0;
            *(ptr.add(14)) = z;
            *(ptr.add(15)) = char;
            *(ptr.add(16)) = 0.0;
            *(ptr.add(17)) = 1.0;
            // bottom right
            *(ptr.add(18)) = x1;
            *(ptr.add(19)) = y0;
            *(ptr.add(20)) = z;
            *(ptr.add(21)) = char;
            *(ptr.add(22)) = 1.0;
            *(ptr.add(23)) = 1.0;

            vec.set_len(vertices_len + 96);

            let indices_len = self.text_indices.len();
            let ptr = self.text_indices.as_mut_ptr().add(indices_len);

            *ptr                 =   len            as u8;
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

            self.text_indices.set_len(indices_len + 12);

            self.text_vertices_len += 4;
        }
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
    pub fn text_vertices(&self) -> &[u8] {
        &self.text_vertices
    }

    #[inline]
    pub fn text_indices(&self) -> &[u8] {
        &self.text_indices
    }

    #[inline]
    pub fn indices_len(&self) -> u32 {
        (self.indices.len() >> 1) as u32
    }

    #[inline]
    pub fn text_indices_len(&self) -> u32 {
        (self.text_indices.len() >> 1) as u32
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
    pub fn draw_texture_stretched(&mut self, x: u32, y: u32, u: u32, v: u32, width: u32, height: u32, x_scale: f32, y_scale: f32) {
        self.draw_texture_stretched_z(x, y, 0.0, u, v, width, height, x_scale, y_scale);
    }

    #[inline]
    pub fn draw_texture_stretched_z(&mut self, x: u32, y: u32, z: f32, u: u32, v: u32, width: u32, height: u32, x_scale: f32, y_scale: f32) {
        unsafe {
            let x = x as f32;
            let y = y as f32;
            let u = u as f32;
            let v = v as f32;
            let width = width as f32;
            let height = height as f32;

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
            *ptr                = x1;
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

            vec.set_len(vertices_len + 80);

            let indices_len = self.indices.len();
            let ptr = self.indices.as_mut_ptr().add(indices_len);

            *ptr                 =   len            as u8;
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
