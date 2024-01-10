use std::time::Instant;
use crate::assets::{ALERT_TEXT_Z, ALERT_UV, ALERT_Z, HEADER_SIZE};
use crate::{smoothstep64, StrExt};
use crate::color::TextColor;
use crate::vertex_buffer_builder::{Vec2u, VertexBufferBuilder};

pub struct Alert {
    timestamp: Option<Instant>,
    title: String,
    title_color: u32,
    message: String,
    width: usize,
}

impl Alert {
    pub fn new(title: impl Into<String>, title_color: TextColor, message: impl Into<String>) -> Self {
        let title = title.into();
        let message = message.into();
        Self {
            timestamp: None,
            width: usize::max(message.width(), title.width()),
            title,
            title_color: title_color.to_raw(),
            message,
        }
    }

    pub fn render(&mut self, builder: &mut VertexBufferBuilder, idx: usize) {
        use core::fmt::Write;

        let pos = Vec2u::new(builder.window_width() - (self.width + 24) + self.get_inset(), idx * 40 + HEADER_SIZE);
        builder.draw_texture_z(pos, ALERT_Z, ALERT_UV, (16, 40));
        builder.draw_texture_region_z(pos + (12, 4), ALERT_Z, ALERT_UV + (12, 4), (self.width + 8, 32), (24, 32));
        builder.draw_texture_z(pos + (self.width + 20, 0), ALERT_Z, ALERT_UV + (36, 0), (4, 40));
        let mut remaining_width = self.width + 16;
        while remaining_width > 0 {
            builder.draw_texture_z(pos + (self.width + 20 - remaining_width, 0), ALERT_Z, ALERT_UV + (2, 0), (36.min(remaining_width), 4));
            builder.draw_texture_z(pos + (self.width + 20 - remaining_width, 36), ALERT_Z, ALERT_UV + (2, 36), (36.min(remaining_width), 4));
            remaining_width = remaining_width.saturating_sub(36);
        }
        builder.settings(pos + (18, 4), true, ALERT_TEXT_Z);
        builder.color = self.title_color;
        let _ = write!(builder, "{}", self.title);
        builder.color = TextColor::White.to_raw();
        builder.settings(pos + (18, 20), true, ALERT_TEXT_Z);
        let _ = write!(builder, "{}", self.message);
    }

    pub fn is_invisible(&mut self) -> bool {
        let ms = Instant::now().duration_since(*self.timestamp.get_or_insert(Instant::now())).as_millis() as usize;
        let display_time = (self.message.len() + self.title.len()) * 200 / 3 + 5000;
        ms > 500 + display_time
    }

    fn get_inset(&mut self) -> usize {
        let mut ms = Instant::now().duration_since(*self.timestamp.get_or_insert(Instant::now())).as_millis() as usize;
        let width = self.width + 24;
        let display_time = (self.message.len() + self.title.len()) * 200 / 3 + 5000;
        if ms < 250 {
            return (smoothstep64((250 - ms) as f64 / 250.0) * width as f64) as usize
        }
        ms -= 250;
        if ms < display_time {
            0
        } else {
            (smoothstep64((ms - display_time) as f64 / 250.0) * width as f64) as usize
        }
    }
}
