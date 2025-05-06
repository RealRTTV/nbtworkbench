use std::time::Duration;

use enum_map::Enum;

use crate::assets::{NOTIFICATION_BAR_BACKDROP_UV, NOTIFICATION_BAR_UV, NOTIFICATION_TEXT_Z, NOTIFICATION_UV, NOTIFICATION_Z};
use crate::render::{TextColor, VertexBufferBuilder};
use crate::util::{now, smoothstep64, split_lines, StrExt, Vec2u};

#[derive(Copy, Clone, Enum)]
pub enum NotificationKind {
    Scale,
    Find,
    Replace
}

pub struct Notification {
    timestamp: Option<Duration>,
    lines: Box<[String]>,
    message_len: usize,
    text_color: u32,
    width: usize,
    raw_message: String,
    kind: NotificationKind,
}

impl Notification {
    pub fn new(message: impl Into<String>, text_color: TextColor, kind: NotificationKind) -> Self {
        let message = message.into();
        let lines = split_lines::<256>(message.clone());
        Self {
            timestamp: None,
            width: lines.iter().map(|s| s.width()).max().unwrap_or(0),
            message_len: lines.iter().map(String::len).sum(),
            lines: lines.into_boxed_slice(),
            text_color: text_color.to_raw(),
            kind,
            raw_message: message,
        }
    }

    pub fn update(&mut self, notification: Notification) {
        let message = notification.raw_message;
        let text_color = notification.text_color;

        let old_display_time = self.message_len * 60 + 3000;
        let old_width = self.width;
        let lines = split_lines::<256>(message.clone());
        self.width = lines.iter().map(|s| s.width()).max().unwrap_or(0);
        self.message_len = lines.iter().map(String::len).sum();
        self.lines = lines.into_boxed_slice();
        self.text_color = text_color;
        self.raw_message = message;
        let now = now();
        if let Some(timestamp) = self.timestamp {
            let diff = (now - timestamp).as_millis();
            if diff <= 250 {
                // readjust based on amount previously out
                let old_px = ((250 - diff) as f64 * (old_width + 10) as f64) as usize;
                if self.width + 10 <= old_px {
                    // already as far out as ours is, so set to full width of new one
                    self.timestamp = Some(now.saturating_sub(Duration::from_millis(250)));
                } else {
                    let ms = 250.0 * (1.0 - (self.width + 10) as f64 / old_px as f64);
                    self.timestamp = Some(now.saturating_sub(Duration::from_nanos((ms * 1_000_000.0) as u64)))
                }
            } else if diff <= 250 + old_display_time as u128 {
                // reset the expiry clock
                self.timestamp = Some(now.saturating_sub(Duration::from_millis(250)));
            } else if diff <= 250 + old_display_time as u128 + 250 {
                // readjust based on amount previously out
                let old_px = ((diff - (250 + old_display_time as u128)) as f64 * (old_width + 10) as f64) as usize;
                if self.width + 10 <= old_px {
                    // already as far out as ours is, so set to full width of new one
                    self.timestamp = Some(now.saturating_sub(Duration::from_millis(250)));
                } else {
                    let ms = 250.0 * (1.0 - (self.width + 10) as f64 / old_px as f64);
                    self.timestamp = Some(now.saturating_sub(Duration::from_nanos((ms * 1_000_000.0) as u64)))
                }
            } else {
                // shouldn't normally happen, but we'll handle it in case
                self.timestamp = Some(now);
            }
        } else {
            self.timestamp = Some(now);
        }
    }

    pub fn render(&mut self, builder: &mut VertexBufferBuilder, y: usize) {
        use core::fmt::Write;

        let pos = Vec2u::new((builder.window_width() + self.get_inset()).saturating_sub(self.width + 10), y, );
        builder.draw_texture_region_z(
            pos + (2, 2),
            NOTIFICATION_Z,
            NOTIFICATION_UV + (6, 2),
            (self.width + 6, self.height() - 4),
            (12, 16),
        );
        builder.draw_texture_z(pos + (2, 2), NOTIFICATION_Z, NOTIFICATION_UV + (2, 2), (4, 16));
        builder.draw_texture_z(
            pos + (6 + self.width + 2, 0),
            NOTIFICATION_Z,
            NOTIFICATION_UV + (18, 0),
            (2, 2),
        );
        builder.draw_texture_z(
            pos + (6 + self.width + 2, self.height() - 2),
            NOTIFICATION_Z,
            NOTIFICATION_UV + (18, 18),
            (2, 2),
        );
        builder.draw_texture_z(
            pos + (0, self.height() - 2),
            NOTIFICATION_Z,
            NOTIFICATION_UV + (0, 18),
            (2, 2),
        );
        builder.draw_texture_z(
            pos + (0, 0),
            NOTIFICATION_Z,
            NOTIFICATION_UV,
            (2, 2),
        );
        {
            let mut remaining_width = self.width + 6;
            while remaining_width > 0 {
                builder.draw_texture_z(
                    pos + (6 + self.width + 2 - remaining_width, 0),
                    NOTIFICATION_Z,
                    NOTIFICATION_UV + (2, 0),
                    (16.min(remaining_width), 2),
                );
                builder.draw_texture_z(
                    pos + (6 + self.width + 2 - remaining_width, self.height() - 2),
                    NOTIFICATION_Z,
                    NOTIFICATION_UV + (2, 18),
                    (16.min(remaining_width), 2),
                );
                remaining_width = remaining_width.saturating_sub(16);
            }
        }
        {
            let mut remaining_height = self.height() - 4;
            while remaining_height > 0 {
                builder.draw_texture_z(
                    pos + (0, self.height() - 4 - remaining_height + 2),
                    NOTIFICATION_Z,
                    NOTIFICATION_UV + (0, 2),
                    (2, 16.min(remaining_height))
                );
                builder.draw_texture_z(
                    pos + (6 + self.width + 2, self.height() - 4 - remaining_height + 2),
                    NOTIFICATION_Z,
                    NOTIFICATION_UV + (18, 2),
                    (2, 16.min(remaining_height))
                );
                remaining_height = remaining_height.saturating_sub(16);
            }
        }
        builder.color = self.text_color;
        for (idx, line) in self.lines.iter().enumerate() {
            builder.settings(pos + (7, 2 + idx * 16), true, NOTIFICATION_TEXT_Z);
            let _ = write!(builder, "{line}");
        }
        let bar_width = self.get_bar_width();
        builder.draw_texture_region_z(
            pos + (3, self.height() - 3),
            NOTIFICATION_Z,
            NOTIFICATION_BAR_UV,
            (bar_width, 1),
            (20, 1)
        );
        builder.draw_texture_region_z(
            pos + (4, self.height() - 2),
            NOTIFICATION_Z,
            NOTIFICATION_BAR_BACKDROP_UV,
            (bar_width, 1),
            (20, 1)
        );
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn is_invisible(&mut self) -> bool {
        let now = now();
        let ms = now.saturating_sub(*self.timestamp.get_or_insert(now)).as_millis() as usize;
        let display_time = self.message_len * 60 + 3000 + 500;
        ms > display_time
    }

    #[must_use]
    fn get_bar_width(&mut self) -> usize {
        let now = now();
        let ms = (now.saturating_sub(*self.timestamp.get_or_insert(now)).as_millis() as usize).saturating_sub(250);
        let width = self.width + 4;
        let display_time = self.message_len * 60 + 3000;
        ((1.0 - (ms as f64 / display_time as f64)).clamp(0.0, 1.0) * width as f64).round() as usize
    }

    #[must_use]
    fn get_inset(&mut self) -> usize {
        let now = now();
        let mut ms = now.saturating_sub(*self.timestamp.get_or_insert(now)).as_millis() as usize;
        let width = self.width + 10;
        let display_time = self.message_len * 60 + 3000;
        if ms < 250 { return (smoothstep64((250 - ms) as f64 / 250.0) * width as f64) as usize }
        ms -= 250;
        if ms < display_time {
            0
        } else {
            (smoothstep64((ms - display_time) as f64 / 250.0) * width as f64) as usize
        }
    }
    
    #[must_use]
    pub fn height(&self) -> usize {
        6 + self.lines.len() * 16
    }
    
    #[must_use]
    pub fn kind(&self) -> NotificationKind {
        self.kind
    }
}
