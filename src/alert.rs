use std::time::Duration;

use crate::{since_epoch, smoothstep64, split_lines, StrExt};
use crate::assets::{ALERT_UV, NOTIFICATION_TEXT_Z, NOTIFICATION_Z};
use crate::color::TextColor;
use crate::vertex_buffer_builder::{Vec2u, VertexBufferBuilder};

pub struct Alert {
	timestamp: Option<Duration>,
	title: String,
	title_color: u32,
	lines: Box<[String]>,
	message_len: usize,
	width: usize,
}

impl Alert {
	pub fn new(title: impl Into<String>, title_color: TextColor, message: impl Into<String>) -> Self {
		let title = title.into();
		let lines = split_lines::<256>(message.into());
		Self {
			timestamp: None,
			title_color: title_color.to_raw(),
			message_len: lines.iter().map(String::len).sum(),
			width: usize::max(title.width(), lines.iter().map(|s| s.width()).max().unwrap_or(0)),
			title,
			lines: lines.into_boxed_slice(),
		}
	}

	pub fn render(&mut self, builder: &mut VertexBufferBuilder, y: usize) {
		use core::fmt::Write;

		let pos = Vec2u::new(
			(builder.window_width() + self.get_inset()).saturating_sub(self.width + 24),
			y,
		);
		builder.draw_texture_region_z(
			pos + (4, 4),
			NOTIFICATION_Z,
			ALERT_UV + (12, 4),
			(self.width + 16, 16 + self.lines.len() * 16),
			(24, 32),
		);
		builder.draw_texture_z(pos + (4, 4), NOTIFICATION_Z, ALERT_UV + (4, 4), (8, 32));
		builder.draw_texture_z(
			pos,
			NOTIFICATION_Z,
			ALERT_UV,
			(4, 4),
		);
		builder.draw_texture_z(
			pos + (self.width + 20, 0),
			NOTIFICATION_Z,
			ALERT_UV + (36, 0),
			(4, 4),
		);
		builder.draw_texture_z(
			pos + (0, 20 + self.lines.len() * 16),
			NOTIFICATION_Z,
			ALERT_UV + (0, 36),
			(4, 4),
		);
		builder.draw_texture_z(
			pos + (self.width + 20, 20 + self.lines.len() * 16),
			NOTIFICATION_Z,
			ALERT_UV + (36, 36),
			(4, 4),
		);
		{
			let mut remaining_width = self.width + 16;
			while remaining_width > 0 {
				builder.draw_texture_z(
					pos + (self.width + 20 - remaining_width, 0),
					NOTIFICATION_Z,
					ALERT_UV + (2, 0),
					(32.min(remaining_width), 4),
				);
				builder.draw_texture_z(
					pos + (self.width + 20 - remaining_width, 20 + self.lines.len() * 16),
					NOTIFICATION_Z,
					ALERT_UV + (2, 36),
					(32.min(remaining_width), 4),
				);
				remaining_width = remaining_width.saturating_sub(32);
			}
		}
		{
			let mut remaining_height = 16 + self.lines.len() * 16;
			while remaining_height > 0 {
				builder.draw_texture_z(
					pos + (0, 20 + self.lines.len() * 16 - remaining_height),
					NOTIFICATION_Z,
					ALERT_UV + (0, 4),
					(4, 32.min(remaining_height)),
				);
				builder.draw_texture_z(
					pos + (self.width + 20, 20 + self.lines.len() * 16 - remaining_height),
					NOTIFICATION_Z,
					ALERT_UV + (36, 4),
					(4, 32.min(remaining_height)),
				);
				remaining_height = remaining_height.saturating_sub(32);
			}
		}
		builder.settings(pos + (18, 4), true, NOTIFICATION_TEXT_Z);
		builder.color = self.title_color;
		let _ = write!(builder, "{}", self.title);
		builder.color = TextColor::White.to_raw();
		for (idx, line) in self.lines.iter().enumerate() {
			builder.settings(pos + (18, 20 + idx * 16), true, NOTIFICATION_TEXT_Z);
			let _ = write!(builder, "{line}");
		}
	}

	#[allow(clippy::wrong_self_convention)]
	pub fn is_invisible(&mut self) -> bool {
		let ms = since_epoch().saturating_sub(*self.timestamp.get_or_insert(since_epoch())).as_millis() as usize;
		let display_time = (self.message_len + self.title.len()) * 60 + 3000;
		ms > 500 + display_time
	}

	fn get_inset(&mut self) -> usize {
		let mut ms = since_epoch().saturating_sub(*self.timestamp.get_or_insert(since_epoch())).as_millis() as usize;
		let width = self.width + 24;
		let display_time = (self.message_len + self.title.len()) * 60 + 3000;
		if ms < 250 { return (smoothstep64((250 - ms) as f64 / 250.0) * width as f64) as usize }
		ms -= 250;
		if ms < display_time {
			0
		} else {
			(smoothstep64((ms - display_time) as f64 / 250.0) * width as f64) as usize
		}
	}

	pub fn height(&mut self) -> usize {
		24 + self.lines.len() * 16
	}
}
