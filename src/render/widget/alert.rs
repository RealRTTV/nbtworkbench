use std::time::Duration;

use crate::assets::{ALERT_UV, NOTIFICATION_BAR_BACKDROP_UV, NOTIFICATION_BAR_UV, NOTIFICATION_TEXT_Z, NOTIFICATION_Z};
use crate::error;
use crate::render::{TextColor, VertexBufferBuilder};
use crate::util::{StrExt, Vec2u, now, smoothstep64, split_lines};

pub struct Alert {
	timestamp: Option<Duration>,
	title: String,
	title_color: u32,
	lines: Box<[String]>,
	message_len: usize,
	width: usize,
}

impl Alert {
	#[must_use]
	pub fn new(title: impl ToString, title_color: TextColor, message: impl ToString) -> Self {
		let title = title.to_string();
		let lines = split_lines::<256>(message.to_string());
		Self {
			timestamp: None,
			title_color: title_color.to_raw(),
			message_len: lines.iter().map(String::len).sum(),
			width: usize::max(title.width(), lines.iter().map(|s| s.width()).max().unwrap_or(0)),
			title,
			lines: lines.into_boxed_slice(),
		}
	}

	#[must_use]
	pub fn error(error: impl ToString) -> Self { Self::new("Error!", TextColor::Red, error) }

	pub fn render(&mut self, builder: &mut VertexBufferBuilder, y: usize) {
		use core::fmt::Write;

		let pos = Vec2u::new((builder.window_width() + self.get_inset()).saturating_sub(self.width + 24), y);
		builder.draw_texture_region_z(pos + (4, 4), NOTIFICATION_Z, ALERT_UV + (12, 4), (self.width + 16, self.height() - 8), (24, 32));
		builder.draw_texture_z(pos + (4, 4), NOTIFICATION_Z, ALERT_UV + (4, 4), (8, 32));
		builder.draw_texture_z(pos, NOTIFICATION_Z, ALERT_UV, (4, 4));
		builder.draw_texture_z(pos + (self.width + 20, 0), NOTIFICATION_Z, ALERT_UV + (36, 0), (4, 4));
		builder.draw_texture_z(pos + (0, self.height() - 4), NOTIFICATION_Z, ALERT_UV + (0, 36), (4, 4));
		builder.draw_texture_z(pos + (self.width + 20, self.height() - 4), NOTIFICATION_Z, ALERT_UV + (36, 36), (4, 4));
		{
			let mut remaining_width = self.width + 16;
			while remaining_width > 0 {
				builder.draw_texture_z(pos + (self.width + 20 - remaining_width, 0), NOTIFICATION_Z, ALERT_UV + (2, 0), (32.min(remaining_width), 4));
				builder.draw_texture_z(pos + (self.width + 20 - remaining_width, self.height() - 4), NOTIFICATION_Z, ALERT_UV + (2, 36), (32.min(remaining_width), 4));
				remaining_width = remaining_width.saturating_sub(32);
			}
		}
		{
			let mut remaining_height = self.height() - 8;
			while remaining_height > 0 {
				builder.draw_texture_z(pos + (0, self.height() - 4 - remaining_height), NOTIFICATION_Z, ALERT_UV + (0, 4), (4, 32.min(remaining_height)));
				builder.draw_texture_z(pos + (self.width + 20, self.height() - 4 - remaining_height), NOTIFICATION_Z, ALERT_UV + (36, 4), (4, 32.min(remaining_height)));
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
		let bar_width = self.get_bar_width();
		builder.draw_texture_region_z(pos + (6, self.height() - 8), NOTIFICATION_Z, NOTIFICATION_BAR_UV, (bar_width, 2), (20, 1));
		builder.draw_texture_region_z(pos + (8, self.height() - 6), NOTIFICATION_Z, NOTIFICATION_BAR_BACKDROP_UV, (bar_width, 2), (20, 1));
	}

	pub fn is_invisible(&mut self) -> bool {
		let ms = now()
			.saturating_sub(*self.timestamp.get_or_insert(now()))
			.as_millis() as usize;
		let display_time = (self.message_len + self.title.len()) * 60 + 3000;
		ms > 500 + display_time
	}

	fn get_bar_width(&mut self) -> usize {
		let now = now();
		let ms = (now
			.saturating_sub(*self.timestamp.get_or_insert(now))
			.as_millis() as usize)
			.saturating_sub(250);
		let width = self.width + 4;
		let display_time = (self.message_len + self.title.len()) * 60 + 3000;
		((1.0 - (ms as f64 / display_time as f64)).clamp(0.0, 1.0) * width as f64).round() as usize
	}

	fn get_inset(&mut self) -> usize {
		let mut ms = now()
			.saturating_sub(*self.timestamp.get_or_insert(now()))
			.as_millis() as usize;
		let width = self.width + 24;
		let display_time = (self.message_len + self.title.len()) * 60 + 3000;
		if ms < 250 {
			return (smoothstep64((250 - ms) as f64 / 250.0) * width as f64) as usize
		}
		ms -= 250;
		if ms < display_time { 0 } else { (smoothstep64((ms - display_time) as f64 / 250.0) * width as f64) as usize }
	}

	pub fn height(&mut self) -> usize { 30 + self.lines.len() * 16 }
}

impl From<anyhow::Error> for Alert {
	fn from(value: anyhow::Error) -> Self {
		error!("{value:?}");
		Self::error(value)
	}
}

impl From<native_dialog::Error> for Alert {
	fn from(value: native_dialog::Error) -> Self {
		error!("{value:?}");
		Self::error(value)
	}
}

impl From<std::io::Error> for Alert {
	fn from(value: std::io::Error) -> Self {
		error!("{value:?}");
		Self::error(value)
	}
}
