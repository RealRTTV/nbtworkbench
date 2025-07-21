use std::time::Duration;

use winit::dpi::PhysicalSize;

use crate::render::assets::{NOTIFICATION_BAR_BACKDROP_UV, NOTIFICATION_BAR_UV, NOTIFICATION_TEXT_Z, NOTIFICATION_UV, NOTIFICATION_Z};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::util::{StrExt, Timestamp, Vec2u, smoothstep, split_lines};
use crate::workbench::mouse::MouseManager;

pub mod manager;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum NotificationKind {
	Scale,
	Find,
	Replace,
	CopiedToClipboard,
}

#[derive(Debug)]
pub struct Notification {
	timestamp: Timestamp,
	lines: Box<[String]>,
	message_len: usize,
	text_color: u32,
	width: usize,
	raw_message: String,
	kind: NotificationKind,
	is_hovering: bool,
	time_elapsed_override: Option<Duration>,
}

impl Notification {
	pub fn new(message: impl Into<String>, text_color: TextColor, kind: NotificationKind) -> Self {
		let message = message.into();
		let lines = split_lines::<256>(message.clone());
		Self {
			timestamp: Timestamp::now(),
			width: lines.iter().map(|s| s.width()).max().unwrap_or(0) + 10,
			message_len: lines.iter().map(String::len).sum(),
			lines: lines.into_boxed_slice(),
			text_color: text_color.to_raw(),
			kind,
			raw_message: message,
			is_hovering: false,
			time_elapsed_override: None,
		}
	}

	pub fn update(&mut self, notification: Notification) {
		let message = notification.raw_message;
		let text_color = notification.text_color;

		let old_display_time = self.message_len * 60 + 3000;
		let old_width = self.width;
		let lines = split_lines::<256>(message.clone());
		self.width = lines.iter().map(|s| s.width()).max().unwrap_or(0) + 10;
		self.message_len = lines.iter().map(String::len).sum();
		self.lines = lines.into_boxed_slice();
		self.text_color = text_color;
		self.raw_message = message;
		let elapsed = self.timestamp.elapsed().as_millis();
		if elapsed <= 250 {
			// readjust based on amount previously out
			let old_px = ((250 - elapsed) as f64 * old_width as f64) as usize;
			if self.width <= old_px {
				// already as far out as ours is, so set to full width of new one
				self.timestamp = Timestamp::now() - Duration::from_millis(250);
			} else {
				let ms = 250.0 * (1.0 - self.width as f64 / old_px as f64);
				self.timestamp = Timestamp::now() - Duration::from_nanos((ms * 1_000_000.0) as u64);
			}
		} else if elapsed <= 250 + old_display_time as u128 {
			// reset the expiry clock
			self.timestamp = Timestamp::now() - Duration::from_millis(250);
		} else if elapsed <= 250 + old_display_time as u128 + 250 {
			// re-adjust based on amount previously out
			let old_px = ((elapsed - (250 + old_display_time as u128)) as f64 * old_width as f64) as usize;
			if self.width <= old_px {
				// already as far out as ours is, so set to full width of new one
				self.timestamp = Timestamp::now() - Duration::from_millis(250);
			} else {
				let ms = 250.0 * (1.0 - self.width as f64 / old_px as f64);
				self.timestamp = Timestamp::now() - Duration::from_nanos((ms * 1_000_000.0) as u64);
			}
		} else {
			// shouldn't normally happen, but we'll handle it in case
			self.timestamp = Timestamp::now();
		}
	}

	#[must_use]
	fn get_bar_width(&self) -> usize {
		let ms = (self.elapsed().as_millis() as usize).saturating_sub(250);
		let width = self.width - 6;
		let display_time = self.message_len * 60 + 3000;
		((1.0 - (ms as f64 / display_time as f64)).clamp(0.0, 1.0) * width as f64).round() as usize
	}

	#[must_use]
	fn get_inset(&self) -> usize {
		let mut ms = (self.elapsed().as_millis() as usize).saturating_sub(250);
		let width = self.width;
		let display_time = self.message_len * 60 + 3000;
		if ms < 250 {
			return (smoothstep((250 - ms) as f64 / 250.0) * width as f64) as usize
		}
		ms -= 250;
		if ms < display_time { 0 } else { (smoothstep((ms - display_time) as f64 / 250.0) * width as f64) as usize }
	}

	#[must_use]
	fn elapsed(&self) -> Duration {
		self.time_elapsed_override.unwrap_or(self.timestamp.elapsed())
	}

	#[must_use]
	pub fn height(&self) -> usize { 6 + self.lines.len() * 16 }

	#[must_use]
	pub fn kind(&self) -> NotificationKind { self.kind }

	#[must_use]
	pub fn is_visible(&self) -> bool {
		let ms = self.elapsed().as_millis() as usize;
		let display_time = self.message_len * 60 + 3000 + 500;
		ms <= display_time
	}
}

impl Widget for Notification {
	fn alignment(&self) -> WidgetAlignment {
		WidgetAlignment::new(
			HorizontalWidgetAlignmentPreference::Right,
			VerticalWidgetAlignmentPreference::Top,
		)
	}
	fn dimensions(&self, _containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> { PhysicalSize::new(self.width as _, self.height() as _) }
	fn is_currently_hovering(&self) -> bool { self.time_elapsed_override.is_some() }

	fn on_hovering(&mut self, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) {
		self.is_hovering = false;
		self.time_elapsed_override = Some(self.elapsed());
	}
	fn on_stop_hovering(&mut self, ctx: &mut WidgetContextMut) {
		self.is_hovering = false;
		if let Some(time_elapsed) = self.time_elapsed_override.take() {
			self.timestamp = Timestamp::now() - time_elapsed;
		}
	}

	fn is_visible(&self, _ctx: &WidgetContext) -> bool { self.is_visible() }

	fn render_at(&self, mut pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, mouse: &MouseManager, ctx: &WidgetContext) {
		use core::fmt::Write;

		pos.x += self.get_inset();
		builder.draw_texture_region_z(pos + (2, 2), NOTIFICATION_Z, NOTIFICATION_UV + (6, 2), (dims.width as usize - 4, dims.height as usize - 4), (12, 16));
		builder.draw_texture_z(pos + (2, 2), NOTIFICATION_Z, NOTIFICATION_UV + (2, 2), (4, 16));
		builder.draw_texture_z(pos + (6 + dims.width as usize - 8, 0), NOTIFICATION_Z, NOTIFICATION_UV + (18, 0), (2, 2));
		builder.draw_texture_z(pos + (6 + dims.width as usize - 8, dims.height as usize - 2), NOTIFICATION_Z, NOTIFICATION_UV + (18, 18), (2, 2));
		builder.draw_texture_z(pos + (0, dims.height as usize - 2), NOTIFICATION_Z, NOTIFICATION_UV + (0, 18), (2, 2));
		builder.draw_texture_z(pos + (0, 0), NOTIFICATION_Z, NOTIFICATION_UV, (2, 2));
		{
			let mut remaining_width = dims.width as usize - 4;
			while remaining_width > 0 {
				builder.draw_texture_z(pos + (6 + dims.width as usize - 8 - remaining_width, 0), NOTIFICATION_Z, NOTIFICATION_UV + (2, 0), (16.min(remaining_width), 2));
				builder.draw_texture_z(
					pos + (6 + dims.width as usize - 8 - remaining_width, dims.height as usize - 2),
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
				builder.draw_texture_z(pos + (0, self.height() - 4 - remaining_height + 2), NOTIFICATION_Z, NOTIFICATION_UV + (0, 2), (2, 16.min(remaining_height)));
				builder.draw_texture_z(
					pos + (6 + dims.width as usize - 8, self.height() - 4 - remaining_height + 2),
					NOTIFICATION_Z,
					NOTIFICATION_UV + (18, 2),
					(2, 16.min(remaining_height)),
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
		builder.draw_texture_region_z(pos + (3, self.height() - 3), NOTIFICATION_Z, NOTIFICATION_BAR_UV, (bar_width, 1), (20, 1));
		builder.draw_texture_region_z(pos + (4, self.height() - 2), NOTIFICATION_Z, NOTIFICATION_BAR_BACKDROP_UV, (bar_width, 1), (20, 1));
	}
}
