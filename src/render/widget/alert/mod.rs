use std::error::Error;
use std::fmt::Display;
use std::time::Duration;

use winit::dpi::PhysicalSize;
use winit::event::MouseButton;
use crate::render::assets::{ALERT_UV, NOTIFICATION_BAR_BACKDROP_UV, NOTIFICATION_BAR_UV, NOTIFICATION_TEXT_Z, NOTIFICATION_Z};
use crate::render::color::TextColor;
use crate::render::vertex_buffer_builder::VertexBufferBuilder;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment, WidgetContext, WidgetContextMut};
use crate::util::{StrExt, Timestamp, Vec2u, smoothstep, split_lines, set_clipboard};
use crate::workbench::mouse::MouseManager;
use crate::{error, log};
use crate::action_result::ActionResult;
use crate::render::widget::notification::{Notification, NotificationKind};

pub mod manager;

pub struct Alert {
	timestamp: Timestamp,
	title: String,
	title_color: u32,
	lines: Box<[String]>,
	message_len: usize,
	width: usize,
	original_message: String,
	is_currently_hovering: bool,
	time_elapsed_override: Option<Duration>,
}

impl Alert {
	#[must_use]
	pub fn new(title: impl ToString, title_color: TextColor, message: impl ToString) -> Self {
		let message = message.to_string();
		let title = title.to_string();
		let lines = split_lines(message.clone(), 256);
		Self {
			timestamp: Timestamp::now(),
			title_color: title_color.to_raw(),
			message_len: lines.iter().map(String::len).sum(),
			width: usize::max(title.width(), lines.iter().map(|s| s.width()).max().unwrap_or(0)) + 24,
			title,
			lines: lines.into_boxed_slice(),
			original_message: message,
			is_currently_hovering: false,
			time_elapsed_override: None,
		}
	}

	#[must_use]
	pub fn error(error: impl Display) -> Self { Self::new("Error!", TextColor::Red, format!("{error}")) }

	pub fn log(&self) {
		if self.title == "Error!" && self.title_color == TextColor::Red.to_raw() {
			error!("ALERT ERROR: {}", self.original_message)
		} else {
			log!("ALERT: {}", self.original_message)
		}
	}

	fn get_bar_width(&self) -> usize {
		let ms = (self.elapsed().as_millis() as usize).saturating_sub(250);
		let width = self.width - 20;
		let display_time = (self.message_len + self.title.len()) * 60 + 3000;
		((1.0 - (ms as f64 / display_time as f64)).clamp(0.0, 1.0) * width as f64).round() as usize
	}

	fn get_inset(&self) -> usize {
		let mut ms = self.elapsed().as_millis() as usize;
		let width = self.width;
		let display_time = (self.message_len + self.title.len()) * 60 + 3000;
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

	fn height(&self) -> usize { 30 + self.lines.len() * 16 }

	#[must_use]
	fn is_visible(&self) -> bool {
		let ms = self.elapsed().as_millis() as usize;
		let display_time = (self.message_len + self.title.len()) * 60 + 3000;
		ms <= 500 + display_time
	}

	#[must_use]
	fn is_actually_within_bounds(&self, pos: Vec2u, dims: PhysicalSize<u32>) -> bool {
		let inset = self.get_inset() as u32;
		inset <= pos.x as u32 && (pos.x as u32) < dims.width
	}
}

impl Widget for Alert {
	fn alignment(&self) -> WidgetAlignment {
		WidgetAlignment::new(
			HorizontalWidgetAlignmentPreference::Right,
			VerticalWidgetAlignmentPreference::Top,
		)
	}
	
	fn dimensions(&self, _containment_dims: PhysicalSize<u32>) -> PhysicalSize<u32> { PhysicalSize::new(self.width as _, self.height() as _) }

	fn is_valid_mouse_button(&self, button: MouseButton, _pos: Vec2u, _dims: PhysicalSize<u32>) -> bool {
		matches!(button, MouseButton::Left | MouseButton::Middle | MouseButton::Right)
	}

	fn on_mouse_down(&mut self, button: MouseButton, pos: Vec2u, dims: PhysicalSize<u32>, ctx: &mut WidgetContextMut) -> ActionResult {
		if !self.is_actually_within_bounds(pos, dims) { return ActionResult::Pass }
		
		if let MouseButton::Left | MouseButton::Middle = button {
			self.timestamp = Timestamp::UNIX_EPOCH;
			self.time_elapsed_override = None;
			ActionResult::Success(())
		} else if let MouseButton::Right = button {
			set_clipboard(self.original_message.clone());
			ctx.notifications.notify(Notification::new("Copied alert to clipboard!", TextColor::Yellow, NotificationKind::CopiedToClipboard));
			ActionResult::Success(())
		} else {
			ActionResult::Pass
		}
	}

	fn is_currently_hovering(&self) -> bool { self.is_currently_hovering }
	
	fn on_hovering(&mut self, pos: Vec2u, dims: PhysicalSize<u32>, _ctx: &mut WidgetContextMut) {
		if !self.is_actually_within_bounds(pos, dims) { self.is_currently_hovering = false; return }
		self.is_currently_hovering = true;
		if self.time_elapsed_override.is_none() {
			self.time_elapsed_override = Some(self.timestamp.elapsed());
		}
	}

	fn on_stop_hovering(&mut self, _ctx: &mut WidgetContextMut) {
		self.is_currently_hovering = false;
		if let Some(time_elapsed) = self.time_elapsed_override.take() {
			self.timestamp = Timestamp::now() - time_elapsed;
		}
	}

	fn is_visible(&self, _ctx: &WidgetContext) -> bool { self.is_visible() }

	fn render_at(&self, mut pos: Vec2u, dims: PhysicalSize<u32>, builder: &mut VertexBufferBuilder, _mouse: &MouseManager, _ctx: &WidgetContext) {
		use core::fmt::Write;

		pos.x += self.get_inset();

		builder.draw_texture_region_z(pos + (4, 4), NOTIFICATION_Z, ALERT_UV + (12, 4), (dims.width as usize - 8, dims.height as usize - 8), (24, 32));
		builder.draw_texture_z(pos + (4, 4), NOTIFICATION_Z, ALERT_UV + (4, 4), (8, 32));
		builder.draw_texture_z(pos, NOTIFICATION_Z, ALERT_UV, (4, 4));
		builder.draw_texture_z(pos + (dims.width as usize - 4, 0), NOTIFICATION_Z, ALERT_UV + (36, 0), (4, 4));
		builder.draw_texture_z(pos + (0, dims.height as usize - 4), NOTIFICATION_Z, ALERT_UV + (0, 36), (4, 4));
		builder.draw_texture_z(pos + (dims.width as usize - 4, dims.height as usize - 4), NOTIFICATION_Z, ALERT_UV + (36, 36), (4, 4));
		{
			let mut remaining_width = dims.width as usize - 8;
			while remaining_width > 0 {
				builder.draw_texture_z(pos + (dims.width as usize - 4 - remaining_width, 0), NOTIFICATION_Z, ALERT_UV + (2, 0), (32.min(remaining_width), 4));
				builder.draw_texture_z(pos + (dims.width as usize - 4 - remaining_width, dims.height as usize - 4), NOTIFICATION_Z, ALERT_UV + (2, 36), (32.min(remaining_width), 4));
				remaining_width = remaining_width.saturating_sub(32);
			}
		}
		{
			let mut remaining_height = dims.height as usize - 8;
			while remaining_height > 0 {
				builder.draw_texture_z(pos + (0, dims.height as usize - 4 - remaining_height), NOTIFICATION_Z, ALERT_UV + (0, 4), (4, 32.min(remaining_height)));
				builder.draw_texture_z(pos + (dims.width as usize - 4, dims.height as usize - 4 - remaining_height), NOTIFICATION_Z, ALERT_UV + (36, 4), (4, 32.min(remaining_height)));
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
		builder.draw_texture_region_z(pos + (6, dims.height as usize - 8), NOTIFICATION_Z, NOTIFICATION_BAR_UV, (bar_width, 2), (20, 1));
		builder.draw_texture_region_z(pos + (8, dims.height as usize - 6), NOTIFICATION_Z, NOTIFICATION_BAR_BACKDROP_UV, (bar_width, 2), (20, 1));
	}
}

impl<E: Error> From<E> for Alert {
	fn from(value: E) -> Self { Self::error(value) }
}
