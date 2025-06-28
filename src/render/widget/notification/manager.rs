use std::mem::MaybeUninit;

use enum_map::EnumMap;

use crate::render::{
	vertex_buffer_builder::VertexBufferBuilder,
	widget::notification::{Notification, NotificationKind},
};

pub struct NotificationManager {
	notifications: EnumMap<NotificationKind, Option<Notification>>,
}

impl NotificationManager {
	#[must_use]
	pub const unsafe fn uninit() -> Self {
		Self {
			notifications: EnumMap::from_array(
				const {
					let mut array = [const { MaybeUninit::<Option<Notification>>::uninit() }; enum_map::enum_len::<NotificationKind>()];
					let mut i = 0;
					while i < enum_map::enum_len::<NotificationKind>() {
						array[i].write(None);
						i += 1;
					}
					unsafe { MaybeUninit::array_assume_init(array) }
				},
			),
		}
	}

	#[must_use]
	pub fn new() -> Self { Self { notifications: EnumMap::default() } }

	pub fn notify(&mut self, notification: Notification) {
		let kind = notification.kind();
		match self.notifications[kind].as_mut() {
			Some(old_notification) => old_notification.update(notification),
			None => self.notifications[kind] = Some(notification),
		}
	}

	pub fn render(&mut self, y: &mut usize, builder: &mut VertexBufferBuilder) {
		for slot in self.notifications.values_mut() {
			if let Some(notification) = slot
				&& notification.is_invisible()
			{
				*slot = None
			}
		}
		for notification in self.notifications.values_mut().filter_map(Option::as_mut) {
			notification.render(builder, *y);
			*y += notification.height();
		}
	}
}
