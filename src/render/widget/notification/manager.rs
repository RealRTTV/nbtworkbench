use std::collections::hash_map::Entry;
use std::ops::BitOrAssign;
use fxhash::{FxBuildHasher, FxHashMap};

use crate::render::assets::HEADER_SIZE;
use crate::render::widget::notification::{Notification, NotificationKind};
use crate::render::widget::vertical_list::VerticalList;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment};

pub struct NotificationManager {
	notifications: FxHashMap<NotificationKind, Notification>,
}

impl NotificationManager {
	const ALIGNMENT: WidgetAlignment = WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Right, VerticalWidgetAlignmentPreference::Static(HEADER_SIZE as _));

	#[must_use]
	pub const unsafe fn uninit() -> Self {
		Self {
			notifications: FxHashMap::with_hasher(FxBuildHasher::new()),
		}
	}

	#[must_use]
	pub fn new() -> Self { Self { notifications: FxHashMap::default() } }

	pub fn notify(&mut self, notification: impl Into<Notification>) {
		let notification = notification.into();
		let kind = notification.kind();
		match self.notifications.entry(kind) {
			Entry::Occupied(mut slot) => slot.get_mut().update(notification),
			Entry::Vacant(slot) => {
				slot.insert(notification);
			}
		}
	}

	pub fn cleanup(&mut self) { self.notifications.retain(|_, notification| notification.is_visible()); }

	#[must_use]
	pub fn as_vertical_list(&mut self) -> VerticalList { VerticalList::new(self.notifications.values_mut().map(|notification| notification as &mut dyn Widget), Self::ALIGNMENT) }
}

impl BitOrAssign for NotificationManager {
	fn bitor_assign(&mut self, rhs: Self) {
		self.notifications.extend(rhs.notifications);
	}
}

pub trait Notifiable<U> {
	fn notify_err(self, notifications: &mut NotificationManager) -> U
	where Self: Sized;
}

impl<T, E: Into<Notification>> Notifiable<Option<T>> for Result<T, E> {
	fn notify_err(self, notifications: &mut NotificationManager) -> Option<T>
	where Self: Sized {
		match self {
			Ok(x) => Some(x),
			Err(e) => {
				notifications.notify(e);
				None
			}
		}
	}
}
