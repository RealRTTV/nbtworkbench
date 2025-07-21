use std::ops::BitOrAssign;
use crate::action_result::ActionResult;
use crate::render::assets::HEADER_SIZE;
use crate::render::widget::alert::Alert;
use crate::render::widget::vertical_list::VerticalList;
use crate::render::widget::{HorizontalWidgetAlignmentPreference, VerticalWidgetAlignmentPreference, Widget, WidgetAlignment};

pub struct AlertManager {
	alerts: Vec<Alert>,
}

impl AlertManager {
	const ALIGNMENT: WidgetAlignment = WidgetAlignment::new(HorizontalWidgetAlignmentPreference::Right, VerticalWidgetAlignmentPreference::Static(HEADER_SIZE as _));

	#[must_use]
	pub const fn new() -> Self { Self { alerts: Vec::new() } }

	pub fn alert(&mut self, alert: impl Into<Alert>) {
		let alert = alert.into();
		alert.log();
		self.alerts.insert(0, alert);
	}

	pub fn cleanup(&mut self) { self.alerts.retain(|alert| alert.is_visible()) }

	#[must_use]
	pub fn as_vertical_list(&mut self) -> VerticalList { VerticalList::new(self.alerts.iter_mut().map(|alert| alert as &mut dyn Widget), Self::ALIGNMENT) }
}

impl BitOrAssign for AlertManager {
	fn bitor_assign(&mut self, rhs: Self) {
		self.alerts.extend(rhs.alerts);
	}
}

pub trait Alertable<U> {
	fn alert_err(self, alerts: &mut AlertManager) -> U
	where Self: Sized;
}

impl<T, E: Into<Alert>> Alertable<Option<T>> for Result<T, E> {
	fn alert_err(self, alerts: &mut AlertManager) -> Option<T>
	where Self: Sized {
		match self {
			Ok(x) => Some(x),
			Err(e) => {
				alerts.alert(e);
				None
			}
		}
	}
}

impl<S, E: Into<Alert>> Alertable<ActionResult<S, ()>> for ActionResult<S, E> {
	fn alert_err(self, alerts: &mut AlertManager) -> ActionResult<S, ()>
	where
		Self: Sized
	{
		match self {
			ActionResult::Success(s) => ActionResult::Success(s),
			ActionResult::Pass => ActionResult::Pass,
			ActionResult::Failure(e) => {
				alerts.alert(e);
				ActionResult::Failure(())
			}
		}
	}
}
