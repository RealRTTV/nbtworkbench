use crate::render::{vertex_buffer_builder::VertexBufferBuilder, widget::alert::Alert};

pub struct AlertManager {
	alerts: Vec<Alert>,
}

impl AlertManager {
	#[must_use]
	pub const fn new() -> Self { Self { alerts: Vec::new() } }

	pub fn alert(&mut self, alert: impl Into<Alert>) {
		let alert = alert.into();
		alert.log();
		self.alerts.insert(0, alert);
	}

	pub fn render(&mut self, y: &mut usize, builder: &mut VertexBufferBuilder) {
		self.alerts.retain_mut(|alert| !alert.is_invisible());
		for alert in &mut self.alerts {
			alert.render(builder, *y);
			*y += alert.height();
		}
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
