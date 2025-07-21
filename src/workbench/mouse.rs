use fxhash::{FxBuildHasher, FxHashSet};
use winit::event::MouseButton;

use crate::util::Vec2u;

pub struct MouseManager {
	pub coords: Vec2u,
	held_mouse_keys: FxHashSet<MouseButton>,
}

impl MouseManager {
	#[must_use]
	pub const fn new() -> Self {
		Self {
			coords: Vec2u::new(0, 0),
			held_mouse_keys: FxHashSet::with_hasher(FxBuildHasher::new()),
		}
	}

	pub fn on_button_pressed(&mut self, button: MouseButton) { self.held_mouse_keys.insert(button); }

	pub fn on_button_released(&mut self, button: MouseButton) { self.held_mouse_keys.remove(&button); }

	pub fn has_button_pressed(&self, button: MouseButton) -> bool { self.held_mouse_keys.contains(&button) }

	pub fn held_keys(&self) -> impl Iterator<Item = MouseButton> { self.held_mouse_keys.iter().copied() }
}
