use fxhash::{FxBuildHasher, FxHashSet};
use winit::keyboard::KeyCode;

pub struct KeyboardManager {
	held_keys: FxHashSet<KeyCode>,
}

impl KeyboardManager {
	#[must_use]
	pub const fn new() -> Self {
		Self {
			held_keys: FxHashSet::with_hasher(FxBuildHasher::new()),
		}
	}

	pub fn on_press(&mut self, key: KeyCode) { self.held_keys.insert(key); }

	pub fn on_release(&mut self, key: KeyCode) { self.held_keys.remove(&key); }

	#[must_use]
	pub fn has_key_held(&self, key: KeyCode) -> bool { self.held_keys.contains(&key) }

	#[cfg(target_os = "macos")]
	#[must_use]
	pub fn ctrl(&self) -> bool { self.held_keys.contains(&KeyCode::ControlLeft) | self.held_keys.contains(&KeyCode::ControlRight) | self.held_keys.contains(&KeyCode::SuperLeft) | self.held_keys.contains(&KeyCode::SuperRight) }

	#[cfg(not(target_os = "macos"))]
	#[must_use]
	pub fn ctrl(&self) -> bool { self.held_keys.contains(&KeyCode::ControlLeft) | self.held_keys.contains(&KeyCode::ControlRight) }

	#[must_use]
	pub fn shift(&self) -> bool { self.held_keys.contains(&KeyCode::ShiftLeft) | self.held_keys.contains(&KeyCode::ShiftRight) }

	#[must_use]
	pub fn alt(&self) -> bool { self.held_keys.contains(&KeyCode::AltLeft) | self.held_keys.contains(&KeyCode::AltRight) }

	pub fn modifiers(&self) -> Modifiers {
		Modifiers {
			ctrl: self.ctrl(),
			shift: self.shift(),
			alt: self.alt(),
		}
	}
}

#[must_use]
#[derive(Copy, Clone)]
pub struct Modifiers {
	pub ctrl: bool,
	pub shift: bool,
	pub alt: bool,
}

impl Modifiers {
	#[must_use]
	pub fn into_bitflags(self) -> u8 { self.ctrl as u8 | ((self.shift as u8) << 1) | ((self.alt as u8) << 2) }
}
