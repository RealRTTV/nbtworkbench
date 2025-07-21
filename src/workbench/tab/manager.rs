use crate::util::Timestamp;
use crate::window_properties;
use crate::workbench::tab::Tab;

pub struct TabManager {
	tabs: Vec<Tab>,
	active_tab_idx: usize,
}

impl TabManager {
	#[must_use]
	pub const fn without_tab() -> Self { Self { tabs: Vec::new(), active_tab_idx: 0 } }

	#[must_use]
	pub fn from_tab(tab: Tab) -> Self { Self { tabs: vec![tab], active_tab_idx: 0 } }

	#[must_use]
	pub fn active_tab(&self) -> &Tab { unsafe { self.tabs.get(self.active_tab_idx).unwrap_unchecked() } }

	#[must_use]
	pub fn active_tab_mut(&mut self) -> &mut Tab { unsafe { self.tabs.get_mut(self.active_tab_idx).unwrap_unchecked() } }

	pub fn set_active_idx(&mut self, idx: usize) {
		self.active_tab_idx = idx.min(self.tabs.len() - 1);
		window_properties().set_window_title(format!("{} - NBT Workbench", self.active_tab().path.name()).as_str());
	}

	pub fn add(&mut self, tab: Tab) {
		self.tabs.push(tab);
		self.set_active_idx(self.tabs.len() - 1);
	}

	/// You might want to consider dropping this on a seperate thread ([`crate::util::drop_on_separate_thread`])
	#[must_use]
	pub fn remove(&mut self, idx: usize) -> Option<Tab> {
		let tab = unsafe { self.tabs.get_unchecked_mut(idx) };
		if tab.history.has_unsaved_changes() && core::mem::replace(&mut tab.last_close_attempt, Timestamp::now()).elapsed() >= Tab::TAB_CLOSE_DOUBLE_CLICK_INTERVAL {
			return None;
		}

		let tab = self.tabs.remove(idx);
		if self.tabs.is_empty() {
			#[cfg(target_arch = "wasm32")]
			if let Some(window) = web_sys::window() {
				let _ = window.close();
			}
			std::process::exit(0);
		}
		if idx <= self.active_tab_idx {
			self.set_active_idx(self.active_tab_idx.saturating_sub(1));
		}
		Some(tab)
	}

	pub fn iter(&self) -> std::slice::Iter<'_, Tab> { self.tabs.iter() }

	pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Tab> { self.tabs.iter_mut() }

	#[must_use]
	pub fn active_tab_idx(&self) -> usize { self.active_tab_idx }
}

impl<'a> IntoIterator for &'a TabManager {
	type Item = &'a Tab;
	type IntoIter = std::slice::Iter<'a, Tab>;

	fn into_iter(self) -> Self::IntoIter { self.iter() }
}

impl<'a> IntoIterator for &'a mut TabManager {
	type Item = &'a mut Tab;
	type IntoIter = std::slice::IterMut<'a, Tab>;

	fn into_iter(self) -> Self::IntoIter { self.iter_mut() }
}
