use std::time::Duration;

use wasm_bindgen::prelude::wasm_bindgen;

use crate::render::{TextColor, run};
use crate::widget::{Alert, Alertable};
use crate::{WINDOW_PROPERTIES, WORKBENCH, config};

#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {
		::web_sys::console::error_1(&wasm_bindgen::JsValue::from(&format!($($arg)*)));
	};
}

#[macro_export]
macro_rules! log {
    ($($arg:tt)*) => {
		::web_sys::console::log_1(&wasm_bindgen::JsValue::from(&format!($($arg)*)));
	};
}

#[wasm_bindgen(start)]
pub fn wasm_main() {
	std::panic::set_hook(Box::new(|info| {
		on_panic(info.to_string());
	}));
	config::read();
	wasm_bindgen_futures::spawn_local(async move {
		run().await;
	});
}

#[wasm_bindgen(module = "/web/script.js")]
extern "C" {
	#[wasm_bindgen(js_name = "getClipboard")]
	pub fn get_clipboard() -> Option<String>;

	#[wasm_bindgen(js_name = "onInput")]
	pub fn on_input();

	#[wasm_bindgen(js_name = "tryOpenDialog")]
	pub fn try_open_dialog();

	#[wasm_bindgen(js_name = "save")]
	pub fn save(name: &str, bytes: Vec<u8>);

	#[wasm_bindgen(js_name = "onPanic")]
	fn on_panic(msg: String);
}

#[wasm_bindgen]
pub fn open_file(name: String, bytes: Vec<u8>) {
	use crate::widget::Alert;

	let workbench = unsafe { &mut WORKBENCH };

	workbench.on_open_file(name.as_str().as_ref(), bytes).alert_err(&mut workbench.alerts);
}

#[wasm_bindgen]
pub fn close() -> usize { unsafe { WORKBENCH.close() } }

pub fn set_clipboard(value: String) -> bool { web_sys::window().map(|window| window.navigator()).map(|navigator| navigator.clipboard()).map(|clipboard| clipboard.write_text(&value)).is_some() }

pub struct FakeScope<'a, 'b>;

pub fn fake_scope<'env, T, F: for<'scope> FnOnce(&'scope FakeScope<'scope, 'env>) -> T>(f: F) -> T {
	let scope = FakeScope;
	f(&scope)
}

impl<'scope, 'env> FakeScope<'scope, 'env> {
	pub fn spawn<T: Send + 'scope, F: FnOnce() -> T>(&'scope self, f: F) -> T { f() }
}
