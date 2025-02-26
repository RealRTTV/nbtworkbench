use std::time::Duration;
use wasm_bindgen::prelude::wasm_bindgen;

use crate::config;
use crate::render::run;
use crate::render::TextColor;
use crate::widget::Alert;
use crate::{WINDOW_PROPERTIES, WORKBENCH};

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
    fn on_input();

    #[wasm_bindgen(js_name = "tryOpenDialog")]
    fn try_open_dialog();

    #[wasm_bindgen(js_name = "save")]
    fn save(name: &str, bytes: Vec<u8>);

    #[wasm_bindgen(js_name = "onPanic")]
    fn on_panic(msg: String);
}

#[wasm_bindgen]
pub fn open_file(name: String, bytes: Vec<u8>) {
    use crate::widget::Alert;

    let workbench = unsafe { &mut WORKBENCH };

    if let Err(e) = workbench.on_open_file(name.as_str().as_ref(), bytes, unsafe { &mut WINDOW_PROPERTIES }) {
        workbench.alert(Alert::new("Error!", TextColor::Red, e.to_string()));
    }
}

#[wasm_bindgen]
pub fn close() -> usize {
    unsafe { WORKBENCH.close() }
}

pub fn set_clipboard(value: String) -> bool {
    web_sys::window().map(|window| window.navigator()).map(|navigator| navigator.clipboard()).map(|clipboard| clipboard.write_text(&value)).is_some()
}

#[must_use]
pub fn now() -> Duration {
    Duration::from_millis(web_sys::js_sys::Date::now() as u64)
}
