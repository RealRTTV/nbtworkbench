use fxhash::FxHashMap;
use winit::window::Theme;

use crate::SortAlgorithm;

struct Config {
    theme: Theme,
    sort_algorithm: SortAlgorithm,
}

static mut CONFIG: Config = Config {
    theme: Theme::Dark,
    sort_algorithm: SortAlgorithm::Type,
};

#[inline]
#[must_use]
fn config() -> &'static mut Config {
    unsafe { &mut CONFIG }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn read() -> bool {
    let Some(config_dir) = dirs::config_dir() else { return false };
    let path = config_dir.join("nbtworkbench/config.txt");
    let Some(map) = std::fs::read(path).ok().and_then(|vec| String::from_utf8(vec).ok()).map(|str| str.lines().filter_map(|line| line.split_once('=')).map(|(a, b)| (a.to_owned(), b.to_owned())).collect::<FxHashMap<String, String>>()) else { return false };
    read0(&map);
    true
}

#[cfg(target_arch = "wasm32")]
pub fn read() -> bool {
    let Some(map) = web_sys::window().and_then(|window| window.local_storage().ok()).flatten().and_then(|storage| storage.get_item("config").ok()).flatten().map(|str| str.lines().filter_map(|line| line.split_once('=')).map(|(a, b)| (a.to_owned(), b.to_owned())).collect::<FxHashMap<String, String>>()) else { return false };
    read0(&map);
    true
}

#[inline]
fn read0(map: &FxHashMap<String, String>) {
    if let Some(theme) = map.get("theme").and_then(|s| match s.as_str() { "dark" => Some(Theme::Dark), "light" => Some(Theme::Light), _ => None }) {
        set_theme(theme);
    }
    if let Some(sort_algorithm) = map.get("sort_algorithm").and_then(|s| match s.as_str() { "none" => Some(SortAlgorithm::None), "name" => Some(SortAlgorithm::Name), "type" => Some(SortAlgorithm::Type), _ => None }) {
        set_sort_algorithm(sort_algorithm);
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn write() -> bool {
    let Some(config_dir) = dirs::config_dir() else { return false };
    let _ = std::fs::create_dir(config_dir.join("nbtworkbench"));
    let path = config_dir.join("nbtworkbench/config.txt");
    let Ok(()) = std::fs::write(path, write0()) else { return false };
    true
}

#[cfg(target_arch = "wasm32")]
pub fn write() -> bool {
    let Some(local_storage) = web_sys::window().and_then(|window| window.local_storage().ok()).flatten();
    let value = write0();
    local_storage.set_item("config", &value).is_ok()
}

#[inline]
#[must_use]
fn write0() -> String {
    use std::fmt::Write;
    
    let mut builder = String::new();
    writeln!(&mut builder, "theme={}", match get_theme() { Theme::Light => "light", Theme::Dark => "dark" }).unwrap_or(());
    writeln!(&mut builder, "sort_algorithm={}", match get_sort_algorithm() { SortAlgorithm::None => "none", SortAlgorithm::Name => "name", SortAlgorithm::Type => "type" }).unwrap_or(());
    builder
}

#[inline]
#[must_use]
pub fn get_theme() -> Theme {
    config().theme
}

#[inline]
pub fn set_theme(theme: Theme) -> Theme {
    let old_theme = core::mem::replace(&mut config().theme, theme);
    write();
    old_theme
}


#[must_use]
pub fn get_sort_algorithm() -> SortAlgorithm {
    config().sort_algorithm
}

#[inline]
pub fn set_sort_algorithm(sort_algorithm: SortAlgorithm) -> SortAlgorithm {
    let old_sort_algorithm = core::mem::replace(&mut config().sort_algorithm, sort_algorithm);
    write();
    old_sort_algorithm
}
