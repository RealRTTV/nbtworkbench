use fxhash::FxHashMap;
use winit::window::Theme;
use crate::search_box::{SearchFlags, SearchMode};

use crate::SortAlgorithm;

struct Config {
    theme: Theme,
    sort_algorithm: SortAlgorithm,
    search_mode: SearchMode,
    search_flags: SearchFlags,
}

static mut CONFIG: Config = Config {
    theme: Theme::Dark,
    sort_algorithm: SortAlgorithm::Type,
    search_mode: SearchMode::String,
    search_flags: SearchFlags::Values,
};

#[cfg(not(target_arch = "wasm32"))]
pub fn read() -> bool {
    let Some(config_dir) = dirs::config_dir() else { return false };
    let path = config_dir.join("nbtworkbench/config.txt");
    let Some(map) = std::fs::read(path).ok().and_then(|vec| String::from_utf8(vec).ok()).map(|str| parse_lines(&str)) else { return false };
    read0(&map);
    true
}

#[cfg(target_arch = "wasm32")]
pub fn read() -> bool {
    let Some(map) = web_sys::window().and_then(|window| window.local_storage().ok()).flatten().and_then(|storage| storage.get_item("config").ok()).flatten().map(|str| parse_lines(&str)) else { return false };
    read0(&map);
    true
}

fn parse_lines(str: &str) -> FxHashMap<String, String> {
    str.lines().filter_map(|line| line.split_once('=')).map(|(a, b)| (a.to_owned(), b.to_owned())).collect::<FxHashMap<String, String>>()
}

#[inline]
fn read0(map: &FxHashMap<String, String>) {
    if let Some(theme) = map.get("theme").and_then(|s| match s.as_str() { "dark" => Some(Theme::Dark), "light" => Some(Theme::Light), _ => None }) {
        set_theme(theme);
    }
    if let Some(sort_algorithm) = map.get("sort_algorithm").and_then(|s| match s.as_str() { "none" => Some(SortAlgorithm::None), "name" => Some(SortAlgorithm::Name), "type" => Some(SortAlgorithm::Type), _ => None }) {
        set_sort_algorithm(sort_algorithm);
    }
    if let Some(search_mode) = map.get("search_mode").and_then(|s| match s.as_str() { "string" => Some(SearchMode::String), "regex" => Some(SearchMode::Regex), "snbt" => Some(SearchMode::Snbt), _ => None }) {
        set_search_mode(search_mode);
    }
    if let Some(search_flags) = map.get("search_flags").and_then(|s| match s.as_str() { "key" => Some(SearchFlags::Keys), "value" => Some(SearchFlags::Values), "all" => Some(SearchFlags::KeysValues), _ => None }) {
        set_search_flags(search_flags);
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
    let Some(local_storage) = web_sys::window().and_then(|window| window.local_storage().ok()).flatten() else { return false };
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
    writeln!(&mut builder, "search_mode={}", match get_search_mode() { SearchMode::String => "string", SearchMode::Regex => "regex", SearchMode::Snbt => "snbt" }).unwrap_or(());
    writeln!(&mut builder, "search_flags={}", match get_search_flags() { SearchFlags::Keys => "key", SearchFlags::Values => "value", SearchFlags::KeysValues => "all" }).unwrap_or(());
    builder
}

#[inline]
#[must_use]
pub fn get_theme() -> Theme {
    unsafe { CONFIG.theme }
}

#[inline]
pub fn set_theme(theme: Theme) -> Theme {
    let old_theme = unsafe { core::mem::replace(&mut CONFIG.theme, theme) };
    write();
    old_theme
}


#[must_use]
pub fn get_sort_algorithm() -> SortAlgorithm {
    unsafe { CONFIG.sort_algorithm }
}

#[inline]
pub fn set_sort_algorithm(sort_algorithm: SortAlgorithm) -> SortAlgorithm {
    let old_sort_algorithm = unsafe { core::mem::replace(&mut CONFIG.sort_algorithm, sort_algorithm) };
    write();
    old_sort_algorithm
}

#[must_use]
pub fn get_search_mode() -> SearchMode {
    unsafe { CONFIG.search_mode }
}

#[inline]
pub fn set_search_mode(search_mode: SearchMode) -> SearchMode {
    let old_search_mode = unsafe { core::mem::replace(&mut CONFIG.search_mode, search_mode) };
    write();
    old_search_mode
}

#[must_use]
pub fn get_search_flags() -> SearchFlags {
    unsafe { CONFIG.search_flags }
}

#[inline]
pub fn set_search_flags(search_flags: SearchFlags) -> SearchFlags {
    let old_search_flags = unsafe { core::mem::replace(&mut CONFIG.search_flags, search_flags) };
    write();
    old_search_flags
}
