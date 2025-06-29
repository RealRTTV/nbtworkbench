use std::{
	path::Path,
	sync::atomic::{AtomicBool, Ordering},
};

use anyhow::{Result, ensure};
use fxhash::FxHashMap;
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};

use crate::{
	error,
	render::{
		widget::{
			replace_box::ReplaceBy,
			search_box::{SearchFlags, SearchMode, SearchOperation},
		},
		window::Theme,
	},
	workbench::SortAlgorithm,
};

#[derive(Serialize, Deserialize, Default)]
struct Config {
	#[serde(default)]
	theme: Theme,

	#[serde(default)]
	sort_algorithm: SortAlgorithm,

	#[serde(default)]
	search_mode: SearchMode,

	#[serde(default)]
	search_flags: SearchFlags,

	#[serde(default)]
	search_operation: SearchOperation,

	#[serde(default)]
	replace_by: ReplaceBy,

	#[serde(default)]
	search_exact_match: bool,

	#[serde(default)]
	scale: Option<f32>,
}

pub static DISABLE_FILE_WRITES: AtomicBool = AtomicBool::new(false);

static CONFIG: RwLock<Config> = RwLock::new(Config {
	theme: Theme::Dark,
	sort_algorithm: SortAlgorithm::Type,
	search_mode: SearchMode::String,
	search_flags: SearchFlags::Values,
	search_operation: SearchOperation::B,
	replace_by: ReplaceBy::SearchHits,
	search_exact_match: false,
	scale: None,
});

#[cfg(not(target_arch = "wasm32"))]
pub fn read() -> bool {
	let Some(config_dir) = dirs::config_dir() else { return false };
	let txt_config = config_dir.join("nbtworkbench/config.txt");
	let toml_config = config_dir.join("nbtworkbench/config.toml");

	match try_read_string(&toml_config).and_then(|str| try_parse_toml(&str)) {
		Ok(config) => {
			*CONFIG.write() = config;
			return true
		}
		Err(e) => error!("Error reading TOML config file: {e}"),
	}

	match try_read_string(&txt_config).and_then(|str| try_parse_txt(&str)) {
		Ok(config) => {
			*CONFIG.write() = config;
			return true
		}
		Err(e) => error!("Error reading TXT config file: {e}"),
	}

	false
}

#[cfg(target_arch = "wasm32")]
pub fn read() -> bool {
	let local_storage = web_sys::window().and_then(|window| window.local_storage().ok()).flatten();

	match local_storage.get_item("config_toml").context("Could not find config toml local storage").and_then(|str| try_parse_toml(&str)) {
		Ok(config) => {
			*CONFIG.write() = config;
			return true
		}
		Err(e) => error!("Error reading TOML config: {e}"),
	}

	match local_storage.get_item("config").context("Could not find config txt local storage").and_then(|str| try_parse_toml(&str)) {
		Ok(config) => {
			*CONFIG.write() = config;
			return true
		}
		Err(e) => error!("Error reading TXT config: {e}"),
	}

	false
}

fn try_read_string(path: &Path) -> Result<String> {
	ensure!(std::fs::exists(path)?, "File does not exist");
	let data = std::fs::read(path)?;
	let data = String::from_utf8(data)?;
	Ok(data)
}

fn try_parse_toml(str: &str) -> Result<Config> {
	let config: Config = toml::from_str(str)?;
	Ok(config)
}

fn try_parse_txt(str: &str) -> Result<Config> {
	let map = str.lines().filter_map(|line| line.split_once('=')).map(|(a, b)| (a.to_owned(), b.to_owned())).collect::<FxHashMap<String, String>>();

	let mut config = Config::default();

	if let Some(theme) = map.get("theme").and_then(|s| match s.as_str() {
		"dark" => Some(Theme::Dark),
		"light" => Some(Theme::Light),
		_ => None,
	}) {
		config.theme = theme;
	}
	if let Some(sort_algorithm) = map.get("sort_algorithm").and_then(|s| match s.as_str() {
		"none" => Some(SortAlgorithm::None),
		"name" => Some(SortAlgorithm::Name),
		"type" => Some(SortAlgorithm::Type),
		_ => None,
	}) {
		config.sort_algorithm = sort_algorithm;
	}
	if let Some(search_mode) = map.get("search_mode").and_then(|s| match s.as_str() {
		"string" => Some(SearchMode::String),
		"regex" => Some(SearchMode::Regex),
		"snbt" => Some(SearchMode::Snbt),
		_ => None,
	}) {
		config.search_mode = search_mode;
	}
	if let Some(search_flags) = map.get("search_flags").and_then(|s| match s.as_str() {
		"key" => Some(SearchFlags::Keys),
		"value" => Some(SearchFlags::Values),
		"all" => Some(SearchFlags::KeysValues),
		_ => None,
	}) {
		config.search_flags = search_flags;
	}
	if let Some(search_operation) = map.get("search_operation").and_then(|s| match s.as_str() {
		"and" => Some(SearchOperation::And),
		"or" => Some(SearchOperation::Or),
		"xor" => Some(SearchOperation::Xor),
		"b" => Some(SearchOperation::B),
		_ => None,
	}) {
		config.search_operation = search_operation;
	}
	if let Some(replace_by) = map.get("replace_by").and_then(|s| match s.as_str() {
		"search_hits" => Some(ReplaceBy::SearchHits),
		"bookmarked_lines" => Some(ReplaceBy::BookmarkedLines),
		_ => None,
	}) {
		config.replace_by = replace_by;
	}
	if let Some(search_exact_match) = map.get("search_exact_match").and_then(|s| s.parse::<bool>().ok()) {
		config.search_exact_match = search_exact_match;
	}
	if let Some(scale) = map.get("scale").and_then(|s| s.strip_prefix("Some(")).and_then(|s| s.strip_suffix(")")).and_then(|s| s.parse::<f32>().ok()) {
		config.scale = Some(scale);
	}

	Ok(config)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn write() -> bool {
	if DISABLE_FILE_WRITES.load(Ordering::Relaxed) {
		return true
	}

	let Some(config_dir) = dirs::config_dir() else { return false };
	let _ = std::fs::create_dir(config_dir.join("nbtworkbench"));
	let path = config_dir.join("nbtworkbench/config.toml");
	let Ok(data) = toml::to_string_pretty(&*CONFIG.read()) else { return false };
	let Ok(()) = std::fs::write(path, data) else { return false };
	true
}

#[cfg(target_arch = "wasm32")]
pub fn write() -> bool {
	if DISABLE_FILE_WRITES.load(Ordering::Relaxed) {
		return true
	}

	let Some(local_storage) = web_sys::window().and_then(|window| window.local_storage().ok()).flatten() else { return false };
	let Ok(value) = toml::to_string(&*CONFIG.read()) else { return false };
	local_storage.set_item("config_toml", &value).is_ok()
}

#[must_use]
pub fn get_theme() -> Theme { CONFIG.read().theme }

pub fn set_theme(theme: Theme) -> Theme {
	let old_theme = core::mem::replace(&mut CONFIG.write().theme, theme);
	write();
	old_theme
}

#[must_use]
pub fn get_sort_algorithm() -> SortAlgorithm { CONFIG.read().sort_algorithm }

pub fn set_sort_algorithm(sort_algorithm: SortAlgorithm) -> SortAlgorithm {
	let old_sort_algorithm = core::mem::replace(&mut CONFIG.write().sort_algorithm, sort_algorithm);
	write();
	old_sort_algorithm
}

#[must_use]
pub fn get_search_mode() -> SearchMode { CONFIG.read().search_mode }

pub fn set_search_mode(search_mode: SearchMode) -> SearchMode {
	let old_search_mode = core::mem::replace(&mut CONFIG.write().search_mode, search_mode);
	write();
	old_search_mode
}

#[must_use]
pub fn get_search_flags() -> SearchFlags { CONFIG.read().search_flags }

pub fn set_search_flags(search_flags: SearchFlags) -> SearchFlags {
	let old_search_flags = core::mem::replace(&mut CONFIG.write().search_flags, search_flags);
	write();
	old_search_flags
}

#[must_use]
pub fn get_search_operation() -> SearchOperation { CONFIG.read().search_operation }

pub fn set_search_operation(search_operation: SearchOperation) -> SearchOperation {
	let old_search_operation = core::mem::replace(&mut CONFIG.write().search_operation, search_operation);
	write();
	old_search_operation
}

#[must_use]
pub fn get_replace_by() -> ReplaceBy { CONFIG.read().replace_by }

pub fn set_replace_by(replace_by: ReplaceBy) -> ReplaceBy {
	let old_replace_by = core::mem::replace(&mut CONFIG.write().replace_by, replace_by);
	write();
	old_replace_by
}

#[must_use]
pub fn get_search_exact_match() -> bool { CONFIG.read().search_exact_match }

pub fn set_search_exact_match(search_exact_match: bool) -> bool {
	let old_search_exact_match = core::mem::replace(&mut CONFIG.write().search_exact_match, search_exact_match);
	write();
	old_search_exact_match
}

#[must_use]
pub fn get_scale() -> Option<f32> { CONFIG.read().scale }

pub fn set_scale(scale: Option<f32>) -> Option<f32> {
	let old_scale = core::mem::replace(&mut CONFIG.write().scale, scale);
	write();
	old_scale
}
