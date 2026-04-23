use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};

use fxhash::FxHashMap;
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::error;
use crate::render::widget::replace_box::ReplaceBy;
use crate::render::widget::search_box::{SearchFlags, SearchMode, SearchOperation};
use crate::render::window::Theme;
use crate::workbench::SortAlgorithm;

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

	match try_read_string(&toml_config) {
		Ok(str) => match try_parse_toml(&str) {
			Ok(config) => {
				*CONFIG.write() = config;
				return true;
			}
			Err(e) => error!("Error parsing config.toml: {e}"),
		},
		Err(e) => error!("Error reading TOML config file: {e}"),
	}

	match try_read_string(&txt_config) {
		Ok(str) => match try_parse_txt(&str) {
			Ok(config) => {
				*CONFIG.write() = config;
				return true
			}
			Err(e) => error!("Error parsing TXT config file: {e}"),
		},
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

fn try_read_string(path: &Path) -> Result<String, StringFromFileError> {
	let data = std::fs::read(path)?;
	let data = String::from_utf8(data)?;
	Ok(data)
}

#[derive(Debug, Error)]
enum StringFromFileError {
	#[error(transparent)]
	IO(#[from] std::io::Error),
	#[error(transparent)]
	Utf8(#[from] std::string::FromUtf8Error),
}

fn try_parse_toml(str: &str) -> Result<Config, toml::de::Error> { toml::from_str(str) }

/// For deprecated txt format, no need to update with new fields
fn try_parse_txt(str: &str) -> Result<Config, TxtParseError> {
	let map = str.lines().filter_map(|line| line.split_once('=')).map(|(a, b)| (a.to_owned(), b.to_owned())).collect::<FxHashMap<String, String>>();

	let mut config = Config::default();

	macro_rules! setting {
        ($field:ident <= $name:literal {
	        $($key:literal => $value:expr),* $(,)?
        }) => {
	        if let Some(value) = map.get($name).and_then(|s| match s.as_str() {
		        $($key => Some($value),)*
				_ => None,
			}) {
				config.$field = value;
			}
        };
	}

	setting!(theme <= "theme" { "dark" => Theme::Dark, "light" => Theme::Light });
	setting!(sort_algorithm <= "sort_algorithm" { "none" => SortAlgorithm::None, "name" => SortAlgorithm::Name, "type" => SortAlgorithm::Type });
	setting!(search_mode <= "search_mode" { "string" => SearchMode::String, "regex" => SearchMode::Regex, "snbt" => SearchMode::Snbt });
	setting!(search_flags <= "search_flags" { "key" => SearchFlags::Keys, "value" => SearchFlags::Values, "all" => SearchFlags::KeysValues });
	setting!(search_operation <= "search_operation" { "and" => SearchOperation::And, "or" => SearchOperation::Or, "xor" => SearchOperation::Xor, "b" => SearchOperation::B });
	setting!(replace_by <= "replace_by" { "search_hits" => ReplaceBy::SearchHits, "bookmarked_lines" => ReplaceBy::BookmarkedLines });
	if let Some(search_exact_match) = map.get("search_exact_match").and_then(|s| s.parse::<bool>().ok()) {
		config.search_exact_match = search_exact_match;
	}
	if let Some(scale) = map.get("scale").and_then(|s| s.strip_prefix("Some(")).and_then(|s| s.strip_suffix(")")).and_then(|s| s.parse::<f32>().ok()) {
		config.scale = Some(scale);
	}

	Ok(config)
}

#[derive(Debug, Error)]
enum TxtParseError {}

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
