use std::fmt::Formatter;
use std::fs::{File, read};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use glob::glob;
use winit::dpi::PhysicalSize;

use crate::elements::element::NbtElement;
use crate::history::WorkbenchAction;
use crate::render::widget::replace_box::{ReplaceBox, SearchReplacement};
use crate::render::widget::search_box::{SearchBox, SearchFlags, SearchMode, SearchPredicate, SearchPredicateInner};
use crate::render::window::{WINDOW_HEIGHT, WINDOW_WIDTH};
use crate::util::create_regex;
use crate::workbench::tab::{NbtFileFormat, Tab};
use crate::{config, error, log, mutable_indices};

struct SearchResult {
	path: PathBuf,
	lines: Vec<usize>,
}

impl std::fmt::Display for SearchResult {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		writeln!(f, "Found {n} matches in file {path:?} at line numbers:", n = self.lines.len(), path = self.path)?;
		for &line in &self.lines {
			writeln!(f, "{line}")?;
		}
		Ok(())
	}
}

#[must_use]
fn get_paths(mut args: Vec<String>) -> (PathBuf, Vec<PathBuf>) {
	if args.is_empty() {
		error!("Could not find path argument");
		std::process::exit(1);
	}
	let path = args.remove(0);
	match glob(&path) {
		Ok(paths) => {
			let root = if let Some(astrix_index) = path.bytes().position(|x| x == b'*')
				&& let Some(slash_index) = path.bytes().take(astrix_index).rposition(|x| x == b'/' || x == b'\\')
			{
				PathBuf::from(&path[..=slash_index])
			} else if let Some(slash_index) = path.bytes().rposition(|x| x == b'/' || x == b'\\') {
				PathBuf::from(&path[..=slash_index])
			} else {
				panic!("{path}")
			};
			let paths = paths.filter_map(|result| result.ok()).filter_map(|p| p.strip_prefix(&root).ok().map(|x| x.to_path_buf())).collect::<Vec<_>>();
			(root, paths)
		}
		Err(e) => {
			error!("Glob error: {e}");
			std::process::exit(1);
		}
	}
}

#[must_use]
fn get_search_predicate(args: &mut Vec<String>) -> SearchPredicate {
	let Some(query) = args.pop() else {
		error!("Could not find <query>");
		std::process::exit(0)
	};

	let search_flags = match get_argument_any(&["--search", "-s"], args).as_deref() {
		Some("key") => SearchFlags::Keys,
		Some("value") => SearchFlags::Values,
		Some("any") | None => SearchFlags::KeysValues,
		Some(x) => {
			error!("Invalid search kind '{x}', valid ones are: `key`, `value`, and `any`.");
			std::process::exit(1);
		}
	};

	let exact_match = get_argument_any(&["-em", "--exact-match"], args).is_some();

	match get_argument_any(&["--mode", "-m"], args).as_deref() {
		Some("normal") | None => SearchPredicate {
			search_flags,
			inner: if exact_match {
				SearchPredicateInner::String(query)
			} else {
				SearchPredicateInner::StringCaseInsensitive(query.to_lowercase())
			},
		},
		Some("regex") =>
			if let Some(regex) = create_regex(query, exact_match) {
				SearchPredicate {
					search_flags,
					inner: SearchPredicateInner::Regex(regex),
				}
			} else {
				error!("Invalid regex, valid regexes look like: `/[0-9]+/g`");
				std::process::exit(1);
			},
		Some("snbt") => match NbtElement::from_str(&query) {
			Ok((key, snbt)) => SearchPredicate {
				search_flags,
				inner: if exact_match { SearchPredicateInner::SnbtExactMatch((key, snbt)) } else { SearchPredicateInner::Snbt((key, snbt)) },
			},
			Err(e) => {
				error!(r#"{e}, valid snbt look like: `key:"minecraft:air"` or `{{id:"minecraft:looting",lvl:3s}}` (note that some terminals use "" to contain one parameter and that inner ones will have to be escaped)"#);
				std::process::exit(1);
			}
		},
		Some(x) => {
			error!("Invalid mode '{x}', valid ones are: `normal', `regex`, and `snbt`.");
			std::process::exit(1);
		}
	}
}

#[must_use]
fn get_search_replacement(args: &mut Vec<String>) -> SearchReplacement {
	config::DISABLE_FILE_WRITES.store(true, Ordering::Relaxed);

	let Some(replacement) = args.pop() else {
		error!("Could not find <replace>");
		std::process::exit(0)
	};

	let Some(find) = args.pop() else {
		error!("Could not find <find>");
		std::process::exit(0)
	};

	let search_flags = match get_argument_any(&["--search", "-s"], args).as_deref() {
		Some("key") => SearchFlags::Keys,
		Some("value") => SearchFlags::Values,
		Some("any") | None => SearchFlags::KeysValues,
		Some(x) => {
			error!("Invalid search kind '{x}', valid ones are: `key`, `value`, and `any`.");
			std::process::exit(1);
		}
	};

	let exact_match = get_argument_any(&["-em", "--exact-match"], args).is_some();

	let search_mode = match get_argument_any(&["--mode", "-m"], args).as_deref() {
		Some("normal") | None => SearchMode::String,
		Some("regex") => SearchMode::Regex,
		Some("snbt") => SearchMode::Snbt,
		Some(x) => {
			error!("Invalid mode '{x}', valid ones are: `normal', `regex`, and `snbt`.");
			std::process::exit(1);
		}
	};

	config::set_search_flags(search_flags);
	config::set_search_exact_match(exact_match);
	config::set_search_mode(search_mode);

	match SearchReplacement::new(find, replacement) {
		Some(replacement) => replacement,
		None => {
			error!("Invalid search replacement (your find value was likely invalid)");
			std::process::exit(1);
		}
	}
}

#[must_use]
fn file_size(path: impl AsRef<Path>) -> Option<u64> { File::open(path).ok().and_then(|file| file.metadata().ok()).map(|metadata| metadata.len()) }

fn increment_progress_bar(completed: &AtomicU64, size: u64, total: u64, action: &str) {
	let finished = completed.fetch_add(size, Ordering::Relaxed);
	print!("\r{action}... ({n} / {total} bytes) ({p:.1}% complete)", n = finished, p = 100.0 * finished as f64 / total as f64);
	let _ = std::io::Write::flush(&mut std::io::stdout());
}

#[must_use]
fn get_argument(key: &str, args: &mut Vec<String>) -> Option<String> { Some(args.remove(args.iter().position(|x| x.strip_prefix(key).is_some_and(|x| x.starts_with("=")))?).split_off(key.len() + 1)) }

#[must_use]
fn get_argument_any(keys: &[&str], args: &mut Vec<String>) -> Option<String> { keys.iter().filter_map(|key| get_argument(key, args)).next() }

pub fn find() -> ! {
	let mut args = std::env::args().collect::<Vec<_>>();
	// one for the exe, one for the `find`
	args.drain(..2).for_each(|_| ());

	let predicate = get_search_predicate(&mut args);
	let (root, paths) = get_paths(args);

	let completed = AtomicU64::new(0);
	let total_size = paths.iter().filter_map(file_size).sum::<u64>();

	print!("Searching... (0 / {total_size} bytes) (0.0% complete)");
	let _ = std::io::Write::flush(&mut std::io::stdout());
	let results = std::thread::scope(|s| {
		let mut results = Vec::new();
		for p in paths {
			let mut path = root.clone();
			path.push(p);
			results.push(s.spawn(|| {
				let bytes = match read(&path) {
					Ok(bytes) => bytes,
					Err(e) => {
						error!("File read error: {e}");
						increment_progress_bar(&completed, file_size(&path).unwrap_or(0), total_size, "Searching");
						return None;
					}
				};

				let len = bytes.len() as u64;

				let tab = match Tab::new_from_path(&path, &bytes, PhysicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT)) {
					Ok(tab) => tab,
					Err(e) => {
						error!("File parse error: {e}");
						increment_progress_bar(&completed, len, total_size, "Searching");
						return None;
					}
				};

				let bookmarks = SearchBox::search0(&tab.root, &predicate);

				increment_progress_bar(&completed, len, total_size, "Searching");
				drop(tab);
				if !bookmarks.is_empty() {
					Some(SearchResult {
						path,
						lines: bookmarks.into_iter().map(|x| x.true_line_number()).collect(),
					})
				} else {
					None
				}
			}));
		}

		results.into_iter().filter_map(|x| x.join().ok()).filter_map(std::convert::identity).collect::<Vec<_>>()
	});

	log!("\rSearching ({total_size} / {total_size} bytes) (100.0% complete)");

	if results.is_empty() {
		log!("No results found.")
	}

	for result in results {
		log!("{result}")
	}

	std::process::exit(0);
}

pub fn replace() -> ! {
	let mut args = std::env::args().collect::<Vec<_>>();
	args.drain(..2).for_each(|_| ());

	let replacement = get_search_replacement(&mut args);
	let (root, paths) = get_paths(args);

	let completed = AtomicU64::new(0);
	let total_size = paths.iter().filter_map(file_size).sum::<u64>();

	print!("Replacing... (0 / {total_size} bytes) (0.0% complete)");
	let _ = std::io::Write::flush(&mut std::io::stdout());
	let results = std::thread::scope(|s| {
		let mut results = vec![];
		for p in paths {
			let mut path = root.clone();
			path.push(p);
			results.push(s.spawn(|| {
				let bytes = match read(&path) {
					Ok(bytes) => bytes,
					Err(e) => {
						error!("File read error: {e}");
						increment_progress_bar(&completed, file_size(&path).unwrap_or(0), total_size, "Replacing");
						return None;
					}
				};

				let len = bytes.len() as u64;

				let mut tab = match Tab::new_from_path(&path, &bytes, PhysicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT)) {
					Ok(tab) => tab,
					Err(e) => {
						error!("File parse error: {e}");
						increment_progress_bar(&completed, len, total_size, "Replacing");
						return None;
					}
				};

				let (bulk, errors) = ReplaceBox::replace_by_search_box0(mutable_indices!(tab), &mut tab.root, &replacement);
				for e in errors {
					error!("Error while replacing line: {e}");
				}
				let actions = if let WorkbenchAction::Bulk { actions } = &bulk { actions.len() } else { 0 };

				if let Err(e) = tab.save(false) {
					error!("File write error: {e}");
				}

				increment_progress_bar(&completed, len, total_size, "Replacing");

				drop(bulk);
				drop(tab);

				Some((path, actions))
			}));
		}
		results.into_iter().filter_map(|x| x.join().ok()).filter_map(std::convert::identity).collect::<Vec<_>>()
	});

	log!("\rReplacing ({total_size} / {total_size} bytes) (100.0% complete)");

	if results.is_empty() {
		log!("No changes made.")
	}

	for (path, replacements) in results {
		log!(
			"{path_str} had {replacements} replacement{replacement_suffix}",
			replacement_suffix = if replacements == 1 { "" } else { "s" },
			path_str = path.to_string_lossy()
		);
	}

	std::process::exit(0)
}

pub fn reformat() -> ! {
	let mut args = std::env::args().collect::<Vec<_>>();
	args.drain(..2);

	let format_arg = get_argument_any(&["--format", "-f"], &mut args);
	let (extension, format) = match format_arg.as_deref() {
		Some(x @ "nbt") => (x, NbtFileFormat::Nbt),
		Some(x @ ("dat" | "dat_old" | "gzip")) => (if x == "gzip" { "dat" } else { x }, NbtFileFormat::Gzip),
		Some(x @ "zlib") => (x, NbtFileFormat::Zlib),
		Some(x @ "snbt") => (x, NbtFileFormat::Snbt),
		Some(x @ ("lnbt" | "lhnbt")) => ("nbt", if x == "lnbt" { NbtFileFormat::LittleEndianNbt } else { NbtFileFormat::LittleEndianHeaderNbt }),
		None => {
			error!("`--format` not specified.");
			std::process::exit(1);
		}
		Some(x) => {
			error!("Invalid format '{x}'");
			std::process::exit(1);
		}
	};

	let extension = if let Some(extension) = get_argument_any(&["--out-ext", "-e"], &mut args) { extension } else { extension.to_owned() };

	let out_dir = get_argument_any(&["--out-dir", "-d"], &mut args).map(PathBuf::from);

	let (root, paths) = get_paths(args);

	let completed = AtomicU64::new(0);
	let total_size = paths.iter().filter_map(file_size).sum::<u64>();

	print!("Reformatting... (0 / {total_size} bytes) (0.0% complete)");
	let _ = std::io::Write::flush(&mut std::io::stdout());
	std::thread::scope(|s| {
		for p in paths {
			let mut pa = root.clone();
			pa.push(&p);
			s.spawn(|| 'a: {
				let p = p;
				let path = pa;

				let bytes = match read(&path) {
					Ok(bytes) => bytes,
					Err(e) => {
						error!("File read error: {e}");
						increment_progress_bar(&completed, file_size(&path).unwrap_or(0), total_size, "Reformatting");
						break 'a;
					}
				};

				let len = bytes.len() as u64;

				let tab = match Tab::new_from_path(&path, &bytes, PhysicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT)) {
					Ok(tab) => tab,
					Err(e) => {
						error!("File parse error: {e}");
						increment_progress_bar(&completed, len, total_size, "Replacing");
						break 'a;
					}
				};

				let out = format.encode(&tab.root);

				let name = path.file_stem().expect("File must have stem").to_string_lossy().into_owned() + "." + &extension;

				let mut new_path = if let Some(out_dir) = out_dir.as_deref() { out_dir.to_path_buf() } else { root.to_path_buf() };
				new_path.push(p);
				let new_path = new_path.with_file_name(&name);
				if let Err(e) = std::fs::create_dir_all(&new_path) {
					error!("File directory creation error: {e}")
				}

				if let Err(e) = std::fs::write(new_path, out) {
					error!("File write error: {e}")
				}

				increment_progress_bar(&completed, len, total_size, "Reformatting");

				drop(tab);
			});
		}
	});

	log!("\rReformatting ({total_size} / {total_size} bytes) (100.0% complete)");

	std::process::exit(0);
}

pub fn help() -> ! {
	println!(
		r#"
Usage:
  nbtworkbench --version|-v
  nbtworkbench -?|-h|--help|/?
  nbtworkbench find <path> [(--mode|-m)=(normal|regex|snbt)] [(--search|-s)=(key|value|any)] [--exact-match|-em] <query>
  nbtworkbench reformat (--format|-f)=<format> [(--out-dir|-d)=<out-dir>] [(--out-ext|-e)=<out-ext>] <path>
  nbtworkbench replace <path> [(--mode|-m)=(normal|regex|snbt)] [(--search|-s)=(key|value|any)] [--exact-match|-em] <find> "<replace>"

Options:
  --version, -v       Displays the version of nbtworkbench you're running.
  -?, -h, --help, /?  Displays this dialog.
  --mode, -m          Changes the `find` mode to take the <query> field as either, a containing substring, a regex (match whole), or snbt. [default: normal]
  --search, -s        Searches for results matching the <query> in either, the key, the value, or both (note that substrings and regex search the same pattern in both key and value, while the regex uses it's key field to match equal strings). [default: any]
  --format, -f        Specifies the format to be reformatted to; either `nbt`, `snbt`, `dat/dat_old/gzip`, `zlib`, 'lnbt' (little endian nbt), or 'lhnbt' (little endian nbt with header).
  --out-dir, -d       Specifies the output directory. [default: ./]
  --out-ext, -e       Specifies the output file extension (if not specified, it will infer from --format)"#
	);

	std::process::exit(0);
}
