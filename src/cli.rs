use std::fmt::Formatter;
use std::fs::{File, read};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use compact_str::CompactString;

use glob::glob;

use crate::{create_regex, error, log, SortAlgorithm, WindowProperties};
use crate::elements::element::NbtElement;
use crate::search_box::{SearchBox, SearchPredicate, SearchPredicateInner};
use crate::tab::FileFormat;
use crate::workbench::Workbench;

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

fn get_paths(mut args: Vec<String>) -> (PathBuf, Vec<PathBuf>) {
    if args.is_empty() {
        error!("Could not find path argument");
        std::process::exit(1);
    }
    let path = args.remove(0);
    match glob(&path) {
        Ok(paths) => {
            let root = if let Some(astrix_index) = path.bytes().position(|x| x == b'*') && let Some(slash_index) = path.bytes().take(astrix_index).rposition(|x| x == b'/' || x== b'\\') {
                PathBuf::from(&path[..=slash_index])
            } else if let Some(slash_index) = path.bytes().rposition(|x| x == b'/' || x== b'\\') {
                PathBuf::from(&path[..=slash_index])
            } else {
                panic!("{path}")
            };
            let paths = paths.filter_map(|result| result.ok()).filter_map(|p| p.strip_prefix(&root).ok().map(|x| x.to_path_buf())).collect::<Vec<_>>();
            (root, paths)
        },
        Err(e) => {
            error!("Glob error: {e}");
            std::process::exit(1);
        }
    }
}

fn get_predicate(args: &mut Vec<String>) -> SearchPredicate {
    let Some(query) = args.pop() else {
        error!("Could not find <query>");
        std::process::exit(0)
    };

    let search_flags = match get_argument("--search", args).or_else(|| get_argument("-s", args)).as_deref() {
        Some("key") => 0b10_u8,
        Some("value") => 0b01_u8,
        Some("all") | None => 0b11_u8,
        Some(x) => {
            error!("Invalid search kind '{x}', valid ones are: `key`, `value`, and `all`.");
            std::process::exit(1);
        }
    };

    match get_argument("--mode", args).or_else(|| get_argument("-m", args)).as_deref() {
        Some("normal") | None => SearchPredicate {
            search_flags,
            inner: SearchPredicateInner::String(query),
        },
        Some("regex") => if let Some(regex) = create_regex(query) {
            SearchPredicate {
                search_flags,
                inner: SearchPredicateInner::Regex(regex),
            }
        } else {
            error!("Invalid regex, valid regexes look like: `/[0-9]+/g`");
            std::process::exit(1);
        },
        Some("snbt") => if let Some((key, snbt)) = NbtElement::from_str(&query, SortAlgorithm::Name) {
            SearchPredicate {
                search_flags,
                inner: SearchPredicateInner::Snbt(key.map(CompactString::into_string), snbt),
            }
        } else {
            error!(r#"Invalid snbt, valid snbt look like: `key:"minecraft:air"` or `{{id:"minecraft:looting",lvl:3s}}` (note that some terminals use "" to contain one parameter and that inner ones will have to be escaped)"#);
            std::process::exit(1);
        },
        Some(x) => {
            error!("Invalid mode '{x}', valid ones are: `normal', `regex`, and `snbt`.");
            std::process::exit(1);
        }
    }
}

fn file_size(path: impl AsRef<Path>) -> Option<u64> {
    File::open(path).ok().and_then(|file| file.metadata().ok()).map(|metadata| metadata.len())
}

fn increment_progress_bar(completed: &AtomicU64, size: u64, total: u64, action: &str) {
    let finished = completed.fetch_add(size, Ordering::Relaxed);
    print!("\r{action}... ({n} / {total} bytes) ({p:.1}% complete)", n = finished, p = 100.0 * finished as f64 / total as f64);
    let _ = std::io::Write::flush(&mut std::io::stdout());
}

fn get_argument(key: &str, args: &mut Vec<String>) -> Option<String> {
    Some(args.remove(args.iter().position(|x| x.strip_prefix(key).is_some_and(|x| x.starts_with("=")))?).split_off(key.len() + 1))
}

#[inline]
pub fn find() -> ! {
    let mut args = std::env::args().collect::<Vec<_>>();
    // one for the exe, one for the `find`
    args.drain(..2).for_each(|_| ());

    let predicate = get_predicate(&mut args);
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
            results.push(s.spawn(|| 'a: {
                let mut workbench = Workbench::new(&mut WindowProperties::Fake);
                workbench.tabs.clear();

                let bytes = match read(&path) {
                    Ok(bytes) => bytes,
                    Err(e) => {
                        error!("File read error: {e}");
                        increment_progress_bar(&completed, file_size(&path).unwrap_or(0), total_size, "Searching");
                        break 'a None;
                    }
                };

                let len = bytes.len() as u64;

                if let Err(e) = workbench.on_open_file(&path, bytes, &mut WindowProperties::Fake) {
                    error!("File parse error: {e}");
                    increment_progress_bar(&completed, len, total_size, "Searching");
                    break 'a None;
                }

                let tab = workbench.tabs.remove(0);
                let bookmarks = SearchBox::search0(&tab.value, &predicate);
                std::thread::Builder::new().stack_size(50_331_648 /*48MiB*/).spawn(move || drop(tab)).expect("Failed to spawn thread");
                increment_progress_bar(&completed, len, total_size, "Searching");
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

#[inline]
pub fn reformat() -> ! {
    let mut args = std::env::args().collect::<Vec<_>>();
    args.drain(..2);

    let format_arg = get_argument("--format", &mut args).or_else(|| get_argument("-f", &mut args));
    let (extension, format) = match format_arg.as_deref() {
        Some(x @ "nbt") => (x, FileFormat::Nbt),
        Some(x @ ("dat" | "dat_old" | "gzip")) => (if x == "gzip" { "dat" } else { x }, FileFormat::Gzip),
        Some(x @ "zlib") => (x, FileFormat::Zlib),
        Some(x @ "snbt") => (x, FileFormat::Snbt),
        None => {
            error!("`--format` not specified.");
            std::process::exit(1);
        }
        Some(x) => {
            error!("Invalid format '{x}'");
            std::process::exit(1);
        }
    };

    let extension = if let Some(extension) = get_argument("--out-ext", &mut args).or_else(|| get_argument("-e", &mut args)) {
        extension
    } else {
        extension.to_owned()
    };

    let out_dir = if let Some(out_dir) = get_argument("--out-dir", &mut args).or_else(|| get_argument("-d", &mut args)) {
        Some(PathBuf::from(out_dir))
    } else {
        None
    };

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
                let mut workbench = Workbench::new(&mut WindowProperties::Fake);
                workbench.tabs.clear();

                let bytes = match read(&path) {
                    Ok(bytes) => bytes,
                    Err(e) => {
                        error!("File read error: {e}");
                        increment_progress_bar(&completed, file_size(&path).unwrap_or(0), total_size, "Reformatting");
                        break 'a;
                    }
                };

                let len = bytes.len() as u64;

                if let Err(e) = workbench.on_open_file(&path, bytes, &mut WindowProperties::Fake) {
                    error!("File parse error: {e}");
                    increment_progress_bar(&completed, len, total_size, "Reformatting");
                    break 'a;
                }

                let tab = workbench.tabs.remove(0);
                if let FileFormat::Nbt | FileFormat::Snbt | FileFormat::Gzip | FileFormat::Zlib = tab.compression {} else {
                    error!("Tab had invalid file format {}", tab.compression.to_string());
                }

                let out = format.encode(&tab.value);
                std::thread::Builder::new().stack_size(50_331_648 /*48MiB*/).spawn(move || drop(tab)).expect("Failed to spawn thread");

                let name = path.file_stem().expect("File must have stem").to_string_lossy().into_owned() + "." + &extension;

                let mut new_path = if let Some(out_dir) = out_dir.as_deref() {
                    out_dir.to_path_buf()
                } else {
                    root.to_path_buf()
                };
                new_path.push(p);
                let new_path = new_path.with_file_name(&name);
                if let Err(e) = std::fs::create_dir_all(&new_path) {
                    error!("File directory creation error: {e}")
                }

                if let Err(e) = std::fs::write(new_path, out) {
                    error!("File write error: {e}")
                }

                increment_progress_bar(&completed, len, total_size, "Reformatting");
            });
        }
    });

    log!("\rReformatting ({total_size} / {total_size} bytes) (100.0% complete)");

    std::process::exit(0);
}
