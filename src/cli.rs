use std::fmt::Formatter;
use std::fs::{read, File};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use glob::glob;

use crate::elements::NbtElement;
use crate::render::WindowProperties;
use crate::util::{create_regex, drop_on_separate_thread};
use crate::widget::{SearchBox, SearchFlags, SearchPredicate, SearchPredicateInner};
use crate::workbench::FileFormat;
use crate::workbench::Workbench;
use crate::{error, log};

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

#[must_use]
fn get_predicate(args: &mut Vec<String>) -> SearchPredicate {
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
            inner: if exact_match { SearchPredicateInner::String(query) } else { SearchPredicateInner::StringCaseInsensitive(query.to_lowercase()) },
        },
        Some("regex") => if let Some(regex) = create_regex(query, exact_match) {
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
            Err(idx) => {
                error!(r#"Invalid snbt at index {idx}, valid snbt look like: `key:"minecraft:air"` or `{{id:"minecraft:looting",lvl:3s}}` (note that some terminals use "" to contain one parameter and that inner ones will have to be escaped)"#);
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
fn file_size(path: impl AsRef<Path>) -> Option<u64> {
    File::open(path).ok().and_then(|file| file.metadata().ok()).map(|metadata| metadata.len())
}

fn increment_progress_bar(completed: &AtomicU64, size: u64, total: u64, action: &str) {
    let finished = completed.fetch_add(size, Ordering::Relaxed);
    print!("\r{action}... ({n} / {total} bytes) ({p:.1}% complete)", n = finished, p = 100.0 * finished as f64 / total as f64);
    let _ = std::io::Write::flush(&mut std::io::stdout());
}

#[must_use]
fn get_argument(key: &str, args: &mut Vec<String>) -> Option<String> {
    Some(args.remove(args.iter().position(|x| x.strip_prefix(key).is_some_and(|x| x.starts_with("=")))?).split_off(key.len() + 1))
}

#[must_use]
fn get_argument_any(keys: &[&str], args: &mut Vec<String>) -> Option<String> {
    keys.iter().filter_map(|key| get_argument(key, args)).next()
}

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
                let mut workbench = Workbench::new(&mut WindowProperties::Fake, None);
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
                drop_on_separate_thread(tab);
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

pub fn reformat() -> ! {
    let mut args = std::env::args().collect::<Vec<_>>();
    args.drain(..2);

    let format_arg = get_argument_any(&["--format", "-f"], &mut args);
    let (extension, format) = match format_arg.as_deref() {
        Some(x @ "nbt") => (x, FileFormat::Nbt),
        Some(x @ ("dat" | "dat_old" | "gzip")) => (if x == "gzip" { "dat" } else { x }, FileFormat::Gzip),
        Some(x @ "zlib") => (x, FileFormat::Zlib),
        Some(x @ "snbt") => (x, FileFormat::Snbt),
        Some(x @ ("lnbt" | "lhnbt")) => ("nbt", if x == "lnbt" { FileFormat::LittleEndianNbt } else { FileFormat::LittleEndianHeaderNbt }),
        None => {
            error!("`--format` not specified.");
            std::process::exit(1);
        }
        Some(x) => {
            error!("Invalid format '{x}'");
            std::process::exit(1);
        }
    };

    let extension = if let Some(extension) = get_argument_any(&["--out-ext", "-e"], &mut args) {
        extension
    } else {
        extension.to_owned()
    };

    let out_dir = if let Some(out_dir) = get_argument_any(&["--out-dir", "-d"], &mut args) {
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
                let mut workbench = Workbench::new(&mut WindowProperties::Fake, None);
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
                if let FileFormat::Nbt | FileFormat::Snbt | FileFormat::Gzip | FileFormat::Zlib = tab.format() {} else {
                    error!("Tab had invalid file format {}", tab.format().to_string());
                }

                let out = format.encode(&tab.value);
                drop_on_separate_thread(tab);

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

pub fn help() -> ! {
    println!(r#"
Usage:
  nbtworkbench --version|-v
  nbtworkbench -?|-h|--help|/?
  nbtworkbench find <path> [(--mode|-m)=(normal|regex|snbt)] [(--search|-s)=(key|value|any)] [--exact-match|-em] <query>
  nbtworkbench reformat (--format|-f)=<format> [(--out-dir|-d)=<out-dir>] [(--out-ext|-e)=<out-ext>] <path>

Options:
  --version, -v       Displays the version of nbtworkbench you're running.
  -?, -h, --help, /?  Displays this dialog.
  --mode, -m          Changes the `find` mode to take the <query> field as either, a containing substring, a regex (match whole), or snbt. [default: normal]
  --search, -s        Searches for results matching the <query> in either, the key, the value, or both (note that substrings and regex search the same pattern in both key and value, while the regex uses it's key field to match equal strings). [default: any]
  --format, -f        Specifies the format to be reformatted to; either `nbt`, `snbt`, `dat/dat_old/gzip`, `zlib`, 'lnbt' (little endian nbt), or 'lhnbt' (little endian nbt with header).
  --out-dir, -d       Specifies the output directory. [default: ./]
  --out-ext, -e       Specifies the output file extension (if not specified, it will infer from --format)"#);

    std::process::exit(0);
}
