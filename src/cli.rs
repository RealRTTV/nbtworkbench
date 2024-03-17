use std::fmt::Formatter;
use std::fs::{File, OpenOptions, read};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use glob::glob;
use regex::{Regex, RegexBuilder};

use crate::{error, log, SortAlgorithm, WindowProperties};
use crate::elements::element::NbtElement;
use crate::search_box::{SearchBox, SearchPredicate};
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

fn create_regex(mut str: String) -> Option<Regex> {
    if !str.starts_with("/") {
        return None
    }

    str = str.split_off(1);

    let mut flags = 0_u8;
    while let Some(char) = str.pop() {
        match char {
            'i' => flags |= 0b000001,
            'g' => flags |= 0b000010,
            'm' => flags |= 0b000100,
            's' => flags |= 0b001000,
            'u' => flags |= 0b010000,
            'y' => flags |= 0b100000,
            '/' => break,
            _ => return None
        }
    }

    RegexBuilder::new(&str)
        .case_insensitive(flags & 0b1 > 0)
        .multi_line(flags & 0b100 > 0)
        .dot_matches_new_line(flags & 0b1000 > 0)
        .unicode(flags & 0b10000 > 0)
        .swap_greed(flags & 0b10000 > 0)
        .build().ok()
}

fn get_paths(args: &mut Vec<String>) -> Vec<PathBuf> {
    if args.is_empty() {
        error!("Could not find path argument");
        std::process::exit(1);
    }
    let path = args.remove(0);
    match glob(&path) {
        Ok(paths) => paths.filter_map(|result| result.ok()).collect::<Vec<_>>(),
        Err(e) => {
            error!("Glob error: {e}");
            std::process::exit(1);
        }
    }
}

fn get_predicate(mut args: Vec<String>) -> SearchPredicate {
    let snbt = {
        if let Some("-s" | "--snbt") = args.get(0).map(String::as_str) {
            args.remove(0);
            true
        } else if let Some("-s" | "--snbt") = args.get(1).map(String::as_str) {
            args.remove(1);
            true
        } else {
            false
        }
    };

    let predicate = args.as_slice().join(" ");
    if predicate.is_empty() {
        error!("Predicate cannot be empty");
        std::process::exit(1);
    }
    if snbt && let Some((key, snbt)) = NbtElement::from_str(&predicate, SortAlgorithm::None) {
        SearchPredicate::Snbt(key.map(|x| x.into_string()), snbt)
    } else if let Some(regex) = create_regex(predicate.clone()) {
        SearchPredicate::Regex(regex)
    } else {
        SearchPredicate::String(predicate)
    }
}

fn file_size(path: impl AsRef<Path>) -> Option<u64> {
    File::open(path).ok().and_then(|file| file.metadata().ok()).map(|metadata| metadata.len() as u64)
}

fn increment_progress_bar(completed: &AtomicU64, size: u64, total: u64) {
    let finished = completed.fetch_add(size, Ordering::Relaxed);
    print!("\rSearching... ({n} / {total} bytes) ({p:.1}% complete)", n = finished, p = 100.0 * finished as f64 / total as f64);
    let _ = std::io::Write::flush(&mut std::io::stdout());
}

#[inline]
#[cfg(not(target_arch = "wasm32"))]
pub fn find() -> ! {
    let mut args = std::env::args().collect::<Vec<_>>();
    // one for the exe, one for the `find`
    args.drain(..2).for_each(|_| ());

    let paths = get_paths(&mut args);
    let predicate = get_predicate(args);

    let completed = AtomicU64::new(0);
    let total_size = paths.iter().filter_map(file_size).sum::<u64>();

    print!("Searching... (0 / {total_size} bytes) (0.0% complete)");
    let _ = std::io::Write::flush(&mut std::io::stdout());
    let results = std::thread::scope(|s| {
        let mut results = Vec::new();
        for path in paths {
            results.push(s.spawn(|| 'a: {
                let mut workbench = Workbench::new(&mut WindowProperties::Fake);
                workbench.tabs.clear();

                let bytes = match read(&path) {
                    Ok(bytes) => bytes,
                    Err(e) => {
                        error!("File read error: {e}");
                        increment_progress_bar(&completed, file_size(&path).unwrap_or(0), total_size);
                        break 'a None
                    }
                };

                let len = bytes.len() as u64;

                if let Err(e) = workbench.on_open_file(&path, bytes, &mut WindowProperties::Fake) {
                    error!("File parse error: {e}");
                    increment_progress_bar(&completed, len, total_size);
                    break 'a None
                }

                let tab = workbench.tabs.remove(0);
                let bookmarks = SearchBox::search0(&tab.value, &predicate);
                std::thread::spawn(move || drop(tab));
                increment_progress_bar(&completed, len, total_size);
                if !bookmarks.is_empty() {
                    Some(SearchResult {
                        path,
                        lines: bookmarks.into_iter().map(|x| x.true_line_number).collect(),
                    })
                } else {
                    None
                }
            }));
        }

        results.into_iter().filter_map(|x| x.join().ok()).filter_map(std::convert::identity).collect::<Vec<_>>()
    });

    log!("\rSearching... ({total_size} / {total_size} bytes) (100.0% complete)");

    if results.is_empty() {
        log!("No results found.")
    }

    for result in results {
        log!("{result}")
    }

    std::process::exit(0);
}

#[inline]
#[cfg(not(target_arch = "wasm32"))]
pub fn reformat() -> ! {
    let mut args = std::env::args().collect::<Vec<_>>();
    args.drain(..2);

    let remap_extension = {
        if let Some("--remap-extension" | "-re") = args.get(0).map(String::as_str) {
            args.remove(0);
            true
        } else {
            false
        }
    };

    let paths = get_paths(&mut args);

    let (extension, format) = {
        match args.get(0).map(String::as_str) {
            Some(x @ "nbt") => (x, FileFormat::Nbt),
            Some(x @ ("dat" | "dat_old" | "gzip")) => (if x == "gzip" { "dat" } else { x }, FileFormat::Gzip),
            Some(x @ "zlib") => (x, FileFormat::Zlib),
            Some(x @ "snbt") => (x, FileFormat::Snbt),
            Some(format) => {
                error!("Unknown format '{format}'");
                std::process::exit(1);
            }
            None => {
                error!("No format supplied");
                std::process::exit(1);
            }
        }
    };

    let completed = AtomicU64::new(0);
    let total_size = paths.iter().filter_map(file_size).sum::<u64>();

    print!("Reformatting... (0 / {total_size} bytes) (0.0% complete)");
    let _ = std::io::Write::flush(&mut std::io::stdout());
    std::thread::scope(|s| {
        for path in paths {
            s.spawn(|| 'a: {
                let mut workbench = Workbench::new(&mut WindowProperties::Fake);
                workbench.tabs.clear();

                let bytes = match read(&path) {
                    Ok(bytes) => bytes,
                    Err(e) => {
                        error!("File read error: {e}");
                        increment_progress_bar(&completed, file_size(&path).unwrap_or(0), total_size);
                        break 'a
                    }
                };

                let len = bytes.len() as u64;

                if let Err(e) = workbench.on_open_file(&path, bytes, &mut WindowProperties::Fake) {
                    error!("File parse error: {e}");
                    increment_progress_bar(&completed, len, total_size);
                    break 'a
                }

                let mut tab = workbench.tabs.remove(0);
                if let FileFormat::Nbt | FileFormat::Snbt | FileFormat::Gzip | FileFormat::Zlib = tab.compression {} else {
                    error!("Tab had invalid file format {}", tab.compression.to_string());
                }

                let out = format.encode(&tab.value);
                std::thread::spawn(move || drop(tab));

                let path = if remap_extension {
                    path.with_extension(extension)
                } else {
                    path
                };

                if let Err(e) = std::fs::write(path, out) {
                    error!("File write error: {e}")
                }

                increment_progress_bar(&completed, len, total_size);
            });
        }
    });

    log!("\rReformatting... ({total_size} / {total_size} bytes) (100.0% complete)");

    std::process::exit(0);
}
