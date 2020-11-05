use std::fs;
use std::path::{Path, PathBuf};

use crate::ast;

/// A list of packages known from the compiler and expected to be available.
pub enum KnownPackage {
    Core,
}

/// The result of a path lookup: either file or a directory
pub enum ResolvedPath {
    Dir(Vec<PathBuf>),
    File(PathBuf),
}

/// A file prepared to be passed to the AST parser.
pub struct PreparedFile {
    pub code: String,
    pub f_id: u16,
    pub file_name: String,
    pub kind: ast::Kind,
}

pub const ZEPHYR_EXTENSION: &str = "zph";
pub const ASM_EXTENSION: &str = "zasm";

/// Returns a list of files pointed by `path` and a flag set to `true` if `path` points directly
/// at a file, false otherwise. In case of success, return at least one path.
pub fn resolve_path<P: AsRef<Path>>(path: P) -> Result<ResolvedPath, String> {
    let path = path.as_ref();
    let file_info = fs::metadata(&path)
        .map_err(|_| format!("Path '{}' does not exist.", path.to_str().unwrap_or("")))?;
    if file_info.is_dir() {
        let dir = fs::read_dir(path).expect("Should never happen");
        let files = resolve_directory_files(dir, path)?;
        Ok(files)
    } else if file_info.is_file() {
        let ext = path.extension().expect("Could not read file extension");
        if ext.eq(ZEPHYR_EXTENSION) || ext.eq(ASM_EXTENSION) {
            Ok(ResolvedPath::File(path.to_owned()))
        } else {
            Err(format!(
                "Invalid file extension '{}'.",
                ext.to_str().unwrap_or("")
            ))
        }
    } else {
        Err(format!(
            "{}' is neither a file nor a directory.",
            path.to_str().unwrap_or("")
        ))
    }
}

/// Given a directory, return a list of all the zephyr files it contains.
/// Rises an error if no file with a zephyr extension are found.
fn resolve_directory_files(dir: fs::ReadDir, path: &Path) -> Result<ResolvedPath, String> {
    let mut paths = Vec::new();
    for entry in dir {
        if let Ok(entry) = entry {
            let path = entry.path();
            if let Some(ext) = path.extension() {
                if ext.eq(ZEPHYR_EXTENSION) || ext.eq(ASM_EXTENSION) {
                    paths.push(path);
                }
            }
        }
    }
    if paths.is_empty() {
        Err(format!(
            "Could not find any zephyr file (.{}) in '{}'",
            ZEPHYR_EXTENSION,
            path.to_str().unwrap_or("")
        ))
    } else {
        Ok(ResolvedPath::Dir(paths))
    }
}

/// Returns the path without the root package.
/// 'std/math/crypo' becomes 'math/crypto'.
pub fn strip_root(path: &str) -> &str {
    let mut iter = path.chars();
    loop {
        if let Some(c) = iter.next() {
            if c == '/' {
                return iter.as_str();
            }
        } else {
            // Empty string
            return &path[0..0];
        }
    }
}

/// Returns the root and alias of a use path if it is well formed, i.e. is a sequence
/// of well formed package names separated by '/'.
pub fn validate_use_path(path: &str) -> Option<(String, String)> {
    let path = path
        .split('/')
        .map(|s| s.to_string())
        .collect::<Vec<String>>();
    if path.len() < 2 {
        return None;
    }
    let mut iter = path.into_iter();
    let root = iter.next().unwrap();
    if !validate_package(&root) {
        return None;
    }
    let mut next = iter.next().unwrap();
    loop {
        if !validate_package(&next) {
            return None;
        }
        next = if let Some(next_next) = iter.next() {
            next_next
        } else {
            return Some((root, next));
        }
    }
}

/// Returns `true` if the package name is correct, that is:
/// - It contrains only lower case characters and underscores
/// - It starts with a lower case character
fn validate_package(package_name: &str) -> bool {
    let mut is_first = true;
    for c in package_name.chars() {
        if c == '_' && !is_first {
            continue;
        } else if !c.is_alphabetic() {
            return false;
        } else if !c.is_lowercase() {
            return false;
        }
        is_first = false;
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    /// test `validate_package`
    fn package_name() {
        assert!(validate_package("greeting"));
        assert!(validate_package("hello_world"));
        assert!(!validate_package("greeting/hello"));
        assert!(!validate_package("Greeting"));
        assert!(!validate_package("_greeting"));
        assert!(!validate_package("#greeting"));
    }

    #[test]
    /// test `validate_use_path`
    fn use_path() {
        assert_eq!(
            validate_use_path("std/math/crypto"),
            Some((String::from("std"), String::from("crypto")))
        );
        assert_eq!(
            validate_use_path("std/math"),
            Some((String::from("std"), String::from("math")))
        );
        assert_eq!(
            validate_use_path("external_package/foo/bar_buzz"),
            Some((String::from("external_package"), String::from("bar_buzz")))
        );
        assert_eq!(validate_use_path("std"), None);
        assert_eq!(validate_use_path("Std/math"), None);
    }

    #[test]
    /// test `strip_root`
    fn _strip_root() {
        assert_eq!(strip_root("std/math/crypto"), "math/crypto");
        assert_eq!(strip_root("std"), "");
    }
}
