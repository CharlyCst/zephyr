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

/// Returns a list of files pointed by `path`.
pub fn resolve_path<P: AsRef<Path>>(path: P) -> Result<ResolvedPath, String> {
    let mut path = path.as_ref().to_owned();
    // look for a file if the path does not exist.
    if !path.exists() {
        let mut zph_path = path.clone();
        zph_path.set_extension(ZEPHYR_EXTENSION);
        if zph_path.is_file() {
            path = zph_path;
        } else {
            let mut asm_path = path.clone();
            asm_path.set_extension(ASM_EXTENSION);
            if asm_path.is_file() {
                path = asm_path;
            }
        }
    }
    let file_info = fs::metadata(&path)
        .map_err(|_| format!("Path '{}' does not exist.", path.to_str().unwrap_or("")))?;
    if file_info.is_dir() {
        let dir = fs::read_dir(&path).expect("Should never happen");
        let files = resolve_directory_files(dir, &path)?;
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
fn resolve_directory_files(dir: fs::ReadDir, path: &PathBuf) -> Result<ResolvedPath, String> {
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

/// Returns an alias for the given path, that is its last component.
pub fn get_alias(path: &str) -> &str {
    path.split('/')
        .last()
        .expect("Unable to retrieve alias from path")
}

/// Returns the package root name and a path to the given subpackage from the root.
pub fn split_package_name(path: &str) -> Option<(String, Option<String>)> {
    if !validate_package_name(path) {
        return None;
    }
    let root = path
        .split('/')
        .collect::<Vec<&str>>()
        .get(0)
        .expect("Internal error: could not retrieve path root")
        .to_string();
    let sub_package = strip_root(path);
    if sub_package == "" {
        Some((root, None))
    } else {
        Some((root, Some(sub_package.to_string())))
    }
}

/// Returns `true` if the package name is correct, that is:
/// - It contrains only lower case characters and underscores
/// - It starts with a lower case character
fn validate_package_name(package_name: &str) -> bool {
    let mut is_first = true;
    for c in package_name.chars() {
        if c == '_' || c =='-' {
            if is_first {
                return false;
            }
        } else if c == '/' {
            if is_first {
                return false;
            }
            is_first = true;
        } else if !c.is_alphabetic() {
            return false;
        } else if !c.is_lowercase() {
            return false;
        } else {
            is_first = false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    /// test `validate_package`
    fn package_name() {
        assert!(validate_package_name("greeting"));
        assert!(validate_package_name("hello_world"));
        assert!(validate_package_name("greeting/hello"));
        assert!(validate_package_name("std/utils/test_name"));
        assert!(!validate_package_name("std/utils/_test_name"));
        assert!(!validate_package_name("Greeting"));
        assert!(!validate_package_name("_greeting"));
        assert!(!validate_package_name("#greeting"));
    }

    #[test]
    /// test `split_package_name`
    fn split_package_names() {
        assert_eq!(
            split_package_name("std/math/crypto"),
            Some((String::from("std"), Some(String::from("math/crypto"))))
        );
        assert_eq!(
            split_package_name("std/math"),
            Some((String::from("std"), Some(String::from("math"))))
        );
        assert_eq!(
            split_package_name("external_package/foo/bar_buzz"),
            Some((
                String::from("external_package"),
                Some(String::from("foo/bar_buzz"))
            ))
        );
        assert_eq!(split_package_name("std"), Some((String::from("std"), None)));
        assert_eq!(split_package_name("Std/math"), None);
    }

    #[test]
    /// test `strip_root`
    fn _strip_root() {
        assert_eq!(strip_root("std/math/crypto"), "math/crypto");
        assert_eq!(strip_root("std"), "");
    }

    #[test]
    fn _get_alias() {
        assert_eq!(get_alias("core/utils"), "utils");
        assert_eq!(get_alias("core"), "core");
        assert_eq!(get_alias("core/mem/malloc"), "malloc");
    }
}
