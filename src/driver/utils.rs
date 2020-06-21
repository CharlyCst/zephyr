use std::fs;
use std::path::{Path, PathBuf};

/// Returns a list of files pointed by `path` and a flag set to `true` if `path` points at a
/// file, false otherwise. In case of success, return at least one path.
pub fn resolve_path<P: AsRef<Path>>(path: P) -> Result<(Vec<PathBuf>, bool), String> {
    let path = path.as_ref();
    let file_info = if let Ok(f) = fs::metadata(&path) {
        f
    } else {
        return Err(format!("Path '{}' does not exist.", path.to_str().unwrap_or("")));
    };
    if file_info.is_dir() {
        if let Ok(dir) = fs::read_dir(path) {
            let mut paths = Vec::new();
            for entry in dir {
                if let Ok(entry) = entry {
                    let path = entry.path();
                    if let Some(ext) = path.extension() {
                        // Only support .frk for now
                        if ext.eq("frk") {
                            paths.push(path);
                        }
                    }
                }
            }
            if paths.len() == 0 {
                Err(format!("Could not find any fork file (.frk) in '{}'", path.to_str().unwrap_or("")))
            } else {
                Ok((paths, false))
            }
        } else {
            Err(String::from(""))
        }
    } else if file_info.is_file() {
        let ext = if let Some(ext) = path.extension() {
            ext
        } else {
            return Err(String::from("Could not read file extension"));
        };
        if ext.eq("frk") {
            Ok((vec![path.to_owned()], true))
        } else {
            Err(format!("Invalid file extension '{}'.", ext.to_str().unwrap_or("")))
        }
    } else {
        Err(format!("{}' is neither a file nor a directory.", path.to_str().unwrap_or("")))
    }
}

/// Returns 
/* pub fn get_subpackage_path(package_path: &str, subpackage: &str) {
    let mut path = PathBuf::new();
    path.push(package_path);
}*/

/// Returns the path without the root package.
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
        } else  {
            return Some((root, next));
        }
    }
}

/// Returns `true` if the package looks like an orphan file (i.e. it starts with an `#`)
pub fn is_orphan(package_name: &str) -> bool {
    if let Some(c) = package_name.chars().next() {
        c == '#'
    } else {
        false
    }
}

/// Returns `true` if the package looks like a single file package
pub fn is_single_file_package(package_name: &str) -> bool {
    // A single file package MUST contain a `/`, and is the only
    // type of package allowed to.
    package_name.chars().find(|c| *c == '/').is_some()
}

/// Returns `true` if the package name is correct, that is:
/// - It contrains only lower case characters and underscores
/// - It starts with a lower case character
pub fn validate_package(package_name: &str) -> bool {
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

/// Returns `true` if the package name is a valid orphan file, that is:
/// - It starts with `#`
/// - It is followed by  a valid package name
pub fn validate_orphan_file(package_name: &str) -> bool {
    match package_name.chars().next() {
        Some('#') => true,
        _ => return false,
    };

    validate_package(&package_name[1..])
}

/// Returns an option containing the host package name (the main package of the directory) and the
/// single file package name if the single file package name is well formad, that is:
/// - It starts with the host package name (a valid package name)
/// - Is followed by a a slash `/`
/// - Itself followed by the single file package name (a valid package name)
pub fn validate_single_file_package(package_name: &str) -> Option<(String, String)> {
    let mut parts = package_name.split('/');
    let host_package = if let Some(host_package) = parts.next() {
        host_package.to_string()
    } else {
        return None;
    };
    let single_file_package = if let Some(single_file_package) = parts.next() {
        single_file_package.to_string()
    } else {
        return None;
    };
    if parts.next() == None
        && validate_package(&host_package)
        && validate_package(&single_file_package)
    {
        Some((host_package, single_file_package))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    /// Test `is_orphan`
    fn orphan() {
        assert_eq!(is_orphan("#orphan"), true);
        assert_eq!(is_orphan("orphan"), false);
        assert_eq!(is_orphan("trap/#orphan"), false);
        assert_eq!(is_orphan(""), false);
    }

    #[test]
    /// Test `is_single_file_package`
    fn single_file_package() {
        assert_eq!(is_single_file_package("greeting/hello"), true);
        assert_eq!(is_single_file_package("greeting"), false);
        assert_eq!(is_single_file_package("#greeting"), false);
        assert_eq!(is_single_file_package(""), false);
    }

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
    /// test `validate_orphan_file`
    fn orphan_file_name() {
        assert!(validate_orphan_file("#greeting"));
        assert!(validate_orphan_file("#greeting_world"));
        assert!(!validate_orphan_file("#greeting/hello"));
        assert!(!validate_orphan_file("greeting"));
        assert!(!validate_orphan_file("#_greeting"));
    }

    #[test]
    /// test `validate_single_file_package`
    fn single_file_package_name() {
        assert_eq!(
            validate_single_file_package("greeting/hello"),
            Some((String::from("greeting"), String::from("hello")))
        );
        assert_eq!(
            validate_single_file_package("greeting_world/hello_world"),
            Some((String::from("greeting_world"), String::from("hello_world")))
        );
        assert_eq!(validate_single_file_package("#greeting/world"), None);
        assert_eq!(validate_single_file_package("greeting/#world"), None);
        assert_eq!(validate_single_file_package("greeting/world/hello"), None);
    }

    #[test]
    /// test `validate_use_path`
    fn use_path() {
        assert_eq!(validate_use_path("std/math/crypto"), Some((String::from("std"), String::from("crypto"))));
        assert_eq!(validate_use_path("std/math"), Some((String::from("std"), String::from("math"))));
        assert_eq!(validate_use_path("external_package/foo/bar_buzz"), Some((String::from("external_package"), String::from("bar_buzz"))));
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
