use std::fs;

/// Returns a list of path to files to be parsed and a flag indicating if the path points at a
/// file. In case of success, return at least one path.
pub fn resolve_path(path: &str) -> Result<(Vec<String>, bool), String> {
    let file_info = if let Ok(f) = fs::metadata(&path) {
        f
    } else {
        return Err(format!("Path '{}' does not exist.", path));
    };
    if file_info.is_dir() {
        if let Ok(dir) = fs::read_dir(path) {
            let mut paths = Vec::new();
            for entry in dir {
                if let Ok(entry) = entry {
                    let path = if let Some(path) = entry.path().to_str() {
                        path.to_string()
                    } else {
                        continue;
                    };
                    if let Some(ext) = get_extension(&path) {
                        if ext.eq("frk") {
                            paths.push(path);
                        }
                    }
                }
            }
            if paths.len() == 0 {
                Err(format!("Could not find any fork file (.frk) in '{}'", path))
            } else {
                Ok((paths, false))
            }
        } else {
            Err(String::from(""))
        }
    } else if file_info.is_file() {
        let extension = if let Some(ext) = get_extension(path) {
            ext
        } else {
            return Err(String::from("Could not read file extension"));
        };
        if extension.eq("frk") {
            Ok((vec![path.to_string()], true))
        } else {
            Err(format!("Invalid file extension '{}'.", &extension))
        }
    } else {
        Err(format!("{}' is neither a file nor a directory.", path))
    }
}

/// Returns the file extension of a given path, if applicable.
pub fn get_extension(path: &str) -> Option<String> {
    match std::path::Path::new(path).extension() {
        Some(ext) => match ext.to_str() {
            Some(ext) => Some(ext.to_string()),
            None => None,
        },
        None => None,
    }
}

/// Returns the path of the directory containing a given file. Path **MUST** point to a file.
pub fn get_directory_path(path: &str) -> String {
    let path = path
        .split('/')
        .map(|s| s.to_string())
        .collect::<Vec<String>>();
    if path.len() <= 1 {
        String::from(".")
    } else {
        path[..path.len() - 1].join("/")
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
    /// Test `get_extension`
    fn extension() {
        assert_eq!(
            get_extension("greeting/hello.frk"),
            Some(String::from("frk"))
        );
        assert_eq!(get_extension("hello.frk"), Some(String::from("frk")));
        assert_eq!(get_extension("hello"), None);
    }

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
    // test `validate_package`
    fn package_name() {
        assert!(validate_package("greeting"));
        assert!(validate_package("hello_world"));
        assert!(!validate_package("greeting/hello"));
        assert!(!validate_package("Greeting"));
        assert!(!validate_package("_greeting"));
        assert!(!validate_package("#greeting"));
    }

    #[test]
    // test `validate_orphan_file`
    fn orphan_file_name() {
        assert!(validate_orphan_file("#greeting"));
        assert!(validate_orphan_file("#greeting_world"));
        assert!(!validate_orphan_file("#greeting/hello"));
        assert!(!validate_orphan_file("greeting"));
        assert!(!validate_orphan_file("#_greeting"));
    }

    #[test]
    // test `validate_single_file_package`
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
    // test `get_directory_path`
    fn directory_path() {
        assert_eq!(
            &get_directory_path("/home/ubuntu/fork/hello.frk"),
            "/home/ubuntu/fork"
        );
        assert_eq!(&get_directory_path("fork/hello.fork"), "fork");
        assert_eq!(&get_directory_path("hello.frk"), ".");
    }
}
