use std::fs;

use crate::ast;
use crate::error;
use crate::mir;
use crate::wasm;

/// Exit the program and return an error code 64 (malformed entry)
#[macro_use]
macro_rules! exit {
    () => {
        std::process::exit(64);
    };
}

/// The Driver is responsible for orchestrating the compilation process, that is resolving
/// packages, starting each phases of the pipeline and mergind code at appropriate time.
pub struct Driver {
    input: String,
    output: String,
    file_id: u16,
}

impl Driver {
    pub fn new(input: String, output: String) -> Driver {
        Driver {
            input: input,
            output: output,
            file_id: 0,
        }
    }

    /// Starts the compilation and exit.
    pub fn compile(&mut self) {
        let (pkg_ast, mut error_handler) =
            if let Ok(res) = self.get_package_ast(&self.input.clone()) {
                res
            } else {
                exit!();
            };
        let mir_program = mir::to_mir(pkg_ast, &mut error_handler);
        let binary = wasm::to_wasm(mir_program, &mut error_handler);

        match fs::write(&self.output, binary) {
            Ok(_) => (),
            Err(e) => println!("{}", e),
        }

        std::process::exit(0);
    }

    /// Return the AST of the package located at the given path.
    fn get_package_ast(&mut self, path: &str) -> Result<(ast::Program, error::ErrorHandler), ()> {
        // Resolve path
        let (paths, is_unique_file) = match self.resolve_path(path) {
            Ok(result) => result,
            Err(err) => {
                println!("{}", &err);
                exit!();
            }
        };
        if paths.len() == 0 {
            println!("Internal error: no file to compile.");
            return Err(());
        }

        // Build package ASTs
        let mut ast_programs = Vec::new();
        let mut files = Vec::new();
        for path in paths {
            let f_id = self.file_id;
            self.file_id += 1;
            let code = fs::read_to_string(&path).expect("Internal error: invalid path.");
            files.push((code, f_id));
        }
        for (code, f_id) in files.into_iter() {
            let mut error_handler = error::ErrorHandler::new(code, f_id);
            let ast_program = ast::get_ast(f_id, &mut error_handler);
            ast_programs.push((ast_program, error_handler));
        }

        // Merge package ASTs
        let mut funs = Vec::new();
        let mut exposed = Vec::new();
        let mut used = Vec::new();
        let mut package: Option<(String, error::ErrorHandler)> = None;

        // Iterate over ast_program of all fork files in the folder
        for (ast, err_handler) in ast_programs {
            let package_name = ast.package.path;
            if let Some((ref name, ref mut error_handler)) = package {
                if &package_name == name {
                    // Extend AST package
                    error_handler.merge(err_handler);
                    funs.extend(ast.funs);
                    exposed.extend(ast.exposed);
                    used.extend(ast.used);
                } else {
                    if is_orphan(&package_name) {
                        if !validate_orphan_file(&package_name) {
                            println!(
                                "Warning: malformed orphan file name: '{}' at '{}'.",
                                &package_name, path
                            );
                        }
                    } else if is_single_file_package(&package_name) {
                        if let Some((host_package, guest_package)) =
                            validate_single_file_package(&package_name)
                        {
                            if &host_package != name {
                                println!("Error: malformed single file package '{}' at '{}', host package should be '{}/{}'.", &package_name, path, name, guest_package);
                                exit!();
                            }
                        } else {
                            println!(
                                "Error: malformed single file package name '{}' at '{}'.",
                                &package_name, path
                            );
                            exit!();
                        }
                    } else {
                        println!(
                            "Error: multiple packages defined in the same directory at '{}'.",
                            path
                        );
                        exit!()
                    }
                }
            } else {
                if is_single_file_package(&package_name) {
                    if let Some(_) = validate_single_file_package(&package_name) {
                        if !is_unique_file {
                            continue;
                        }
                    // else add package to AST
                    } else {
                        println!(
                            "Error: malformed single file package '{}' at '{}'.",
                            &package_name, path
                        );
                        exit!();
                    }
                } else if is_orphan(&package_name) {
                    if validate_orphan_file(&package_name) {
                        if !is_unique_file {
                            continue;
                        }
                    } else {
                        println!(
                            "Error: malformed orphan file name: '{}' at '{}'.",
                            &package_name, path
                        );
                        exit!();
                    }
                } else {
                    if !validate_package(&package_name) {
                        println!("Package name '{}' is malformed. A package name must contain only lower case characters or underscores '_'.", package_name);
                        continue;
                    } else if is_unique_file {
                        // Unique belonging to a well formed package, scan the whole directory for
                        // other files that may belong to that same package.
                        return self.get_package_ast(&get_directory_path(path));
                    }
                }
                // Well formed package name
                package = Some((package_name, err_handler));
                funs.extend(ast.funs);
                exposed.extend(ast.exposed);
                used.extend(ast.used);
            }
        }

        if let Some((name, error_handler)) = package {
            let package = ast::Package {
                path: name,
                loc: error::Location::dummy(),
            };
            Ok((
                ast::Program {
                    package: package,
                    exposed: exposed,
                    used: used,
                    funs: funs,
                },
                error_handler,
            ))
        } else {
            println!("Could not find a valid package at '{}'.", path);
            Err(())
        }
    }

    /// Returns a list of path to files to be parsed and a flag indicating if the path point at a
    /// file. In case of success, return at least one path.
    fn resolve_path(&self, path: &str) -> Result<(Vec<String>, bool), String> {
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
}

/// Returns the file extension of a given path, if applicable.
fn get_extension(path: &str) -> Option<String> {
    match std::path::Path::new(path).extension() {
        Some(ext) => match ext.to_str() {
            Some(ext) => Some(ext.to_string()),
            None => None,
        },
        None => None,
    }
}

fn get_directory_path(path: &str) -> String {
    let path = path.split('/').map(|s| s.to_string()).collect::<Vec<String>>();
    if path.len() <= 1 {
        String::from(".")
    } else {
        path[..path.len() -1].join("/")
    }
}

/// Returns `true` if the package looks like an orphan file (i.e. it starts with an `#`)
fn is_orphan(package_name: &str) -> bool {
    if let Some(c) = package_name.chars().next() {
        c == '#'
    } else {
        false
    }
}

/// Returns `true` if the package looks like a single file package
fn is_single_file_package(package_name: &str) -> bool {
    // A single file package MUST contain a `/`, and is the only
    // type of package allowed to.
    package_name.chars().find(|c| *c == '/').is_some()
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

/// Returns `true` if the package name is a valid orphan file, that is:
/// - It starts with `#`
/// - It is followed by  a valid package name
fn validate_orphan_file(package_name: &str) -> bool {
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
fn validate_single_file_package(package_name: &str) -> Option<(String, String)> {
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
        assert_eq!(&get_directory_path("/home/ubuntu/fork/hello.frk"), "/home/ubuntu/fork");
        assert_eq!(&get_directory_path("fork/hello.fork"), "fork");
        assert_eq!(&get_directory_path("hello.frk"), ".");
    }
}
