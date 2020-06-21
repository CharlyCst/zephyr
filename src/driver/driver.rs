use std::fs;

use super::utils::*;
use crate::ast;
use crate::error;
use crate::mir;
use crate::wasm;

/// Takes `self` as argument, exit the program and return an error code 64 (malformed entry)
#[macro_use]
macro_rules! exit {
    ($v:expr) => {{
        $v.err.flush();
        std::process::exit(64);
    }};
}

/// The Driver is responsible for orchestrating the compilation process, that is resolving
/// packages, starting each phases of the pipeline and merging code at appropriate time.
pub struct Driver {
    input: String,
    output: String,
    file_id: u16,
    package_id: u32,
    err: error::ErrorHandler,
}

impl Driver {
    pub fn new(input: String, output: String) -> Driver {
        Driver {
            input: input,
            output: output,
            file_id: 0,
            package_id: 0,
            err: error::ErrorHandler::new_no_file(),
        }
    }

    /// Starts the compilation and exit.
    pub fn compile(&mut self) {
        let (pkg_ast, mut error_handler) =
            if let Ok(res) = self.get_package_ast(&self.input.clone()) {
                res
            } else {
                exit!(self);
            };
        let mir_program = mir::to_mir(pkg_ast, &mut error_handler);
        let binary = wasm::to_wasm(mir_program, &mut error_handler);

        // Write down compiled code
        match fs::write(&self.output, binary) {
            Ok(_) => {
                self.err.flush();
                std::process::exit(0)
            }
            Err(e) => {
                self.err.report_no_loc(e.to_string());
                exit!(self);
            }
        }
    }

    /// Return the AST of the package located at the given path.
    fn get_package_ast(&mut self, path: &str) -> Result<(ast::Program, error::ErrorHandler), ()> {
        // Resolve path
        let (paths, is_unique_file) = match resolve_path(path) {
            Ok(result) => result,
            Err(err) => {
                self.err.report_no_loc(err);
                exit!(self);
            }
        };
        if paths.len() == 0 {
            self.err
                .report_internal_no_loc(String::from("No file to compile: bad path."));
            return Err(());
        }

        // Build package ASTs
        let mut ast_programs = Vec::new();
        let mut files = Vec::new();
        let package_id = self.fresh_package_id();
        for path in paths {
            let f_id = self.file_id;
            self.file_id += 1;
            let code = fs::read_to_string(&path).expect("Internal error: invalid path.");
            files.push((code, f_id));
        }
        for (code, f_id) in files.into_iter() {
            let mut error_handler = error::ErrorHandler::new(code, f_id);
            let ast_program = ast::get_ast(f_id, package_id, &mut error_handler);
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
                            self.err.warn_no_loc(format!(
                                "Malformed orphan file name: '{}' at '{}'.",
                                &package_name, path
                            ));
                        }
                    } else if is_single_file_package(&package_name) {
                        if let Some((host_package, guest_package)) =
                            validate_single_file_package(&package_name)
                        {
                            if &host_package != name {
                                self.err.report_no_loc(format!("Malformed single file package '{}' at '{}', host package should be '{}/{}'.", &package_name, path, name, guest_package));
                                exit!(self);
                            }
                        } else {
                            self.err.report_no_loc(format!(
                                "Malformed single file package name '{}' at '{}'.",
                                &package_name, path
                            ));
                            exit!(self);
                        }
                    } else {
                        self.err.report_no_loc(format!(
                            "Multiple packages defined in the same directory at '{}'.",
                            path
                        ));
                        exit!(self)
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
                        self.err.report_no_loc(format!(
                            "Malformed single file package '{}' at '{}'.",
                            &package_name, path
                        ));
                        exit!(self);
                    }
                } else if is_orphan(&package_name) {
                    if validate_orphan_file(&package_name) {
                        if !is_unique_file {
                            continue;
                        }
                    } else {
                        self.err.report_no_loc(format!(
                            "Malformed orphan file name: '{}' at '{}'.",
                            &package_name, path
                        ));
                        exit!(self);
                    }
                } else {
                    if !validate_package(&package_name) {
                        self.err.warn_no_loc(format!("Package name '{}' is malformed. A package name must contain only lower case characters or underscores '_'.", package_name));
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
                    package_id: package_id,
                },
                error_handler,
            ))
        } else {
            self.err
                .report_no_loc(format!("Could not find a valid package at '{}'.", path));
            Err(())
        }
    }

    /// Returns a fresh package ID.
    fn fresh_package_id(&mut self) -> u32 {
        let package_id = self.package_id;
        self.package_id += 1;
        package_id
    }
}
