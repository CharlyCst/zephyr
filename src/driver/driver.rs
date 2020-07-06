use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use super::utils::*;
use crate::ast;
use crate::cli::Config;
use crate::error;
use crate::mir;
use crate::wasm;

/// Takes `self` as argument, flush all errors then exit the program and return an error code 64 (malformed entry)
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
    config: Config,
    package_root: Option<PathBuf>,
    package_name: String,
    file_id: u16,
    package_id: u32,
    err: error::ErrorHandler,
    pub_decls: HashMap<String, HashMap<String, mir::Declaration>>, // package -> (decl -> type)
}

impl Driver {
    pub fn new(config: Config) -> Driver {
        Driver {
            config: config,
            package_root: None,
            package_name: String::from(""),
            file_id: 0,
            package_id: 0,
            err: error::ErrorHandler::new_no_file(),
            pub_decls: HashMap::new(),
        }
    }

    /// Starts the compilation and exit.
    pub fn compile(&mut self) {
        let (pkg_mir, mut error_handler) = if let Ok(res) =
            self.get_package_mir(self.config.input.clone(), true, HashSet::new())
        {
            res
        } else {
            exit!(self);
        };
        let binary = wasm::to_wasm(pkg_mir, &mut error_handler, &self.config);

        let output = if let Some(output) = &self.config.output {
            output.clone()
        } else {
            if self.package_name != "" {
                self.package_name.push_str(".wasm");
                PathBuf::from(self.package_name.clone())
            } else {
                PathBuf::from("a.wasm")
            }
        };
        // Write down compiled code
        match fs::write(&output, binary) {
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

    /// Returns the MIR of the package located at the given path.
    fn get_package_mir<P: AsRef<Path>>(
        &mut self,
        path: P,
        is_root: bool,
        imported: HashSet<String>,
    ) -> Result<(mir::Program, error::ErrorHandler), ()> {
        let (mut pkg_ast, mut error_handler) = if let Ok(res) = self.get_package_ast(path, is_root)
        {
            res
        } else {
            return Err(());
        };

        let mut package_import = HashSet::new();
        let mut namespaces = HashMap::new();
        let mut mir_funs = Vec::new();
        for used in pkg_ast.used.iter_mut() {
            if let Some((used_root, used_alias)) = validate_use_path(&used.path) {
                if imported.contains(&used.path) {
                    self.err.report_no_loc(format!(
                        "Circular import detected: '{}' already imported.",
                        &used.path
                    ));
                    exit!(self);
                }
                if package_import.contains(&used.path) {
                    self.err.warn_no_loc(format!(
                        "Package '{}' is imported multiple times from the same package.",
                        &used.path
                    ));
                }
                package_import.insert(used.path.clone());
                // In the future we ill check `used_root` against known package such as `std`.
                // For now we only handle subpackages.
                // TODO: Should we hide exposed declarations of imported packages?
                let pub_decls = if let Some(pub_decls) = self.pub_decls.get(&used.path) {
                    // Already processed and cached.
                    pub_decls.clone()
                } else if used_root == self.package_name && self.package_root.is_some() {
                    // Process used package.
                    let mut package_path = self.package_root.clone().unwrap();
                    package_path.push(strip_root(&used.path));
                    if !package_path.exists() {
                        // No directory found, look for a single file package.
                        package_path = self.package_root.clone().unwrap();
                        let mut file_path = strip_root(&used.path).to_owned();
                        file_path.push_str(".frk");
                        package_path.push(file_path);
                    }
                    let mut imported = imported.clone();
                    imported.insert(used.path.clone());
                    if let Ok((sub_pkg_mir, err_handler)) =
                        self.get_package_mir(package_path, false, imported.clone())
                    {
                        error_handler.merge(err_handler);
                        mir_funs.extend(sub_pkg_mir.funs);
                        self.pub_decls
                            .insert(used.path.clone(), sub_pkg_mir.pub_decls.clone());
                        sub_pkg_mir.pub_decls
                    } else {
                        self.err.merge(error_handler);
                        exit!(self);
                    }
                } else {
                    self.err
                        .report_no_loc(format!("Unknown package '{}'.", &used.path));
                    self.err.merge(error_handler);
                    exit!(self);
                };
                if let Some(alias) = &used.alias {
                    namespaces.insert(alias.clone(), pub_decls);
                } else {
                    namespaces.insert(used_alias.clone(), pub_decls);
                }
            } else {
                self.err
                    .report_no_loc(format!("Use path is not well formatted '{}'.", &used.path));
                self.err.merge(error_handler);
                exit!(self);
            }
        }
        let mut mir_program = mir::to_mir(pkg_ast, namespaces, &mut error_handler, &self.config);
        mir_program.funs.extend(mir_funs);
        Ok((mir_program, error_handler))
    }

    /// Returns the AST of the package located at the given path.
    fn get_package_ast<P: AsRef<Path>>(
        &mut self,
        path: P,
        is_root: bool,
    ) -> Result<(ast::Program, error::ErrorHandler), ()> {
        // Resolve path
        let (paths, is_unique_file) = match resolve_path(&path) {
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
        let path = path.as_ref();
        let directory = if is_root {
            // We do not check the directory for root package.
            ""
        } else {
            if path.is_file() {
                path.parent()
                    .expect("Could not retireve directory name.")
                    .file_name()
            } else {
                // Already a directory.
                path.file_name()
            }
            .expect("Failed retrieving the directory name.")
            .to_str()
            .expect("Directory name contains unexpected characters.")
        };

        // Build package ASTs
        let mut ast_programs = Vec::new();
        let mut files = Vec::new();
        let package_id = self.fresh_package_id();
        for path in paths {
            let f_id = self.file_id;
            self.file_id += 1;
            let code = fs::read_to_string(&path).expect("Internal error: invalid path.");
            let file_name = path
                .file_stem()
                .expect("Internal error: path is not a file.")
                .to_str()
                .expect("File name at seems to use non standard characters")
                .to_string();
            files.push((code, f_id, file_name));
        }
        for (code, f_id, file_name) in files.into_iter() {
            let mut error_handler = error::ErrorHandler::new(code, f_id);
            let ast_program = ast::get_ast(f_id, package_id, &mut error_handler, &self.config);
            ast_programs.push((ast_program, error_handler, file_name));
        }

        // Merge package ASTs
        let mut funs = Vec::new();
        let mut exposed = Vec::new();
        let mut used = Vec::new();
        let mut package: Option<(String, error::ErrorHandler)> = None;
        let mut expected_package_name: Option<String> = None;

        // Iterate over ast_program of all fork files in the folder
        for (ast, err_handler, file_name) in ast_programs {
            let package_name = ast.package.path;
            match get_package_type(&package_name) {
                Ok(package_type) => {
                    if let Some((ref name, ref mut error_handler)) = package {
                        match package_type {
                            PackageType::Package { name: package_name } => {
                                if &package_name == name {
                                    // Extend AST package
                                    error_handler.merge(err_handler);
                                    funs.extend(ast.funs);
                                    exposed.extend(ast.exposed);
                                    used.extend(ast.used);
                                } else {
                                    self.err.report_no_loc(format!(
                                        "Unexpectec package name '{}' at '{}'",
                                        package_name,
                                        path.to_str().unwrap_or("")
                                    ));
                                    exit!(self);
                                }
                            }
                            PackageType::SingleFilePackage {
                                parent,
                                name: package_name,
                            } => {
                                self.verify_single_package(
                                    &parent,
                                    &package_name,
                                    name,
                                    &file_name,
                                );
                            }
                            PackageType::OrphanFile { .. } => {}
                        }
                    } else {
                        let mut is_orphan = false;

                        // Ignore single files
                        match package_type {
                            PackageType::OrphanFile { .. } => {
                                if !is_unique_file {
                                    // Ingore it if we are not looking for a single file.
                                    continue;
                                }
                                is_orphan = true;
                            }
                            PackageType::SingleFilePackage { parent, name } => {
                                if !is_unique_file {
                                    // Verify that parent name corresponds to the current package.
                                    match expected_package_name {
                                        Some(ref expected_name) => {
                                            self.verify_single_package(
                                                &parent,
                                                &name,
                                                expected_name,
                                                &file_name,
                                            );
                                        }
                                        None => {
                                            expected_package_name = Some(parent);
                                        }
                                    }
                                    // Ingore it if we are not looking for a single file.
                                    continue;
                                }
                            }
                            PackageType::Package { name } => {
                                if let Some(ref expected_name) = expected_package_name {
                                    if expected_name != &name {
                                        self.err.report_no_loc(format!("Found single file package whose parent name '{}' does not correspond to the actual parent '{}'.", expected_name, &name));
                                        exit!(self);
                                    }
                                }
                                if !is_root && &name != directory {
                                    self.err.report_no_loc(format!("Package name '{}' should be the same as its directory: '{}'.", name, directory));
                                    exit!(self);
                                }
                            }
                        }

                        // Add the package
                        if is_root {
                            if !is_orphan {
                                self.package_root = Some(path.to_owned());
                            }
                            self.package_name = package_name.clone();
                        }
                        package = Some((package_name, err_handler));
                        funs.extend(ast.funs);
                        exposed.extend(ast.exposed);
                        used.extend(ast.used);
                    }
                }
                Err(error) => {
                    self.err.report_no_loc(format!(
                        "{} at '{}'",
                        error,
                        path.to_str().unwrap_or("")
                    ));
                    exit!(self);
                }
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
            self.err.report_no_loc(format!(
                "Could not find a valid package at '{}'.",
                path.to_str().unwrap_or("")
            ));
            Err(())
        }
    }

    /// Returns a fresh package ID.
    fn fresh_package_id(&mut self) -> u32 {
        let package_id = self.package_id;
        self.package_id += 1;
        package_id
    }

    /// Verify integrity of a single file package name and parent directory. Raise an error if
    /// needed.
    fn verify_single_package(
        &mut self,
        parent: &str,
        name: &str,
        expected_parent: &str,
        file_name: &str,
    ) {
        if parent != expected_parent {
            self.err.report_no_loc(format!("Found single file package whose parent name '{}' does not correspond to the actual parent '{}'.", parent, expected_parent));
            exit!(self);
        } else if file_name != name {
            self.err.report_no_loc(format!(
                "Found single file package whose name '{}' does not correspond to filename '{}'",
                name, file_name
            ));
            exit!(self);
        }
    }
}
