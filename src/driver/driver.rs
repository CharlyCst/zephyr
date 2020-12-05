use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use super::utils::*;
use crate::ast;
use crate::cli::Config;
use crate::error;
use crate::hir;
use crate::mir;
use crate::wasm;

/// Expectend environment variable pointing to Zephyr known packages.
const ZEPHYR_LIB: &'static str = "ZEPHYR_LIB";

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
    with_known_packages: bool,
    known_package_path: Option<PathBuf>,
    file_id: u16,
    package_id: u32,
    err: error::ErrorHandler,
    pub_decls: PublicDeclarations,
}

impl Driver {
    pub fn new(config: Config) -> Driver {
        let known_package_path = std::env::var(ZEPHYR_LIB)
            .ok()
            .map(|path| PathBuf::from(path));
        let with_known_packages = !config.no_std;
        Driver {
            config,
            with_known_packages,
            known_package_path,
            file_id: 0,
            package_id: 0,
            err: error::ErrorHandler::new_no_file(),
            pub_decls: PublicDeclarations::new(),
        }
    }

    /// Starts the compilation and exit.
    pub fn compile(&mut self) {
        let path = self
            .config
            .input
            .clone()
            .canonicalize()
            .expect("Could not resolve path");
        let mut root_path = path.clone();
        if root_path.is_file() {
            root_path = root_path
                .parent()
                .expect("Could not resolve directory root")
                .to_owned();
        }
        let (pkg_mir, mut error_handler) =
            if let Ok(res) = self.get_package_mir(path, None, root_path, HashSet::new()) {
                res
            } else {
                exit!(self);
            };
        let name = pkg_mir.name.clone();
        let binary = wasm::to_wasm(pkg_mir, &mut error_handler, &self.config);
        let output = if let Some(output) = &self.config.output {
            output.clone()
        } else {
            PathBuf::from(&format!("{}.zph.wasm", name))
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
    ///
    /// Params:
    ///  - path: Path to the target file or directory
    ///  - root: The root of the corresponding package, if known
    ///  - root_path: The path to the root of the package
    ///  - imported: A set of already imported packages, to prevent circular imports
    fn get_package_mir<P: AsRef<Path>>(
        &mut self,
        path: P,
        root: Option<&str>,
        root_path: P,
        imported: HashSet<String>,
    ) -> Result<(mir::Program, error::ErrorHandler), ()> {
        // Get AST
        let (mut pkg_ast, mut error_handler) = self.get_package_ast(path)?;
        let root = root.unwrap_or(&pkg_ast.package.name);
        // Prepare MIR
        let mut package_import = HashSet::new();
        let mut namespaces = PublicDeclarations::new();
        let mut mir_funs = Vec::new();
        let mut mir_imports = Vec::new();
        let mut runtime_modules = HashSet::new();
        // Collect dependencies
        for used in pkg_ast.used.iter_mut() {
            self.detect_circular_imports(&used.path, &imported);
            self.detect_multiple_imports(&used.path, &package_import);
            package_import.insert(used.path.clone());
            // Collect dependencies
            // TODO: Should we hide exposed declarations of imported packages?
            let pub_decls = if let Some(pub_decls) = self.pub_decls.get(&used.path) {
                // Already processed and cached.
                pub_decls.clone()
            } else if let Some((package_root, subpackage_path)) = split_package_name(&used.path) {
                let mut imported = imported.clone();
                imported.insert(used.path.clone());
                let (sub_pkg_mir, err_handler) = if package_root == root {
                    // Current package
                    self.get_subpackage_mir(&used, root, root_path.as_ref().to_owned(), imported)?
                } else if let Some(package) = self.as_known_pachage(&package_root) {
                    // Known package
                    self.get_known_package_mir(package, subpackage_path, imported)?
                } else {
                    self.err
                        .report_no_loc(format!("Unknown package '{}'.", &used.path));
                    self.err.merge(error_handler);
                    exit!(self);
                };
                // Merge package content
                error_handler.merge(err_handler);
                self.detect_duplicate_runtime_modules(&mut runtime_modules, &sub_pkg_mir.imports);
                mir_funs.extend(sub_pkg_mir.funs);
                mir_imports.extend(sub_pkg_mir.imports);
                self.pub_decls
                    .insert(used.path.clone(), sub_pkg_mir.pub_decls.clone());
                sub_pkg_mir.pub_decls
            } else {
                self.err
                    .report_no_loc(format!("Use path is not well formatted '{}'.", &used.path));
                self.err.merge(error_handler);
                exit!(self);
            };
            if let Some(alias) = &used.alias {
                namespaces.insert(alias.clone(), pub_decls);
            } else {
                namespaces.insert(get_alias(&used.path).to_owned(), pub_decls);
            }
        }
        let hir_program = hir::to_hir(pkg_ast, namespaces, &mut error_handler, &self.config);
        let mut mir_program = mir::to_mir(hir_program, &mut error_handler, &self.config);
        // Insert imported functions
        mir_program.funs.extend(mir_funs);
        mir_program.imports.extend(mir_imports);
        Ok((mir_program, error_handler))
    }

    /// Returns the MIR of a known package.
    fn get_known_package_mir(
        &mut self,
        package: KnownPackage,
        subpackage_path: Option<String>,
        imported: HashSet<String>,
    ) -> Result<(mir::Program, error::ErrorHandler), ()> {
        if let Some(known_package_path) = &self.known_package_path {
            let mut path = known_package_path.clone();
            let suffix = match package {
                KnownPackage::Core => "core",
            };
            path.push(suffix);
            let root_path = path.clone();
            if let Some(subpackage_path) = subpackage_path {
                path.extend(subpackage_path.split('/'));
            }
            self.get_package_mir(path.clone(), Some(suffix), root_path, imported)
        } else {
            self.err.report_no_loc(format!(
                "Can't localize standard packages, the environment variable {} is not set.",
                ZEPHYR_LIB
            ));
            exit!(self);
        }
    }

    /// Returns the MIR of a subpackage
    ///
    /// params:
    ///  - used: zephyr 'use' declaration
    ///  - root: root of the package
    ///  - root_path: the path to the package root
    ///  - imported: a set of already imported packages
    fn get_subpackage_mir(
        &mut self,
        used: &ast::Use,
        root: &str,
        root_path: PathBuf,
        mut imported: HashSet<String>,
    ) -> Result<(mir::Program, error::ErrorHandler), ()> {
        let mut package_path = root_path.clone();
        package_path.push(strip_root(&used.path));
        if !package_path.exists() {
            // No directory found, look for a single file package.
            package_path = root_path.clone();
            let mut file_name = strip_root(&used.path).to_string();
            file_name.push('.');
            // try to find ASM file first
            let mut asm_file_name = file_name.clone();
            asm_file_name.push_str(ASM_EXTENSION);
            let mut asm_path = package_path.clone();
            asm_path.push(&asm_file_name);
            if asm_path.exists() {
                package_path.push(asm_file_name);
            } else {
                file_name.push_str(ZEPHYR_EXTENSION);
                package_path.push(file_name);
            }
        }
        imported.insert(used.path.clone());
        self.get_package_mir(package_path, Some(root), root_path, imported)
    }

    /// Returns the AST of the package located at the given path.
    fn get_package_ast<P: AsRef<Path>>(
        &mut self,
        path: P,
    ) -> Result<(ast::Program, error::ErrorHandler), ()> {
        // Resolve path
        let resolved_path = match resolve_path(&path) {
            Ok(result) => result,
            Err(err) => {
                self.err.report_no_loc(err);
                exit!(self);
            }
        };
        let is_single_file = match resolved_path {
            ResolvedPath::Dir(_) => false,
            ResolvedPath::File(_) => true,
        };
        let path = path.as_ref();

        // Build package ASTs
        let mut ast_programs = Vec::new();
        let package_id = self.fresh_package_id();
        let files = self.prepare_files(resolved_path);
        for file in files.into_iter() {
            let mut error_handler = error::ErrorHandler::new(file.code, file.f_id);
            let ast_program = ast::get_ast(
                file.f_id,
                package_id,
                file.kind,
                &mut error_handler,
                &self.config,
            );
            ast_programs.push((ast_program, error_handler, file.file_name));
        }

        // Merge package ASTs
        let mut funs = Vec::new();
        let mut exposed = Vec::new();
        let mut imported = Vec::new();
        let mut used = Vec::new();
        let mut error_handler: Option<error::ErrorHandler> = None;
        let mut package_definition: Option<ast::Package> = None;

        // Iterate over ast_program of all zephyr files in the folder
        for (ast, err_handler, file_name) in ast_programs {
            let package_name = &ast.package.name;
            match ast.package.t {
                ast::PackageType::Standard => {
                    // If we got only a single file from a standard package, search for other
                    if is_single_file {
                        return self.get_package_ast(
                            path.parent().expect("Could not resolve parent directory"),
                        );
                    }
                    self.check_ast_coherence(&mut package_definition, &ast.package);
                    // Extend AST package
                    if let Some(ref mut error_handler) = error_handler {
                        error_handler.merge(err_handler);
                    } else {
                        error_handler = Some(err_handler);
                    }
                    funs.extend(ast.funs);
                    exposed.extend(ast.exposed);
                    used.extend(ast.used);
                }
                ast::PackageType::Standalone => {
                    // Discard any standalone package if compiling the standard package of the directory
                    if is_single_file {
                        if package_name != &file_name {
                            self.err.warn_no_loc(format!(
                                "Standalone package '{}' file should be named '{}.zph'",
                                &package_name, &package_name
                            ))
                        }
                        error_handler = Some(err_handler);
                        package_definition = Some(ast.package);
                        funs.extend(ast.funs);
                        exposed.extend(ast.exposed);
                        imported.extend(ast.imports);
                        used.extend(ast.used);
                    }
                }
            }
        }
        // Validate and build final package
        if let (Some(package), Some(error_handler)) = (package_definition, error_handler) {
            Ok((
                ast::Program {
                    package,
                    exposed,
                    imports: imported,
                    used,
                    funs,
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

    /// Matches the package root against well known packages, such as 'core'.
    /// Return some if the package is known from the compiler.
    pub fn as_known_pachage(&self, package_root: &str) -> Option<KnownPackage> {
        if !self.with_known_packages {
            return None;
        }
        match package_root {
            "core" => Some(KnownPackage::Core),
            _ => None,
        }
    }

    /// Look at a file extension to decide of its kind.
    fn get_file_kind(&mut self, path: &Path) -> ast::Kind {
        if let Some(ext) = path.extension() {
            if ext.eq(ZEPHYR_EXTENSION) {
                ast::Kind::Zephyr
            } else if ext.eq(ASM_EXTENSION) {
                ast::Kind::Asm
            } else {
                self.err.report_internal_no_loc(format!(
                    "Allowed unknown extension at: '{}'.",
                    path.to_str().unwrap_or("UNWRAP_ERROR")
                ));
                exit!(self);
            }
        } else {
            self.err.report_internal_no_loc(format!(
                "Allowed file without extension: '{}'.",
                path.to_str().unwrap_or("UNWRAP_ERROR")
            ));
            exit!(self);
        }
    }

    /// Iterates through all the path, reads the files and bundle code with medat-data.
    ///
    /// Each file gets its own ID, a kind (zephyr or asm) and the code is read.
    pub fn prepare_files(&mut self, resolved_path: ResolvedPath) -> Vec<PreparedFile> {
        let mut files = Vec::new();
        let paths = match resolved_path {
            ResolvedPath::Dir(paths) => paths,
            ResolvedPath::File(file) => vec![file],
        };
        for path in paths {
            // Get a fresh f_id
            let f_id = self.file_id;
            self.file_id += 1;

            let kind = self.get_file_kind(&path);
            let code = fs::read_to_string(&path).expect("Internal error: invalid path.");
            let file_name = path
                .file_stem()
                .expect("Internal error: path is not a file.")
                .to_str()
                .expect("File name at seems to use non standard characters")
                .to_string();
            files.push(PreparedFile {
                code,
                f_id,
                file_name,
                kind,
            });
        }
        files
    }

    /// Raises an error if an AST package declare different package names or kinds.
    ///
    /// Params:
    ///  - package_definition: The expected package caracteristics, or None
    ///  - package_shard: A shard of the package (typically the content of a file)
    ///
    /// If `package_definition` is none, it will be set to `package_shard`
    pub fn check_ast_coherence(
        &mut self,
        package_definition: &mut Option<ast::Package>,
        package_shard: &ast::Package,
    ) {
        if let Some(package_def) = package_definition {
            if &package_shard.name != &package_def.name {
                self.err.report_no_loc(
                    format!(
                        "Expected package '{}', found '{}'. This may happen if you have multiple packages in the same directory.",
                        &package_def.name,&package_shard.name
                    ),
                );
                exit!(self);
            }
            if &package_shard.kind != &package_def.kind {
                self.err.report_no_loc(String::from("All files of a package must share the same package kind (`package` or `runtime`)"));
                exit!(self);
            }
        } else {
            *package_definition = Some(package_shard.clone());
        }
    }

    /// Raises and error if a circular import is detected.
    ///
    /// Params:
    ///  - path: path to import (from 'use' statement)
    ///  - imported: a set oof paths already imported
    pub fn detect_circular_imports(&mut self, path: &str, imported: &HashSet<String>) {
        if imported.contains(path) {
            self.err.report_no_loc(format!(
                "Circular import detected: '{}' already imported.",
                path
            ));
            exit!(self);
        }
    }

    /// Creates a warning if a same dependency is imported multiple times.
    ///
    /// Params:
    ///  - path: path to import (from 'use' statement)
    ///  - package_imports: set of packages path already imported.
    pub fn detect_multiple_imports(&mut self, path: &str, package_imports: &HashSet<String>) {
        if package_imports.contains(path) {
            self.err.warn_no_loc(format!(
                "Package '{}' is imported multiple times from the same package.",
                path
            ));
        }
    }

    /// Raises an error if a runtime module has already been defined in the `modules` set
    /// and insert the newly defines modules into that set.
    ///
    /// params:
    ///  - modules: a set of modules already imported.
    ///  - imports: the new import definitions.
    pub fn detect_duplicate_runtime_modules(
        &mut self,
        modules: &mut HashSet<String>,
        imports: &Vec<mir::Imports>,
    ) {
        for import in imports {
            if modules.contains(&import.from) {
                self.err.report_no_loc(format!(
                    "The runtime module '{}' is defined in multiple places.",
                    &import.from
                ));
            } else {
                modules.insert(import.from.clone());
            }
        }
    }
}
