//! The standard resolver
//!
//! This is the implementation used in the official binary of the Zephyr compiler.

use std::cell::Cell;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::{ErrorHandler, FileKind, ModuleKind, ModulePath, PreparedFile, Resolver};

// File extensions
pub const ZEPHYR_EXTENSION: &str = "zph";
pub const ASM_EXTENSION: &str = "zasm";

// Packages
pub const CORE: &str = "core";
pub const STD: &str = "std";

/// Expectend environment variable pointing to Zephyr known packages.
const ZEPHYR_LIB: &'static str = "ZEPHYR_LIB";

type FileId = u16;

/// The result of a path lookup: either file or a directory
pub enum ResolvedPath {
    Dir(Vec<PathBuf>),
    File(PathBuf),
}

/// The standard implementation of a resolver, used by the zephyr CLI application.
pub struct StandardResolver {
    package_paths: HashMap<String, PathBuf>,
    file_id: Cell<FileId>,
}

impl StandardResolver {
    pub fn new() -> Self {
        let mut package_paths = HashMap::new();

        // Get libraries path from environment
        let known_package_path = std::env::var(ZEPHYR_LIB).expect(&format!(
            "The environment variable '{}' was not found.",
            ZEPHYR_LIB
        ));
        let zephyr_path = PathBuf::from(&known_package_path);

        // Build path to known packages
        let mut core_path = zephyr_path.clone();
        core_path.push(CORE);
        let mut std_path = zephyr_path;
        std_path.push(STD);

        // Map package roots to paths
        package_paths.insert(String::from(CORE), core_path);
        package_paths.insert(String::from(STD), std_path);
        Self {
            package_paths,
            file_id: Cell::new(0),
        }
    }

    /// Register a new package so that modules of this package can be resolved in the future.
    pub fn add_package(&mut self, pkg_name: String, path: PathBuf) {
        self.package_paths.insert(pkg_name, path);
    }

    /// Prepare files at a given path.
    pub fn prepare_files<P: AsRef<Path>>(
        &self,
        path: P,
        err: &mut impl ErrorHandler,
    ) -> Result<(Vec<PreparedFile>, ModuleKind), ()> {
        let path = match resolve_path(path) {
            Ok(path) => path,
            Err(e) => {
                err.report_no_loc(e);
                return Err(());
            }
        };
        match self.prepare_files_from_resolved_path(path, err) {
            Ok(files) => Ok(files),
            Err(()) => return Err(()),
        }
    }

    /// Iterates through all the path, reads the files and bundle code with medat-data.
    ///
    /// Each file gets its own ID, a kind (zephyr or asm) and the code is read.
    fn prepare_files_from_resolved_path(
        &self,
        resolved_path: ResolvedPath,
        err: &mut impl ErrorHandler,
    ) -> Result<(Vec<PreparedFile>, ModuleKind), ()> {
        let mut files = Vec::new();
        let (paths, kind) = match resolved_path {
            ResolvedPath::Dir(paths) => (paths, ModuleKind::Standard),
            ResolvedPath::File(file) => (vec![file], ModuleKind::Standalone),
        };
        for path in paths {
            let f_id = self.fresh_f_id();
            let kind = self.get_file_kind(&path, err)?;
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
        Ok((files, kind))
    }

    /// Look at a file extension to decide of its kind.
    fn get_file_kind(&self, path: &Path, err: &mut impl ErrorHandler) -> Result<FileKind, ()> {
        if let Some(ext) = path.extension() {
            if ext.eq(ZEPHYR_EXTENSION) {
                Ok(FileKind::Zephyr)
            } else if ext.eq(ASM_EXTENSION) {
                Ok(FileKind::Asm)
            } else {
                err.report_internal_no_loc(format!(
                    "Allowed unknown extension at: '{}'.",
                    path.to_str().unwrap_or("UNWRAP_ERROR")
                ));
                Err(())
            }
        } else {
            err.report_internal_no_loc(format!(
                "Allowed file without extension: '{}'.",
                path.to_str().unwrap_or("UNWRAP_ERROR")
            ));
            Err(())
        }
    }

    /// Return an unique file ID, will panic when running out of identifier.
    fn fresh_f_id(&self) -> FileId {
        let f_id = self.file_id.get();
        self.file_id
            .set(f_id.checked_add(1).expect("Error: too much files"));
        f_id
    }
}

impl Resolver for StandardResolver {
    fn resolve_module(
        &self,
        module: &ModulePath,
        err: &mut impl ErrorHandler,
    ) -> Result<(Vec<PreparedFile>, ModuleKind), ()> {
        let mut path = match self.package_paths.get(&module.root) {
            Some(path) => path.to_owned(),
            None => {
                err.report_no_loc(format!("Could not find package '{}'", &module.root));
                return Err(());
            }
        };
        path.extend(&module.path);
        self.prepare_files(path, err)
    }
}

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
