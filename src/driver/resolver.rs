//! # Resolver
//!
//! A resolver is a piece responsible for resolving the path of packages and fetching the code,
//! it is used by the Ctx to retrieve imported modules.

use super::utils::{
    resolve_path, ModuleKind, ModulePath, PreparedFile, ResolvedPath, ASM_EXTENSION,
    ZEPHYR_EXTENSION,
};
use crate::ast;
use crate::error::ErrorHandler;
use std::cell::Cell;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

pub type FileId = u16;

pub trait Resolver {
    /// Given a module path return a list of files for that module.
    fn resolve_module(
        &self,
        module: &ModulePath,
        err: &mut ErrorHandler,
    ) -> Result<(Vec<PreparedFile>, ModuleKind), ()>;
}

// Standard Resolver implementation

/// Expectend environment variable pointing to Zephyr known packages.
const ZEPHYR_LIB: &'static str = "ZEPHYR_LIB";

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
        let mut core_path = zephyr_path;
        core_path.push("core");

        // Map package roots to paths
        package_paths.insert(String::from("core"), core_path);
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
        err: &mut ErrorHandler,
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
        err: &mut ErrorHandler,
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
    fn get_file_kind(&self, path: &Path, err: &mut ErrorHandler) -> Result<ast::Kind, ()> {
        if let Some(ext) = path.extension() {
            if ext.eq(ZEPHYR_EXTENSION) {
                Ok(ast::Kind::Zephyr)
            } else if ext.eq(ASM_EXTENSION) {
                Ok(ast::Kind::Asm)
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
        err: &mut ErrorHandler,
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
