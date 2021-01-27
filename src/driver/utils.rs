use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use crate::ast;
use crate::hir;

pub const ZEPHYR_EXTENSION: &str = "zph";
pub const ASM_EXTENSION: &str = "zasm";

/// A namespaced collection of public declarations and collection of all type definitions.
pub struct PublicDeclarations {
    decls: HashMap<String, ModuleDeclarations>,
}

impl PublicDeclarations {
    pub fn new() -> Self {
        Self {
            decls: HashMap::new(),
        }
    }

    pub fn insert(&mut self, path: String, declarations: ModuleDeclarations) {
        self.decls.insert(path, declarations);
    }

    /// Returns the exposed declaration of a package at `path`.
    pub fn get(&self, path: &str) -> Option<&ModuleDeclarations> {
        self.decls.get(path)
    }
}

/// A list of public declarations in a given package.
#[derive(Clone)]
pub struct ModuleDeclarations {
    pub val_decls: HashMap<String, hir::ValueDeclaration>,
    pub type_decls: HashMap<String, hir::TypeDeclaration>,
    pub runtime_modules: HashSet<String>,
}

impl ModuleDeclarations {
    pub fn new() -> Self {
        Self {
            val_decls: HashMap::new(),
            type_decls: HashMap::new(),
            runtime_modules: HashSet::new(),
        }
    }
}

/// A list of packages known from the compiler and expected to be available.
#[allow(dead_code)]
pub enum KnownPackage {
    Core,
}

impl KnownPackage {
    #[allow(dead_code)]
    pub fn as_str(&self) -> &'static str {
        match self {
            KnownPackage::Core => "core",
        }
    }
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

/// A module can be either standalone (inside a single file) or standard (occupate the whole
/// directory).
#[derive(Eq, PartialEq)]
#[allow(dead_code)]
pub enum ModuleKind {
    Standalone,
    Standard,
}

/// A path to a module from the package root.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct ModulePath {
    pub root: String,
    pub path: Vec<String>,
}

impl ModulePath {
    pub fn from_root(root: String) -> Self {
        Self {
            root,
            path: Vec::new(),
        }
    }

    #[allow(dead_code)]
    pub fn from_known_package(pkg: KnownPackage) -> Self {
        let root = pkg.as_str().to_owned();
        Self {
            root,
            path: Vec::new(),
        }
    }

    pub fn alias(&self) -> &str {
        if let Some(module) = self.path.last() {
            module.as_str()
        } else {
            self.root.as_str()
        }
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut path = Vec::with_capacity(1 + self.path.len());
        path.push(self.root.clone());
        path.extend(self.path.clone());
        write!(f, "{}", path.join("."))
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
