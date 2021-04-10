//! The Compilation Context
use std::cell::Cell;
use std::collections::{HashMap, HashSet};

use super::known_functions;
use super::known_functions::{
    KnownFunctionPaths, KnownFunctions, KnownStructPaths, KnownStructs, KnownValues,
};
use super::utils::ModuleDeclarations;
use crate::ast;
use crate::error::ErrorHandler;
use crate::hir;
use crate::mir;
use crate::resolver::{ModuleKind, ModulePath, PreparedFile, Resolver};
use crate::wasm;

pub type ModId = u32;

type StructMap = HashMap<hir::StructId, hir::Struct>;
type TupleMap = HashMap<hir::TupleId, hir::Tuple>;
type DataMap = HashMap<hir::DataId, hir::Data>;
type TypeMap = HashMap<hir::TypeId, hir::Type>;
type FunMap = HashMap<hir::FunId, hir::FunKind>;
type ModMap = HashMap<ModId, ModulePath>;
type ReverseModMap = HashMap<ModulePath, ModId>;
type DeclMap = HashMap<ModulePath, ModuleDeclarations>;

/// The global compilation context.
pub struct Ctx {
    // HIR elements
    structs: StructMap,
    tuples: TupleMap,
    types: TypeMap,
    data: DataMap,
    funs: FunMap,
    mods: ModMap,
    mods_ids: ReverseModMap,
    public_decls: DeclMap,
    imports: Vec<hir::Import>,
    packages: Vec<hir::Package>,

    // Configuration
    knwon_values: KnownValues,
    mod_id: Cell<ModId>,
    verbose: bool,
}

impl Ctx {
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
            tuples: HashMap::new(),
            types: HashMap::new(),
            data: HashMap::new(),
            funs: HashMap::new(),
            mods: HashMap::new(),
            mods_ids: HashMap::new(),
            imports: Vec::new(),
            packages: Vec::new(),
            public_decls: HashMap::new(),
            knwon_values: KnownValues::uninitialized(),
            mod_id: Cell::new(1), // ModId 0 is reserverd
            verbose: false,
        }
    }

    /// Toggle verbose mode, default to `false`.
    pub fn set_verbose(&mut self, verbose: bool) {
        self.verbose = verbose;
    }

    /// Get a structure from its ID.
    pub fn get_struct(&self, s_id: hir::StructId) -> Option<&hir::Struct> {
        self.structs.get(&s_id)
    }

    /// Get a tuple from its ID.
    pub fn get_tuple(&self, tup_id: hir::TupleId) -> Option<&hir::Tuple> {
        self.tuples.get(&tup_id)
    }

    /// Get a type from its ID.
    pub fn get_type(&self, t_id: hir::TypeId) -> Option<&hir::Type> {
        self.types.get(&t_id)
    }

    /// Get a function from its ID.
    pub fn get_fun(&self, fun_id: hir::FunId) -> Option<&hir::FunKind> {
        self.funs.get(&fun_id)
    }

    /// Get module declarations from the module ID.
    pub fn get_mod_from_id(&self, mod_id: ModId) -> Option<&ModuleDeclarations> {
        let mod_path = self.mods.get(&mod_id)?;
        self.public_decls.get(mod_path)
    }

    /// Get module declarations from the module path.
    pub fn get_mod_from_path(&self, mod_path: &ModulePath) -> Option<&ModuleDeclarations> {
        self.public_decls.get(mod_path)
    }

    /// Get a module ID from its path.
    pub fn get_mod_id_from_path(&self, mod_path: &ModulePath) -> Option<ModId> {
        self.mods_ids.get(mod_path).map(|id| *id)
    }

    pub fn get_mod_path_from_id(&self, mod_id: ModId) -> Option<&ModulePath> {
        self.mods.get(&mod_id)
    }

    pub fn hir_funs(&self) -> &FunMap {
        &self.funs
    }

    pub fn hir_structs(&self) -> &StructMap {
        &self.structs
    }

    pub fn hir_tuples(&self) -> &TupleMap {
        &self.tuples
    }

    pub fn hir_imports(&self) -> &Vec<hir::Import> {
        &self.imports
    }

    pub fn hir_data(&self) -> &DataMap {
        &self.data
    }

    /// Given a list of files return the corresponding module.
    pub fn get_module_name(
        &mut self,
        files: Vec<PreparedFile>,
        err: &mut impl ErrorHandler,
    ) -> Result<String, ()> {
        let is_single_file = files.len() == 1;
        let ast_programs = self.parse_files(files, err)?;
        let mut module: Option<String> = None;
        let mut other_modules = HashSet::new();
        for (ast_program, _, _) in ast_programs {
            match ast_program.package.t {
                ast::PackageType::Standalone => {
                    if is_single_file {
                        return Ok(ast_program.package.name);
                    }
                }
                ast::PackageType::Standard => {
                    let new_mod_name = ast_program.package.name;
                    if let Some(ref mod_name) = module {
                        if &new_mod_name != mod_name {
                            if !other_modules.contains(&new_mod_name) {
                                err.report(
                                    ast_program.package.loc,
                                    format!(
                                        "Expected module '{}', found '{}'",
                                        &mod_name, &new_mod_name
                                    ),
                                );
                                other_modules.insert(new_mod_name);
                            }
                        }
                    } else {
                        module = Some(new_mod_name);
                    }
                }
            }
        }
        if let Some(mod_name) = module {
            Ok(mod_name)
        } else {
            err.report_no_loc(String::from("Could not find standard module"));
            Err(())
        }
    }

    /// Add a module to the context.
    pub fn add_module(
        &mut self,
        module: ModulePath,
        err: &mut impl ErrorHandler,
        resolver: &impl Resolver,
    ) -> Result<(), ()> {
        self.initialize_known_values(err, resolver)?;
        let hir = self.get_hir(&module, HashSet::new(), err, resolver)?;
        self.extend_hir(hir, module);
        Ok(())
    }

    /// Generate WebAssembly from the HIR in the current compilation context.
    pub fn get_wasm(
        &mut self,
        err: &mut impl ErrorHandler,
        resolver: &impl Resolver,
    ) -> Result<Vec<u8>, ()> {
        self.initialize_known_values(err, resolver)?;
        let known_funs = self.get_known_functions(err, resolver)?;
        let mir = mir::to_mir(&self, &known_funs, err, self.verbose);
        Ok(wasm::to_wasm(mir, err, self.verbose))
    }

    /// Parses a module and return its AST (abstract syntax tree).
    fn get_ast(
        &self,
        module: &ModulePath,
        err: &mut impl ErrorHandler,
        resolver: &impl Resolver,
    ) -> Result<ast::Program, ()> {
        let (files, module_kind) = resolver.resolve_module(module, err)?;
        let ast_programs = self.parse_files(files, err)?;
        let mut package: Option<ast::Program> = None;
        let mut package_definition: Option<ast::Package> = None;

        // Iterate over ast_program of all zephyr files in the folder
        for (ast, mut err_handler, file_name) in ast_programs {
            match ast.package.t {
                ast::PackageType::Standard => {
                    if module_kind == ModuleKind::Standalone {
                        err.report_no_loc(format!("Module path '{}' points to a single file, but contains a standard module: expected standalone module.", module));
                        return Err(());
                    }
                    self.check_ast_coherence(
                        &mut package_definition,
                        &ast.package,
                        &mut err_handler,
                    )?;
                    // Extend AST package
                    err.merge(err_handler);
                    if let Some(ref mut pkg) = package {
                        pkg.merge(ast);
                    } else {
                        package = Some(ast);
                    }
                }
                ast::PackageType::Standalone => {
                    // Discard any standalone package if compiling the standard package of the directory
                    if module_kind == ModuleKind::Standalone {
                        let package_name = &ast.package.name;
                        if package_name != &file_name {
                            err.warn_no_loc(format!(
                                "Standalone module '{}' file should be named '{}.zph'",
                                &package_name, &package_name
                            ))
                        }
                        err.merge(err_handler);
                        if let Some(ref mut pkg) = package {
                            pkg.merge(ast);
                        } else {
                            package = Some(ast);
                        }
                    }
                }
            }
        }
        if let Some(pkg) = package {
            Ok(pkg)
        } else {
            err.report_no_loc(format!("'{}' is not a valid module.", module));
            Err(())
        }
    }

    /// Produces HIR (High-level Intermediate Representation) for a modyle by collecting and
    /// lowering its AST.
    ///
    /// ! Caution: the caller is responsible for storing the HIR in Ctx after usage.
    ///
    /// params:
    ///  - module: the path of the module of interest.
    ///  - imported: a set of already imported modules.
    ///  - err: and error handler.
    ///  - resolver: a path resolver.
    fn get_hir(
        &mut self,
        module: &ModulePath,
        imported: HashSet<ModulePath>,
        err: &mut impl ErrorHandler,
        resolver: &impl Resolver,
    ) -> Result<hir::Program, ()> {
        // Get AST
        let mut pkg_ast = self.get_ast(module, err, resolver)?;
        // Prepare HIR
        let mut namespaces = HashMap::new();
        let mut package_import = HashSet::new();
        // Collect dependencies
        for used in pkg_ast.used.iter_mut() {
            self.detect_circular_imports(&used.path, &imported, err)?;
            self.detect_multiple_imports(&used.path, &package_import, err);
            package_import.insert(used.path.clone());
            // Collect dependencies
            // TODO: Should we hide exposed declarations of imported packages?
            let mod_id = if let Some(pub_decls) = self.public_decls.get(&used.path) {
                // Already processed and cached.
                pub_decls.mod_id
            } else {
                let mut imported = imported.clone();
                imported.insert(used.path.clone());
                let module_hir = self.get_hir(&used.path, imported, err, resolver)?;
                // Merge package content
                let mod_id = module_hir.package.id;
                self.extend_hir(module_hir, used.path.clone());
                mod_id
            };
            if let Some(alias) = &used.alias {
                namespaces.insert(alias.clone(), mod_id);
            } else {
                namespaces.insert(used.path.alias().to_owned(), mod_id);
            }
        }
        let hir_program = hir::to_hir(
            pkg_ast,
            namespaces,
            &self,
            &self.knwon_values,
            err,
            self.verbose,
        );
        Ok(hir_program)
    }

    /// Initialize the known values (values such as `Str` or `malloc`) if they are not yet defined,
    /// otherwise return imediately.
    fn initialize_known_values(
        &mut self,
        err: &mut impl ErrorHandler,
        resolver: &impl Resolver,
    ) -> Result<(), ()> {
        if self.knwon_values.is_initialized() {
            return Ok(());
        }
        let funs = self.get_known_functions(err, resolver)?;
        let structs = self.get_known_structs(err, resolver)?;
        self.knwon_values = KnownValues { funs, structs };
        Ok(())
    }

    /// Return the IDs of the known functions, that is functions that are known to the compiler and
    /// needed for the latter phases of the compilation.
    fn get_known_functions(
        &mut self,
        err: &mut impl ErrorHandler,
        resolver: &impl Resolver,
    ) -> Result<KnownFunctions, ()> {
        let modules = KnownFunctionPaths::get();
        let malloc_decl = self
            .get_public_decls(&modules.malloc, err, resolver)?
            .clone();
        let malloc = self.get_fun_from_decls(&malloc_decl, "malloc", &modules.malloc, err)?;
        let malloc = known_functions::validate_malloc(malloc, err)?;
        Ok(KnownFunctions { malloc })
    }

    /// Return the IDs of known structs.
    fn get_known_structs(
        &mut self,
        err: &mut impl ErrorHandler,
        resolver: &impl Resolver,
    ) -> Result<KnownStructs, ()> {
        let modules = KnownStructPaths::get();
        let str_decl = self.get_public_decls(&modules.str, err, resolver)?.clone();
        let str = self.get_struct_from_decl(&str_decl, "Str", &modules.str, err)?;
        let str = known_functions::validate_str(str, err)?;
        Ok(KnownStructs { str })
    }

    /// Return the public declarations of a given module, will build HIR if not already in Ctx.
    ///
    /// params:
    ///  - module: the module from which to get public declarations.
    ///  - err: an error handler.
    ///  - resolver: a path resolver.
    fn get_public_decls(
        &mut self,
        module: &ModulePath,
        err: &mut impl ErrorHandler,
        resolver: &impl Resolver,
    ) -> Result<&ModuleDeclarations, ()> {
        if self.public_decls.get(module).is_none() {
            let hir = self.get_hir(module, HashSet::new(), err, resolver)?;
            self.extend_hir(hir, module.clone());
        }
        Ok(&self.public_decls[module])
    }

    /// Return a function from the public declarations of a module.
    /// Raises an error if the function is not found.
    ///
    /// params:
    ///  - public_decls: public declarations of a module, expected to contain the function.
    ///  - fun: the identifier of the function.
    ///  - module: a path to the module.
    ///  - err: an error handler.
    fn get_fun_from_decls(
        &self,
        public_decls: &ModuleDeclarations,
        fun: &str,
        module: &ModulePath,
        err: &mut impl ErrorHandler,
    ) -> Result<&hir::FunKind, ()> {
        if let Some(fun) = public_decls.val_decls.get(fun) {
            let fun_id = match fun {
                hir::ValueDeclaration::Function(fun_id) => *fun_id,
                _ => return Err(()),
            };
            let fun = self
                .funs
                .get(&fun_id)
                .expect("Function declared but not in context");
            Ok(fun)
        } else {
            err.report_no_loc(format!("Could not find funtion '{}' at '{}'", fun, module));
            Err(())
        }
    }

    /// Return a struct from the public declarations of a module.
    /// Raises an error if the struct is not found.
    ///
    /// params:
    ///  - public_decls: public declarations of a module, expected to contain the struct.
    ///  - struc: the identifier of the struct.
    ///  - module: a path to the module.
    ///  - err: an error handler.
    fn get_struct_from_decl(
        &self,
        public_decls: &ModuleDeclarations,
        struc: &str,
        module: &ModulePath,
        err: &mut impl ErrorHandler,
    ) -> Result<&hir::Struct, ()> {
        if let Some(struc) = public_decls.type_decls.get(struc) {
            let struct_id = if let hir::Type::Struct(s_id) = struc {
                s_id
            } else {
                err.report_no_loc(format!("Expected a struct, got '{}'", &struc));
                return Err(());
            };
            let struc = self
                .structs
                .get(&struct_id)
                .expect("Struct declared but not in context");
            Ok(struc)
        } else {
            err.report_no_loc(format!("Could not find struct '{}' at '{}'", struc, module));
            Err(())
        }
    }

    /// Extend the Ctx HIR with a package.
    ///
    /// params:
    ///  - hir: The hir package to add
    ///  - module: the import path of the hir module, can be use to get public declarations.
    fn extend_hir(&mut self, hir: hir::Program, module: ModulePath) {
        for (s_id, struc) in hir.structs {
            let prev = self.structs.insert(s_id, struc);
            debug_assert!(prev.is_none()); // s_id must be unique
        }
        for (tup_id, tup) in hir.tuples {
            let prev = self.tuples.insert(tup_id, tup);
            debug_assert!(prev.is_none()); // tup_id must be unique
        }
        for fun in hir.funs {
            let prev = self.funs.insert(fun.fun_id, hir::FunKind::Fun(fun));
            assert!(prev.is_none()); // fun_id must be unique
        }
        for (d_id, data) in hir.data {
            let prev = self.data.insert(d_id, data);
            debug_assert!(prev.is_none()); // d_id must be unique
        }
        for import in hir.imports {
            let mut prototypes = Vec::new();
            for fun in import.prototypes {
                prototypes.push(fun.fun_id);
                let prev = self.funs.insert(fun.fun_id, hir::FunKind::Extern(fun));
                debug_assert!(prev.is_none()); // fun_id must be unique
            }
            self.imports.push(hir::Import {
                from: import.from,
                loc: import.loc,
                prototypes,
            })
        }
        self.mods.insert(hir.package.id, module.clone());
        self.mods_ids.insert(module.clone(), hir.package.id);
        self.packages.push(hir.package);
        self.public_decls.insert(module, hir.pub_decls);
    }

    /// Parses files and return the a tuple (AST, error_handler, file_name) per file.
    fn parse_files<E: ErrorHandler>(
        &self,
        files: Vec<PreparedFile>,
        _err: &mut E,
    ) -> Result<Vec<(ast::Program, E, String)>, ()> {
        let mut ast_programs = Vec::with_capacity(files.len());
        let mod_id = self.fresh_mod_id();
        for file in files.into_iter() {
            let mut error_handler = E::new(file.code, file.f_id);
            let ast_program = ast::get_ast(
                file.f_id,
                mod_id,
                file.kind,
                &mut error_handler,
                self.verbose,
            );
            ast_programs.push((ast_program, error_handler, file.file_name));
        }
        Ok(ast_programs)
    }

    /// Raises an error if an AST package declare different package names or kinds.
    ///
    /// Params:
    ///  - package_definition: The expected package caracteristics, or None.
    ///  - package_shard: A shard of the package (typically the content of a file).
    ///
    /// If `package_definition` is none, it will be set to `package_shard`.
    fn check_ast_coherence(
        &self,
        package_definition: &mut Option<ast::Package>,
        package_shard: &ast::Package,
        err: &mut impl ErrorHandler,
    ) -> Result<(), ()> {
        if let Some(package_def) = package_definition {
            if &package_shard.name != &package_def.name {
                err.report_no_loc(
                    format!(
                        "Expected module '{}', found '{}'. This may happen if you have multiple modules in the same directory.",
                        &package_def.name,&package_shard.name
                    ),
                );
                return Err(());
            }
            if &package_shard.kind != &package_def.kind {
                err.report_no_loc(String::from(
                    "All files of a module must share the same module kind (`mod` or `runtime`)",
                ));
                return Err(());
            }
        } else {
            *package_definition = Some(package_shard.clone());
        }
        Ok(())
    }

    /// Raises and error if a circular import is detected.
    ///
    /// Params:
    ///  - path: path to import (from 'use' statement).
    ///  - imported: a set oof paths already imported.
    ///  - err: an error handler.
    fn detect_circular_imports(
        &self,
        path: &ModulePath,
        imported: &HashSet<ModulePath>,
        err: &mut impl ErrorHandler,
    ) -> Result<(), ()> {
        if imported.contains(path) {
            err.report_no_loc(format!(
                "Circular import detected: '{}' already imported.",
                path
            ));
            Err(())
        } else {
            Ok(())
        }
    }

    /// Creates a warning if a same dependency is imported multiple times.
    ///
    /// Params:
    ///  - path: path to import (from 'use' statement).
    ///  - module_imports: set of path already imported by the module.
    ///  - err: an error handler.
    fn detect_multiple_imports(
        &self,
        path: &ModulePath,
        module_imports: &HashSet<ModulePath>,
        err: &mut impl ErrorHandler,
    ) {
        if module_imports.contains(path) {
            err.warn_no_loc(format!(
                "Package '{}' is imported multiple times from the same package.",
                path
            ));
        }
    }

    /// Generates a fresh (unique) module ID.
    fn fresh_mod_id(&self) -> ModId {
        let mod_id = self.mod_id.get();
        self.mod_id.set(mod_id + 1);
        mod_id
    }
}
