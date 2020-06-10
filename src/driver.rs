use std::fs;

use crate::ast;
use crate::error;
use crate::mir;
use crate::wasm;

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
        // let ast_program = ast::get_ast(&code, &mut error_handler);
        // let mut error_handler = error::ErrorHandler::new(&code, 0);
        // let mir_program = mir::to_mir(ast_program, &mut error_handler);
        // let binary = wasm::to_wasm(mir_program, &mut error_handler);


        let (pkg_ast, mut error_handler) = if let Ok(res) = self.get_package_ast(&self.input.clone()) {
            res
        } else {
            std::process::exit(64);
        };
        let mir_program = mir::to_mir(pkg_ast, &mut error_handler);
        let binary = wasm::to_wasm(mir_program, &mut error_handler);

        match fs::write(&self.output, binary) {
            Ok(_) => (),
            Err(e) => println!("{}", e),
        }

        std::process::exit(0);
    }

    fn get_package_ast(&mut self, path: &str) -> Result<(ast::Program, error::ErrorHandler), ()> {
        // Resolve path
        let paths = match self.resolve_path(path) {
            Ok(paths) => paths,
            Err(err) => {
                println!("{}", &err);
                std::process::exit(64);
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
            let code = error_handler.get_file(f_id).unwrap();
            let ast_program = ast::get_ast(f_id, &mut error_handler);
            ast_programs.push((ast_program, error_handler));
        }

        // Merge package ASTs
        let mut funs = Vec::new();
        let mut exposed = Vec::new();
        let mut used = Vec::new();

        let (ast_program, mut error_handler) = ast_programs.pop().unwrap();
        funs.extend(ast_program.funs);
        exposed.extend(ast_program.exposed);
        used.extend(ast_program.used);

        let package = ast_program.package;

        for (ast, err_handler) in ast_programs {
            funs.extend(ast.funs);
            exposed.extend(ast.exposed);
            used.extend(ast.used);
            error_handler._merge(err_handler);
        }

        return Ok((
            ast::Program {
                package: package,
                exposed: exposed,
                used: used,
                funs: funs,
            },
            error_handler,
        ));
    }

    /// Returns a list of path to files to be parsed.
    /// In case of success, return at least one path.
    fn resolve_path(&self, path: &str) -> Result<Vec<String>, String> {
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
                    Ok(paths)
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
                Ok(vec![path.to_string()])
            } else {
                Err(format!("Invalid file extension '{}'.", &extension))
            }
        } else {
            Err(format!("{}' is neither a file nor a directory.", path))
        }
    }
}

/// Return the file extension of a given path, if applicable.
fn get_extension(path: &str) -> Option<String> {
    match std::path::Path::new(path).extension() {
        Some(ext) => match ext.to_str() {
            Some(ext) => Some(ext.to_string()),
            None => None,
        },
        None => None,
    }
}
