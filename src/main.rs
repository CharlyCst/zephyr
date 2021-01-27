use std::fs;
use std::path;

mod ast;
mod cli;
mod driver;
mod error;
mod hir;
mod mir;
mod wasm;

fn main() {
    let config = cli::parse();
    let mut resolver = driver::StandardResolver::new();
    let mut ctx = driver::Ctx::new();
    let mut err = error::ErrorHandler::new_no_file();

    // Resolve paths
    let path = config
        .input
        .clone()
        .canonicalize()
        .expect("Could not resolve path");

    // Prepare files & resolver
    let (module_files, _) = match resolver.prepare_files(&path, &mut err) {
        Ok(files) => files,
       Err(()) => {
            err.flush();
            std::process::exit(65);
        }
    };
    let module_name = match ctx.get_module_name(module_files, &mut err) {
        Ok(module_name) => module_name,
        Err(()) => {
            err.flush();
            std::process::exit(65);
        }
    };
    let module = driver::ModulePath::from_root(module_name.clone());
    resolver.add_package(module_name.clone(), path);

    // Compile
    let _ = ctx.add_module(module, &mut err, &mut resolver);
    err.flush_and_exit_if_err();
    let wasm = match ctx.get_wasm(&mut err, &resolver) {
        Ok(wasm) => wasm,
        Err(()) => {
            err.flush();
            std::process::exit(65);
        }
    };

    // Chose a name for the output
    let output = if let Some(output) = &config.output {
        output.clone()
    } else {
        path::PathBuf::from(&format!("{}.zph.wasm", module_name))
    };

    // Write down compiled code
    match fs::write(&output, wasm) {
        Ok(_) => {
            err.flush();
            std::process::exit(0);
        }
        Err(e) => {
            err.report_no_loc(e.to_string());
            err.flush();
            std::process::exit(0);
        }
    }
}
