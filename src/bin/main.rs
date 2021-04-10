//! The compiler binary
//!
//! This file define de CLI of the zephyr compiler, it is build on top of the compiler library.

use clap;
use std::fs;
use std::path;

use zephyr::{Ctx, ErrorHandler, ModulePath, StandardErrorHandler, StandardResolver};

use clap::Clap;
use std::path::PathBuf;

/// The Zephyr compiler.
#[derive(Clap, Debug)]
#[clap(version = "0.1.0")]
pub struct Config {
    /// Use verbose output
    #[clap(short, long)]
    pub verbose: bool,

    /// Package to build
    #[clap(default_value = ".", parse(from_os_str))]
    pub input: PathBuf,

    /// Output location
    #[clap(short, long, parse(from_os_str))]
    pub output: Option<PathBuf>,

    /// Type check the package
    #[clap(long)]
    pub check: bool,
}

fn main() {
    let config = Config::parse();
    let mut resolver = StandardResolver::new();
    let mut err = StandardErrorHandler::new_no_file();
    let mut ctx = Ctx::new();
    ctx.set_verbose(config.verbose);

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
    let module = ModulePath::from_root(module_name.clone());
    resolver.add_package(module_name.clone(), path);

    // Compile
    let _ = ctx.add_module(module, &mut err, &mut resolver);
    err.flush_and_exit_if_err();
    if config.check {
        std::process::exit(0);
    }
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
