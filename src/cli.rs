extern crate clap;

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

    /// Exclude standard packages (core, std...)
    #[clap(long)]
    pub no_std: bool,
}

/// Parse CLI args, may terminate the program.
pub fn parse() -> Config {
    Config::parse()
}
