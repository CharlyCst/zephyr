//! Diagnostics
#![allow(dead_code)]

#[derive(Default)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
    /// Wether a diagnostic with the Error level has been produced.
    has_error: bool,
}

pub struct Diagnostic {
    error: Box<dyn Error>,
    level: Level,
    loc: Option<Location>,
}

/// Error level
enum Level {
    /// Fatal error, the program can't be compiled.
    Error,
    /// Issue that does not prevent compilation, but that should probably get fixed.
    Warning,
}

pub struct Location {
    pub line: u32,
    pub column: u32,
}

pub trait Error {
    fn message(&self) -> String;
}

pub trait AsBoxedError {
    fn boxed(self) -> Box<dyn Error>;
}

impl AsBoxedError for Box<dyn Error> {
    fn boxed(self) -> Box<dyn Error> {
        self
    }
}

impl<E: 'static + Error> AsBoxedError for E {
    fn boxed(self) -> Box<dyn Error> {
        Box::new(self)
    }
}

impl Diagnostics {
    pub fn report<E: AsBoxedError>(&mut self, error: E, loc: Location) {
        self.has_error = true;
        self.diagnostics.push(Diagnostic {
            error: error.boxed(),
            level: Level::Error,
            loc: Some(loc),
        });
    }

    pub fn report_no_loc<E: AsBoxedError>(&mut self, error: E) {
        self.has_error = true;
        self.diagnostics.push(Diagnostic {
            error: error.boxed(),
            level: Level::Error,
            loc: None,
        });
    }

    pub fn warn<E: AsBoxedError>(&mut self, error: E, loc: Location) {
        self.diagnostics.push(Diagnostic {
            error: error.boxed(),
            level: Level::Warning,
            loc: Some(loc),
        });
    }

    pub fn warn_no_loc<E: AsBoxedError>(&mut self, error: E) {
        self.diagnostics.push(Diagnostic {
            error: error.boxed(),
            level: Level::Warning,
            loc: None,
        })
    }
}
