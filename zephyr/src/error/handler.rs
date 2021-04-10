use super::errors::{Level, Location};
use crate::resolver::FileId;

pub trait ErrorHandler {
    fn new(code: String, f_id: FileId) -> Self;
    fn new_no_file() -> Self;
    fn get_file(&self, f_id: FileId) -> Option<&str>;
    fn has_error(&self) -> bool;
    fn silent_report(&mut self);
    fn merge(&mut self, other: Self);
    fn flush(&mut self);

    /// Log an error encountered during the compilation.
    fn log(&mut self, message: String, level: Level, loc: Option<Location>);

    fn warn_no_loc(&mut self, message: String) {
        self.log(message, Level::Warning, None);
    }

    fn warn(&mut self, loc: Location, message: String) {
        self.log(message, Level::Warning, Some(loc));
    }

    fn report_no_loc(&mut self, message: String) {
        self.log(message, Level::Error, None);
    }

    fn report(&mut self, loc: Location, message: String) {
        self.log(message, Level::Error, Some(loc));
    }

    fn report_internal(&mut self, loc: Location, message: String) {
        self.log(message, Level::Internal, Some(loc));
    }

    fn report_internal_no_loc(&mut self, message: String) {
        self.log(message, Level::Internal, None);
    }

    /// If at least one error has been reported, print the errors and exit.
    /// Return immediately without exiting otherwise.
    fn flush_and_exit_if_err(&mut self) {
        if !self.has_error() {
            return;
        }
        self.flush();
        exit();
    }
}

fn exit() {
    std::process::exit(65);
}

/// A mocked error handler for unit testing.
pub struct DummyHandler {
    has_error: bool,
}

impl ErrorHandler for DummyHandler {
    fn new(_code: String, _f_id: FileId) -> Self {
        Self { has_error: false }
    }

    fn new_no_file() -> Self {
        Self { has_error: false }
    }

    fn get_file(&self, _f_id: FileId) -> Option<&str> {
        None
    }

    fn has_error(&self) -> bool {
        self.has_error
    }

    fn silent_report(&mut self) {
        self.has_error = true;
    }
    fn merge(&mut self, other: Self) {
        self.has_error = self.has_error || other.has_error;
    }

    fn flush(&mut self) {}

    /// Log an error encountered during the compilation.
    fn log(&mut self, _message: String, level: Level, _loc: Option<Location>) {
        match level {
            Level::Error | Level::Internal => self.has_error = true,
            Level::Warning => (),
        }
    }
}
