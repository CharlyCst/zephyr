use super::errors::Location;

pub trait ErrorHandler {
    fn new(code: String, f_id: u16) -> Self;
    fn new_no_file() -> Self;
    fn get_file(&self, f_id: u16) -> Option<&str>;
    fn has_error(&self) -> bool;
    fn warn_no_loc(&mut self, message: String);
    fn warn(&mut self, loc: Location, message: String);
    fn report_no_loc(&mut self, message: String);
    fn report(&mut self, loc: Location, message: String);
    fn report_internal(&mut self, loc: Location, message: String);
    fn report_internal_no_loc(&mut self, message: String);
    fn silent_report(&mut self);
    fn merge(&mut self, other: Self);
    fn flush(&mut self);

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
