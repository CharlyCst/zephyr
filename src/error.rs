// pub struct Error {
//     line: usize,
//     message: String,
// }

pub struct ErrorHandler {
    has_error: bool,
}

impl ErrorHandler {
    pub fn new() -> ErrorHandler {
        ErrorHandler { has_error: false }
    }

    pub fn report(&mut self, line: usize, message: &str) {
        self.has_error = true;
        println!("An error was found line {}: {}", line, message);
    }

    pub fn report_internal(&mut self, line: usize, message: &str) {
        self.has_error = true;
        println!("Internal error at line {}: {}", line, message);
    }

    pub fn silent_report(&mut self) {
        self.has_error = true;
    }

    pub fn success(&self) -> bool {
        !self.has_error
    }
}
