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

    pub fn report(&mut self, line: usize, message: String) {
        self.has_error = true;
        println!("An error was found line {}: {}", line, message);
    }
}
