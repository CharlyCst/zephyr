#[derive(Copy, Clone)]
pub struct Location {
    pub line: u32,
    pub pos: u32,
    pub len: u32,
}

impl Location {
    // Use to create empty location when needed
    pub fn dummy() -> Location {
        Location {
            line: 0,
            pos: 0,
            len: 0,
        }
    }
}

pub struct ErrorHandler {
    has_error: bool,
}

impl ErrorHandler {
    pub fn new() -> ErrorHandler {
        ErrorHandler { has_error: false }
    }

    pub fn report_line(&mut self, line: usize, message: &str) {
        self.has_error = true;
        println!("An error was found line {}: {}", line, message);
    }

    pub fn report(&mut self, loc: Location, message: &str) {
        self.has_error = true;
        println!("An error was found line {}: {}", loc.line, message);
    }

    pub fn report_internal_loc(&mut self, loc: Location, message: &str) {
        self.has_error = true;
        println!("Internal error at line {}: {}", loc.line, message);
    }

    pub fn report_internal(&mut self, message: &str) {
        self.has_error = true;
        println!("Internal error: {}", message);
    }

    pub fn silent_report(&mut self) {
        self.has_error = true;
    }

    pub fn success(&self) -> bool {
        !self.has_error
    }
}
