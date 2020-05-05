#[derive(Copy, Clone)]
pub struct Location {
    pub pos: u32,
    pub len: u32,
}

struct Error {
    loc: Option<Location>,
    t: ErrorType,
    message: String,
}

enum ErrorType {
    Internal,
    Any,
}

impl Location {
    // Use to create empty location when needed
    pub fn dummy() -> Location {
        Location { pos: 0, len: 0 }
    }
}

pub struct ErrorHandler {
    has_error: bool,
    errors: Vec<Error>,
}

impl ErrorHandler {
    pub fn new() -> ErrorHandler {
        ErrorHandler {
            has_error: false,
            errors: Vec::new(),
        }
    }

    pub fn report_no_loc(&mut self, message: String) {
        self.has_error = true;
        self.errors.push(Error {
            loc: None,
            t: ErrorType::Any,
            message: message,
        })
    }

    pub fn report(&mut self, loc: Location, message: String) {
        self.has_error = true;
        self.errors.push(Error {
            loc: Some(loc),
            t: ErrorType::Any,
            message: message,
        })
    }

    pub fn report_internal(&mut self, loc: Location, message: String) {
        self.has_error = true;
        self.errors.push(Error {
            loc: Some(loc),
            t: ErrorType::Internal,
            message: message,
        })
    }

    pub fn report_internal_no_loc(&mut self, message: String) {
        self.has_error = true;
        self.errors.push(Error {
            loc: None,
            t: ErrorType::Internal,
            message: message,
        })
    }

    pub fn silent_report(&mut self) {
        self.has_error = true;
    }

    pub fn failed(&self) -> bool {
        self.has_error
    }
}
