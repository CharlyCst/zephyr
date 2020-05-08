use std::cmp::Ordering;

const RED: &'static str = "\x1B[31m";
const MAGENTA: &'static str = "\x1B[35m";
const BOLD: &'static str = "\x1B[1m";
const END: &'static str = "\x1B[0m";

#[derive(Copy, Clone, Ord, Eq, PartialEq, PartialOrd)]
pub struct Location {
    pub pos: u32,
    pub len: u32,
}

struct Error {
    loc: Option<Location>,
    t: ErrorType,
    message: String,
}

#[derive(Copy, Clone)]
enum ErrorType {
    Internal,
    Any,
}

// Error without location are the smallest
impl Ord for Error {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.loc, other.loc) {
            (None, None) => Ordering::Equal,
            (Some(_), None) => Ordering::Greater,
            (None, Some(_)) => Ordering::Less,
            (Some(loc_1), Some(loc_2)) => loc_1.cmp(&loc_2),
        }
    }
}

impl PartialOrd for Error {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for Error {}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        self.loc == other.loc
    }
}

impl Location {
    // Use to create empty location when needed
    pub fn dummy() -> Location {
        Location { pos: 0, len: 0 }
    }
}

pub struct ErrorHandler<'a> {
    has_error: bool,
    errors: Vec<Error>,
    code: &'a str,
}

impl<'a> ErrorHandler<'a> {
    pub fn new(code: &str) -> ErrorHandler {
        ErrorHandler {
            has_error: false,
            errors: Vec::new(),
            code: code,
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

    // If at least one error has been reported, print the errors and exit
    pub fn print_and_exit(&mut self) {
        if !self.has_error {
            return;
        } else if self.errors.len() == 0 {
            exit();
        }

        self.errors.sort_unstable();

        let mut err = &self.errors[0];
        let mut err_idx = 0;

        // Print all errors without location
        while err.loc.is_none() {
            println!("{:?}", err.loc.is_none());
            self.print(err);

            if err_idx + 1 >= self.errors.len() {
                exit();
            } else {
                err_idx += 1;
                err = &self.errors[err_idx];
            }
        }

        // Print all errors with location
        let mut line = 1;
        let mut lines_pos = 0;
        let mut pos = 0;
        let mut loc = err.loc.unwrap();

        let mut iter = self.code.chars();
        let mut line_iter = iter.clone();
        loop {
            if let Some(c) = iter.next() {
                if c == '\n' {
                    line += 1;
                    lines_pos = pos;
                    line_iter = iter.clone();
                }
                if pos == loc.pos {
                    let new_loc = Location {
                        pos: pos - lines_pos,
                        len: loc.len,
                    };
                    let min_size = new_loc.pos + loc.len;
                    let erroneous_code = self.get_substr(line_iter.clone(), min_size);
                    self.print_line(err, erroneous_code, new_loc, line);

                    if err_idx + 1 >= self.errors.len() {
                        break;
                    } else {
                        err_idx += 1;
                        err = &self.errors[err_idx];
                        loc = err.loc.unwrap();
                    }
                }
                pos += 1;
            } else {
                break;
            }
        }

        exit();
    }

    fn print_line(&self, e: &Error, code: String, loc: Location, line: usize) {
        let color = get_color(e.t);
        let err_name = get_err_name(e.t);

        println!("{:>5} | {}", line, code);
        println!(
            "       {:blank$}{}{:^<underline$}{}",
            " ",
            color,
            "^",
            END,
            blank = loc.pos as usize,
            underline = loc.len as usize
        );
        println!(
            "{}{}{}:{}{} {}{}\n",
            color, BOLD, err_name, END, color, e.message, END
        );
    }

    fn print(&self, e: &Error) {
        let color = get_color(e.t);
        let err_name = get_err_name(e.t);

        println!(
            "{}{}{}:{}{} {}{}\n",
            color, BOLD, err_name, END, color, e.message, END
        );
    }

    fn get_substr(&self, iter: std::str::Chars<'_>, min_size: u32) -> String {
        let mut idx = 0;
        iter.take_while(|c| {
            idx += 1;
            idx < min_size as usize || *c != '\n'
        })
        .collect()
    }
}

fn get_color(t: ErrorType) -> &'static str {
    match t {
        ErrorType::Internal => MAGENTA,
        ErrorType::Any => RED,
    }
}

fn get_err_name(t: ErrorType) -> &'static str {
    match t {
        ErrorType::Internal => "Internal",
        ErrorType::Any => "Error",
    }
}

fn exit() {
    std::process::exit(65);
}
