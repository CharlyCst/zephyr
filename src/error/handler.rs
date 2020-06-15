use super::errors::{Error, ErrorType, Location};
use std::collections::HashMap;

const RED: &'static str = "\x1B[31m";
const MAGENTA: &'static str = "\x1B[35m";
const BOLD: &'static str = "\x1B[1m";
const END: &'static str = "\x1B[0m";

pub struct ErrorHandler {
    has_error: bool,
    errors: Vec<Error>,
    codes: HashMap<u16, String>,
}

/// Store errors encountered during compilation and generate a report on demand.
/// Each file should be attributed to a single ErrorHandler. ErrorHandlers can be
/// merged as needed when proceeding through the pipeline.
impl ErrorHandler {
    pub fn new<'a>(code: String, f_id: u16) -> ErrorHandler {
        let mut codes = HashMap::new();
        codes.insert(f_id, code);
        ErrorHandler {
            has_error: false,
            errors: Vec::new(),
            codes: codes,
        }
    }

    /// Returns a file owned by the ErrorHandler.
    pub fn get_file(&self, f_id: u16) -> Option<&str> {
        if let Some(s) = self.codes.get(&f_id) {
            Some(&*s)
        } else {
            None
        }
    }

    /// Report an error without location.
    pub fn report_no_loc(&mut self, message: String) {
        self.has_error = true;
        self.errors.push(Error {
            loc: None,
            t: ErrorType::Any,
            message: message,
        })
    }

    /// Report an error.
    pub fn report(&mut self, loc: Location, message: String) {
        self.has_error = true;
        self.errors.push(Error {
            loc: Some(loc),
            t: ErrorType::Any,
            message: message,
        })
    }

    /// Report an internal error, this is reserved for errors due to the compiler itself.
    pub fn report_internal(&mut self, loc: Location, message: String) {
        self.has_error = true;
        self.errors.push(Error {
            loc: Some(loc),
            t: ErrorType::Internal,
            message: message,
        })
    }

    /// Report an internal error without location, this is reserved for errors due to the compiler itself.
    pub fn report_internal_no_loc(&mut self, message: String) {
        self.has_error = true;
        self.errors.push(Error {
            loc: None,
            t: ErrorType::Internal,
            message: message,
        })
    }

    /// The compilation will fail silently. Prefer reporting an error if possible.
    pub fn silent_report(&mut self) {
        self.has_error = true;
    }

    /// Merge another ErrorHandler into self, taking ownership of its errors.
    pub fn merge(&mut self, other: ErrorHandler) {
        self.has_error = self.has_error || other.has_error;
        self.errors.extend(other.errors);

        for (f_id, code) in other.codes.into_iter() {
            if self.codes.contains_key(&f_id) {
                self.report_internal_no_loc(format!(
                    "Merging two ErrorHandlers collecting errors for the same file with id {}.",
                    f_id
                ));
            } else {
                self.codes.insert(f_id, code);
            }
        }
    }

    /// If at least one error has been reported, print the errors and exit.
    /// Return immediately without exiting otherwise.
    pub fn print_and_exit(&mut self) {
        if !self.has_error {
            return;
        } else if self.errors.len() == 0 {
            exit();
        }

        // Sort errors on file ID.
        let mut errors_no_loc = Vec::new();
        let mut errors_by_files: HashMap<u16, Vec<&Error>> = HashMap::new();
        for err in self.errors.iter() {
            if let Some(loc) = err.loc {
                if let Some(errors) = errors_by_files.get_mut(&loc.f_id) {
                    errors.push(err);
                } else {
                    errors_by_files.insert(loc.f_id, vec![err]);
                }
            } else {
                errors_no_loc.push(err);
            }
        }

        // Print all errors without location.
        for err in errors_no_loc {
            self.print(&err);
        }

        // Print all errors with location.
        for (f_id, errors) in errors_by_files.into_iter() {
            if let Some(code) = self.codes.get(&f_id) {
                self.print_errors_with_loc(code, errors);
            } else {
                let err = Error {
                    loc: None,
                    t: ErrorType::Internal,
                    message: String::from("Found errors with unknown file ID."),
                };
                self.print(&err);
            }
        }

        exit();
    }

    /// Pretty print errors with code context.
    /// All errors **must** have a location corresponding to `code`.
    fn print_errors_with_loc(&self, code: &str, mut errors: Vec<&Error>) {
        // Sort errors by locations.
        errors.sort_unstable();

        let mut error_iterator = errors.iter();
        let mut err = if let Some(err) = error_iterator.next() {
            err
        } else {
            return;
        };

        // Maintain position information
        let mut line = 1;
        let mut lines_pos = 0;
        let mut pos = 0;
        let mut loc = err.loc.unwrap();

        // Iterate the code, keep an extra iterator pointing to the beginning of the current line.
        let mut iter = code.chars();
        let mut line_iter = iter.clone();
        while let Some(c) = iter.next() {
            if c == '\n' {
                // Next line
                line += 1;
                lines_pos = pos;
                line_iter = iter.clone();
            }
            if pos == loc.pos {
                // Found the location of an error
                let error_pos = pos - lines_pos;
                let min_size = error_pos + loc.len;
                let erroneous_code = self.get_substr(line_iter.clone(), min_size);
                self.print_line(err, erroneous_code, error_pos, loc.len, line);

                // Continue while at least one error remains.
                err = if let Some(err) = error_iterator.next() {
                    err
                } else {
                    return;
                };
                loc = err.loc.unwrap();
            }
            pos += 1;
        }
    }

    /// Pretty print an error with position information.
    fn print_line(&self, e: &Error, code: String, pos: u32, len: u32, line: usize) {
        let color = get_color(e.t);
        let err_name = get_err_name(e.t);

        println!("{:>5} | {}", line, code);
        println!(
            "       {:blank$}{}{:^<underline$}{}",
            " ",
            color,
            "^",
            END,
            blank = pos as usize,
            underline = len as usize
        );
        println!(
            "{}{}{}:{}{} {}{}\n",
            color, BOLD, err_name, END, color, e.message, END
        );
    }

    /// Pretty print an error without position information.
    fn print(&self, e: &Error) {
        let color = get_color(e.t);
        let err_name = get_err_name(e.t);

        println!(
            "{}{}{}:{}{} {}{}\n",
            color, BOLD, err_name, END, color, e.message, END
        );
    }

    /// Returns a copy of the smallest number of full lines starting at `iter`
    /// and spanning at least `min_size` characters.
    /// Used to extract lines containing an error.
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
