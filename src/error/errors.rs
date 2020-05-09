use std::cmp::Ordering;

#[derive(Debug, Copy, Clone, Ord, Eq, PartialEq, PartialOrd)]
pub struct Location {
    pub pos: u32,
    pub len: u32,
}

pub struct Error {
    pub loc: Option<Location>,
    pub t: ErrorType,
    pub message: String,
}

#[derive(Copy, Clone)]
pub enum ErrorType {
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

    pub fn merge(self, other: Location) -> Location {
        let pos = std::cmp::min(self.pos, other.pos);
        let len = std::cmp::max(self.pos + self.len, other.pos + other.len) - pos;
        Location { pos: pos, len: len }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn locations() {
        let loc_1 = Location { pos: 10, len: 5 };
        let loc_2 = Location { pos: 12, len: 8 };
        let loc_3 = Location { pos: 11, len: 3 };

        assert_eq!(loc_1.merge(loc_2), Location { pos: 10, len: 10 });
        assert_eq!(loc_2.merge(loc_3), Location { pos: 11, len: 9 });
        assert_eq!(loc_1.merge(loc_3), loc_1);
        assert_eq!(loc_1.merge(loc_2), loc_2.merge(loc_1));
    }
}
