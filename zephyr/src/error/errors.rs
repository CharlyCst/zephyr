use std::cmp::Ordering;

use crate::resolver::FileId;

#[derive(Debug, Copy, Clone, Ord, Eq, PartialEq, PartialOrd)]
pub struct Location {
    pub pos: u32,
    pub len: u32,
    pub f_id: FileId,
}

pub struct Error {
    pub loc: Option<Location>,
    pub level: Level,
    pub message: String,
}

pub enum Level {
    /// The compilation failed.
    Error,
    /// Not critical but should be fixed.
    Warning,
    /// An error due to internal failling of the compiler.
    Internal,
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
    // Used to create empty location when needed
    pub fn dummy() -> Location {
        Location {
            pos: 0,
            len: 0,
            f_id: FileId(0),
        }
    }

    /// Return a new `Location` spanning `self` to `other` (ordering does not matter).
    pub fn merge(self, other: Location) -> Location {
        if self.f_id != other.f_id {
            println!("Internal error: merging two locations with distinct f_id");
            std::process::exit(1);
        }
        let pos = std::cmp::min(self.pos, other.pos);
        let len = std::cmp::max(self.pos + self.len, other.pos + other.len) - pos;
        Location {
            pos,
            len,
            f_id: self.f_id,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn locations() {
        let loc_1 = Location {
            pos: 10,
            len: 5,
            f_id: FileId(0),
        };
        let loc_2 = Location {
            pos: 12,
            len: 8,
            f_id: FileId(0),
        };
        let loc_3 = Location {
            pos: 11,
            len: 3,
            f_id: FileId(0),
        };

        assert_eq!(
            loc_1.merge(loc_2),
            Location {
                pos: 10,
                len: 10,
                f_id: FileId(0)
            }
        );
        assert_eq!(
            loc_2.merge(loc_3),
            Location {
                pos: 11,
                len: 9,
                f_id: FileId(0)
            }
        );
        assert_eq!(loc_1.merge(loc_3), loc_1);
        assert_eq!(loc_1.merge(loc_2), loc_2.merge(loc_1));
    }
}
