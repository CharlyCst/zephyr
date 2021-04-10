use std::cmp::Ordering;
use zephyr::error::{Level, Location};

pub struct Error {
    pub loc: Option<Location>,
    pub level: Level,
    pub message: String,
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
