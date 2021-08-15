//! Locations

use std::cmp;

use crate::id::FileID;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Offset(u32);

#[derive(Clone, Copy)]
pub struct Location {
    start: Offset,
    end: Offset,
}

pub struct LocationTable {
    file_id: FileID,
    table: Vec<Offset>,
}

impl Location {
    pub fn merge(self, other: Location) -> Location {
        let start = cmp::min(self.start, other.start);
        let end = cmp::max(self.end, other.end);
        Location { start, end }
    }
}

impl LocationTable {
    pub fn new(file_id: FileID) -> LocationTable {
        LocationTable {
            file_id,
            table: Vec::new(),
        }
    }
}
