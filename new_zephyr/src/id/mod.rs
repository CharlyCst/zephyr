mod stable_hash;

/// Stable identifiers
#[derive(Clone, Copy)]
pub struct NodeID(u64, u64);

/// An unique ID representing a file
#[derive(Clone, Copy)]
pub struct FileID(u32);

