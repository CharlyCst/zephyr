//!Stable Hash
//!
//! Stable hasher used to derive position-stable hashes for use as identifiers in the Zephyr
//! compiler. This is inspired from Rustc's StableHasher [1], Fingerprint [2] DefPathHash [3].
//!
//! [1]: https://github.com/rust-lang/rust/blob/master/compiler/rustc_data_structures/src/stable_hasher.rs
//! [2]: https://doc.rust-lang.org/beta/nightly-rustc/src/rustc_data_structures/fingerprint.rs.html#8
//! [3]: https://github.com/rust-lang/rust/blob/master/compiler/rustc_span/src/def_id.rs

use std::hash::{Hash, Hasher};

use siphasher::sip::SipHasher13;

pub struct StableHasher {
    mod_id: ModuleID,
}

impl StableHasher {
    // pub fn get_mod_id(module_path: String) -> ModuleID {
    //     let mut hasher = SipHasher13::new();
    //     module_path.hash(&mut hasher);
    //     ModuleID(hasher.finish())
    // }

    // pub fn from_mod_id(mod_id: ModuleID) -> Self {
    //     Self { mod_id }
    // }

    // pub fn get_item_id(&mut self, item_path: String) -> ItemID {
    //     let mut hasher = SipHasher13::new();
    //     item_path.hash(&mut hasher);
    //     ItemID(hasher.finish())
    // }
}
