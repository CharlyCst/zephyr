//! # Arena allocator for ctx object storage
//!
//! Inspired by Rustc [`TypedArena`](https://github.com/rust-lang/rust/blob/25f39fe80293f77bd86f64a1261a3e2c0ca23847/compiler/rustc_arena/src/lib.rs#L40)

#![allow(dead_code)]
use std::alloc;
use std::cell::{Cell, RefCell};
use std::marker::PhantomData;
use std::mem;
use std::ptr;

// Minimal and maximum size of chunk respectively matches the size of a PAGE and HUGE_PAGE.
const PAGE: usize = 4096;
const HUGE_PAGE: usize = 2 * 1024 * 1024;

/// A chunk of memory that can hold objects of type T.
struct ArenaChunk<T> {
    ptr: *const T,
    capacity: usize,
    _own: PhantomData<T>,
}

/// An Arena is a data structure that can hold values while returning a &mut reference with 'self
/// lifetime to them. The values are freed when the arena is dropped.
///
/// ! Caution: values inserted in the arena will not be dropped!
pub struct Arena<T> {
    /// Pointer to next object to allocate.
    ptr: Cell<*mut T>,
    /// Pointer to the end of the allocated chunk. Once reached, a new chunk must be allocated.
    end: Cell<*mut T>,
    chunks: RefCell<Vec<ArenaChunk<T>>>,
}

impl<T> ArenaChunk<T> {
    /// Allocates a chunk that can contain up to `capacity` objects.
    fn new(capacity: usize) -> Self {
        let ptr = unsafe {
            let layout = Self::get_layout(capacity);
            let ptr = alloc::alloc(layout) as *mut T;
            if ptr.is_null() {
                panic!("Out of memory");
            }
            ptr
        };

        ArenaChunk {
            ptr,
            capacity,
            _own: PhantomData,
        }
    }

    /// Destroys this arena chunk.
    unsafe fn destroy(&mut self) {
        let layout = Self::get_layout(self.capacity);
        alloc::dealloc(self.ptr as *mut u8, layout);
    }

    fn get_layout(capacity: usize) -> alloc::Layout {
        let align = mem::align_of::<T>();
        let elem_size = mem::size_of::<T>();
        alloc::Layout::from_size_align(elem_size * capacity, align).unwrap()
    }
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        assert!(mem::size_of::<T>() != 0);
        Self {
            ptr: Cell::new(ptr::null_mut()),
            end: Cell::new(ptr::null_mut()),
            chunks: RefCell::new(Vec::new()),
        }
    }

    /// Allocates an object in the arena, the object will live as long as the arena itself.
    pub fn alloc(&self, object: T) -> &mut T {
        // Make some place if necessary
        if self.ptr == self.end {
            self.grow();
        }

        // Store object in an empty spot and advance the pointer to the next spot
        unsafe {
            let ptr = self.ptr.get();
            ptr::write(ptr, object);
            self.ptr.set(ptr.wrapping_offset(1));
            &mut *ptr
        }

    }

    /// Grows the arena.
    fn grow(&self) {
        unsafe {
            let obj_size = mem::size_of::<T>();
            let mut chunks = self.chunks.borrow_mut();
            let mut capacity;
            if let Some(last_chunk) = chunks.last() {
                capacity = last_chunk.capacity.min(HUGE_PAGE / obj_size / 2);
                capacity = capacity * 2;
            } else {
                capacity = PAGE / obj_size;
            }
            let chunk = ArenaChunk::new(capacity);
            let ptr = chunk.ptr as *mut T;
            self.ptr.set(ptr);
            self.end.set(ptr.offset(capacity as isize));
            chunks.push(chunk);
        }
    }
}

impl<T> Drop for Arena<T> {
    fn drop(&mut self) {
        unsafe {
            let mut chunks = self.chunks.borrow_mut();
            for chunk in chunks.iter_mut() {
                chunk.destroy();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Eq, PartialEq, Debug, Clone)]
    struct Point {
        x: usize,
        y: usize,
    }

    impl Point {
        fn new(x: usize, y: usize) -> Self {
            Self { x, y }
        }
    }

    #[test]
    /// Creates an arena and insert a few values, then mutate and access them.
    fn arena() {
        let arena = Arena::new();
        let a = Point::new(1, 2);
        let b = Point::new(2, 2);
        let a_2 = arena.alloc(a.clone());
        let b_2 = arena.alloc(b.clone());
        assert_eq!(a_2, &a);
        assert_eq!(b_2, &b);
    }

    #[test]
    /// Forces a second chunk allocation.
    fn chunk_alloc() {
        let arena = Arena::new();
        let mut refs = Vec::new();
        let pt_size = mem::size_of::<Point>();
        let nb_insertions = 2 * PAGE / pt_size; // enough to allocate 2 chunks

        // Allocate enough objects to force a new chunk allocation.
        for i in 0..nb_insertions {
            let obj_ref = arena.alloc(Point::new(i, 2 * i));
            refs.push(obj_ref);
        }
        // Access the allocated objects
        for obj in refs {
            obj.x = obj.x + obj.y;
        }

        assert!(arena.chunks.borrow().len() > 1);
    }
}
