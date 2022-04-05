#![no_std]

extern crate rusty_c_macro;

pub use rusty_c_macro::*;

pub mod string;

/// Should not be used directly
#[doc(hidden)]
pub mod ops;
