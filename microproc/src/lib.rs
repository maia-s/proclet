#![cfg_attr(feature = "nightly", feature(doc_auto_cfg))]
#![doc = include_str!("../README.md")]

#[cfg(feature = "proc-macro")]
extern crate proc_macro;

mod literal;
mod span;

pub use literal::{Literal, LiteralValue, Suffixed};
