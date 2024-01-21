#![cfg_attr(feature = "nightly", feature(doc_auto_cfg))]
#![doc = include_str!("../README.md")]

#[cfg(feature = "proc-macro")]
extern crate proc_macro;

pub mod prelude {
    pub use crate::TokenTreeExt as _;
}

mod literal;
mod span;
mod token_tree;

pub use literal::{Literal, LiteralValue, Suffixed};
pub use token_tree::TokenTreeExt;
