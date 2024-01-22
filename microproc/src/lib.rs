#![cfg_attr(feature = "nightly", feature(doc_auto_cfg))]
#![doc = include_str!("../README.md")]

#[cfg(feature = "proc-macro")]
extern crate proc_macro;

pub mod prelude {
    pub use crate::TokenStream as _;
    pub use crate::TokenStreamExt as _;
    pub use crate::TokenTree as _;
    pub use crate::TokenTreeExt as _;
}

mod base;
mod error;
mod literal;
mod span;
mod token;
mod token_stream;
mod token_tree;

pub use base::ProcMacro;
pub use error::Error;
pub use literal::{Literal, LiteralValue, Suffixed};
pub use token_stream::{TokenStream, TokenStreamExt};
pub use token_tree::{TokenTree, TokenTreeExt};
