#![cfg_attr(feature = "nightly", feature(doc_auto_cfg))]
#![doc = include_str!("../README.md")]

#[cfg(feature = "proc-macro")]
extern crate proc_macro;

pub mod prelude {
    pub use crate::ProcMacro as _;
    pub use crate::ProcMacroExt as _;
    pub use crate::Span as _;
    pub use crate::SpanExt as _;
    pub use crate::TokenStream as _;
    #[cfg(feature = "token-stream-ext")]
    pub use crate::TokenStreamExt as _;
    pub use crate::TokenTree as _;
    #[cfg(feature = "token-tree-ext")]
    pub use crate::TokenTreeExt as _;
}

mod base;
mod error;
mod literal;
mod span;
mod token;
mod token_stream;
mod token_tree;

pub use base::{ProcMacro, ProcMacroExt};
pub use error::Error;
pub use literal::{Literal, LiteralValue, Suffixed};
pub use span::{Span, SpanExt};
pub use token_stream::TokenStream;
#[cfg(feature = "token-stream-ext")]
pub use token_stream::TokenStreamExt;
pub use token_tree::TokenTree;
#[cfg(feature = "token-tree-ext")]
pub use token_tree::TokenTreeExt;
