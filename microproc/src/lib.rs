#![cfg_attr(feature = "nightly", feature(doc_auto_cfg))]
#![doc = include_str!("../README.md")]

#[cfg(feature = "proc-macro")]
extern crate proc_macro;

pub mod prelude {
    pub use crate::Delimiter as _;
    pub use crate::DelimiterExt as _;
    pub use crate::Group as _;
    pub use crate::GroupExt as _;
    pub use crate::Ident as _;
    pub use crate::IdentExt as _;
    pub use crate::Literal as _;
    pub use crate::LiteralExt as _;
    pub use crate::ProcMacro as _;
    pub use crate::ProcMacroExt as _;
    pub use crate::Punct as _;
    pub use crate::PunctExt as _;
    pub use crate::Spacing as _;
    pub use crate::SpacingExt as _;
    pub use crate::Span as _;
    pub use crate::SpanExt as _;
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

pub use base::{ProcMacro, ProcMacroExt};
pub use error::Error;
pub use literal::{Literal, LiteralExt};
#[cfg(feature = "literal-value")]
pub use literal::{LiteralValue, Suffixed};
pub use span::{Span, SpanExt};
pub use token_stream::TokenStream;
pub use token_stream::TokenStreamExt;
pub use token_tree::TokenTreeExt;
pub use token_tree::{
    Delimiter, DelimiterExt, Group, GroupExt, Ident, IdentExt, Punct, PunctExt, Spacing,
    SpacingExt, TokenTree,
};
