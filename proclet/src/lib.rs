#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]
#![doc = include_str!("../README.md")]

#[cfg(feature = "proc-macro")]
extern crate proc_macro;

pub mod prelude {
    pub use crate::AsToken as _;
    pub use crate::AsTokenAuto as _;
    #[cfg(feature = "token-buffer")]
    pub use crate::DefaultParser as _;
    pub use crate::Delimiter as _;
    pub use crate::DelimiterExt as _;
    pub use crate::Group as _;
    pub use crate::GroupExt as _;
    pub use crate::Ident as _;
    pub use crate::IdentExt as _;
    pub use crate::Literal as _;
    pub use crate::LiteralExt as _;
    #[cfg(feature = "token-buffer")]
    pub use crate::Parse as _;
    #[cfg(feature = "token-buffer")]
    pub use crate::Parser as _;
    pub use crate::ProcMacro as _;
    pub use crate::ProcMacroExt as _;
    pub use crate::Punct as _;
    pub use crate::PunctExt as _;
    pub use crate::Spacing as _;
    pub use crate::SpacingExt as _;
    pub use crate::Span as _;
    pub use crate::SpanExt as _;
    pub use crate::ToTokenTrees as _;
    pub use crate::Token as _;
    pub use crate::TokenAuto as _;
    pub use crate::TokenStream as _;
    pub use crate::TokenStreamExt as _;
    pub use crate::TokenTree as _;
    pub use crate::TokenTreeExt as _;
}

mod base;
mod error;
mod literal;
#[cfg(feature = "op")]
mod op;
mod span;
mod token;
#[cfg(feature = "token-buffer")]
mod token_buffer;
mod token_stream;
mod token_tree;

pub use base::{PMExt, ProcMacro, ProcMacroExt, PM, PM1, PM2};
pub use error::Error;
pub use literal::{Literal, LiteralExt};
#[cfg(feature = "literal-value")]
pub use literal::{LiteralToken, LiteralValue, Suffixed};
#[cfg(feature = "op")]
pub use op::{rust_op_parser, Op, OpParser, OpParserInstance, Puncts};
pub use span::{Span, SpanExt};
pub use token::{AsToken, AsTokenAuto, ToTokenTrees, Token, TokenAuto, TokenTrees};
#[cfg(feature = "token-buffer")]
pub use token_buffer::{DefaultParser, Parse, Parser, TokenBuf, TokenBuffer};
pub use token_stream::{TokenStream, TokenStreamExt};
pub use token_tree::{
    Delimiter, DelimiterExt, DelimiterKind, Group, GroupExt, Ident, IdentExt, Punct, PunctExt,
    Spacing, SpacingExt, TokenTree, TokenTreeExt, TokenTreeKind,
};

pub enum Match<T> {
    Complete(T),
    Partial(T),
    NeedMore,
    NoMatch,
}

use internal::*;
mod internal {
    use std::{fmt::Debug, str::FromStr};

    pub trait Sealed {}

    pub trait FromStrDebug: FromStr<Err = Self::ErrDbg> {
        type ErrDbg: Debug;
    }
    impl<T: FromStr<Err = E>, E: Debug> FromStrDebug for T {
        type ErrDbg = E;
    }

    pub trait TryFromDebug<X>: TryFrom<X, Error = Self::ErrDbg> {
        type ErrDbg: Debug;
    }
    impl<X, T: TryFrom<X, Error = E>, E: Debug> TryFromDebug<X> for T {
        type ErrDbg = E;
    }

    pub trait TryIntoDebug<X>: TryInto<X, Error = Self::ErrDbg> {
        type ErrDbg: Debug;
    }
    impl<X, T: TryInto<X, Error = E>, E: Debug> TryIntoDebug<X> for T {
        type ErrDbg = E;
    }
}
