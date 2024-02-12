#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]
#![doc = include_str!("../README.md")]
#![warn(missing_docs)]

#[cfg(feature = "proc-macro")]
extern crate proc_macro;

pub mod prelude {
    //! The prelude imports all the crate's traits with `use ... as _`.
    #[cfg(feature = "token-buffer")]
    pub use crate::AsTokenBuf as _;
    #[cfg(feature = "token-buffer")]
    pub use crate::AsTokenBufMut as _;
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
    #[cfg(feature = "token-buffer")]
    pub use crate::ToTokenBuffer as _;
    pub use crate::ToTokenStream as _;
    pub use crate::Token as _;
    pub use crate::TokenAuto as _;
    pub use crate::TokenStream as _;
    pub use crate::TokenStreamExt as _;
    pub use crate::TokenTree as _;
    pub use crate::TokenTreeExt as _;
}

mod base;
#[cfg(feature = "token-buffer")]
mod delimited;
mod error;
mod literal;
mod op;
mod span;
mod token;
#[cfg(feature = "token-buffer")]
mod token_buffer;
mod token_stream;
mod token_tree;

#[cfg(feature = "proc-macro")]
pub use base::PM1;
#[cfg(feature = "proc-macro2")]
pub use base::PM2;
pub use base::{PMExt, ProcMacro, ProcMacroExt, PM};
#[cfg(feature = "token-buffer")]
pub use delimited::{delimited, Delimited, DelimitedParser};
pub use error::Error;
pub use literal::{Literal, LiteralExt};
#[cfg(feature = "literal-value")]
pub use literal::{LiteralToken, LiteralValue, StringToken, Suffixed};
pub use op::{op, MatchOpFn, Op, OpParser, OpParserInstance, Puncts};
pub use span::{Span, SpanExt};
use token::def_tokens;
pub use token::{ToTokenStream, ToTokens, Token, TokenAuto, TokenObject};
#[cfg(feature = "token-buffer")]
pub use token_buffer::{
    AsTokenBuf, AsTokenBufMut, DefaultParser, Optional, Parse, Parser, ToTokenBuffer, TokenBuf,
    TokenBuffer,
};
pub use token_stream::{TokenStream, TokenStreamExt};
pub use token_tree::{
    Delimiter, DelimiterExt, DelimiterKind, Group, GroupExt, Ident, IdentExt, Punct, PunctExt,
    Spacing, SpacingExt, TokenTree, TokenTreeExt, TokenTreeKind,
};

/// Match results.
pub enum Match<T> {
    /// The match is complete; stop looking.
    Complete(T),

    /// A match was found, but there may be more; keep looking.
    Partial(T),

    /// More data is needed to find a match.
    NeedMore,

    /// There's no more matches to be found. Stop looking.
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
}
