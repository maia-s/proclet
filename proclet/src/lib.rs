#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]
#![doc = include_str!("../README.md")]
#![warn(missing_docs)]

#[cfg(feature = "proc-macro")]
extern crate proc_macro;

pub mod prelude {
    //! The prelude imports all the crate's traits with `use ... as _`.
    pub use crate::AsTokenBuf as _;
    pub use crate::AsTokenBufMut as _;
    pub use crate::DefaultParser as _;
    pub use crate::Delimiter as _;
    pub use crate::DelimiterExt as _;
    pub use crate::Group as _;
    pub use crate::GroupExt as _;
    pub use crate::Ident as _;
    pub use crate::IdentExt as _;
    pub use crate::Literal as _;
    pub use crate::LiteralExt as _;
    pub use crate::Parse as _;
    pub use crate::Parser as _;
    pub use crate::ProcMacro as _;
    pub use crate::ProcMacroExt as _;
    pub use crate::Punct as _;
    pub use crate::PunctExt as _;
    pub use crate::Spacing as _;
    pub use crate::SpacingExt as _;
    pub use crate::Span as _;
    pub use crate::SpanExt as _;
    pub use crate::ToTokenBuffer as _;
    pub use crate::ToTokenStream as _;
    pub use crate::ToTokens as _;
    pub use crate::TokenStream as _;
    pub use crate::TokenStreamExt as _;
    pub use crate::TokenTree as _;
    pub use crate::TokenTreeExt as _;
}

mod base;
mod delimited;
mod error;
mod literal;
mod op;
mod span;
mod token;
mod token_buffer;
mod token_stream;
mod token_tree;

#[cfg(feature = "proc-macro")]
pub use base::PM1;
#[cfg(feature = "proc-macro2")]
pub use base::PM2;
pub use base::{PMExt, ProcMacro, ProcMacroExt, PM};
pub use delimited::{delimited, Delimited, DelimitedParser};
pub use error::Error;
#[cfg(feature = "literal-value")]
pub use literal::{
    ByteCharacterLiteral, ByteStringLiteral, CharacterLiteral, F32Literal, F64Literal,
    FloatLiteral, I128Literal, I16Literal, I32Literal, I64Literal, I8Literal, IntLiteral,
    IsizeLiteral, LiteralValue, StringLiteral, U128Literal, U16Literal, U32Literal, U64Literal,
    U8Literal, UsizeLiteral,
};
pub use literal::{Literal, LiteralExt};
pub use op::{op, MatchOpFn, Op, OpParser, OpParserInstance, Puncts};
pub use span::{Span, SpanExt};
pub use token::{IntoTokens, ToTokenStream, ToTokens, TokenObject};
pub use token_buffer::{
    AsTokenBuf, AsTokenBufMut, DefaultParser, Optional, Parse, Parser, ToTokenBuffer, TokenBuf,
    TokenBuffer,
};
pub use token_stream::{TokenStream, TokenStreamExt};
pub use token_tree::{
    Delimiter, DelimiterExt, DelimiterKind, Group, GroupExt, Ident, IdentExt, Punct, PunctExt,
    Spacing, SpacingExt, TokenTree, TokenTreeExt, TokenTreeKind,
};

/// Optional wrapper for proc macros.
pub fn proclet<T: TokenStreamExt, U: ToTokenStream<T>>(
    input: impl Into<TokenBuffer<T::TokenTree>>,
    f: impl FnOnce(&mut &TokenBuf<T::TokenTree>) -> Result<U, Error<T::Span>>,
) -> T {
    let buffer = input.into();
    match f(&mut buffer.as_buf()) {
        Ok(out) => out.to_token_stream(),
        Err(e) => e.to_compile_error(),
    }
}

/// Match results.
pub enum Match<T> {
    /// The match is complete; stop looking.
    Complete(T),

    /// A match was found, but there may be a longer match; keep looking.
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
