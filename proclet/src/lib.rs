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
mod error;
mod literal;
mod op;
mod punctuated;
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
pub use error::Error;
#[cfg(feature = "literal-value")]
pub use literal::{
    ByteCharacterLiteral, ByteStringLiteral, CStringLiteral, CharacterLiteral, F32Literal,
    F64Literal, FloatLiteral, I128Literal, I16Literal, I32Literal, I64Literal, I8Literal,
    IntLiteral, IsizeLiteral, LiteralValue, StringLiteral, U128Literal, U16Literal, U32Literal,
    U64Literal, U8Literal, UsizeLiteral,
};
pub use literal::{Literal, LiteralExt};
pub use op::{op, MatchOpFn, Op, OpParser, OpParserInstance, Puncts};
pub use punctuated::{punctuated, Punctuated, PunctuatedParser};
pub use span::{Span, SpanExt};
pub use token::{IntoTokens, ToTokenStream, ToTokens};
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
        Ok(out) => out.into_token_stream(),
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

macro_rules! pm {
    ($($mod:ident: $pm:ident: $feature:literal),*) => { $(
        #[cfg(feature = $feature)]
        #[doc = concat!("Type aliases for use with ", $feature, ".")]
        pub mod $mod {
            pub use crate::{Punctuated, Optional};
            use $pm::*;
            pm! {
                @ $feature;
                Error = Error<Span>;
                PunctuatedParser<M, D> = PunctuatedParser<TokenTree, M, D>;
                "literal-value" LiteralValue = LiteralValue<Span>;
                "literal-value" StringLiteral = StringLiteral<Span>;
                "literal-value" ByteStringLiteral = ByteStringLiteral<Span>;
                "literal-value" CStringLiteral = CStringLiteral<Span>;
                "literal-value" CharacterLiteral = CharacterLiteral<Span>;
                "literal-value" ByteCharacterLiteral = ByteCharacterLiteral<Span>;
                "literal-value" IntLiteral = IntLiteral<Span>;
                "literal-value" FloatLiteral = FloatLiteral<Span>;
                "literal-value" I8Literal = I8Literal<Span>;
                "literal-value" I16Literal = I16Literal<Span>;
                "literal-value" I32Literal = I32Literal<Span>;
                "literal-value" I64Literal = I64Literal<Span>;
                "literal-value" I128Literal = I128Literal<Span>;
                "literal-value" IsizeLiteral = IsizeLiteral<Span>;
                "literal-value" U8Literal = U8Literal<Span>;
                "literal-value" U16Literal = U16Literal<Span>;
                "literal-value" U32Literal = U32Literal<Span>;
                "literal-value" U64Literal = U64Literal<Span>;
                "literal-value" U128Literal = U128Literal<Span>;
                "literal-value" UsizeLiteral = UsizeLiteral<Span>;
                "literal-value" F32Literal = F32Literal<Span>;
                "literal-value" F64Literal = F64Literal<Span>;
                Op = Op<Span>;
                OpParser<F> = OpParser<Punct, F>;
                OpParserInstance<F> = OpParserInstance<Punct, F>;
                Puncts<'a> = Puncts<'a, Punct>;
                TokenBuffer = TokenBuffer<TokenTree>;
                TokenBuf = TokenBuf<TokenTree>;
            }
        }
    )* };

    (@ $pm:literal; $($($feat:literal)? $alias:ident $(<$($gl:lifetime),* $(,)? $($g:ident),*>)? = $t:ident$(<$($tgl:lifetime),* $(,)? $($tg:ident),*>)?;)*) => { $(
        $( #[cfg(feature = $feat)] )?
        #[doc = concat!("`", stringify!($alias), "` for ", $pm, ".")]
        pub type $alias $(<$($gl,)* $($g),*>)? = crate::$t$(<$($tgl,)* $($tg),*>)?;
    )* };
}

pm!(pm1: proc_macro: "proc-macro", pm2: proc_macro2: "proc-macro2");

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
}
