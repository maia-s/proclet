#![doc = include_str!("../README.md")]

#[doc(hidden)]
pub use proclet::{Match, MatchOpFn, OpParser, PunctExt};
#[doc(hidden)]
pub use proclet_utils_macros::_define_ops;

/// Macro for making a function that will create an [`OpParser`] for parsing a specific set of operators.
#[macro_export]
macro_rules! define_ops {
    ($(#[$attr:meta])* $vis:vis $ident:ident, $($ops:literal),* $(,)?) => {
        $(#[$attr])*
        #[inline]
        // rust fails type inference if this function is const
        $vis fn $ident<P: $crate::PunctExt>() -> $crate::OpParser<P, impl MatchOpFn> {
            $crate::_define_ops!($($ops),*);
            $crate::OpParser::new(__proclet_define_ops)
        }
    };
}

define_ops! {
    /// Parse operators defined by the Rust language.
    pub rust_op_parser,
    "!", "!=", "#", "$", "%", "%=", "&", "&&", "&=", "*", "*=", "+", "+=", ",", "-", "-=", "->",
    ".", "..", "...", "..=", "/", "/=", ":", "::", ";", "<", "<-", "<<", "<<=", "<=", "=", "==",
    "=>", ">", ">=", ">>", ">>=", "?", "@", "^", "^=", "|", "|=", "||", "~",
}
