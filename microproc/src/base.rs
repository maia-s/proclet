use crate::{Delimiter, Group, Literal, Span, TokenStream, TokenTree};
use std::fmt::Debug;

macro_rules! impl_base {
    ($t:tt) => {
        impl_base!(@t1 $t $t);
        impl_base!(@i1 proc_macro: "proc-macro" $t $t);
        impl_base!(@i1 proc_macro2: "proc-macro2" $t $t);
    };

    (@t1 {$($t:ident: $ts:ident $(: $tf:literal)?),* $(,)?} $t2:tt) => {
        /// Base trait with associated types from `proc-macro`/`proc-macro2`.
        /// See also [`ProcMacroExt`].
        pub trait ProcMacro: Clone + Debug {
            $( impl_base!(@t2 $t: $ts, $t2); )*
            type TokenStreamIntoIter: Clone + Iterator<Item = Self::TokenTree>;
        }

        /// This trait adds extra bounds to the associated types of `ProcMacro` to enable extension
        /// traits. You can keep using the types defined in `ProcMacro`; the associated types of this
        /// trait should be considered an implementation detail.
        pub trait ProcMacroExt: ProcMacro<
            $($t = Self::$ts,)*
            TokenStreamIntoIter = Self::TokenStreamExtIntoIter
        > {
            $( impl_base!(@t3 $t: $ts $(: $tf)?, $t2); )*

            type TokenStreamExtIntoIter: Clone + Iterator<Item = Self::TokenTreeExt>;
        }
    };

    (@t2 $t:ident: $ts:ident, {$($t2:ident: $t2s:ident $(: $t2f:literal)?),* $(,)?}) => {
        type $t: $t<$($t2 = Self::$t2,)*
            TokenStreamIntoIter = Self::TokenStreamIntoIter,
        >;
    };

    (@t3 $t:ident: $ts:ident $(: $tf:literal)?, {$($t2:ident: $t2s:ident $(: $t2f:literal)?),* $(,)?}) => {
        $( #[cfg(feature = $tf)] )?
        type $ts: crate::$ts<$($t2s = Self::$t2s,)*
            TokenStreamExtIntoIter = Self::TokenStreamExtIntoIter,
        >;
        $(
            #[cfg(not(feature = $tf))]
            type $ts;
        )?
    };

    (@i1 $pm:ident: $feature:literal {$($t:ident: $ts:ident $(: $tf:literal)?),* $(,)?} $t2:tt) => {
        $( impl_base!(@i2 $pm: $feature: $t, $t2); )*
    };

    (@i2 $pm:ident: $feature:literal: $t:ident, {$($t2:ident: $t2s:ident $(: $t2f:literal)?),* $(,)?}) => {
        #[cfg(feature = $feature)]
        impl ProcMacro for $pm::$t {
            $( type $t2 = $pm::$t2; )*
            type TokenStreamIntoIter = $pm::token_stream::IntoIter;
        }

        #[cfg(feature = $feature)]
        impl ProcMacroExt for $pm::$t {
            $( type $t2s = $pm::$t2; )*
            type TokenStreamExtIntoIter = $pm::token_stream::IntoIter;
        }
    }
}

impl_base!({
    Delimiter: DelimiterExt,
    Group: GroupExt,
    Literal: LiteralExt,
    Span: SpanExt,
    TokenStream: TokenStreamExt,
    TokenTree: TokenTreeExt,
});
