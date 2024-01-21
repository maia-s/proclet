use crate::{span::Span, TokenStreamExt, TokenTreeExt};
use std::fmt::Debug;

macro_rules! impl_base {
    ($t:tt) => {
        impl_base!(@t1 $t $t);
        impl_base!(@i1 proc_macro: "proc-macro" $t $t);
        impl_base!(@i1 proc_macro2: "proc-macro2" $t $t);
    };

    (@t1 {$($t:ident: $ts:ident),* $(,)?} $t2:tt) => {
        pub trait ProcMacro: Clone + Debug {
            $( impl_base!(@t2 $t: $ts, $t2); )*
            type TokenStreamIntoIter: Clone + Iterator<Item = Self::TokenTree>;
        }
    };

    (@t2 $t:ident: $ts:ident, {$($t2:ident: $t2s:ident),* $(,)?}) => {
        type $t: $ts<$($t2 = Self::$t2,)*
            TokenStreamIntoIter = Self::TokenStreamIntoIter,
        >;
    };

    (@i1 $pm:ident: $feature:literal {$($t:ident: $ts:ident),* $(,)?} $t2:tt) => {
        $( impl_base!(@i2 $pm: $feature: $t, $t2); )*
    };

    (@i2 $pm:ident: $feature:literal: $t:ident, {$($t2:ident: $t2s:ident),* $(,)?}) => {
        #[cfg(feature = $feature)]
        impl ProcMacro for $pm::$t {
            $( type $t2 = $pm::$t2; )*
            type TokenStreamIntoIter = $pm::token_stream::IntoIter;
        }
    }
}

impl_base!({
    Span: Span,
    TokenStream: TokenStreamExt,
    TokenTree: TokenTreeExt,
});
