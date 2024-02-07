use crate::{
    Delimiter, Group, Ident, Literal, Punct, Sealed, Spacing, Span, TokenStream, TokenTree,
};
use std::fmt::Debug;

#[cfg(feature = "proc-macro")]
pub mod proc_macro {
    /// Marker type for the `proc-macro` crate.
    #[derive(Clone, Copy, Debug)]
    pub struct PM;

    impl crate::PM for PM {}
    impl crate::PMExt for PM {}

    pub use ::proc_macro::*;
}

#[cfg(feature = "proc-macro2")]
pub mod proc_macro2 {
    /// Marker type for the `proc-macro2` crate.
    #[derive(Clone, Copy, Debug)]
    pub struct PM;
    impl crate::PM for PM {}
    impl crate::PMExt for PM {}

    pub use ::proc_macro2::*;
}

#[cfg(feature = "proc-macro")]
pub use proc_macro::PM as PM1;

#[cfg(feature = "proc-macro2")]
pub use proc_macro2::PM as PM2;

/// Proc-macro selector for when there isn't one specific proc-macro type to use.
pub trait PM: ProcMacro<PM = Self> {}

/// Enable extension traits for [`PM`].
pub trait PMExt: ProcMacroExt<PMExt = Self> + PM {}

// There are simpler and more contained ways to do this, but they fail type inference more.
// For example, having the types in a fully bounded associated type instead of a base trait
// fails type inference.

macro_rules! impl_base {
    ($t:tt) => {
        impl_base!(@t1 $t $t);
        impl_base!(@i1 proc_macro: "proc-macro" $t $t);
        impl_base!(@i1 proc_macro2: "proc-macro2" $t $t);
    };

    (@t1 {$($t:ident: $ts:ident),* $(,)?} $t2:tt) => {
        /// Base trait with associated type aliases for types from `proc-macro`/`proc-macro2`.
        /// See also [`ProcMacroExt`].
        pub trait ProcMacro: 'static + Clone + Debug + Sealed {
            $( impl_base!(@t2 $t: $ts, $t2); )*
            /// Proc macro selected type alias.
            type TokenStreamIntoIter: Clone + Iterator<Item = Self::TokenTree>;
        }

        /// Adds extra bounds to the associated types of [`ProcMacro`] to enable extension traits.
        ///
        /// You can keep using the types defined in `ProcMacro`; the associated types of this trait
        /// should be considered an implementation detail.
        pub trait ProcMacroExt: ProcMacro<
            $( $t = Self::$ts, )*
            TokenStreamIntoIter = Self::TokenStreamExtIntoIter
        > {
            $( impl_base!(@t3 $t: $ts, $t2); )*
            /// Implementation detail to enable extension traits. Use [`ProcMacro::TokenStreamIntoIter`] instead.
            type TokenStreamExtIntoIter: Clone + Iterator<Item = Self::TokenTreeExt>;
        }
    };

    (@t2 $t:ident: $ts:ident, {$($t2:ident: $t2s:ident),* $(,)?}) => {
        /// Proc macro selected type alias.
        type $t: $t<
            $( $t2 = Self::$t2, )*
            TokenStreamIntoIter = Self::TokenStreamIntoIter,
        >;
    };

    (@t3 $t:ident: $ts:ident, {$($t2:ident: $t2s:ident),* $(,)?}) => {
        #[doc = concat!("Implementation detail to enable extension traits. Use [`ProcMacro::", stringify!($t), "`] instead.")]
        type $ts: crate::$ts<
            $( $t2s = Self::$t2s, )*
            TokenStreamExtIntoIter = Self::TokenStreamExtIntoIter,
        >;
    };

    (@i1 $pm:ident: $feature:literal {$($t:ident: $ts:ident),* $(,)?} $t2:tt) => {
        $( impl_base!(@i2 $pm: $feature: $t, $t2); )*
    };

    (@i2 $pm:ident: $feature:literal: $t:ident, {$($t2:ident: $t2s:ident),* $(,)?}) => {
        #[cfg(feature = $feature)]
        impl Sealed for $pm::$t {}

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
    PM: PMExt,
    Delimiter: DelimiterExt,
    Group: GroupExt,
    Ident: IdentExt,
    Literal: LiteralExt,
    Punct: PunctExt,
    Spacing: SpacingExt,
    Span: SpanExt,
    TokenStream: TokenStreamExt,
    TokenTree: TokenTreeExt,
});
