use crate::{span::Span, TokenStreamExt, TokenTreeExt};

macro_rules! impl_base {
    ($t:tt) => {
        impl_base!(@0 $t);
        impl_base!(@1 proc_macro: "proc-macro" $t $t);
        impl_base!(@1 proc_macro2: "proc-macro2" $t $t);
    };

    (@0 {$($t:ident: $ts:ident),* $(,)?}) => {
        pub trait ProcMacro {
            $( type $t: $ts; )*
        }
    };

    (@1 $pm:ident: $feature:literal {$($t:ident: $ts:ident),* $(,)?} $t2:tt) => {
        $( impl_base!(@2 $pm: $feature: $t, $t2); )*
    };

    (@2 $pm:ident: $feature:literal: $t:ident, {$($t2:ident: $t2s:ident),* $(,)?}) => {
        #[cfg(feature = $feature)]
        impl ProcMacro for $pm::$t {
            $( type $t2 = $pm::$t2; )*
        }
    }
}

impl_base!({
    Span: Span,
    TokenStream: TokenStreamExt,
    TokenTree: TokenTreeExt,
});
