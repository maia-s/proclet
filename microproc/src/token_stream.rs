use crate::{token::Token, TokenTreeExt};
use core::iter;

pub trait TokenStreamExt: Sized {
    type TokenTree: TokenTreeExt;
    fn expect(self, tokens: impl Iterator<Item = Self::TokenTree>) -> Option<(Self, Self)>;
}

macro_rules! impl_token_stream_ext {
    ($($pm:ident: $feature:literal),*) => { $(
        #[cfg(feature = $feature)]
        impl TokenStreamExt for $pm::TokenStream {
            type TokenTree = $pm::TokenTree;

            #[inline]
            fn expect(self, tokens: impl Iterator<Item = Self::TokenTree>) -> Option<(Self, Self)> {
                let mut it = self.into_iter();
                let mut n = 0;
                for (a, b) in it.clone().zip(tokens) {
                    n += 1;
                    if a.to_string() != b.to_string() {
                        return None;
                    }
                }
                let mut matched = Self::new();
                while n != 0 {
                    n -= 1;
                    matched.extend(iter::once(it.next().unwrap()))
                }
                Some((matched, Self::from_iter(it)))
            }
        }
    )* };
}

impl_token_stream_ext!(proc_macro: "proc-macro", proc_macro2: "proc-macro2");
