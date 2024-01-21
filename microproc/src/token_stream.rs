use crate::base::ProcMacro;
use std::{iter, str::FromStr};

pub trait TokenStreamExt: ProcMacro + Sized + FromStr {
    #[must_use]
    fn apply_span(self, span: Self::Span) -> Self;

    #[must_use]
    fn expect(self, tokens: impl Iterator<Item = Self::TokenTree>) -> Option<(Self, Self)>;
}

macro_rules! impl_token_stream_ext {
    ($($pm:ident: $feature:literal),*) => { $(
        #[cfg(feature = $feature)]
        impl TokenStreamExt for $pm::TokenStream {
            #[inline]
            fn apply_span(self, span: Self::Span) -> Self {
                self.into_iter().map(|mut tt|{ tt.set_span(span); tt }).collect()
            }

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
