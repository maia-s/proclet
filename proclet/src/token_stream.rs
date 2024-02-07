use crate::{FromStrDebug, ProcMacro, TokenTree, TokenTreeExt};
use std::{fmt::Display, iter, str::FromStr};

/// `TokenStream` API trait. See [`proc_macro::TokenStream`](https://doc.rust-lang.org/stable/proc_macro/struct.TokenStream.html).
///
/// This trait is implemented for `TokenStream` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
///
/// See also [`TokenStreamExt`].
//
// The ProcMacro bound should be ProcMacro<TokenStream = Self>, but that fails to compile.
// (this comment is up here because rustfmt stops working if trait bounds have comments)
pub trait TokenStream:
    ProcMacro
    + Default
    + Display
    + Extend<Self::TokenStream>
    + Extend<Self::TokenTree>
    + From<Self::TokenTree>
    + FromIterator<Self::TokenStream>
    + FromIterator<Self::TokenTree>
    + FromStr
    + IntoIterator<IntoIter = Self::TokenStreamIntoIter, Item = Self::TokenTree>
{
    /// Make an empty `TokenStream`.
    fn new() -> Self;

    /// Check if this `TokenStream` is empty.
    fn is_empty(&self) -> bool;
}

/// Extensions for [`TokenStream`].
///
/// This trait is implemented for `TokenStream` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
pub trait TokenStreamExt:
    crate::ProcMacroExt<TokenStreamExt = Self> + TokenStream + FromStrDebug
{
    /// Apply a span to every [`TokenTree`] in the `TokenStream`, using [`TokenTree::set_span`]
    #[inline]
    #[must_use]
    fn apply_span(self, span: Self::Span) -> Self {
        self.into_iter()
            .map(|mut tt| {
                tt.set_span(span);
                tt
            })
            .collect()
    }

    /// Match `tokens` and split the `TokenStream`.
    /// The first value in the returned tuple is a `TokenStream` with the matched tokens
    /// (taken from `self`), and the second is the rest of the `TokenStream` with the
    /// matching tokens removed.
    #[inline]
    #[must_use]
    fn expect(self, mut tokens: impl Iterator<Item = Self::TokenTree>) -> Option<(Self, Self)> {
        let mut it = self.into_iter();
        let mut n = 0;
        'check: {
            for a in it.clone() {
                let Some(b) = tokens.next() else { break 'check };
                n += 1;
                if a.to_string() != b.to_string() {
                    return None;
                }
            }
            if tokens.next().is_some() {
                // not enough tokens in self
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

    /// Compare the contents of this `TokenStream` with another, ignoring spans.
    #[inline]
    fn eq_except_span(self, other: Self) -> bool {
        let mut oi = other.into_iter();
        for s in self.into_iter() {
            let Some(o) = oi.next() else { return false };
            if !s.eq_except_span(&o) {
                return false;
            }
        }
        oi.next().is_none()
    }
}

macro_rules! impl_token_stream {
    ($($pm:ident: $feature:literal),*) => { $(
        #[cfg(feature = $feature)]
        impl TokenStream for $pm::TokenStream {
            #[inline]
            fn new() -> Self {
                Self::new()
            }

            #[inline]
            fn is_empty(&self) -> bool {
                self.is_empty()
            }
        }

        #[cfg(feature = $feature)]
        impl TokenStreamExt for $pm::TokenStream {}
    )* };
}

impl_token_stream!(proc_macro: "proc-macro", proc_macro2: "proc-macro2");
