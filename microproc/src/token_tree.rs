use crate::ProcMacro;
use std::fmt::Display;

/// `TokenTree` API trait.
///
/// This trait is implemented for `TokenTree` in `proc_macro` and `proc_macro2` if the
/// corresponding features are enabled.
///
/// See also [`TokenTreeExt`].
pub trait TokenTree: ProcMacro + Display {
    /// Get the span of this `TokenTree`.
    fn span(&self) -> Self::Span;

    /// Set the span of this `TokenTree`. If the `TokenTree` is a [`Group`], this will use [`Group::set_span`].
    fn set_span(&mut self, span: Self::Span);
}

/// Extra utilities for [`TokenTree`].
///
/// This trait is implemented for `TokenTree` in `proc_macro` and `proc_macro2` if the
/// corresponding features are enabled.
pub trait TokenTreeExt: crate::ProcMacroExt<TokenTreeExt = Self> + TokenTree {
    fn group(&self) -> Option<&Self::GroupExt>;

    /// If the `TokenTree` is a group with delimiter `None` containing a single item,
    /// replace the group with that item, recursively.
    #[inline]
    fn flatten_group(&mut self) {
        while let Some(group) = self.group() {
            if group.delimiter().is_none() {
                let mut stream = group.stream().into_iter();
                if let Some(tt) = stream.next() {
                    if stream.next().is_none() {
                        *self = tt;
                        continue;
                    }
                }
            }
            break;
        }
    }
}

pub trait Group: ProcMacro<Group = Self> {
    /// Create a new `Group`. The span will be set to `Span::call_site()`.
    fn new(delimiter: Self::Delimiter, stream: Self::TokenStream) -> Self;

    /// Get the delimiter of this `Group`.
    fn delimiter(&self) -> Self::Delimiter;

    /// Get the delimited `TokenStream`, not including delimiters.
    fn stream(&self) -> Self::TokenStream;

    /// Get the span of this `Group`.
    fn span(&self) -> Self::Span;

    /// Get the span of the group's opening delimiter.
    fn span_open(&self) -> Self::Span;

    /// Get the span of the group's closing delimiter.
    fn span_close(&self) -> Self::Span;

    /// Set the span of this `Group`. This does *not* set the span of the contained `TokenStream`.
    fn set_span(&mut self, span: Self::Span);
}

pub trait GroupExt: crate::ProcMacroExt<GroupExt = Self> + Group {}

pub trait Delimiter: ProcMacro<Delimiter = Self> {}

pub trait DelimiterExt: crate::ProcMacroExt<DelimiterExt = Self> + Delimiter {
    fn is_none(&self) -> bool;
}

macro_rules! impl_token_tree {
    ($($pm:ident: $feature:literal),*) => { $(
        #[cfg(feature = $feature)]
        impl TokenTree for $pm::TokenTree {
            #[inline]
            fn span(&self) -> Self::Span {
                self.span()
            }

            #[inline]
            fn set_span(&mut self, span: Self::Span) {
                self.set_span(span);
            }
        }

        #[cfg(feature = $feature)]
        impl TokenTreeExt for $pm::TokenTree {
            #[inline]
            fn group(&self) -> Option<&Self::GroupExt> {
                if let Self::Group(group) = self {
                    Some(group)
                } else {
                    None
                }
            }
        }

        #[cfg(feature = $feature)]
        impl Group for $pm::Group {
            #[inline]
            fn new(delimiter: Self::Delimiter, stream: Self::TokenStream) -> Self {
                Self::new(delimiter, stream)
            }

            #[inline]
            fn delimiter(&self) -> Self::Delimiter {
                self.delimiter()
            }

            #[inline]
            fn stream(&self) -> Self::TokenStream {
                self.stream()
            }

            #[inline]
            fn span(&self) -> Self::Span {
                self.span()
            }

            #[inline]
            fn span_open(&self) -> Self::Span {
                self.span_open()
            }

            #[inline]
            fn span_close(&self) -> Self::Span {
                self.span_close()
            }

            #[inline]
            fn set_span(&mut self, span: Self::Span) {
                self.set_span(span)
            }
        }

        #[cfg(feature = $feature)]
        impl GroupExt for $pm::Group {}

        #[cfg(feature = $feature)]
        impl Delimiter for $pm::Delimiter {}

        #[cfg(feature = $feature)]
        impl DelimiterExt for $pm::Delimiter {
            #[inline]
            fn is_none(&self) -> bool {
                matches!(self, $pm::Delimiter::None)
            }
        }
    )* };
}

impl_token_tree!(proc_macro: "proc-macro", proc_macro2: "proc-macro2");
