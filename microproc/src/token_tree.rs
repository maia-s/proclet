use std::fmt::Display;

use crate::base::ProcMacro;

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
pub trait TokenTreeExt: TokenTree {
    /// If the TokenTree is a group with delimiter None containing a single item,
    /// replace the group with that item, recursively.
    fn flatten_group(&mut self);
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
            fn flatten_group(&mut self) {
                while let Self::Group(group) = self {
                    if matches!(group.delimiter(), $pm::Delimiter::None) {
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
    )* };
}

impl_token_tree!(proc_macro: "proc-macro", proc_macro2: "proc-macro2");
