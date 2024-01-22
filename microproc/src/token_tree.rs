use crate::ProcMacro;
use std::fmt::Display;

/// `TokenTree` API trait.
///
/// This trait is implemented for `TokenTree` in `proc_macro` and `proc_macro2` if the
/// corresponding features are enabled.
///
/// See also [`TokenTreeExt`].
pub trait TokenTree:
    ProcMacro<TokenTree = Self>
    + Display
    + From<Self::Group>
    + From<Self::Ident>
    + From<Self::Punct>
    + From<Self::Literal>
{
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
    fn do_match<T>(
        &self,
        group: impl FnOnce(&Self::Group) -> T,
        ident: impl FnOnce(&Self::Ident) -> T,
        punct: impl FnOnce(&Self::Punct) -> T,
        literal: impl FnOnce(&Self::Literal) -> T,
    ) -> T;

    #[inline]
    fn is_group(&self) -> bool {
        self.group().is_some()
    }

    fn group(&self) -> Option<&Self::Group>;
    fn group_mut(&mut self) -> Option<&mut Self::Group>;

    #[inline]
    fn is_ident(&self) -> bool {
        self.ident().is_some()
    }

    fn ident(&self) -> Option<&Self::Ident>;
    fn ident_mut(&mut self) -> Option<&mut Self::Ident>;

    #[inline]
    fn is_punct(&self) -> bool {
        self.punct().is_some()
    }

    fn punct(&self) -> Option<&Self::Punct>;
    fn punct_mut(&mut self) -> Option<&mut Self::Punct>;

    #[inline]
    fn is_literal(&self) -> bool {
        self.literal().is_some()
    }

    fn literal(&self) -> Option<&Self::Literal>;
    fn literal_mut(&mut self) -> Option<&mut Self::Literal>;

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

pub trait Group: ProcMacro<Group = Self> + Display {
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
    fn do_match<T>(
        &self,
        parenthesis: impl FnOnce() -> T,
        brace: impl FnOnce() -> T,
        bracket: impl FnOnce() -> T,
        none: impl FnOnce() -> T,
    ) -> T;
    fn is_parenthesis(&self) -> bool;
    fn is_brace(&self) -> bool;
    fn is_bracket(&self) -> bool;
    fn is_none(&self) -> bool;
}

pub trait Ident: ProcMacro<Ident = Self> + Display {
    /// Create a new `Ident` with the specified `span`.
    fn new(string: &str, span: Self::Span) -> Self;

    /// Create a new raw identifier with the specified `span`.
    fn new_raw(string: &str, span: Self::Span) -> Self;

    /// The span of this `Ident`.
    fn span(&self) -> Self::Span;

    /// Set the span of this `Ident`.
    fn set_span(&mut self, span: Self::Span);
}

pub trait IdentExt: crate::ProcMacroExt<IdentExt = Self> + Ident {}

pub trait Punct: ProcMacro<Punct = Self> + Display {
    /// Create a new `Punct`.
    fn new(ch: char, spacing: Self::Spacing) -> Self;

    /// The value of this `Punct` as a `char`
    fn as_char(&self) -> char;

    /// The `Spacing` of this `Punct`.
    fn spacing(&self) -> Self::Spacing;

    /// The span of this `Punct`.
    fn span(&self) -> Self::Span;

    /// Set the span of this `Punct`.
    fn set_span(&mut self, span: Self::Span);
}

pub trait PunctExt: crate::ProcMacroExt<PunctExt = Self> + Punct {}

pub trait Spacing: ProcMacro<Spacing = Self> + Copy + Eq {}

pub trait SpacingExt: crate::ProcMacroExt<SpacingExt = Self> + Spacing {
    fn is_joint(&self) -> bool;
    fn is_alone(&self) -> bool;
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
            fn do_match<T>(
                &self,
                on_group: impl FnOnce(&<Self as ProcMacro>::Group) -> T,
                on_ident: impl FnOnce(&<Self as ProcMacro>::Ident) -> T,
                on_punct: impl FnOnce(&<Self as ProcMacro>::Punct) -> T,
                on_literal: impl FnOnce(&<Self as ProcMacro>::Literal) -> T,
            ) -> T {
                match self {
                    Self::Group(group) => on_group(group),
                    Self::Ident(ident) => on_ident(ident),
                    Self::Punct(punct) => on_punct(punct),
                    Self::Literal(literal) => on_literal(literal),
                }
            }

            #[inline]
            fn group(&self) -> Option<&<Self as ProcMacro>::Group> {
                if let Self::Group(group) = self {
                    Some(group)
                } else {
                    None
                }
            }

            #[inline]
            fn group_mut(&mut self) -> Option<&mut <Self as ProcMacro>::Group> {
                if let Self::Group(group) = self {
                    Some(group)
                } else {
                    None
                }
            }

            #[inline]
            fn ident(&self) -> Option<&<Self as ProcMacro>::Ident> {
                if let Self::Ident(ident) = self {
                    Some(ident)
                } else {
                    None
                }
            }

            #[inline]
            fn ident_mut(&mut self) -> Option<&mut <Self as ProcMacro>::Ident> {
                if let Self::Ident(ident) = self {
                    Some(ident)
                } else {
                    None
                }
            }

            #[inline]
            fn punct(&self) -> Option<&<Self as ProcMacro>::Punct> {
                if let Self::Punct(punct) = self {
                    Some(punct)
                } else {
                    None
                }
            }

            #[inline]
            fn punct_mut(&mut self) -> Option<&mut <Self as ProcMacro>::Punct> {
                if let Self::Punct(punct) = self {
                    Some(punct)
                } else {
                    None
                }
            }

            #[inline]
            fn literal(&self) -> Option<&<Self as ProcMacro>::Literal> {
                if let Self::Literal(lit) = self {
                    Some(lit)
                } else {
                    None
                }
            }

            #[inline]
            fn literal_mut(&mut self) -> Option<&mut <Self as ProcMacro>::Literal> {
                if let Self::Literal(lit) = self {
                    Some(lit)
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
            fn do_match<T>(
                &self,
                parenthesis: impl FnOnce() -> T,
                brace: impl FnOnce() -> T,
                bracket: impl FnOnce() -> T,
                none: impl FnOnce() -> T,
            ) -> T {
                match *self {
                    Self::Parenthesis => parenthesis(),
                    Self::Brace => brace(),
                    Self::Bracket => bracket(),
                    Self::None => none(),
                }
            }

            #[inline]
            fn is_parenthesis(&self) -> bool {
                matches!(self, $pm::Delimiter::Parenthesis)
            }

            #[inline]
            fn is_brace(&self) -> bool {
                matches!(self, $pm::Delimiter::Brace)
            }

            #[inline]
            fn is_bracket(&self) -> bool {
                matches!(self, $pm::Delimiter::Bracket)
            }

            #[inline]
            fn is_none(&self) -> bool {
                matches!(self, $pm::Delimiter::None)
            }
        }

        #[cfg(feature = $feature)]
        impl Ident for $pm::Ident {
            #[inline]
            fn new(string: &str, span: Self::Span) -> Self {
                Self::new(string, span)
            }

            #[inline]
            fn new_raw(string: &str, span: Self::Span) -> Self {
                Self::new_raw(string, span)
            }

            #[inline]
            fn span(&self) -> Self::Span {
                self.span()
            }

            #[inline]
            fn set_span(&mut self, span: Self::Span) {
                self.set_span(span)
            }
        }

        #[cfg(feature = $feature)]
        impl IdentExt for $pm::Ident {}

        #[cfg(feature = $feature)]
        impl Punct for $pm::Punct {
            #[inline]
            fn new(ch: char, spacing: Self::Spacing) -> Self {
                Self::new(ch, spacing)
            }

            #[inline]
            fn as_char(&self) -> char {
                self.as_char()
            }

            #[inline]
            fn spacing(&self) -> Self::Spacing {
                self.spacing()
            }

            #[inline]
            fn span(&self) -> Self::Span {
                self.span()
            }

            #[inline]
            fn set_span(&mut self, span: Self::Span) {
                self.set_span(span)
            }
        }

        #[cfg(feature = $feature)]
        impl PunctExt for $pm::Punct {}

        #[cfg(feature = $feature)]
        impl Spacing for $pm::Spacing {}

        #[cfg(feature = $feature)]
        impl SpacingExt for $pm::Spacing {
            #[inline]
            fn is_joint(&self) -> bool {
                matches!(self, $pm::Spacing::Joint)
            }

            #[inline]
            fn is_alone(&self) -> bool {
                matches!(self, $pm::Spacing::Alone)
            }
        }
    )* };
}

impl_token_tree!(proc_macro: "proc-macro", proc_macro2: "proc-macro2");
