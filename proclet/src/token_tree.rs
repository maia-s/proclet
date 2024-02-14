use crate::{IntoTokens, Parse, ProcMacro, ToTokenStream};
use std::fmt::Display;

/// The kind of a `TokenTree`. This is like the enum in `proc_macro*::TokenTree`, but
/// without any contained data. It doesn't depend on which proc-macro crate you use.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenTreeKind {
    /// `TokenTree::Group`
    Group,

    /// `TokenTree::Ident`
    Ident,

    /// `TokenTree::Punct`
    Punct,

    /// `TokenTree::Literal`
    Literal,
}

/// `TokenTree` API trait. See [`proc_macro::TokenTree`](https://doc.rust-lang.org/stable/proc_macro/enum.TokenTree.html).
///
/// This trait is implemented for `TokenTree` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
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

/// Extensions for [`TokenTree`].
///
/// This trait is implemented for `TokenTree` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
pub trait TokenTreeExt:
    crate::ProcMacroExt<TokenTreeExt = Self>
    + TokenTree
    + Parse<Self>
    + IntoTokens<Self>
    + ToTokenStream<Self::TokenStream>
{
    /// Get the kind of this `TokenTree`.
    fn kind(&self) -> TokenTreeKind;

    /// Check if this `TokenTree` is a `TokenTree::Group`.
    #[inline]
    fn is_group(&self) -> bool {
        self.kind() == TokenTreeKind::Group
    }

    /// If this `TokenTree` is a `TokenTree::Group`, return a reference to the `Group`.
    fn group(&self) -> Option<&Self::Group>;

    /// If this `TokenTree` is a `TokenTree::Group`, return a mutable reference to the `Group`.
    fn group_mut(&mut self) -> Option<&mut Self::Group>;

    /// If this `TokenTree` is a `TokenTree::Group`, return the `Group`.
    fn into_group(self) -> Option<Self::Group>;

    /// Check if this `TokenTree` is a `TokenTree::Ident`.
    #[inline]
    fn is_ident(&self) -> bool {
        self.kind() == TokenTreeKind::Ident
    }

    /// If this `TokenTree` is a `TokenTree::Ident`, return a reference to the `Ident`.
    fn ident(&self) -> Option<&Self::Ident>;

    /// If this `TokenTree` is a `TokenTree::Ident`, return a mutable reference to the `Ident`.
    fn ident_mut(&mut self) -> Option<&mut Self::Ident>;

    /// If this `TokenTree` is a `TokenTree::Ident`, return the `Ident`.
    fn into_ident(self) -> Option<Self::Ident>;

    /// Check if this `TokenTree` is a `TokenTree::Punct`.
    #[inline]
    fn is_punct(&self) -> bool {
        self.kind() == TokenTreeKind::Punct
    }

    /// If this `TokenTree` is a `TokenTree::Punct`, return a reference to the `Punct`.
    fn punct(&self) -> Option<&Self::Punct>;

    /// If this `TokenTree` is a `TokenTree::Punct`, return a mutable reference to the `Punct`.
    fn punct_mut(&mut self) -> Option<&mut Self::Punct>;

    /// Check if this `TokenTree` is a `TokenTree::Punct`.
    fn into_punct(self) -> Option<Self::Punct>;

    /// Check if this `TokenTree` is a `TokenTree::Literal`.
    #[inline]
    fn is_literal(&self) -> bool {
        self.kind() == TokenTreeKind::Literal
    }

    /// If this `TokenTree` is a `TokenTree::Literal`, return a reference to the `Literal`.
    fn literal(&self) -> Option<&Self::Literal>;

    /// If this `TokenTree` is a `TokenTree::Literal`, return a mutable reference to the `Literal`.
    fn literal_mut(&mut self) -> Option<&mut Self::Literal>;

    /// Check if this `TokenTree` is a `TokenTree::Literal`.
    fn into_literal(self) -> Option<Self::Literal>;

    /// If the `TokenTree` is a group with delimiter `None` containing a single item,
    /// replace the group with that item, recursively.
    #[inline]
    fn flatten_group(&mut self) {
        if let Some(group) = self.group() {
            if let Some(tt) = group.flatten() {
                *self = tt;
            }
        }
    }
}

/// `Group` API trait. See [`proc_macro::Group`](https://doc.rust-lang.org/stable/proc_macro/struct.Group.html).
///
/// This trait is implemented for `Group` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
///
/// See also [`GroupExt`].
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

/// Extensions for [`Group`].
///
/// This trait is implemented for `Group` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
pub trait GroupExt:
    crate::ProcMacroExt<GroupExt = Self>
    + Group
    + Parse<Self::TokenTree>
    + IntoTokens<Self::TokenTree>
    + ToTokenStream<Self::TokenStream>
{
    /// Create a new `Group` with a custom span.
    fn with_span(delimiter: Self::Delimiter, stream: Self::TokenStream, span: Self::Span) -> Self {
        let mut group = Self::new(delimiter, stream);
        group.set_span(span);
        group
    }

    /// Get the delimiter of this `Group` as a matchable enum.
    #[inline]
    fn delimiter_kind(&self) -> DelimiterKind {
        self.delimiter().into()
    }

    /// Get the delimited `TokenStream` as a `TokenBuffer`, not including delimiters.
    fn stream_buffer(&self) -> crate::TokenBuffer<Self::TokenTree>;

    /// If the group has delimiter `None` and contains a single item, extract that item,
    /// and if that item is a group, flatten that too, recursively. Then return the item,
    /// or `None` if the conditions weren't met.
    #[inline]
    fn flatten(&self) -> Option<Self::TokenTree> {
        if self.delimiter().is_none() {
            let mut stream = self.stream().into_iter();
            if let Some(tt) = stream.next() {
                if stream.next().is_none() {
                    if let Some(group) = tt.group() {
                        if let Some(tt) = group.flatten() {
                            return Some(tt);
                        }
                    }
                    return Some(tt);
                }
            }
        }
        None
    }
}

/// Delimiter. This is exactly like `proc_macro*::Delimiter`, but doesn't depend on
/// which proc-macro crate you use.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DelimiterKind {
    /// `(` ... `)`
    Parenthesis,

    /// `{` ... `}`
    Brace,

    /// `[` ... `]`
    Bracket,

    /// No delimiter.
    None,
}

/// `Delimiter` API trait. See [`proc_macro::Delimiter`](https://doc.rust-lang.org/stable/proc_macro/enum.Delimiter.html).
///
/// This trait is implemented for `Delimiter` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
///
/// See also [`DelimiterExt`].
#[allow(non_upper_case_globals)]
pub trait Delimiter: ProcMacro<Delimiter = Self> + Copy + Eq {
    /// `proc_macro*::Delimiter::Parenthesis` (`(` ... `)`)
    const Parenthesis: Self;

    /// `proc_macro*::Delimiter::Brace` `{` ... `}`
    const Brace: Self;

    /// `proc_macro*::Delimiter::Bracket` `[` ...  `]`
    const Bracket: Self;

    /// `proc_macro*::Delimiter::None` (No delimiter)
    const None: Self;
}

/// Extensions for [`Delimiter`].
///
/// This trait is implemented for `Delimiter` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
pub trait DelimiterExt:
    crate::ProcMacroExt<DelimiterExt = Self>
    + Delimiter
    + From<DelimiterKind>
    + Into<DelimiterKind>
    + PartialEq<DelimiterKind>
{
    /// Get the kind of this `Delimiter`.
    #[inline]
    fn kind(&self) -> DelimiterKind {
        (*self).into()
    }

    /// Check if this delimiter is `Delimiter::Parenthesis`.
    #[inline]
    fn is_parenthesis(&self) -> bool {
        *self == Self::Parenthesis
    }

    /// Check if this delimiter is `Delimiter::Brace`.
    #[inline]
    fn is_brace(&self) -> bool {
        *self == Self::Brace
    }

    /// Check if this delimiter is `Delimiter::Bracket`.
    #[inline]
    fn is_bracket(&self) -> bool {
        *self == Self::Bracket
    }

    /// Check if this delimiter is `Delimiter::None`.
    #[inline]
    fn is_none(&self) -> bool {
        *self == Self::None
    }
}

/// `Ident` API trait. See [`proc_macro::Ident`](https://doc.rust-lang.org/stable/proc_macro/struct.Ident.html).
///
/// This trait is implemented for `Ident` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
///
/// See also [`IdentExt`].
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

/// Extensions for [`Ident`].
///
/// This trait is implemented for `Ident` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
pub trait IdentExt:
    crate::ProcMacroExt<IdentExt = Self>
    + Ident
    + Parse<Self::TokenTree>
    + IntoTokens<Self::TokenTree>
    + ToTokenStream<Self::TokenStream>
{
}

/// `Punct` API trait. See [`proc_macro::Punct`](https://doc.rust-lang.org/stable/proc_macro/struct.Punct.html).
///
/// This trait is implemented for `Punct` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
///
/// See also [`PunctExt`].
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

/// Extensions for [`Punct`].
///
/// This trait is implemented for `Punct` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
pub trait PunctExt:
    crate::ProcMacroExt<PunctExt = Self>
    + Punct
    + Parse<Self::TokenTree>
    + IntoTokens<Self::TokenTree>
    + ToTokenStream<Self::TokenStream>
{
    /// Create a new `Punct` with a custom `Span`.
    #[inline]
    fn with_span(ch: char, spacing: Self::Spacing, span: Self::Span) -> Self {
        let mut punct = Self::Punct::new(ch, spacing);
        punct.set_span(span);
        punct
    }

    /// Set the spacing of this `Punct`.
    #[inline]
    fn set_spacing(&mut self, spacing: Self::Spacing) {
        *self = Self::with_span(self.as_char(), spacing, self.span());
    }
}

/// `Spacing` API trait. See [`proc_macro::Spacing`](https://doc.rust-lang.org/stable/proc_macro/enum.Spacing.html).
///
/// This trait is implemented for `Spacing` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
///
/// See also [`SpacingExt`].
#[allow(non_upper_case_globals)]
pub trait Spacing: ProcMacro<Spacing = Self> + Copy + Eq {
    /// `proc_macro*::Spacing::Joint`.
    const Joint: Self;

    /// `proc_macro*::Spacing::Alone`.
    const Alone: Self;
}

/// Extensions for [`Spacing`].
///
/// This trait is implemented for `Spacing` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
pub trait SpacingExt: crate::ProcMacroExt<SpacingExt = Self> + Spacing {
    /// Check if this is `Spacing::Joint`.
    #[inline]
    fn is_joint(&self) -> bool {
        *self == Self::Joint
    }

    /// Check if this is `Spacing::Alone`.
    #[inline]
    fn is_alone(&self) -> bool {
        *self == Self::Alone
    }
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
            fn kind(&self) -> TokenTreeKind {
                match self {
                    Self::Group(_) => TokenTreeKind::Group,
                    Self::Ident(_) => TokenTreeKind::Ident,
                    Self::Punct(_) => TokenTreeKind::Punct,
                    Self::Literal(_) => TokenTreeKind::Literal,
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
            fn into_group(self) -> Option<<Self as ProcMacro>::Group> {
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
            fn into_ident(self) -> Option<<Self as ProcMacro>::Ident> {
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
            fn into_punct(self) -> Option<<Self as ProcMacro>::Punct> {
                if let Self::Punct(punct) = self {
                    Some(punct)
                } else {
                    None
                }
            }

            #[inline]
            fn literal(&self) -> Option<&<Self as ProcMacro>::Literal> {
                if let Self::Literal(literal) = self {
                    Some(literal)
                } else {
                    None
                }
            }

            #[inline]
            fn literal_mut(&mut self) -> Option<&mut <Self as ProcMacro>::Literal> {
                if let Self::Literal(literal) = self {
                    Some(literal)
                } else {
                    None
                }
            }

            #[inline]
            fn into_literal(self) -> Option<<Self as ProcMacro>::Literal> {
                if let Self::Literal(literal) = self {
                    Some(literal)
                } else {
                    None
                }
            }
        }

        #[cfg(feature = $feature)]
        impl crate::Parse<$pm::TokenTree> for $pm::TokenTree {
            #[inline]
            fn parse(buf: &mut &crate::TokenBuf<$pm::TokenTree>) -> Option<Self> {
                let result = buf.first()?.clone();
                *buf = &buf[1..];
                Some(result)
            }
        }

        #[cfg(all(feature = $feature))]
        impl crate::IntoTokens<$pm::TokenTree> for $pm::TokenTree {
            #[inline]
            fn into_tokens(mut self) -> impl Iterator<Item = crate::TokenObject<$pm::TokenTree>> {
                self.flatten_group();
                std::iter::once(self)
            }
        }

        #[cfg(feature = $feature)]
        impl crate::ToTokenStream<$pm::TokenStream> for $pm::TokenTree {
            #[inline]
            fn extend_token_stream(&self, token_stream: &mut $pm::TokenStream) {
                token_stream.extend(self.clone().into_tokens())
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
        impl GroupExt for $pm::Group {
            #[inline]
            fn stream_buffer(&self) -> crate::TokenBuffer<$pm::TokenTree> {
                crate::TokenBuffer::from(self.stream())
            }
        }

        #[cfg(feature = $feature)]
        impl crate::Parse<$pm::TokenTree> for $pm::Group {
            #[inline]
            fn parse(buf: &mut &crate::TokenBuf<$pm::TokenTree>) -> Option<Self> {
                buf.parse_prefix(|token| {
                    if let $pm::TokenTree::Group(t) = token {
                        crate::Match::Complete(t.clone())
                    } else {
                        crate::Match::NoMatch
                    }
                })
            }
        }

        #[cfg(all(feature = $feature))]
        impl crate::IntoTokens<$pm::TokenTree> for $pm::Group {
            #[inline]
            fn into_tokens(self) -> impl Iterator<Item = crate::TokenObject<$pm::TokenTree>> {
                std::iter::once($pm::TokenTree::Group(self))
            }
        }

        #[cfg(feature = $feature)]
        impl crate::ToTokenStream<$pm::TokenStream> for $pm::Group {
            #[inline]
            fn extend_token_stream(&self, token_stream: &mut $pm::TokenStream) {
                token_stream.extend([$pm::TokenTree::from(self.clone())])
            }
        }

        #[cfg(feature = $feature)]
        impl From<$pm::Delimiter> for DelimiterKind {
            #[inline]
            fn from(value: $pm::Delimiter) -> Self {
                match value {
                    $pm::Delimiter::Parenthesis => Self::Parenthesis,
                    $pm::Delimiter::Brace => Self::Brace,
                    $pm::Delimiter::Bracket => Self::Bracket,
                    $pm::Delimiter::None => Self::None,
                }
            }
        }

        #[cfg(feature = $feature)]
        impl From<DelimiterKind> for $pm::Delimiter {
            #[inline]
            fn from(value: DelimiterKind) -> Self {
                match value {
                    DelimiterKind::Parenthesis => Self::Parenthesis,
                    DelimiterKind::Brace => Self::Brace,
                    DelimiterKind::Bracket => Self::Bracket,
                    DelimiterKind::None => Self::None,
                }
            }
        }

        #[cfg(feature = $feature)]
        impl PartialEq<$pm::Delimiter> for DelimiterKind {
            #[inline]
            fn eq(&self, rhs: &$pm::Delimiter) -> bool {
                *self == rhs.kind()
            }
        }

        #[cfg(feature = $feature)]
        impl PartialEq<DelimiterKind> for $pm::Delimiter {
            #[inline]
            fn eq(&self, rhs: &DelimiterKind) -> bool {
                self.kind() == *rhs
            }
        }

        #[cfg(feature = $feature)]
        #[allow(non_upper_case_globals)]
        impl Delimiter for $pm::Delimiter {
            const Parenthesis: Self = Self::Parenthesis;
            const Brace: Self = Self::Brace;
            const Bracket: Self = Self::Bracket;
            const None: Self = Self::None;
        }

        #[cfg(feature = $feature)]
        impl DelimiterExt for $pm::Delimiter {}

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
        impl crate::Parse<$pm::TokenTree> for $pm::Ident {
            #[inline]
            fn parse(buf: &mut &crate::TokenBuf<$pm::TokenTree>) -> Option<Self> {
                buf.parse_prefix(|token| {
                    if let $pm::TokenTree::Ident(t) = token {
                        crate::Match::Complete(t.clone())
                    } else {
                        crate::Match::NoMatch
                    }
                })
            }
        }

        #[cfg(all(feature = $feature))]
        impl crate::IntoTokens<$pm::TokenTree> for $pm::Ident {
            #[inline]
            fn into_tokens(self) -> impl Iterator<Item = crate::TokenObject<$pm::TokenTree>> {
                std::iter::once($pm::TokenTree::Ident(self))
            }
        }

        #[cfg(feature = $feature)]
        impl crate::ToTokenStream<$pm::TokenStream> for $pm::Ident {
            #[inline]
            fn extend_token_stream(&self, token_stream: &mut $pm::TokenStream) {
                token_stream.extend([$pm::TokenTree::from(self.clone())])
            }
        }

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
        impl crate::Parse<$pm::TokenTree> for $pm::Punct {
            #[inline]
            fn parse(buf: &mut &crate::TokenBuf<$pm::TokenTree>) -> Option<Self> {
                buf.parse_prefix(|token| {
                    if let $pm::TokenTree::Punct(t) = token {
                        crate::Match::Complete(t.clone())
                    } else {
                        crate::Match::NoMatch
                    }
                })
            }
        }

        #[cfg(feature = $feature)]
        impl crate::IntoTokens<$pm::TokenTree> for $pm::Punct {
            #[inline]
            fn into_tokens(self) -> impl Iterator<Item = crate::TokenObject<$pm::TokenTree>> {
                std::iter::once($pm::TokenTree::Punct(self))
            }
        }

        #[cfg(feature = $feature)]
        impl crate::ToTokenStream<$pm::TokenStream> for $pm::Punct {
            #[inline]
            fn extend_token_stream(&self, token_stream: &mut $pm::TokenStream) {
                token_stream.extend([$pm::TokenTree::from(self.clone())])
            }
        }

        #[cfg(feature = $feature)]
        #[allow(non_upper_case_globals)]
        impl Spacing for $pm::Spacing {
            const Joint: Self = Self::Joint;
            const Alone: Self = Self::Alone;
        }

        #[cfg(feature = $feature)]
        impl SpacingExt for $pm::Spacing {}
    )* };
}

impl_token_tree!(proc_macro: "proc-macro", proc_macro2: "proc-macro2");
