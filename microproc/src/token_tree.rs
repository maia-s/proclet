use crate::{ProcMacro, ToTokenTrees, Token, TokenStreamExt, TokenTrees};
use std::{any::Any, fmt::Display};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenTreeKind {
    Group,
    Ident,
    Punct,
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
    crate::ProcMacroExt<TokenTreeExt = Self> + TokenTree + Into<Box<dyn Token<Self::PM>>>
{
    fn into_token(self) -> Box<dyn Token<Self::PM>>;

    fn kind(&self) -> TokenTreeKind;

    #[inline]
    fn is_group(&self) -> bool {
        self.kind() == TokenTreeKind::Group
    }

    fn group(&self) -> Option<&Self::Group>;
    fn group_mut(&mut self) -> Option<&mut Self::Group>;
    fn into_group(self) -> Option<Self::Group>;

    #[inline]
    fn is_ident(&self) -> bool {
        self.kind() == TokenTreeKind::Ident
    }

    fn ident(&self) -> Option<&Self::Ident>;
    fn ident_mut(&mut self) -> Option<&mut Self::Ident>;
    fn into_ident(self) -> Option<Self::Ident>;

    #[inline]
    fn is_punct(&self) -> bool {
        self.kind() == TokenTreeKind::Punct
    }

    fn punct(&self) -> Option<&Self::Punct>;
    fn punct_mut(&mut self) -> Option<&mut Self::Punct>;
    fn into_punct(self) -> Option<Self::Punct>;

    #[inline]
    fn is_literal(&self) -> bool {
        self.kind() == TokenTreeKind::Literal
    }

    fn literal(&self) -> Option<&Self::Literal>;
    fn literal_mut(&mut self) -> Option<&mut Self::Literal>;
    fn into_literal(self) -> Option<Self::Literal>;

    fn eq_except_span(&self, other: &Self) -> bool;

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
pub trait GroupExt: crate::ProcMacroExt<GroupExt = Self> + Group + Token<Self::PM> {
    /// Get the delimiter of this `Group` as a matchable enum.
    #[inline]
    fn delimiter_kind(&self) -> DelimiterKind {
        self.delimiter().into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DelimiterKind {
    Parenthesis,
    Brace,
    Bracket,
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
    const Parenthesis: Self;
    const Brace: Self;
    const Bracket: Self;
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
    #[inline]
    fn kind(&self) -> DelimiterKind {
        (*self).into()
    }

    #[inline]
    fn is_parenthesis(&self) -> bool {
        *self == Self::Parenthesis
    }

    #[inline]
    fn is_brace(&self) -> bool {
        *self == Self::Brace
    }

    #[inline]
    fn is_bracket(&self) -> bool {
        *self == Self::Bracket
    }

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
pub trait IdentExt: crate::ProcMacroExt<IdentExt = Self> + Ident + Token<Self::PM> {}

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
pub trait PunctExt: crate::ProcMacroExt<PunctExt = Self> + Punct + Token<Self::PM> {
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
    const Joint: Self;
    const Alone: Self;
}

/// Extensions for [`Spacing`].
///
/// This trait is implemented for `Spacing` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
pub trait SpacingExt: crate::ProcMacroExt<SpacingExt = Self> + Spacing {
    #[inline]
    fn is_joint(&self) -> bool {
        *self == Self::Joint
    }

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
        impl From<$pm::TokenTree> for Box<dyn Token<crate::base::$pm::PM>> {
            #[inline]
            fn from(value: $pm::TokenTree) -> Self {
                value.into_token()
            }
        }

        #[cfg(feature = $feature)]
        impl TokenTreeExt for $pm::TokenTree {
            #[inline]
            fn into_token(self) -> Box<dyn Token<crate::base::$pm::PM>> {
                match self {
                    Self::Group(t) => Box::new(t) as Box<dyn Token<crate::base::$pm::PM>>,
                    Self::Ident(t) => Box::new(t) as Box<dyn Token<crate::base::$pm::PM>>,
                    Self::Punct(t) => Box::new(t) as Box<dyn Token<crate::base::$pm::PM>>,
                    Self::Literal(t) => Box::new(t) as Box<dyn Token<crate::base::$pm::PM>>,
                }
            }

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

            #[inline]
            fn eq_except_span(&self, other: &Self) -> bool {
                match (self, other) {
                    (Self::Group(s), Self::Group(o)) => s.eq_except_span(o),
                    (Self::Ident(s), Self::Ident(o)) => s.eq_except_span(o),
                    (Self::Punct(s), Self::Punct(o)) => s.eq_except_span(o),
                    (Self::Literal(s), Self::Literal(o)) => s.eq_except_span(o),
                    _ => false,
                }
            }
        }

        #[cfg(feature = $feature)]
        impl ToTokenTrees<$pm::TokenTree> for $pm::TokenTree {
            #[inline]
            fn to_token_trees(&self) -> TokenTrees<Self> {
                self.clone().into()
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
        impl Token<crate::base::$pm::PM> for $pm::Group {
            #[inline]
            fn as_any(&self) -> &dyn Any {
                self
            }

            #[inline]
            fn as_any_mut(&mut self) -> &mut dyn Any {
                self
            }

            #[inline]
            fn eq_except_span(&self, other: &dyn Token<crate::base::$pm::PM>) -> bool {
                other.downcast_ref::<Self>().map(|other|
                    self.delimiter() == other.delimiter() && self.stream().eq_except_span(other.stream())
                ).unwrap_or(false)
            }
        }

        #[cfg(feature = $feature)]
        impl ToTokenTrees<$pm::TokenTree> for $pm::Group {
            #[inline]
            fn to_token_trees(&self) -> TokenTrees<$pm::TokenTree> {
                $pm::TokenTree::from(self.clone()).into()
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
        impl Token<crate::base::$pm::PM> for $pm::Ident {
            #[inline]
            fn as_any(&self) -> &dyn Any {
                self
            }

            #[inline]
            fn as_any_mut(&mut self) -> &mut dyn Any {
                self
            }

            #[inline]
            fn eq_except_span(&self, other: &dyn Token<crate::base::$pm::PM>) -> bool {
                #[allow(clippy::cmp_owned)] // it's the only way to get their value, clippy
                other.downcast_ref::<Self>().map(
                    |other| self.to_string() == other.to_string()
                ).unwrap_or(false)
            }
        }

        #[cfg(feature = $feature)]
        impl ToTokenTrees<$pm::TokenTree> for $pm::Ident {
            #[inline]
            fn to_token_trees(&self) -> TokenTrees<$pm::TokenTree> {
                $pm::TokenTree::from(self.clone()).into()
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
        impl Token<crate::base::$pm::PM> for $pm::Punct {
            #[inline]
            fn as_any(&self) -> &dyn Any {
                self
            }

            #[inline]
            fn as_any_mut(&mut self) -> &mut dyn Any {
                self
            }

            #[inline]
            fn eq_except_span(&self, other: &dyn Token<crate::base::$pm::PM>) -> bool {
                other.downcast_ref::<Self>().map(|other| self.as_char() == other.as_char()).unwrap_or(false)
            }
        }

        #[cfg(feature = $feature)]
        impl ToTokenTrees<$pm::TokenTree> for $pm::Punct {
            #[inline]
            fn to_token_trees(&self) -> TokenTrees<$pm::TokenTree> {
                $pm::TokenTree::from(self.clone()).into()
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
