use crate::{prelude::*, Punct, PunctExt, Span, Token};
use std::{
    fmt::Display,
    iter::{self, FusedIterator, Peekable},
    marker::PhantomData,
    mem, slice,
    str::Chars,
};

pub trait Op<S: Span>: Token<S::TokenTree> {
    fn as_str(&self) -> &'static str;
    fn puncts(&self) -> Puncts<S::Punct>;
    fn spans(&self) -> &[S];
    fn set_spans(&mut self, spans: &[S]);
    fn span(&self) -> S;
    fn set_span(&mut self, span: S);
}

#[derive(Debug)]
pub struct InvalidOpError<P: Punct>(pub Vec<P>);

impl<P: Punct> std::error::Error for InvalidOpError<P> {}

impl<P: Punct> Display for InvalidOpError<P> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid op: `")?;
        for punct in self.0.iter() {
            write!(f, "{}", punct.as_char())?;
        }
        write!(f, "`")
    }
}

#[derive(Clone)]
pub struct OpParser<P: PunctExt, F> {
    str: String,
    spans: Vec<P::Span>,
    puncts: Vec<P>,
    next: Option<P>,
    make_op: F,
}

impl<P: PunctExt, F: Fn(&str, Option<char>, &[P::Span]) -> Option<Box<dyn Op<P::Span>>>>
    OpParser<P, F>
{
    /// Create a new `OpParser`. `make_op` is used to implement [`OpParser::make_op`]; see that
    /// for information about how `make_op` should work.
    #[inline]
    pub const fn new(make_op: F) -> Self {
        Self {
            str: String::new(),
            spans: Vec::new(),
            puncts: Vec::new(),
            next: None,
            make_op,
        }
    }

    /// Clear the state to remove any accumulated op.
    #[inline]
    pub fn clear(&mut self) {
        self.str.clear();
        self.spans.clear();
        self.puncts.clear();
        self.next = None;
    }

    /// Returns an operator with span `spans` if `str` is a valid op, unless, if `next` is
    /// `Some`, appending `next` to `str` with joint spacing would also be a valid op.
    /// In other words, this returns an op if that op is the only possible valid op given
    /// `str` and `next`.
    ///
    /// For example, if `+` (`Plus`) and `+=` (`PlusEq`) are valid ops and `+-` is invalid:
    ///
    /// - `make_op("+", Some('='), span)` returns `None`
    /// - `make_op("+", Some('-'), span)` returns `Some(Box<Plus>)`
    /// - `make_op("+", None, span)` returns `Some(Box<Plus>)`
    /// - `make_op("+=", None, span)` returns `Some(Box<PlusEq>)`
    /// - `make_op("+-", None, span)` returns `None`
    ///
    /// `spans` can be one span per char, or just one span for the op to share. If `spans`
    /// doesn't have either length `1` or the same length as `str`, the result is unspecified.
    #[inline]
    pub fn make_op(
        &self,
        str: &str,
        next: Option<char>,
        spans: &[P::Span],
    ) -> Option<Box<dyn Op<P::Span>>> {
        (self.make_op)(str, next, spans)
    }

    /// Add a `Punct` to the currently accumulating op. This may return a new [`Op`], or `None`
    /// if more puncts are needed. If `punct` has alone spacing and the accumulated op isn't
    /// valid, it returns an error. Call [`OpParser::finish`] to finish parsing.
    #[inline]
    #[allow(clippy::type_complexity)]
    pub fn apply(&mut self, punct: P) -> Option<Result<Box<dyn Op<P::Span>>, InvalidOpError<P>>> {
        let (mut punct, mut next_ch) = if let Some(next) = mem::take(&mut self.next) {
            let next_ch = next.spacing().is_joint().then_some(punct.as_char());
            self.next = Some(punct);
            (next, next_ch)
        } else if punct.spacing().is_alone() {
            (punct, None)
        } else {
            self.next = Some(punct);
            return None;
        };

        loop {
            self.str.push(punct.as_char());
            self.spans.push(punct.span());
            self.puncts.push(punct);

            if let Some(op) = self.make_op(&self.str, next_ch, &self.spans) {
                self.str.clear();
                self.spans.clear();
                self.puncts.clear();
                return Some(Ok(op));
            } else if let Some(next) = self.next.as_ref() {
                if next.spacing().is_alone() {
                    punct = mem::take(&mut self.next).unwrap();
                    next_ch = None;
                    continue;
                } else {
                    return None;
                }
            } else {
                self.str.clear();
                self.spans.clear();
                return Some(Err(InvalidOpError(mem::take(&mut self.puncts))));
            }
        }
    }

    /// Finish parsing the currently accumulated op.
    #[inline]
    #[allow(clippy::type_complexity)]
    pub fn finish(&mut self) -> Option<Result<Box<dyn Op<P::Span>>, InvalidOpError<P>>> {
        mem::take(&mut self.next).map(|punct| {
            self.str.push(punct.as_char());
            self.spans.push(punct.span());
            let op = self.make_op(&self.str, None, &self.spans);
            self.str.clear();
            self.spans.clear();
            self.puncts.push(punct);
            let puncts = mem::take(&mut self.puncts);
            if let Some(op) = op {
                Ok(op)
            } else {
                Err(InvalidOpError(puncts))
            }
        })
    }
}

/// Iterator over `Punct`s.
#[derive(Clone, Debug)]
pub struct Puncts<'a, P: Punct>(
    Peekable<Chars<'a>>,
    iter::Cycle<iter::Copied<slice::Iter<'a, P::Span>>>,
    PhantomData<fn() -> P>,
);

impl<'a, P: Punct> Puncts<'a, P> {
    pub fn new(str: &'a str, span: &'a [P::Span]) -> Self {
        Self(
            str.chars().peekable(),
            span.iter().copied().cycle(),
            PhantomData,
        )
    }
}

impl<'a, P: PunctExt> Iterator for Puncts<'a, P> {
    type Item = P;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|ch| {
            P::with_span(
                ch,
                if self.0.peek().is_some() {
                    P::Spacing::Joint
                } else {
                    P::Spacing::Alone
                },
                self.1.next().unwrap(),
            )
        })
    }
}

impl<'a, P: PunctExt> FusedIterator for Puncts<'a, P> where Puncts<'a, P>: Iterator {}

#[doc(hidden)]
#[macro_export]
macro_rules! __define_ops_private {
    (type new $ident:ident[$($tt:tt)*]) => {
        #[doc = ::core::concat!("`", ::core::stringify!($($tt)*), "`")]
        //#[derive(Clone, Copy, Debug)] // error: derive can't be used on items with type macros
        #[repr(transparent)]
        pub struct $ident<S: $crate::Span> {
            pub spans: [S; ::core::stringify!($($tt)*).len()]
        }

        impl<S: $crate::Span> ::core::clone::Clone for $ident<S> {
            #[inline]
            fn clone(&self) -> Self {
                *self
            }
        }

        impl<S: $crate::Span> ::core::marker::Copy for $ident<S> {}

        impl<S: $crate::Span> ::core::fmt::Debug for $ident<S> {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                write!(f, "{}({:?})", ::core::stringify!($ident), self.spans)
            }
        }

        impl<S: $crate::SpanExt> $crate::Token<S::TokenTree> for $ident<S> {
            #[inline]
            fn as_any(&self) -> &dyn ::core::any::Any {
                self
            }

            #[inline]
            fn as_any_mut(&mut self) -> &mut dyn ::core::any::Any {
                self
            }

            #[inline]
            fn eq_except_span(&self, other: &dyn $crate::Token<S::TokenTree>) -> bool {
                other.is::<Self>()
            }
        }

        impl<S: $crate::SpanExt> $crate::ToTokens<S::TokenTree> for $ident<S> {
            #[inline]
            fn to_token_trees(&self) -> $crate::TokenTrees<S::TokenTree>{
                $crate::TokenTrees::new(self.puncts().map(|p| S::TokenTree::from(p)))
            }
        }

        impl<S: $crate::SpanExt> $crate::Op<S> for $ident<S> {
            #[inline]
            fn as_str(&self) -> &'static str {
                ::core::stringify!($($tt)*)
            }

            #[inline]
            fn puncts(&self) -> $crate::op::Puncts<S::Punct> {
                $crate::op::Puncts::new(self.as_str(), &self.spans)
            }

            #[inline]
            fn spans(&self) -> &[S] {
                &self.spans
            }

            #[inline]
            fn set_spans(&mut self, spans: &[S]) {
                for (s, span) in self.spans.iter_mut().zip(spans.into_iter().cycle()) {
                    *s = *span;
                }
            }

            #[inline]
            fn span(&self) -> S {
                self.spans[0]
            }

            #[inline]
            fn set_span(&mut self, span: S) {
                for s in self.spans.iter_mut() {
                    *s = span;
                }
            }
        }

        impl<S: $crate::Span> ::core::convert::TryFrom<&str> for $ident<S> {
            type Error = ();

            #[inline]
            fn try_from(value: &str) -> ::core::result::Result<Self, Self::Error> {
                if value == ::core::stringify!($($tt)*) {
                    Ok($ident { spans: [S::call_site(); ::core::stringify!($($tt)*).len()] })
                } else {
                    Err(())
                }
            }
        }
    };

    (type $($tt:tt)*) => {};

    (macro macro $(#[$attr:meta])* $macro:ident $mpath:path, $($ident:ident[$($tt:tt)*],)*) => {
        $(#[$attr])*
        macro_rules! $macro {$(
            ($($tt)*) => { $mpath::$ident };
        )*}
    };

    (macro , $($tt:tt)*) => {};

    (
        fn fn $fn:ident
        $($($first:literal($follow:pat))? $($ident:ident[$($tt:tt)*])?,)*
    ) => {
        /// Returns an operator with span `spans` if `str` is a valid op, unless, if `next` is
        /// `Some`, appending `next` to `str` with joint spacing would also be a valid op.
        ///
        /// `spans` can be one span per char, or just one span for the op to share. If `spans`
        /// doesn't have either length `1` or the same length as `str`, the result is unspecified.
        #[inline]
        pub fn $fn<S: $crate::SpanExt>(str: &str, next: Option<char>, spans: &[S]) -> Option<Box<dyn Op<S>>> {
            match (str, next) {
                $(
                    $(($first, Some($follow)) => None,)?
                    $((::core::stringify!($($tt)*), _) => {
                        let mut spans = spans.into_iter().cycle();
                        let mut s = [S::call_site(); ::core::stringify!($($tt)*).len()];
                        for s in s.iter_mut() {
                            *s = *spans.next().unwrap();
                        }
                        Some(Box::new($ident { spans: s }))
                    })?
                )*
                _ => None,
            }
        }
    };

    (fn $($tt:tt)*) => {};
}

#[macro_export]
macro_rules! define_ops {
    (
        $([$(#[$attr:meta])* $macro:ident! $mpath:path])? $([$fn:ident()])?
        $($($first:literal($follow:pat))? $($ident:ident[$($tt:tt)*] $($new:ident)?)?,)*
    ) => {
        $($( $crate::__define_ops_private!(type $($new)? $ident[$($tt)*]); )?)*
        $crate::__define_ops_private!(macro $(macro $(#[$attr])* $macro $mpath)?, $($($ident[$($tt)*],)?)*);
        $crate::__define_ops_private!(fn $(fn $fn)? $($($first($follow))? $($ident[$($tt)*])?,)*);
    };
}
