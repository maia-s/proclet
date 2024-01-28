use crate::{prelude::*, Punct, PunctExt, Span};
use std::{
    fmt::Display,
    iter::{FusedIterator, Peekable},
    marker::PhantomData,
    mem,
    str::Chars,
};

pub trait Op<S: Span> {
    fn as_str(&self) -> &'static str;
    fn puncts(&self) -> Puncts<S::Punct>;
    fn to_token_stream(&self) -> S::TokenStream;
    fn span(&self) -> S;
    fn set_span(&mut self, span: S);
}

impl<S: Span> PartialEq for dyn Op<S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl<S: Span> Eq for dyn Op<S> {}

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
    puncts: Vec<P>,
    next: Option<P>,
    span: Option<P::Span>,
    make_op: F,
}

impl<P: PunctExt, F: Fn(&str, Option<char>, P::Span) -> Option<Box<dyn Op<P::Span>>>>
    OpParser<P, F>
{
    /// Create a new `OpParser`. `make_op` is used to implement [`OpParser::make_op`]; see that
    /// for information about how `make_op` should work.
    #[inline]
    pub const fn new(make_op: F) -> Self {
        Self {
            str: String::new(),
            next: None,
            puncts: Vec::new(),
            span: None,
            make_op,
        }
    }

    /// Clear the state to remove any accumulated op.
    #[inline]
    pub fn clear(&mut self) {
        self.str.clear();
        self.puncts.clear();
        self.next = None;
    }

    /// Returns an operator with span `span` if `str` is a valid op, unless, if `next` is `Some`,
    /// appending `next` to `str` with joint spacing would also be a valid op. In other words,
    /// this returns an op if that op is the only possible valid op given `str` and `next`.
    ///
    /// For example, if `+` (`Plus`) and `+=` (`PlusEq`) are valid ops and `+-` is invalid:
    ///
    /// - `make_op("+", Some('='), span)` returns `None`
    /// - `make_op("+", Some('-'), span)` returns `Some(Box<Plus>)`
    /// - `make_op("+", None, span)` returns `Some(Box<Plus>)`
    /// - `make_op("+=", None, span)` returns `Some(Box<PlusEq>)`
    /// - `make_op("+-", None, span)` returns `None`
    #[inline]
    pub fn make_op(
        &self,
        str: &str,
        next: Option<char>,
        span: P::Span,
    ) -> Option<Box<dyn Op<P::Span>>> {
        (self.make_op)(str, next, span)
    }

    /// Add a `Punct` to the currently accumulating op. This may return a new [`Op`], or `None`
    /// if more puncts are needed. If `punct` has alone spacing and the accumulated op isn't
    /// valid, it returns an error. Call [`OpParser::finish`] to finish parsing.
    #[inline]
    #[allow(clippy::type_complexity)]
    pub fn apply(&mut self, punct: P) -> Option<Result<Box<dyn Op<P::Span>>, InvalidOpError<P>>> {
        let (mut punct, mut next_ch, span) = if let Some(next) = mem::take(&mut self.next) {
            let next_ch = next.spacing().is_joint().then_some(punct.as_char());
            let span = self.span.unwrap_or_else(|| punct.span());
            self.next = Some(punct);
            (next, next_ch, span)
        } else if punct.spacing().is_alone() {
            let span = self.span.unwrap_or_else(|| punct.span());
            (punct, None, span)
        } else {
            self.next = Some(punct);
            return None;
        };

        loop {
            self.str.push(punct.as_char());
            self.puncts.push(punct);

            if let Some(op) = self.make_op(&self.str, next_ch, span) {
                self.str.clear();
                self.span = None;
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
                self.span = None;
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
            let op = self.make_op(&self.str, None, punct.span());
            self.str.clear();
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
pub struct Puncts<P: Punct>(Peekable<Chars<'static>>, P::Span, PhantomData<fn() -> P>);

impl<P: Punct> Puncts<P> {
    pub fn new(str: &'static str, span: P::Span) -> Self {
        Self(str.chars().peekable(), span, PhantomData)
    }
}

impl<P: PunctExt> Iterator for Puncts<P> {
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
                self.1,
            )
        })
    }
}

impl<P: PunctExt> FusedIterator for Puncts<P> {}

#[doc(hidden)]
#[macro_export]
macro_rules! __define_ops_private {
    (type new $ident:ident[$($tt:tt)*]) => {
        #[doc = ::core::concat!("`", ::core::stringify!($($tt)*), "`")]
        #[derive(Clone, Copy, Debug)]
        #[repr(transparent)]
        pub struct $ident<S: $crate::Span>(S);

        impl<S: $crate::SpanExt> $crate::Op<S> for $ident<S> {
            #[inline]
            fn as_str(&self) -> &'static str {
                ::core::stringify!($($tt)*)
            }

            #[inline]
            fn puncts(&self) -> $crate::op::Puncts<S::Punct> {
                $crate::op::Puncts::new(self.as_str(), self.0)
            }

            #[inline]
            fn to_token_stream(&self) -> S::TokenStream {
                self.puncts().map(S::TokenTree::from).collect()
            }

            #[inline]
            fn span(&self) -> S {
                self.0
            }

            #[inline]
            fn set_span(&mut self, span: S) {
                self.0 = span;
            }
        }

        impl<S: $crate::Span> ::core::convert::TryFrom<&str> for $ident<S> {
            type Error = ();

            #[inline]
            fn try_from(value: &str) -> ::core::result::Result<Self, Self::Error> {
                if value == ::core::stringify!($($tt)*) {
                    Ok($ident(S::call_site()))
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
        /// Returns an operator with span `span` if `str` is a valid op, unless, if `next` is `Some`,
        /// appending `next` to `str` with joint spacing would also be a valid op.
        #[inline]
        pub fn $fn<S: $crate::SpanExt>(str: &str, next: Option<char>, span: S) -> Option<Box<dyn Op<S>>> {
            match (str, next) {
                $(
                    $(($first, Some($follow)) => None,)?
                    $((stringify!($($tt)*), _) => Some(Box::new($ident(span))),)?
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
