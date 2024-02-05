use crate::{prelude::*, Match, Punct, PunctExt, Span, SpanExt, ToTokens, Token, TokenTrees};
use std::{
    fmt::Display,
    iter::{self, FusedIterator, Peekable},
    marker::PhantomData,
    mem, slice,
    str::Chars,
};

pub trait OpParserFn: Clone + Fn(&str, Option<char>) -> Match<&'static str> {}
impl<T> OpParserFn for T where T: Clone + Fn(&str, Option<char>) -> Match<&'static str> {}

#[derive(Clone, Debug)]
pub struct Op<S: Span> {
    str: Box<str>,
    spans: Box<[S]>,
}

impl<S: Span> Op<S> {
    #[inline]
    pub fn new(str: impl Into<String>) -> Self {
        Self::with_span(str, S::call_site())
    }

    #[inline]
    pub fn with_span(str: impl Into<String>, span: S) -> Self {
        let str = str.into().into_boxed_str();
        let spans = vec![span; str.chars().count()].into_boxed_slice();
        Self { str, spans }
    }

    #[inline]
    pub fn with_spans(str: impl Into<String>, spans: Vec<S>) -> Self {
        let str = str.into().into_boxed_str();
        let spans = spans.into_boxed_slice();
        assert_eq!(str.chars().count(), spans.len());
        Self { str, spans }
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.str
    }

    #[inline]
    pub fn puncts(&self) -> Puncts<S::Punct> {
        Puncts::new(self.as_str(), &self.spans)
    }

    #[inline]
    pub const fn spans(&self) -> &[S] {
        &self.spans
    }

    #[inline]
    pub fn set_spans(&mut self, spans: &[S]) {
        // rust-analyzer incorrectly marks this code as an error without this mut
        #[allow(unused_mut)]
        for (mut s, span) in self.spans.iter_mut().zip(spans.iter().cycle()) {
            *s = *span;
        }
    }

    #[inline]
    pub const fn span(&self) -> S {
        self.spans[0]
    }

    #[inline]
    pub fn set_span(&mut self, span: S) {
        // rust-analyzer incorrectly marks this code as an error without this mut
        #[allow(unused_mut)]
        for mut s in self.spans.iter_mut() {
            *s = span;
        }
    }
}

impl<S: SpanExt> Op<S> {
    #[inline]
    pub fn split(
        &self,
        parser: &OpParser<S::Punct, impl OpParserFn>,
    ) -> ParseOps<S, Puncts<S::Punct>, impl OpParserFn> {
        parser.parse_ops(self.puncts())
    }
}

impl<S: SpanExt> Token<S::TokenTree> for Op<S> {
    #[inline]
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    #[inline]
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    #[inline]
    fn eq_except_span(&self, other: &dyn Token<S::TokenTree>) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self.as_str() == other.as_str())
            .unwrap_or(false)
    }
}

impl<S: SpanExt> ToTokens<S::TokenTree> for Op<S> {
    #[inline]
    fn to_token_trees(&self) -> TokenTrees<S::TokenTree> {
        TokenTrees::new(self.puncts().map(S::TokenTree::from))
    }
}

impl<S: Span> From<&'static str> for Op<S> {
    #[inline]
    fn from(value: &'static str) -> Self {
        Self::new(value)
    }
}

#[cfg(feature = "token-buffer")]
impl<S: SpanExt> crate::Parse<S::TokenTree> for Op<S> {
    /// Generic op parser. This doesn't check against valid ops.
    #[inline]
    fn parse(buf: &mut &crate::TokenBuf<S::TokenTree>) -> Option<Self> {
        let mut str = String::new();
        let mut spans = Vec::new();
        buf.match_prefix(|token| {
            if let Some(punct) = token.downcast_ref::<S::Punct>() {
                str.push(punct.as_char());
                spans.push(punct.span());
                if punct.spacing().is_joint() {
                    Match::Partial((str.len(), spans.len()))
                } else {
                    Match::Complete((str.len(), spans.len()))
                }
            } else {
                Match::NoMatch
            }
        })
        .map(|(strlen, spanslen)| {
            str.truncate(strlen);
            spans.truncate(spanslen);
            Op::with_spans(str, spans)
        })
    }
}

impl<S: SpanExt> IntoIterator for Op<S> {
    type Item = S::Punct;
    type IntoIter = OpIntoIter<S>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        OpIntoIter {
            op: self,
            ci: 0,
            si: 0,
        }
    }
}

#[derive(Clone)]
pub struct OpIntoIter<S: Span> {
    op: Op<S>,
    ci: usize,
    si: usize,
}

impl<S: SpanExt> FusedIterator for OpIntoIter<S> {}

impl<S: SpanExt> Iterator for OpIntoIter<S> {
    type Item = S::Punct;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.op.str[self.ci..].chars().next().map(|ch| {
            let span = self.op.spans[self.si];
            self.ci += ch.len_utf8();
            self.si += 1;
            S::Punct::with_span(
                ch,
                if self.ci < self.op.str.len() {
                    S::Spacing::Joint
                } else {
                    S::Spacing::Alone
                },
                span,
            )
        })
    }
}

#[derive(Clone)]
pub struct ParseOps<S: SpanExt, I: Iterator<Item = S::Punct>, F: OpParserFn>(
    I,
    OpParserInstance<S::Punct, F>,
);

impl<S: SpanExt, I: Iterator<Item = S::Punct>, F: OpParserFn> FusedIterator for ParseOps<S, I, F> {}

impl<S: SpanExt, I: Iterator<Item = S::Punct>, F: OpParserFn> Iterator for ParseOps<S, I, F> {
    type Item = Result<Op<S>, InvalidOpError<S::Punct>>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        for punct in self.0.by_ref() {
            if let Some(x) = self.1.apply(punct) {
                return Some(x);
            }
        }
        self.1.finish()
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
    #[inline]
    pub fn new(str: &'a str, span: &'a [P::Span]) -> Self {
        Self(
            str.chars().peekable(),
            span.iter().copied().cycle(),
            PhantomData,
        )
    }
}

impl<'a, P: PunctExt> FusedIterator for Puncts<'a, P> where Puncts<'a, P>: Iterator {}

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
pub struct OpParser<P: PunctExt, F: OpParserFn>(F, PhantomData<fn() -> P>);

impl<P: PunctExt, F: OpParserFn> OpParser<P, F> {
    #[inline]
    pub fn new(f: F) -> Self {
        Self(f, PhantomData)
    }

    #[inline]
    pub fn create(&self) -> OpParserInstance<P, F> {
        OpParserInstance::new(self.0.clone())
    }

    #[inline]
    pub fn parse_ops<I: Iterator<Item = P>>(&self, puncts: I) -> ParseOps<P::Span, I, F> {
        ParseOps(puncts, self.create())
    }

    /// Check if `str` is a valid op.
    ///
    /// For example, if `+` and `+=` are valid ops and `+-` is invalid:
    ///
    /// - `match_op("+", Some('='))` returns `Match::Partial("+")`
    /// - `match_op("+", Some('-'))` returns `Match::Complete("+")`
    /// - `match_op("+", None)` returns `Match::Complete("+")`
    /// - `match_op("+=", None)` returns `Match::Complete("+=")`
    /// - `match_op("+-", None)` returns `Match::None`
    ///
    /// If `-=` is a valid op, and `-` and `-+` are invalid:
    ///
    /// - `match_op("-", Some('='))` returns `Match::NeedMore`
    /// - `match_op("-", Some('+'))` returns `Match::None`
    /// - `match_op("-", None)` returns `Match::None`
    /// - `match_op("-=", None)` returns `Match::Complete("-=")`
    #[inline]
    pub fn match_op(&self, str: &str, next: Option<char>) -> Match<&'static str> {
        (self.0)(str, next)
    }
}

#[cfg(feature = "token-buffer")]
impl<P: PunctExt, F: OpParserFn> crate::Parser<P::TokenTree> for OpParser<P, F> {
    type Output<'p, 'b> = Op<P::Span> where Self:'p;

    #[inline]
    fn parse<'p, 'b>(
        &'p self,
        buf: &mut &'b crate::TokenBuf<P::TokenTree>,
    ) -> Option<Self::Output<'p, 'b>> {
        use std::ops::Deref;
        let mut string = String::new();
        let mut spans = Vec::new();
        buf.match_prefix_buf(move |buf, next| {
            let token = buf[buf.len() - 1].deref();
            if let Some(punct) = token.downcast_ref::<P::Punct>() {
                let next = if punct.spacing().is_joint() {
                    next.and_then(|next| next.downcast_ref::<P::Punct>().map(|next| next.as_char()))
                } else {
                    None
                };
                string.push(punct.as_char());
                spans.push(punct.span());

                match self.match_op(&string, next) {
                    Match::Complete(str) => {
                        string.clear();
                        let op = Op::with_spans(str, mem::take(&mut spans));
                        Match::Complete(op)
                    }
                    Match::Partial(_) | Match::NeedMore => Match::NeedMore,
                    Match::NoMatch => Match::NoMatch,
                }
            } else {
                Match::NoMatch
            }
        })
    }
}

#[derive(Clone)]
pub struct OpParserInstance<P: PunctExt, F> {
    str: String,
    spans: Vec<P::Span>,
    puncts: Vec<P>,
    next: Option<P>,
    match_op: F,
}

impl<P: PunctExt, F: OpParserFn> OpParserInstance<P, F> {
    /// Create a new `OpParser`. `make_op` is used to implement [`OpParser::make_op`]; see that
    /// for information about how `make_op` should work.
    #[inline]
    const fn new(match_op: F) -> Self {
        Self {
            str: String::new(),
            spans: Vec::new(),
            puncts: Vec::new(),
            next: None,
            match_op,
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

    /// Add a `Punct` to the currently accumulating op. This may return a new [`Op`], or `None`
    /// if more puncts are needed. If `punct` has alone spacing and the accumulated op isn't
    /// valid, it returns an error. Call [`OpParserInstance::finish`] to finish parsing.
    #[inline]
    #[allow(clippy::type_complexity)]
    pub fn apply(&mut self, punct: P) -> Option<Result<Op<P::Span>, InvalidOpError<P>>> {
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

            match (self.match_op)(&self.str, next_ch) {
                Match::Complete(str) => {
                    self.str.clear();
                    self.puncts.clear();
                    return Some(Ok(Op::with_spans(str, mem::take(&mut self.spans))));
                }
                Match::Partial(_) | Match::NeedMore => {
                    if self.next.as_ref().unwrap().spacing().is_alone() {
                        punct = mem::take(&mut self.next).unwrap();
                        next_ch = None;
                        continue;
                    } else {
                        return None;
                    }
                }
                Match::NoMatch => {
                    self.str.clear();
                    self.spans.clear();
                    return Some(Err(InvalidOpError(mem::take(&mut self.puncts))));
                }
            }
        }
    }

    /// Finish parsing the currently accumulated op.
    #[inline]
    #[allow(clippy::type_complexity)]
    pub fn finish(&mut self) -> Option<Result<Op<P::Span>, InvalidOpError<P>>> {
        mem::take(&mut self.next).map(|punct| {
            self.str.push(punct.as_char());
            let m = (self.match_op)(&self.str, None);
            self.str.clear();
            if let Match::Complete(str) | Match::Partial(str) = m {
                self.spans.push(punct.span());
                self.puncts.clear();
                Ok(Op::with_spans(str, mem::take(&mut self.spans)))
            } else {
                self.puncts.push(punct);
                Err(InvalidOpError(mem::take(&mut self.puncts)))
            }
        })
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __define_ops_private {
    (type new $ident:ident[$($tt:tt)*]) => {
        #[doc = ::core::concat!("`", ::core::stringify!($($tt)*), "`")]
        #[allow(non_snake_case)]
        pub fn $ident<S: $crate::Span>() -> $crate::Op<S> {
            $crate::Op::new(::core::stringify!($($tt)*))
        }
    };

    (type $($tt:tt)*) => {};

    (macro macro $(#[$attr:meta])* $macro:ident, $($ident:ident[$($tt:tt)*],)*) => {
        $(#[$attr])*
        macro_rules! $macro {$(
            ($($tt)*) => { $crate::Op::new(::core::stringify!($($tt)*)) };
        )*}
    };

    (macro , $($tt:tt)*) => {};

    (
        fn fn $fn:ident
        $($($first:literal($follow:pat))? $($ident:ident[$($tt:tt)*])?,)*
    ) => {
        #[inline]
        pub fn $fn<P: $crate::PunctExt>() -> $crate::OpParser<
            P,
            impl ::core::clone::Clone + ::core::ops::Fn(
                &::core::primitive::str,
                ::core::option::Option<::core::primitive::char>
            ) -> $crate::Match<&'static ::core::primitive::str>
        > {
            fn $fn(
                str: &::core::primitive::str,
                next: ::core::option::Option<::core::primitive::char>
            ) -> $crate::Match<&'static ::core::primitive::str> {
                match (str, next) {
                    $(
                        $(($first, ::core::option::Option::Some($follow)) => {
                            if let $crate::Match::Complete(m) = $fn(str, ::core::option::Option::None) {
                                $crate::Match::Partial(m)
                            } else {
                                $crate::Match::NeedMore
                            }
                        })?
                        $((::core::stringify!($($tt)*), _) => $crate::Match::Complete(::core::stringify!($($tt)*)),)?
                    )*
                    _ => $crate::Match::NoMatch,
                }
            }
            $crate::OpParser::new($fn)
        }
    };

    (fn $($tt:tt)*) => {};
}

#[macro_export]
macro_rules! define_ops {
    (
        $([$(#[$attr:meta])* $macro:ident!])? $([$fn:ident()])?
        $($($first:literal($follow:pat))? $($ident:ident[$($tt:tt)*] $($new:ident)?)?,)*
    ) => {
        $($( $crate::__define_ops_private!(type $($new)? $ident[$($tt)*]); )?)*
        $crate::__define_ops_private!(macro $(macro $(#[$attr])* $macro)?, $($($ident[$($tt)*],)?)*);
        $crate::__define_ops_private!(fn $(fn $fn)? $($($first($follow))? $($ident[$($tt)*])?,)*);
    };
}
