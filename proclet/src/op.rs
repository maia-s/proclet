use crate::{
    prelude::*, Match, Punct, PunctExt, Span, SpanExt, ToTokenStream, ToTokens, TokenObject,
    TokenTreeExt,
};
use std::{borrow::Cow, fmt::Display, iter::FusedIterator, marker::PhantomData, mem};

/// Function for OpParser to use to match operators.
pub trait MatchOpFn: Clone + Fn(&str, Option<char>) -> Match<Cow<'static, str>> {}
impl<T> MatchOpFn for T where T: Clone + Fn(&str, Option<char>) -> Match<Cow<'static, str>> {}

/// Convenience function for calling [`Op::new_static`]. It can be used for parsing a specific op.
pub const fn op<S: Span>(op: &'static str) -> Op<S> {
    Op::new_static(op)
}

/// An operator. These can be parsed from tokens, and can also be used as parsers
/// to parse one specific operator.
#[derive(Clone, Debug)]
pub struct Op<S: Span> {
    str: Cow<'static, str>,
    spans: Option<Box<[S]>>,
}

impl<S: Span> Op<S> {
    /// Create a new operator. `str` can't be empty.
    #[inline]
    pub fn new(str: impl Into<Cow<'static, str>>) -> Self {
        let str = str.into();
        assert!(!str.is_empty());
        Self { str, spans: None }
    }

    /// Create a new operator (const). `str` can't be empty.
    #[inline]
    pub const fn new_static(str: &'static str) -> Self {
        assert!(!str.is_empty());
        Self {
            str: Cow::Borrowed(str),
            spans: None,
        }
    }

    /// Create a new operator with one specific span used for the whole operator.
    /// `str` can't be empty.
    #[inline]
    pub fn with_span(str: impl Into<Cow<'static, str>>, span: S) -> Self {
        let str = str.into();
        assert!(!str.is_empty());
        let spans = Some(vec![span; str.chars().count()].into_boxed_slice());
        Self { str, spans }
    }

    /// Create a new operator with a specific span for each character of the op.
    /// The length of `spans` must be the same as the number of characters in `str`.
    /// `str` can't be empty.
    #[inline]
    pub fn with_spans(str: impl Into<Cow<'static, str>>, spans: Vec<S>) -> Self {
        let str = str.into();
        assert!(!str.is_empty());
        let spans = spans.into_boxed_slice();
        assert_eq!(str.chars().count(), spans.len());
        Self {
            str,
            spans: Some(spans),
        }
    }

    /// Get this op as a string.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.str
    }

    /// Get an iterator over the `Punct`s that make up this op.
    #[inline]
    pub fn puncts(&self) -> Puncts<S::Punct> {
        Puncts::new(self.as_str(), self.spans.as_deref())
    }

    #[inline]
    fn alloced_spans(&mut self) -> &mut [S] {
        if self.spans.is_none() {
            self.spans = Some(vec![S::call_site(); self.str.chars().count()].into_boxed_slice());
        }
        self.spans.as_mut().unwrap()
    }

    /// Get the spans of this op, or `None` if the default span is used.
    #[inline]
    pub fn spans(&self) -> Option<&[S]> {
        self.spans.as_ref().map(|s| s as _)
    }

    /// Set the spans of this op. The lengths of `spans` must be the same as the number of
    /// characters in the string representation of this op.
    #[inline]
    pub fn set_spans(&mut self, spans: &[S]) {
        // rust-analyzer incorrectly marks this code as an error without this mut
        #[allow(unused_mut)]
        for (mut s, span) in self.alloced_spans().iter_mut().zip(spans.iter().cycle()) {
            *s = *span;
        }
    }

    /// Get the span of this op. If the op has more than one span, this will currently
    /// return the first span. This may change in the future if proc-macro* exposes the
    /// ability to merge spans.
    #[inline]
    pub fn span(&self) -> Option<S> {
        self.spans.as_ref().map(|s| s[0])
    }

    /// Set a single span for this op.
    #[inline]
    pub fn set_span(&mut self, span: S) {
        // rust-analyzer incorrectly marks this code as an error without this mut
        #[allow(unused_mut)]
        for mut s in self.alloced_spans().iter_mut() {
            *s = span;
        }
    }
}

impl<S: SpanExt> Op<S> {
    /// Split this op using the provided [`OpParser`].
    #[inline]
    pub fn split(
        &self,
        parser: &OpParser<S::Punct, impl MatchOpFn>,
    ) -> ParseOps<S, Puncts<S::Punct>, impl MatchOpFn> {
        parser.parse_ops(self.puncts())
    }
}

impl<T: crate::TokenTreeExt> crate::Parser<T> for Op<T::Span> {
    type Output<'p, 'b> = Op<T::Span> where Self: 'p;

    #[inline]
    fn parse<'p, 'b>(&'p self, buf: &mut &'b crate::TokenBuf<T>) -> Option<Self::Output<'p, 'b>> {
        OpParser::<T::Punct, _>::new(|str, next| {
            if self.str == str {
                Match::Complete(self.str.clone())
            } else if let Some(rest) = self.str.strip_prefix(str) {
                if rest.chars().next() == next {
                    Match::NeedMore
                } else {
                    Match::NoMatch
                }
            } else {
                Match::NoMatch
            }
        })
        .parse(buf)
    }
}

impl<T: TokenTreeExt> ToTokens<T> for Op<T::Span> {
    #[inline]
    fn into_tokens(self) -> impl Iterator<Item = TokenObject<T>> {
        self.into_iter().flat_map(|p| p.into_tokens())
    }
}

impl<S: SpanExt> ToTokenStream<S::TokenStream> for Op<S> {
    #[inline]
    fn extend_token_stream(&self, token_stream: &mut S::TokenStream) {
        token_stream.extend(self.puncts().map(S::TokenTree::from))
    }
}

impl<S: Span> From<&'static str> for Op<S> {
    #[inline]
    fn from(value: &'static str) -> Self {
        Self::new(value)
    }
}

impl<S: SpanExt> crate::Parse<S::TokenTree> for Op<S> {
    /// Generic op parser. This doesn't check against valid ops.
    #[inline]
    fn parse(buf: &mut &crate::TokenBuf<S::TokenTree>) -> Option<Self> {
        let mut str = String::new();
        let mut spans = Vec::new();
        buf.parse_prefix(|token| {
            if let Some(punct) = token.punct() {
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
    type IntoIter = Puncts<'static, S::Punct>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        Puncts::new(self.str, self.spans.map(|s| s.into_vec()))
    }
}

#[derive(Clone)]
pub struct ParseOps<S: SpanExt, I: Iterator<Item = S::Punct>, F: MatchOpFn>(
    I,
    OpParserInstance<S::Punct, F>,
);

impl<S: SpanExt, I: Iterator<Item = S::Punct>, F: MatchOpFn> FusedIterator for ParseOps<S, I, F> {}

impl<S: SpanExt, I: Iterator<Item = S::Punct>, F: MatchOpFn> Iterator for ParseOps<S, I, F> {
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
pub struct Puncts<'a, P: Punct> {
    str: Cow<'a, str>,
    spans: Option<Cow<'a, [P::Span]>>,
    stri: usize,
    spansi: usize,
}

impl<'a, P: Punct> Puncts<'a, P> {
    /// Create a new iterator that will generate `Punct`s from the provided characters and spans.
    /// Every `Punct` except for the last one will have `Joint` spacing, and the last one will be `Alone`.
    /// If `spans` is `None`, the puncts will use the default span.
    #[inline]
    pub fn new(str: impl Into<Cow<'a, str>>, spans: Option<impl Into<Cow<'a, [P::Span]>>>) -> Self {
        Self {
            str: str.into(),
            spans: spans.map(|s| s.into()),
            stri: 0,
            spansi: 0,
        }
    }
}

impl<'a, P: PunctExt> FusedIterator for Puncts<'a, P> where Puncts<'a, P>: Iterator {}

impl<'a, P: PunctExt> Iterator for Puncts<'a, P> {
    type Item = P;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ch) = self.str[self.stri..].chars().next() {
            let span = if let Some(spans) = &self.spans {
                spans[self.spansi]
            } else {
                P::Span::call_site()
            };
            self.stri += ch.len_utf8();
            self.spansi += 1;
            Some(P::with_span(
                ch,
                if self.str.len() > self.stri {
                    P::Spacing::Joint
                } else {
                    P::Spacing::Alone
                },
                span,
            ))
        } else {
            None
        }
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

/// Parser for specific sets of operators. Ops can be parsed via [`Parser`] or manually from `Punct`s.
#[derive(Clone)]
pub struct OpParser<P: PunctExt, F: MatchOpFn>(F, PhantomData<fn() -> P>);

impl<P: PunctExt, F: MatchOpFn> OpParser<P, F> {
    /// Create a new `OpParser` using `match_op` to define the valid operators.
    /// Usually you wouldn't call this directly, but create the `OpParser` using
    /// a function generated by the [`define_ops`] macro instead, like [`rust_op_parser`].
    /// If you want to parse a single Op using `Parser`, use `Op` instead.
    ///
    /// See [`OpParser::match_op`] for details of how `match_op` should work.
    #[inline]
    pub const fn new(match_op: F) -> Self {
        Self(match_op, PhantomData)
    }

    /// Create a new `OpParserInstance` for this `OpParser`. You only need this if you
    /// don't use `Parser::parse` or `parse_ops`.
    #[inline]
    pub fn create(&self) -> OpParserInstance<P, F> {
        OpParserInstance::new(self.0.clone())
    }

    /// Use this `OpParser` to parse operators from an iterator of `Punct`s.
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
    pub fn match_op(&self, str: &str, next: Option<char>) -> Match<Cow<'static, str>> {
        (self.0)(str, next)
    }
}

impl<P: PunctExt, F: MatchOpFn> crate::Parser<P::TokenTree> for OpParser<P, F> {
    type Output<'p, 'b> = Op<P::Span> where Self:'p;

    #[inline]
    fn parse<'p, 'b>(
        &'p self,
        buf: &mut &'b crate::TokenBuf<P::TokenTree>,
    ) -> Option<Self::Output<'p, 'b>> {
        let mut string = String::new();
        let mut spans = Vec::new();
        buf.parse_prefix_next(move |token, next| {
            if let Some(punct) = token.punct() {
                let next = if punct.spacing().is_joint() {
                    next.and_then(|next| next.punct().map(|next| next.as_char()))
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

/// An instance for doing manual parsing of operators.
#[derive(Clone)]
pub struct OpParserInstance<P: PunctExt, F> {
    str: String,
    spans: Vec<P::Span>,
    puncts: Vec<P>,
    next: Option<P>,
    match_op: F,
}

impl<P: PunctExt, F: MatchOpFn> OpParserInstance<P, F> {
    /// Create a new `OpParserInstance`. Usually you'd do this by calling [`OpParser::create`] instead.
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
