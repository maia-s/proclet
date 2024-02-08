use crate::{Match, PMExt, ToTokenStream, Token, TokenStream, TokenTreeExt, PM};
use std::{
    borrow::{Borrow, BorrowMut},
    marker::PhantomData,
    mem::transmute,
    ops::{
        Deref, DerefMut, Index, IndexMut, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo,
        RangeToInclusive,
    },
    slice,
};

/// Parse from a `TokenBuf`.
pub trait Parse<T: PM>: Sized + DefaultParser<T, Parser = DefaultParserImpl<T, Self>> {
    /// Parse an object from a `TokenBuf`.
    fn parse(buf: &mut &TokenBuf<T>) -> Option<Self>;

    /// Parse an object from a `TokenBuf`, but return `None` if there was
    /// more data in the buffer after parsing.
    #[inline]
    fn parse_all(buf: &mut &TokenBuf<T>) -> Option<Self> {
        match Self::parse(buf) {
            Some(result) if buf.is_empty() => Some(result),
            _ => None,
        }
    }
}

/// A parser for parsing objects from a `TokenBuf`.
pub trait Parser<T: PM>: Sized {
    /// The output type of this parser.
    type Output<'p, 'b>
    where
        Self: 'p;

    /// Parse an object from a `TokenBuf` using this parser.
    fn parse<'p, 'b>(&'p self, buf: &mut &'b TokenBuf<T>) -> Option<Self::Output<'p, 'b>>;

    /// Parse an object from a `TokenBuf` using this parser, but return `None` if there was
    /// more data in the buffer after parsing.
    #[inline]
    fn parse_all<'p, 'b>(&'p self, buf: &mut &'b TokenBuf<T>) -> Option<Self::Output<'p, 'b>> {
        match self.parse(buf) {
            Some(result) if buf.is_empty() => Some(result),
            _ => None,
        }
    }
}

/// Trait for making a default parser. This is automatically implemented for objects
/// that implement the `Parse` trait.
pub trait DefaultParser<T: PM> {
    /// The parser that will be created.
    type Parser: Parser<T> + Copy + Default;

    /// Create a new parser.
    #[inline(always)]
    fn parser() -> Self::Parser {
        Self::Parser::default()
    }
}

impl<T: PM, X: Parse<T>> DefaultParser<T> for X {
    type Parser = DefaultParserImpl<T, X>;
}

pub struct DefaultParserImpl<T: PM, X: Parse<T>>(PhantomData<fn() -> (T, X)>);

impl<T: PM, X: Parse<T>> Clone for DefaultParserImpl<T, X> {
    #[inline(always)]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: PM, X: Parse<T>> Copy for DefaultParserImpl<T, X> {}

impl<T: PM, X: Parse<T>> Default for DefaultParserImpl<T, X> {
    #[inline(always)]
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<T: PM, X: Parse<T>> Parser<T> for DefaultParserImpl<T, X> {
    type Output<'p, 'b> = X where Self: 'p;

    #[inline]
    fn parse<'p, 'b>(&'p self, buf: &mut &'b TokenBuf<T>) -> Option<Self::Output<'p, 'b>> {
        X::parse(buf)
    }
}

/// An owned buffer of tokens.
#[derive(Debug, Default)]
pub struct TokenBuffer<T: PM>(Vec<Box<dyn Token<T>>>);

impl<T: PM> TokenBuffer<T> {
    /// Get this buffer as a `&TokenBuf`.
    #[inline]
    pub fn as_buf(&self) -> &TokenBuf<T> {
        self
    }

    /// Get this buffer as a `&mut TokenBuf`.
    #[inline]
    pub fn as_buf_mut(&mut self) -> &mut TokenBuf<T> {
        self
    }
}

impl<T: PM> TokenBuffer<T> {
    /// Create a new `TokenBuffer`.
    #[inline]
    pub const fn new() -> Self {
        Self(Vec::new())
    }
}

impl<T: PMExt> TokenBuffer<T> {
    /// Create a new `TokenBuffer` from a `TokenStream`.
    #[inline]
    pub fn from_token_stream(ts: T::TokenStream) -> Self {
        ts.into_iter()
            .map(|mut t| {
                t.flatten_group();
                t
            })
            .collect()
    }
}

impl<T: PM> AsRef<TokenBuf<T>> for TokenBuffer<T> {
    #[inline]
    fn as_ref(&self) -> &TokenBuf<T> {
        self.as_buf()
    }
}

impl<T: PM> AsMut<TokenBuf<T>> for TokenBuffer<T> {
    #[inline]
    fn as_mut(&mut self) -> &mut TokenBuf<T> {
        self.as_buf_mut()
    }
}

impl<T: PM> Borrow<TokenBuf<T>> for TokenBuffer<T> {
    #[inline]
    fn borrow(&self) -> &TokenBuf<T> {
        self.as_buf()
    }
}

impl<T: PM> BorrowMut<TokenBuf<T>> for TokenBuffer<T> {
    #[inline]
    fn borrow_mut(&mut self) -> &mut TokenBuf<T> {
        self.as_buf_mut()
    }
}

impl<T: PM> Clone for TokenBuffer<T> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.iter().map(|i| i.clone_boxed()).collect())
    }
}

impl<T: PM> Deref for TokenBuffer<T> {
    type Target = TokenBuf<T>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        TokenBuf::from_ref(&self.0[..])
    }
}

impl<T: PM> DerefMut for TokenBuffer<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        TokenBuf::from_mut(&mut self.0[..])
    }
}

impl<T: PM, X: Into<Box<dyn Token<T>>>> Extend<X> for TokenBuffer<T> {
    #[inline]
    fn extend<I: IntoIterator<Item = X>>(&mut self, iter: I) {
        self.0.extend(iter.into_iter().map(|x| x.into()));
    }
}

#[cfg(feature = "proc-macro")]
impl From<proc_macro::TokenStream> for TokenBuffer<crate::PM1> {
    #[inline]
    fn from(value: proc_macro::TokenStream) -> Self {
        Self::from_token_stream(value)
    }
}

#[cfg(feature = "proc-macro2")]
impl From<proc_macro2::TokenStream> for TokenBuffer<crate::PM2> {
    #[inline]
    fn from(value: proc_macro2::TokenStream) -> Self {
        Self::from_token_stream(value)
    }
}

#[cfg(feature = "proc-macro")]
impl From<TokenBuffer<crate::PM1>> for proc_macro::TokenStream {
    #[inline]
    fn from(value: TokenBuffer<crate::PM1>) -> Self {
        value.to_token_stream()
    }
}

#[cfg(feature = "proc-macro2")]
impl From<TokenBuffer<crate::PM2>> for proc_macro2::TokenStream {
    #[inline]
    fn from(value: TokenBuffer<crate::PM2>) -> Self {
        value.to_token_stream()
    }
}

impl<T: PM, X: Into<Box<dyn Token<T>>>> FromIterator<X> for TokenBuffer<T> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = X>>(iter: I) -> Self {
        Self(iter.into_iter().map(|x| x.into()).collect())
    }
}

impl<T: PM, I: TokenBufferIndex<T>> Index<I> for TokenBuffer<T> {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        index.index(&self.0)
    }
}

impl<T: PM, I: TokenBufferIndex<T>> IndexMut<I> for TokenBuffer<T> {
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        index.index_mut(&mut self.0)
    }
}

impl<T: PM> IntoIterator for TokenBuffer<T> {
    type IntoIter = <Vec<Box<dyn Token<T>>> as IntoIterator>::IntoIter;
    type Item = Box<dyn Token<T>>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// Borrowed version of [`TokenBuffer`].
#[derive(Debug)]
#[repr(transparent)]
pub struct TokenBuf<T: PM>([Box<dyn Token<T>>]);

impl<T: PM> TokenBuf<T> {
    #[inline]
    fn from_ref(r: &[Box<dyn Token<T>>]) -> &Self {
        unsafe {
            // # Safety
            // It's a reference to the same type in a transparent struct
            transmute::<&[Box<dyn Token<T>>], &Self>(r)
        }
    }

    #[inline]
    fn from_mut(r: &mut [Box<dyn Token<T>>]) -> &mut Self {
        unsafe {
            // # Safety
            // It's a reference to the same type in a transparent struct
            transmute::<&mut [Box<dyn Token<T>>], &mut Self>(r)
        }
    }

    /// Parse a value from this buffer.
    ///
    /// The referenced `&self` will be modified to point past the parsed tokens on success.
    #[inline]
    pub fn parse<P: Parse<T>>(self: &mut &Self) -> Option<P> {
        P::parse(self)
    }

    /// Parse a value from this buffer, but return `None` if there's data left in the buffer afterwards.
    ///
    /// The referenced `&self` will be modified to point past the parsed tokens on success.
    #[inline]
    pub fn parse_all<P: Parse<T>>(self: &mut &Self) -> Option<P> {
        P::parse_all(self)
    }

    /// Parse a prefix from this buffer. `match_fn` is called for each token in the buffer
    /// starting from the beginning. Parsing stops if `match_fn` returns `Match::Complete`
    /// or `Match::NoMatch`, or if the buffer runs out of tokens.
    ///
    /// The referenced `&self` will be modified to point past the parsed tokens on success.
    #[inline]
    pub fn parse_prefix<'a, M: 'a>(
        self: &mut &'a Self,
        mut match_fn: impl FnMut(&dyn Token<T>) -> Match<M>,
    ) -> Option<M> {
        self.parse_prefix_buf(|_, token, _| match_fn(token))
    }

    /// Parse a prefix from this buffer. `match_fn` is called for each token in the buffer
    /// starting from the beginning. Parsing stops if `match_fn` returns `Match::Complete`
    /// or `Match::NoMatch`, or if the buffer runs out of tokens.
    ///
    /// `match_fn` is called with the token at the current position and the next token, if any.
    ///
    /// The referenced `&self` will be modified to point past the parsed tokens on success.
    #[inline]
    pub fn parse_prefix_next<'a, M: 'a>(
        self: &mut &'a Self,
        mut match_fn: impl FnMut(&dyn Token<T>, Option<&dyn Token<T>>) -> Match<M>,
    ) -> Option<M> {
        self.parse_prefix_buf(|_, token, next| match_fn(token, next))
    }

    /// Parse a prefix from this buffer. `match_fn` is called for each token in the buffer
    /// starting from the beginning. Parsing stops if `match_fn` returns `Match::Complete`
    /// or `Match::NoMatch`, or if the buffer runs out of tokens.
    ///
    /// `match_fn` is called with the currently matched buffer including the current token,
    /// the token at the current position, and the next token, if any.
    ///
    /// The referenced `&self` will be modified to point past the parsed tokens on success.
    #[inline]
    pub fn parse_prefix_buf<'a, M: 'a>(
        self: &mut &'a Self,
        mut match_fn: impl FnMut(&'a Self, &dyn Token<T>, Option<&dyn Token<T>>) -> Match<M>,
    ) -> Option<M> {
        let mut result = None;
        for i in 1..=self.len() {
            match match_fn(
                &self[..i],
                self[i - 1].deref(),
                self.get(i).map(|i| i.deref()),
            ) {
                Match::Complete(m) => {
                    *self = &self[i..];
                    return Some(m);
                }
                Match::Partial(m) => result = Some((m, &self[i..])),
                Match::NeedMore => (),
                Match::NoMatch => break,
            }
        }
        result.map(|(result, rest)| {
            *self = rest;
            result
        })
    }

    /// Parse a specific set of tokens from this buffer. All of the tokens in the provided
    /// iterator must match, or the buffer won't advance and this will return `None`.
    ///
    /// The referenced `&self` will be modified to point past the parsed tokens on success.
    #[inline]
    pub fn parse_prefix_tokens<'a>(
        self: &mut &'a Self,
        tokens: impl IntoIterator<Item = impl AsRef<dyn Token<T>>>,
    ) -> Option<&'a Self> {
        let mut tokens = tokens.into_iter().peekable();
        self.parse_prefix_buf(move |buf, token, _| {
            if let Some(t) = tokens.next() {
                if token.eq_except_span(t.as_ref()) {
                    if tokens.peek().is_some() {
                        Match::NeedMore
                    } else {
                        Match::Complete(buf)
                    }
                } else {
                    Match::NoMatch
                }
            } else {
                // empty input
                Match::Complete(&buf[..0])
            }
        })
    }

    /// Parse a specific set of tokens from this buffer. At least one of the tokens in the provided
    /// iterator must match, or the buffer won't advance and this will return `None`.
    ///
    /// The referenced `&self` will be modified to point past the parsed tokens on success.
    #[inline]
    pub fn parse_prefix_tokens_partial(
        self: &mut &Self,
        tokens: impl IntoIterator<Item = impl AsRef<dyn Token<T>>>,
    ) -> Option<&Self> {
        let mut tokens = tokens.into_iter().peekable();
        self.parse_prefix_buf(move |buf, token, _| {
            if let Some(t) = tokens.next() {
                if token.eq_except_span(t.as_ref()) {
                    if tokens.peek().is_some() {
                        Match::Partial(buf)
                    } else {
                        Match::Complete(buf)
                    }
                } else {
                    Match::NoMatch
                }
            } else {
                // empty input
                Match::Complete(&buf[..0])
            }
        })
    }

    /// Parse a suffix from this buffer. `match_fn` is called for each token in the buffer
    /// starting from the end. Parsing stops if `match_fn` returns `Match::Complete`
    /// or `Match::NoMatch`, or if the buffer runs out of tokens.
    ///
    /// The referenced `&self` will be modified to end before the parsed tokens on success.
    #[inline]
    pub fn parse_suffix<'a, M: 'a>(
        self: &mut &'a Self,
        mut match_fn: impl FnMut(&dyn Token<T>) -> Match<M>,
    ) -> Option<M> {
        self.parse_suffix_buf(|_, token, _| match_fn(token))
    }

    /// Parse a suffix from this buffer. `match_fn` is called for each token in the buffer
    /// starting from the end. Parsing stops if `match_fn` returns `Match::Complete`
    /// or `Match::NoMatch`, or if the buffer runs out of tokens.
    ///
    /// `match_fn` is called with the token at the current position and the preceding token, if any.
    ///
    /// The referenced `&self` will be modified to end before the parsed tokens on success.
    #[inline]
    pub fn parse_suffix_next<'a, M: 'a>(
        self: &mut &'a Self,
        mut match_fn: impl FnMut(&dyn Token<T>, Option<&dyn Token<T>>) -> Match<M>,
    ) -> Option<M> {
        self.parse_suffix_buf(|_, token, next| match_fn(token, next))
    }

    /// Parse a suffix from this buffer. `match_fn` is called for each token in the buffer
    /// starting from the end. Parsing stops if `match_fn` returns `Match::Complete`
    /// or `Match::NoMatch`, or if the buffer runs out of tokens.
    ///
    /// `match_fn` is called with the currently matched buffer including the current token,
    /// the token at the current position, and the preceding token, if any.
    ///
    /// The referenced `&self` will be modified to end before the parsed tokens on success.
    #[inline]
    pub fn parse_suffix_buf<'a, M: 'a>(
        self: &mut &'a Self,
        mut match_fn: impl FnMut(&'a Self, &dyn Token<T>, Option<&dyn Token<T>>) -> Match<M>,
    ) -> Option<M> {
        let mut result = None;
        for i in (0..self.len()).rev() {
            match match_fn(
                &self[i..],
                self[i].deref(),
                (i > 0).then(|| self[i - 1].deref()),
            ) {
                Match::Complete(m) => {
                    *self = &self[..i];
                    return Some(m);
                }
                Match::Partial(m) => result = Some((m, &self[..i])),
                Match::NeedMore => (),
                Match::NoMatch => break,
            }
        }
        result.map(|(result, rest)| {
            *self = rest;
            result
        })
    }

    /// Parse a specific set of tokens from this buffer. All of the tokens in the provided
    /// iterator must match, or the buffer won't advance and this will return `None`.
    ///
    /// The matching starts from the end of the buffer against the end of the iterator
    /// (i.e. this will match the iterator's sequence of tokens at the end of the
    /// buffer in forwards order)
    ///
    /// The referenced `&self` will be modified to end before the parsed tokens on success.
    #[inline]
    pub fn parse_suffix_tokens(
        self: &mut &Self,
        tokens: impl IntoIterator<IntoIter = impl DoubleEndedIterator<Item = impl AsRef<dyn Token<T>>>>,
    ) -> Option<&Self> {
        self.parse_suffix_tokens_reverse(tokens.into_iter().rev())
    }

    /// Parse a specific set of tokens from this buffer. All of the tokens in the provided
    /// iterator must match, or the buffer won't advance and this will return `None`.
    ///
    /// The matching starts from the end of the buffer against the start of the iterator
    /// (i.e. this will match the iterator's sequence of tokens at the end of the
    /// buffer in reverse order)
    ///
    /// The referenced `&self` will be modified to end before the parsed tokens on success.
    #[inline]
    pub fn parse_suffix_tokens_reverse(
        self: &mut &Self,
        tokens: impl IntoIterator<Item = impl AsRef<dyn Token<T>>>,
    ) -> Option<&Self> {
        let mut tokens = tokens.into_iter().peekable();
        self.parse_suffix_buf(move |buf, token, _| {
            if let Some(t) = tokens.next() {
                if token.eq_except_span(t.as_ref()) {
                    if tokens.peek().is_some() {
                        Match::NeedMore
                    } else {
                        Match::Complete(buf)
                    }
                } else {
                    Match::NoMatch
                }
            } else {
                // empty input
                Match::Complete(&buf[..0])
            }
        })
    }

    /// Parse a specific set of tokens from this buffer. At least one of the tokens in the provided
    /// iterator must match, or the buffer won't advance and this will return `None`.
    ///
    /// The matching starts from the end of the buffer against the start of the iterator
    /// (i.e. this will match the iterator's sequence of tokens at the end of the
    /// buffer in reverse order)
    ///
    /// The referenced `&self` will be modified to end before the parsed tokens on success.
    #[inline]
    pub fn parse_suffix_tokens_partial(
        self: &mut &Self,
        tokens: impl IntoIterator<IntoIter = impl DoubleEndedIterator<Item = impl AsRef<dyn Token<T>>>>,
    ) -> Option<&Self> {
        self.parse_suffix_tokens_reverse_partial(tokens.into_iter().rev())
    }

    /// Parse a specific set of tokens from this buffer. At least one of the tokens in the provided
    /// iterator must match, or the buffer won't advance and this will return `None`.
    ///
    /// The matching starts from the end of the buffer against the start of the iterator
    /// (i.e. this will match the iterator's sequence of tokens at the end of the
    /// buffer in reverse order)
    ///
    /// The referenced `&self` will be modified to end before the parsed tokens on success.
    #[inline]
    pub fn parse_suffix_tokens_reverse_partial(
        self: &mut &Self,
        tokens: impl IntoIterator<Item = impl AsRef<dyn Token<T>>>,
    ) -> Option<&Self> {
        let mut tokens = tokens.into_iter().peekable();
        self.parse_suffix_buf(move |buf, token, _| {
            if let Some(t) = tokens.next() {
                if token.eq_except_span(t.as_ref()) {
                    if tokens.peek().is_some() {
                        Match::Partial(buf)
                    } else {
                        Match::Complete(buf)
                    }
                } else {
                    Match::NoMatch
                }
            } else {
                // empty input
                Match::Complete(&buf[..0])
            }
        })
    }
}

impl<T: PM> Deref for TokenBuf<T> {
    type Target = [Box<dyn Token<T>>];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: PM> DerefMut for TokenBuf<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg(feature = "proc-macro")]
impl From<&TokenBuf<crate::PM1>> for proc_macro::TokenStream {
    #[inline]
    fn from(value: &TokenBuf<crate::PM1>) -> Self {
        value.to_token_stream()
    }
}

#[cfg(feature = "proc-macro")]
impl From<&mut TokenBuf<crate::PM1>> for proc_macro::TokenStream {
    #[inline]
    fn from(value: &mut TokenBuf<crate::PM1>) -> Self {
        value.to_token_stream()
    }
}

#[cfg(feature = "proc-macro2")]
impl From<&TokenBuf<crate::PM2>> for proc_macro2::TokenStream {
    #[inline]
    fn from(value: &TokenBuf<crate::PM2>) -> Self {
        value.to_token_stream()
    }
}

#[cfg(feature = "proc-macro2")]
impl From<&mut TokenBuf<crate::PM2>> for proc_macro2::TokenStream {
    #[inline]
    fn from(value: &mut TokenBuf<crate::PM2>) -> Self {
        value.to_token_stream()
    }
}

impl<T: PM, I: TokenBufferIndex<T>> Index<I> for TokenBuf<T> {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        index.index(&self.0)
    }
}

impl<T: PM, I: TokenBufferIndex<T>> IndexMut<I> for TokenBuf<T> {
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        index.index_mut(&mut self.0)
    }
}

impl<'a, T: PM> IntoIterator for &'a TokenBuf<T> {
    type IntoIter = slice::Iter<'a, Box<dyn Token<T>>>;
    type Item = &'a Box<dyn Token<T>>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, T: PM> IntoIterator for &'a mut TokenBuf<T> {
    type IntoIter = slice::IterMut<'a, Box<dyn Token<T>>>;
    type Item = &'a mut Box<dyn Token<T>>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl<T: PM> ToOwned for TokenBuf<T> {
    type Owned = TokenBuffer<T>;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        TokenBuffer(self.0.iter().map(|i| i.clone_boxed()).collect())
    }
}

impl<T: TokenStream<TokenStream = T>> ToTokenStream<T> for TokenBuf<T::PM> {
    #[inline]
    fn extend_token_stream(&self, token_stream: &mut T) {
        for i in self.0.iter() {
            i.extend_token_stream(token_stream);
        }
    }
}

pub trait TokenBufferIndex<T: PM> {
    type Output: ?Sized;
    fn index(self, slice: &[Box<dyn Token<T>>]) -> &Self::Output;
    fn index_mut(self, slice: &mut [Box<dyn Token<T>>]) -> &mut Self::Output;
}

impl<T: PM> TokenBufferIndex<T> for usize {
    type Output = Box<dyn Token<T>>;

    #[inline]
    fn index(self, slice: &[Box<dyn Token<T>>]) -> &Self::Output {
        &slice[self]
    }

    #[inline]
    fn index_mut(self, slice: &mut [Box<dyn Token<T>>]) -> &mut Self::Output {
        &mut slice[self]
    }
}

impl<T: PM> TokenBufferIndex<T> for Range<usize> {
    type Output = TokenBuf<T>;

    #[inline]
    fn index(self, slice: &[Box<dyn Token<T>>]) -> &Self::Output {
        TokenBuf::from_ref(&slice[self.start..self.end])
    }

    #[inline]
    fn index_mut(self, slice: &mut [Box<dyn Token<T>>]) -> &mut Self::Output {
        TokenBuf::from_mut(&mut slice[self.start..self.end])
    }
}

impl<T: PM> TokenBufferIndex<T> for RangeFrom<usize> {
    type Output = TokenBuf<T>;

    #[inline]
    fn index(self, slice: &[Box<dyn Token<T>>]) -> &Self::Output {
        TokenBuf::from_ref(&slice[self.start..])
    }

    #[inline]
    fn index_mut(self, slice: &mut [Box<dyn Token<T>>]) -> &mut Self::Output {
        TokenBuf::from_mut(&mut slice[self.start..])
    }
}

impl<T: PM> TokenBufferIndex<T> for RangeFull {
    type Output = TokenBuf<T>;

    #[inline]
    fn index(self, slice: &[Box<dyn Token<T>>]) -> &Self::Output {
        TokenBuf::from_ref(slice)
    }

    #[inline]
    fn index_mut(self, slice: &mut [Box<dyn Token<T>>]) -> &mut Self::Output {
        TokenBuf::from_mut(slice)
    }
}

impl<T: PM> TokenBufferIndex<T> for RangeInclusive<usize> {
    type Output = TokenBuf<T>;

    #[inline]
    fn index(self, slice: &[Box<dyn Token<T>>]) -> &Self::Output {
        TokenBuf::from_ref(&slice[*self.start()..=*self.end()])
    }

    #[inline]
    fn index_mut(self, slice: &mut [Box<dyn Token<T>>]) -> &mut Self::Output {
        TokenBuf::from_mut(&mut slice[*self.start()..=*self.end()])
    }
}

impl<T: PM> TokenBufferIndex<T> for RangeTo<usize> {
    type Output = TokenBuf<T>;

    #[inline]
    fn index(self, slice: &[Box<dyn Token<T>>]) -> &Self::Output {
        TokenBuf::from_ref(&slice[..self.end])
    }

    #[inline]
    fn index_mut(self, slice: &mut [Box<dyn Token<T>>]) -> &mut Self::Output {
        TokenBuf::from_mut(&mut slice[..self.end])
    }
}

impl<T: PM> TokenBufferIndex<T> for RangeToInclusive<usize> {
    type Output = TokenBuf<T>;

    #[inline]
    fn index(self, slice: &[Box<dyn Token<T>>]) -> &Self::Output {
        TokenBuf::from_ref(&slice[..=self.end])
    }

    #[inline]
    fn index_mut(self, slice: &mut [Box<dyn Token<T>>]) -> &mut Self::Output {
        TokenBuf::from_mut(&mut slice[..=self.end])
    }
}
