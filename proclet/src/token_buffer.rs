use crate::{
    Error, IntoTokens, Match, Span, ToTokenStream, ToTokens, TokenStreamExt, TokenTree,
    TokenTreeExt,
};
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
pub trait Parse<T: TokenTreeExt>:
    Sized + DefaultParser<T, Parser = DefaultParserImpl<T, Self>>
{
    /// Parse a value from a `TokenBuf`.
    ///
    /// The referenced `&buf` will be modified to point past the parsed tokens on success.
    fn parse(buf: &mut &TokenBuf<T>) -> Result<Self, Error<T::Span>>;

    /// Parse a value from a `TokenBuf` buffer, but return an error with the remaining tokens if
    /// there's any left in the buffer after parsing. If parsing fails, an error with an empty
    /// buffer is returned.
    ///
    /// The referenced `&buf` will be modified to point past the parsed tokens on success.
    #[inline]
    fn parse_all(buf: &mut &TokenBuf<T>) -> Result<Self, Error<T::Span>> {
        Self::parser().parse_all(buf)
    }
}

impl<T: TokenTreeExt, const LENGTH: usize> Parse<T> for [T; LENGTH] {
    #[inline]
    fn parse(buf: &mut &TokenBuf<T>) -> Result<Self, Error<T::Span>> {
        // can't use MaybeUninit for array init as rust claims the size is unknown when transmuting it
        if buf.len() >= LENGTH {
            let parsed = buf[..LENGTH]
                .into_iter()
                .cloned()
                .collect::<Vec<T>>()
                .try_into()
                .unwrap();
            *buf = &buf[LENGTH..];
            Ok(parsed)
        } else {
            Err(Error::with_span(buf.first_span_or_default(), "no match"))
        }
    }
}

impl<T: TokenTreeExt, const LENGTH: usize> Parse<T> for Box<[T; LENGTH]> {
    #[inline]
    fn parse(buf: &mut &TokenBuf<T>) -> Result<Self, Error<T::Span>> {
        if buf.len() >= LENGTH {
            let parsed = buf[..LENGTH]
                .into_iter()
                .cloned()
                .collect::<Vec<T>>()
                .try_into()
                .unwrap();
            *buf = &buf[LENGTH..];
            Ok(parsed)
        } else {
            Err(Error::with_span(buf.first_span_or_default(), "no match"))
        }
    }
}

impl<T: TokenTreeExt, X: Parse<T>> Parse<T> for Option<X> {
    #[inline]
    fn parse(buf: &mut &TokenBuf<T>) -> Result<Self, Error<T::Span>> {
        Ok(X::parse(buf).ok())
    }
}

impl<T: TokenTreeExt, X: Parse<T>> Parse<T> for Vec<X> {
    /// Parse a non-empty vector of items. If you want to accept an empty vector, use `Option<Vec<...>>::parse`.
    #[inline]
    fn parse(buf: &mut &TokenBuf<T>) -> Result<Self, Error<T::Span>> {
        let mut vec = Vec::new();
        while let Ok(item) = X::parse(buf) {
            vec.push(item);
        }
        if vec.is_empty() {
            Err(Error::with_span(buf.first_span_or_default(), "no match"))
        } else {
            Ok(vec)
        }
    }
}

/// A parser for parsing values from a `TokenBuf`.
pub trait Parser<T: TokenTreeExt> {
    /// The output type of this parser.
    type Output<'p, 'b>
    where
        Self: 'p;

    /// Parse a value from a `TokenBuf` using this parser.
    ///
    /// The referenced `&buf` will be modified to point past the parsed tokens on success.
    fn parse<'p, 'b>(
        &'p self,
        buf: &mut &'b TokenBuf<T>,
    ) -> Result<Self::Output<'p, 'b>, Error<T::Span>>;

    /// Parse a value from a `TokenBuf` buffer, but return an error with the remaining tokens if
    /// there's any left in the buffer after parsing. If parsing fails, an error with an empty
    /// buffer is returned.
    ///
    /// The referenced `&buf` will be modified to point past the parsed tokens on success.
    #[inline]
    fn parse_all<'p, 'b>(
        &'p self,
        buf: &mut &'b TokenBuf<T>,
    ) -> Result<Self::Output<'p, 'b>, Error<T::Span>> {
        match self.parse(buf) {
            Ok(result) if buf.is_empty() => Ok(result),
            Err(e) => Err(e),
            _ => Err(Error::with_span(
                buf.first_span_or_default(),
                "unexpected tokens after input",
            )),
        }
    }

    /// Wrap this parser in [`Optional`] to make it always succeed and return an option.
    #[inline]
    fn optional(self) -> Optional<Self>
    where
        Self: Sized,
    {
        Optional(self)
    }
}

impl<T: TokenTreeExt, X: Parser<T>> Parser<T> for [X] {
    type Output<'p, 'b> = Vec<X::Output<'p, 'b>> where Self: 'p;

    #[inline]
    fn parse<'p, 'b>(
        &'p self,
        buf: &mut &'b TokenBuf<T>,
    ) -> Result<Self::Output<'p, 'b>, Error<T::Span>> {
        self.iter().map(|x| x.parse(buf)).collect()
    }
}

/// Trait for making a default parser. This is automatically implemented for objects
/// that implement the `Parse` trait.
pub trait DefaultParser<T: TokenTreeExt> {
    /// The parser that will be created.
    type Parser: Parser<T> + Copy + Default;

    /// Create a new parser.
    #[inline(always)]
    fn parser() -> Self::Parser {
        Self::Parser::default()
    }
}

impl<T: TokenTreeExt, X: Parse<T>> DefaultParser<T> for X {
    type Parser = DefaultParserImpl<T, X>;
}

/// Wrap a parser in this to make it always succeed and return an `Option`.
#[repr(transparent)]
pub struct Optional<T>(pub T);

impl<T: TokenTreeExt, X: Parser<T>> Parser<T> for Optional<X> {
    type Output<'p, 'b> = Option<X::Output<'p, 'b>> where Self: 'p;

    #[inline]
    fn parse<'p, 'b>(
        &'p self,
        buf: &mut &'b TokenBuf<T>,
    ) -> Result<Self::Output<'p, 'b>, Error<T::Span>> {
        Ok(self.0.parse(buf).ok())
    }
}

pub struct DefaultParserImpl<T: TokenTreeExt, X: Parse<T>>(PhantomData<fn() -> (T, X)>);

impl<T: TokenTreeExt, X: Parse<T>> Clone for DefaultParserImpl<T, X> {
    #[inline(always)]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: TokenTreeExt, X: Parse<T>> Copy for DefaultParserImpl<T, X> {}

impl<T: TokenTreeExt, X: Parse<T>> Default for DefaultParserImpl<T, X> {
    #[inline(always)]
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<T: TokenTreeExt, X: Parse<T>> Parser<T> for DefaultParserImpl<T, X> {
    type Output<'p, 'b> = X where Self: 'p;

    #[inline]
    fn parse<'p, 'b>(
        &'p self,
        buf: &mut &'b TokenBuf<T>,
    ) -> Result<Self::Output<'p, 'b>, Error<T::Span>> {
        X::parse(buf)
    }
}

/// Methods for making or extending a `TokenBuffer` with tokens representing this object.
/// This is automatically implemented for types that implement the [`IntoTokens`] trait.
pub trait ToTokenBuffer<T: TokenTree> {
    /// Extend the given `TokenBuffer` with tokens representing this object.
    fn extend_token_buffer(&self, token_buffer: &mut TokenBuffer<T>);

    /// Make a new `TokenBuffer` with tokens representing this object.
    #[inline]
    fn to_token_buffer(&self) -> TokenBuffer<T> {
        let mut tb = TokenBuffer::new();
        self.extend_token_buffer(&mut tb);
        tb
    }
}

impl<T: TokenTree, X: IntoTokens<T> + Clone> ToTokenBuffer<T> for X {
    #[inline]
    fn extend_token_buffer(&self, token_buffer: &mut TokenBuffer<T>) {
        token_buffer.0.extend(self.to_tokens())
    }
}

/// Automatically implemented for types that implement `Into<&TokenBuf>` for `&Type`.
pub trait AsTokenBuf<'a, T: TokenTree> {
    /// Get a reference to this as a `TokenBuf`.
    fn as_token_buf(&'a self) -> &'a TokenBuf<T>;
}

impl<'a, T: TokenTree, X: 'a> AsTokenBuf<'a, T> for X
where
    &'a X: Into<&'a TokenBuf<T>>,
{
    #[inline]
    fn as_token_buf(&'a self) -> &'a TokenBuf<T> {
        self.into()
    }
}

/// Automatically implemented for types that implement `Into<&mut TokenBuf>` for `&mut Type`.
pub trait AsTokenBufMut<'a, T: TokenTree> {
    /// Get a mutable reference to this as a `TokenBuf`.
    fn as_token_buf_mut(&'a mut self) -> &'a mut TokenBuf<T>;
}

impl<'a, T: TokenTree, X: 'a> AsTokenBufMut<'a, T> for X
where
    &'a mut X: Into<&'a mut TokenBuf<T>>,
{
    #[inline]
    fn as_token_buf_mut(&'a mut self) -> &'a mut TokenBuf<T> {
        self.into()
    }
}

/// An owned buffer of tokens.
#[derive(Clone, Debug, Default)]
pub struct TokenBuffer<T: TokenTree>(Vec<T>);

impl<T: TokenTreeExt> TokenBuffer<T> {
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

    /// Parse a value from this buffer, but return an error with the remaining tokens if
    /// there's any left in the buffer after parsing. If parsing fails, an error with an empty
    /// buffer is returned.
    ///
    /// Unlike `TokenBuf::parse_all`, this doesn't modify the reference to self.
    #[inline]
    pub fn parse_all<P: Parse<T>>(&self) -> Result<P, Error<T::Span>> {
        self.as_buf().parse_all()
    }
}

impl<T: TokenTree> TokenBuffer<T> {
    /// Create a new `TokenBuffer`.
    #[inline]
    pub const fn new() -> Self {
        Self(Vec::new())
    }
}

impl<T: TokenTreeExt> AsRef<TokenBuf<T>> for TokenBuffer<T> {
    #[inline]
    fn as_ref(&self) -> &TokenBuf<T> {
        self.as_buf()
    }
}

impl<T: TokenTreeExt> AsMut<TokenBuf<T>> for TokenBuffer<T> {
    #[inline]
    fn as_mut(&mut self) -> &mut TokenBuf<T> {
        self.as_buf_mut()
    }
}

impl<T: TokenTreeExt> Borrow<TokenBuf<T>> for TokenBuffer<T> {
    #[inline]
    fn borrow(&self) -> &TokenBuf<T> {
        self.as_buf()
    }
}

impl<T: TokenTreeExt> BorrowMut<TokenBuf<T>> for TokenBuffer<T> {
    #[inline]
    fn borrow_mut(&mut self) -> &mut TokenBuf<T> {
        self.as_buf_mut()
    }
}

impl<T: TokenTreeExt> Deref for TokenBuffer<T> {
    type Target = TokenBuf<T>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        TokenBuf::from_ref(&self.0[..])
    }
}

impl<T: TokenTreeExt> DerefMut for TokenBuffer<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        TokenBuf::from_mut(&mut self.0[..])
    }
}

impl<T: TokenTree, X: ToTokenBuffer<T>> Extend<X> for TokenBuffer<T> {
    #[inline]
    fn extend<I: IntoIterator<Item = X>>(&mut self, iter: I) {
        for i in iter {
            i.extend_token_buffer(self);
        }
    }
}

#[cfg(feature = "proc-macro")]
impl From<proc_macro::TokenStream> for TokenBuffer<proc_macro::TokenTree> {
    #[inline]
    fn from(value: proc_macro::TokenStream) -> Self {
        Self::from_iter(value)
    }
}

#[cfg(feature = "proc-macro2")]
impl From<proc_macro2::TokenStream> for TokenBuffer<proc_macro2::TokenTree> {
    #[inline]
    fn from(value: proc_macro2::TokenStream) -> Self {
        Self::from_iter(value)
    }
}

#[cfg(feature = "proc-macro")]
impl From<TokenBuffer<proc_macro::TokenTree>> for proc_macro::TokenStream {
    #[inline]
    fn from(value: TokenBuffer<proc_macro::TokenTree>) -> Self {
        value.to_token_stream()
    }
}

#[cfg(feature = "proc-macro2")]
impl From<TokenBuffer<proc_macro2::TokenTree>> for proc_macro2::TokenStream {
    #[inline]
    fn from(value: TokenBuffer<proc_macro2::TokenTree>) -> Self {
        value.to_token_stream()
    }
}

impl<T: TokenTree> From<TokenBuffer<T>> for Box<[T]> {
    #[inline]
    fn from(value: TokenBuffer<T>) -> Self {
        value.0.into()
    }
}

impl<T: TokenTree> From<TokenBuffer<T>> for Vec<T> {
    #[inline]
    fn from(value: TokenBuffer<T>) -> Self {
        value.0
    }
}

impl<T: TokenTree> From<Vec<T>> for TokenBuffer<T> {
    #[inline]
    fn from(value: Vec<T>) -> Self {
        Self(value)
    }
}

impl<T: TokenTree, X: ToTokenBuffer<T>> FromIterator<X> for TokenBuffer<T> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = X>>(iter: I) -> Self {
        let mut buf = TokenBuffer::new();
        for i in iter {
            i.extend_token_buffer(&mut buf);
        }
        buf
    }
}

impl<T: TokenTree, I: TokenBufferIndex<T>> Index<I> for TokenBuffer<T> {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        index.index(&self.0)
    }
}

impl<T: TokenTree, I: TokenBufferIndex<T>> IndexMut<I> for TokenBuffer<T> {
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        index.index_mut(&mut self.0)
    }
}

impl<T: TokenTree> IntoIterator for TokenBuffer<T> {
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;
    type Item = T;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T: TokenTree> IntoTokens<T> for TokenBuffer<T> {
    #[inline]
    fn into_tokens(self) -> impl Iterator<Item = T>
    where
        Self: Sized,
    {
        self.0.into_iter()
    }
}

impl<T: TokenTree, const LENGTH: usize> TryFrom<TokenBuffer<T>> for [T; LENGTH] {
    type Error = <Self as TryFrom<Vec<T>>>::Error;

    #[inline]
    fn try_from(value: TokenBuffer<T>) -> Result<Self, Self::Error> {
        value.0.try_into()
    }
}

/// Borrowed version of [`TokenBuffer`].
#[derive(Debug)]
#[repr(transparent)]
pub struct TokenBuf<T: TokenTree>([T]);

impl<T: TokenTreeExt> TokenBuf<T> {
    #[inline]
    fn from_ref(r: &[T]) -> &Self {
        unsafe {
            // # Safety
            // It's a reference to the same type in a transparent struct
            transmute::<&[T], &Self>(r)
        }
    }

    #[inline]
    fn from_mut(r: &mut [T]) -> &mut Self {
        unsafe {
            // # Safety
            // It's a reference to the same type in a transparent struct
            transmute::<&mut [T], &mut Self>(r)
        }
    }

    /// Get the span of the first token in this buffer, or the default span if the buffer is empty.
    #[inline]
    pub fn first_span_or_default(&self) -> T::Span {
        self.0
            .first()
            .map(|t| t.span())
            .unwrap_or(T::Span::call_site())
    }

    /// Parse a value from this buffer.
    ///
    /// The referenced `&self` will be modified to point past the parsed tokens on success.
    #[inline]
    pub fn parse<P: Parse<T>>(self: &mut &Self) -> Result<P, Error<T::Span>> {
        P::parse(self)
    }

    /// Parse a value from this buffer, but return an error with the remaining tokens if
    /// there's any left in the buffer after parsing. If parsing fails, an error with an empty
    /// buffer is returned.
    ///
    /// The referenced `&self` will be modified to point past the parsed tokens on success.
    #[inline]
    pub fn parse_all<P: Parse<T>>(self: &mut &Self) -> Result<P, Error<T::Span>> {
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
        mut match_fn: impl FnMut(&T) -> Match<M>,
    ) -> Result<M, Error<T::Span>> {
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
        mut match_fn: impl FnMut(&T, Option<&T>) -> Match<M>,
    ) -> Result<M, Error<T::Span>> {
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
        mut match_fn: impl FnMut(&'a Self, &T, Option<&T>) -> Match<M>,
    ) -> Result<M, Error<T::Span>> {
        let mut result = None;
        for i in 1..=self.len() {
            match match_fn(&self[..i], &self[i - 1], self.get(i)) {
                Match::Complete(m) => {
                    *self = &self[i..];
                    return Ok(m);
                }
                Match::Partial(m) => result = Some((m, &self[i..])),
                Match::NeedMore => (),
                Match::NoMatch => break,
            }
        }
        result
            .ok_or_else(|| Error::with_span(self.first_span_or_default(), "no match"))
            .map(|(result, rest)| {
                *self = rest;
                result
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
        mut match_fn: impl FnMut(&T) -> Match<M>,
    ) -> Result<M, Error<T::Span>> {
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
        mut match_fn: impl FnMut(&T, Option<&T>) -> Match<M>,
    ) -> Result<M, Error<T::Span>> {
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
        mut match_fn: impl FnMut(&'a Self, &T, Option<&T>) -> Match<M>,
    ) -> Result<M, Error<T::Span>> {
        let mut result = None;
        for i in (0..self.len()).rev() {
            match match_fn(&self[i..], &self[i], (i > 0).then(|| &self[i - 1])) {
                Match::Complete(m) => {
                    *self = &self[..i];
                    return Ok(m);
                }
                Match::Partial(m) => result = Some((m, &self[..i])),
                Match::NeedMore => (),
                Match::NoMatch => break,
            }
        }
        result
            .ok_or_else(|| Error::with_span(self.first_span_or_default(), "no match"))
            .map(|(result, rest)| {
                *self = rest;
                result
            })
    }
}

impl<T: TokenTree> Deref for TokenBuf<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: TokenTree> DerefMut for TokenBuf<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg(feature = "proc-macro")]
impl From<&TokenBuf<proc_macro::TokenTree>> for proc_macro::TokenStream {
    #[inline]
    fn from(value: &TokenBuf<proc_macro::TokenTree>) -> Self {
        value.to_token_stream()
    }
}

#[cfg(feature = "proc-macro")]
impl From<&mut TokenBuf<proc_macro::TokenTree>> for proc_macro::TokenStream {
    #[inline]
    fn from(value: &mut TokenBuf<proc_macro::TokenTree>) -> Self {
        value.to_token_stream()
    }
}

#[cfg(feature = "proc-macro2")]
impl From<&TokenBuf<proc_macro2::TokenTree>> for proc_macro2::TokenStream {
    #[inline]
    fn from(value: &TokenBuf<proc_macro2::TokenTree>) -> Self {
        value.to_token_stream()
    }
}

#[cfg(feature = "proc-macro2")]
impl From<&mut TokenBuf<proc_macro2::TokenTree>> for proc_macro2::TokenStream {
    #[inline]
    fn from(value: &mut TokenBuf<proc_macro2::TokenTree>) -> Self {
        value.to_token_stream()
    }
}

impl<'a, T: TokenTreeExt> From<&'a TokenBuffer<T>> for &'a TokenBuf<T> {
    #[inline]
    fn from(value: &'a TokenBuffer<T>) -> Self {
        value.as_buf()
    }
}

impl<'a, T: TokenTreeExt> From<&'a mut TokenBuffer<T>> for &'a mut TokenBuf<T> {
    #[inline]
    fn from(value: &'a mut TokenBuffer<T>) -> Self {
        value.as_buf_mut()
    }
}

impl<'a, T: TokenTreeExt> From<&'a [T]> for &'a TokenBuf<T> {
    #[inline]
    fn from(value: &'a [T]) -> Self {
        TokenBuf::from_ref(value)
    }
}

impl<'a, T: TokenTreeExt> From<&'a mut [T]> for &'a mut TokenBuf<T> {
    #[inline]
    fn from(value: &'a mut [T]) -> Self {
        TokenBuf::from_mut(value)
    }
}

impl<T: TokenTree, I: TokenBufferIndex<T>> Index<I> for TokenBuf<T> {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        index.index(&self.0)
    }
}

impl<T: TokenTree, I: TokenBufferIndex<T>> IndexMut<I> for TokenBuf<T> {
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        index.index_mut(&mut self.0)
    }
}

impl<'a, T: TokenTree> IntoIterator for &'a TokenBuf<T> {
    type IntoIter = slice::Iter<'a, T>;
    type Item = &'a T;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, T: TokenTree> IntoIterator for &'a mut TokenBuf<T> {
    type IntoIter = slice::IterMut<'a, T>;
    type Item = &'a mut T;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl<T: TokenTreeExt> ToOwned for TokenBuf<T> {
    type Owned = TokenBuffer<T>;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        TokenBuffer(self.0.to_vec())
    }
}

impl<T: TokenTree> ToTokens<T> for TokenBuf<T> {
    #[inline]
    fn to_tokens(&self) -> impl Iterator<Item = T> {
        // to_owned doesn't work here bc rust
        TokenBuffer::<T>(self.0.to_vec()).into_tokens()
    }
}

impl<T: TokenStreamExt> ToTokenStream<T> for TokenBuf<T::TokenTree> {
    #[inline]
    fn extend_token_stream(&self, token_stream: &mut T) {
        for i in self.0.iter() {
            i.extend_token_stream(token_stream);
        }
    }
}

pub trait TokenBufferIndex<T: TokenTree> {
    type Output: ?Sized;
    fn index(self, slice: &[T]) -> &Self::Output;
    fn index_mut(self, slice: &mut [T]) -> &mut Self::Output;
}

impl<T: TokenTree> TokenBufferIndex<T> for usize {
    type Output = T;

    #[inline]
    fn index(self, slice: &[T]) -> &Self::Output {
        &slice[self]
    }

    #[inline]
    fn index_mut(self, slice: &mut [T]) -> &mut Self::Output {
        &mut slice[self]
    }
}

impl<T: TokenTreeExt> TokenBufferIndex<T> for Range<usize> {
    type Output = TokenBuf<T>;

    #[inline]
    fn index(self, slice: &[T]) -> &Self::Output {
        TokenBuf::from_ref(&slice[self.start..self.end])
    }

    #[inline]
    fn index_mut(self, slice: &mut [T]) -> &mut Self::Output {
        TokenBuf::from_mut(&mut slice[self.start..self.end])
    }
}

impl<T: TokenTreeExt> TokenBufferIndex<T> for RangeFrom<usize> {
    type Output = TokenBuf<T>;

    #[inline]
    fn index(self, slice: &[T]) -> &Self::Output {
        TokenBuf::from_ref(&slice[self.start..])
    }

    #[inline]
    fn index_mut(self, slice: &mut [T]) -> &mut Self::Output {
        TokenBuf::from_mut(&mut slice[self.start..])
    }
}

impl<T: TokenTreeExt> TokenBufferIndex<T> for RangeFull {
    type Output = TokenBuf<T>;

    #[inline]
    fn index(self, slice: &[T]) -> &Self::Output {
        TokenBuf::from_ref(slice)
    }

    #[inline]
    fn index_mut(self, slice: &mut [T]) -> &mut Self::Output {
        TokenBuf::from_mut(slice)
    }
}

impl<T: TokenTreeExt> TokenBufferIndex<T> for RangeInclusive<usize> {
    type Output = TokenBuf<T>;

    #[inline]
    fn index(self, slice: &[T]) -> &Self::Output {
        TokenBuf::from_ref(&slice[*self.start()..=*self.end()])
    }

    #[inline]
    fn index_mut(self, slice: &mut [T]) -> &mut Self::Output {
        TokenBuf::from_mut(&mut slice[*self.start()..=*self.end()])
    }
}

impl<T: TokenTreeExt> TokenBufferIndex<T> for RangeTo<usize> {
    type Output = TokenBuf<T>;

    #[inline]
    fn index(self, slice: &[T]) -> &Self::Output {
        TokenBuf::from_ref(&slice[..self.end])
    }

    #[inline]
    fn index_mut(self, slice: &mut [T]) -> &mut Self::Output {
        TokenBuf::from_mut(&mut slice[..self.end])
    }
}

impl<T: TokenTreeExt> TokenBufferIndex<T> for RangeToInclusive<usize> {
    type Output = TokenBuf<T>;

    #[inline]
    fn index(self, slice: &[T]) -> &Self::Output {
        TokenBuf::from_ref(&slice[..=self.end])
    }

    #[inline]
    fn index_mut(self, slice: &mut [T]) -> &mut Self::Output {
        TokenBuf::from_mut(&mut slice[..=self.end])
    }
}
