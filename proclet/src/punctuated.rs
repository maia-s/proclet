use crate::{IntoTokens, Parse, Parser, ToTokenStream, TokenStream, TokenTree, TokenTreeExt};
use std::{marker::PhantomData, ops::Deref};

/// Parsed punctuated values.
#[derive(Clone, Default, Debug)]
pub struct Punctuated<M, D>(Vec<(M, Option<D>)>);

impl<M, D> Punctuated<M, D> {
    /// Create a new empty set of punctuated values.
    pub const fn new() -> Self {
        Self(Vec::new())
    }
}

impl<M, D> Deref for Punctuated<M, D> {
    type Target = Vec<(M, Option<D>)>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<M, D> IntoIterator for Punctuated<M, D> {
    type Item = (M, Option<D>);
    type IntoIter = <Vec<(M, Option<D>)> as IntoIterator>::IntoIter;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T: TokenTreeExt, M: Parse<T>, D: Parse<T>> Parse<T> for Punctuated<M, D> {
    #[inline]
    fn parse(buf: &mut &crate::TokenBuf<T>) -> Option<Self> {
        punctuated(M::parser(), D::parser()).parse(buf)
    }
}

impl<T: TokenTree, M: IntoTokens<T>, D: IntoTokens<T>> IntoTokens<T> for Punctuated<M, D> {
    #[inline]
    fn into_tokens(self) -> impl Iterator<Item = T>
    where
        Self: Sized,
    {
        self.0.into_iter().flat_map(|i| i.into_tokens())
    }
}

impl<T: TokenStream, M: ToTokenStream<T>, D: ToTokenStream<T>> ToTokenStream<T>
    for Punctuated<M, D>
{
    #[inline]
    fn extend_token_stream(&self, token_stream: &mut T) {
        for (m, d) in self.0.iter() {
            m.extend_token_stream(token_stream);
            d.extend_token_stream(token_stream);
        }
    }
}

/// Parser for punctuated values.
#[derive(Clone, Debug)]
pub struct PunctuatedParser<T: TokenTree, M, D>(M, D, PhantomData<fn() -> T>);

impl<T: TokenTreeExt, M: Parser<T>, D: Parser<T>> PunctuatedParser<T, M, D> {
    /// Create a new `PunctuatedParser` using `main` as the parser for the main part and
    /// `delim` as the parser for the delimiter.
    #[inline]
    pub const fn new(main: M, delim: D) -> Self {
        Self(main, delim, PhantomData)
    }
}

impl<T: TokenTreeExt, M: Parser<T>, D: Parser<T>> Parser<T> for PunctuatedParser<T, M, D> {
    type Output<'p, 'b> = Punctuated<M::Output<'p, 'b>, D::Output<'p, 'b>> where Self: 'p;

    #[inline]
    fn parse<'p, 'b>(&'p self, buf: &mut &'b crate::TokenBuf<T>) -> Option<Self::Output<'p, 'b>> {
        let mut vec = Vec::new();
        while let Some(main) = self.0.parse(buf) {
            let delim = self.1.parse(buf);
            let got_delim = delim.is_some();
            vec.push((main, delim));
            if !got_delim {
                break;
            }
        }
        Some(Punctuated(vec))
    }
}

/// Create a new parser for parsing things with `main` punctuated by `delim`.
/// Convenience function for calling [`PunctuatedParser::new`].
#[inline]
pub const fn punctuated<T: TokenTreeExt, M: Parser<T>, D: Parser<T>>(
    main: M,
    delim: D,
) -> PunctuatedParser<T, M, D> {
    PunctuatedParser::new(main, delim)
}
