use crate::{Parse, Parser, ToTokenStream, TokenStream, PM};
use std::{marker::PhantomData, ops::Deref};

#[derive(Clone, Default, Debug)]
pub struct Delimited<M, D>(Vec<(M, Option<D>)>);

impl<M, D> Delimited<M, D> {
    pub const fn new() -> Self {
        Self(Vec::new())
    }
}

impl<M, D> Deref for Delimited<M, D> {
    type Target = Vec<(M, Option<D>)>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: PM, M: Parse<T>, D: Parse<T>> Parse<T> for Delimited<M, D> {
    #[inline]
    fn parse(buf: &mut &crate::TokenBuf<T>) -> Option<Self> {
        delimited(M::parser(), D::parser()).parse(buf)
    }
}

impl<T: TokenStream, M: ToTokenStream<T>, D: ToTokenStream<T>> ToTokenStream<T>
    for Delimited<M, D>
{
    fn extend_token_stream(&self, token_stream: &mut T) {
        for (m, d) in self.0.iter() {
            m.extend_token_stream(token_stream);
            d.extend_token_stream(token_stream);
        }
    }
}

#[derive(Clone, Debug)]
pub struct DelimitedParser<T: PM, M, D>(M, D, PhantomData<fn() -> T>);

impl<T: PM, M: Parser<T>, D: Parser<T>> DelimitedParser<T, M, D> {
    #[inline]
    pub const fn new(main: M, delim: D) -> Self {
        Self(main, delim, PhantomData)
    }
}

impl<T: PM, M: Parser<T>, D: Parser<T>> Parser<T> for DelimitedParser<T, M, D> {
    type Output<'p, 'b> = Delimited<M::Output<'p, 'b>, D::Output<'p, 'b>> where Self: 'p;

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
        Some(Delimited(vec))
    }
}

#[inline]
pub const fn delimited<T: PM, M: Parser<T>, D: Parser<T>>(
    main: M,
    delim: D,
) -> DelimitedParser<T, M, D> {
    DelimitedParser::new(main, delim)
}
