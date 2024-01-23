use crate::{span::WrappedSpan, SpanExt};
use std::{
    borrow::Cow,
    fmt::{self, Display},
};

#[derive(Debug)]
pub struct Error {
    message: Cow<'static, str>,
    span: WrappedSpan,
}

impl Error {
    #[inline]
    pub fn new(message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            message: message.into(),
            span: WrappedSpan::call_site(),
        }
    }

    #[inline]
    pub fn with_span(span: impl SpanExt, message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            message: message.into(),
            span: span.into(),
        }
    }

    #[inline]
    pub fn set_span(&mut self, span: impl SpanExt) {
        self.span = span.into();
    }

    #[inline]
    pub fn to_compile_error<TokenStream: crate::TokenStreamExt>(&self) -> TokenStream {
        let ts: TokenStream = format!("::core::compile_error!({:?});", self.message)
            .parse()
            .unwrap();
        ts.apply_span(self.span.try_into().unwrap())
    }
}

impl std::error::Error for Error {}

impl Display for Error {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.message, f)
    }
}

impl From<&'static str> for Error {
    #[inline]
    fn from(value: &'static str) -> Self {
        Self::new(Cow::Borrowed(value))
    }
}

impl From<String> for Error {
    #[inline]
    fn from(value: String) -> Self {
        Self::new(Cow::Owned(value))
    }
}
