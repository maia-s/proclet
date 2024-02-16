use crate::{SpanExt, TokenStreamExt};
use std::{
    borrow::Cow,
    fmt::{self, Debug, Display},
};

/// An error.
#[derive(Debug)]
pub struct Error<S> {
    message: Cow<'static, str>,
    span: S,
}

impl<S: SpanExt> Error<S> {
    /// Create a new error with the provided message.
    #[inline]
    pub fn new(message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            message: message.into(),
            span: S::call_site(),
        }
    }

    /// Create a new error with the provided message and span.
    #[inline]
    pub fn with_span(span: S, message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }

    /// Set a new error message for this error, keeping the span.
    pub fn set_message(&mut self, message: impl Into<Cow<'static, str>>) {
        self.message = message.into();
    }

    /// Set the span of this error.
    #[inline]
    pub fn set_span(&mut self, span: S) {
        self.span = span;
    }

    /// Convert this error into a `TokenStream` that will trigger a compile error
    /// at the error's span.
    #[inline]
    pub fn to_compile_error(&self) -> S::TokenStream {
        let ts: S::TokenStream = format!("::core::compile_error!({:?});", self.message)
            .parse()
            .unwrap();
        ts.apply_span(self.span)
    }
}

impl<S: Debug> std::error::Error for Error<S> {}

impl<S> Display for Error<S> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.message, f)
    }
}

impl<S: SpanExt> From<&'static str> for Error<S> {
    #[inline]
    fn from(value: &'static str) -> Self {
        Self::new(Cow::Borrowed(value))
    }
}

impl<S: SpanExt> From<String> for Error<S> {
    #[inline]
    fn from(value: String) -> Self {
        Self::new(Cow::Owned(value))
    }
}
