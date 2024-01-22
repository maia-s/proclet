use std::error::Error;
use std::fmt::{Debug, Display};

use crate::base::ProcMacro;
use crate::ProcMacroExt;

#[derive(Clone, Copy)]
pub struct IncompatibleSpanError;

impl Error for IncompatibleSpanError {}

impl Display for IncompatibleSpanError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Can't convert proc_macro2::Span to proc_macro::Span")
    }
}

impl Debug for IncompatibleSpanError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum WrappedSpan {
    #[cfg(feature = "proc-macro")]
    PM1(proc_macro::Span),

    #[cfg(feature = "proc-macro2")]
    PM2(proc_macro2::Span),

    #[cfg(not(any(feature = "proc-macro", feature = "proc-macro2")))]
    None,
}

impl WrappedSpan {
    #[inline]
    pub fn call_site() -> Self {
        #[cfg(feature = "proc-macro")]
        {
            Self::PM1(proc_macro::Span::call_site())
        }
        #[cfg(all(not(feature = "proc-macro"), feature = "proc-macro2"))]
        {
            Self::PM2(proc_macro2::Span::call_site())
        }
        #[cfg(not(any(feature = "proc-macro", feature = "proc-macro2")))]
        {
            Self::None
        }
    }

    #[inline]
    pub fn mixed_site() -> Self {
        #[cfg(feature = "proc-macro")]
        {
            Self::PM1(proc_macro::Span::mixed_site())
        }
        #[cfg(all(not(feature = "proc-macro"), feature = "proc-macro2"))]
        {
            Self::PM2(proc_macro2::Span::mixed_site())
        }
        #[cfg(not(any(feature = "proc-macro", feature = "proc-macro2")))]
        {
            Self::None
        }
    }
}

#[cfg(feature = "proc-macro")]
impl From<proc_macro::Span> for WrappedSpan {
    #[inline]
    fn from(value: proc_macro::Span) -> Self {
        Self::PM1(value)
    }
}

#[cfg(feature = "proc-macro2")]
impl From<proc_macro2::Span> for WrappedSpan {
    #[inline]
    fn from(value: proc_macro2::Span) -> Self {
        Self::PM2(value)
    }
}

#[cfg(feature = "proc-macro")]
impl TryFrom<WrappedSpan> for proc_macro::Span {
    type Error = IncompatibleSpanError;

    #[inline]
    fn try_from(value: WrappedSpan) -> Result<Self, Self::Error> {
        match value {
            WrappedSpan::PM1(span) => Ok(span),

            #[cfg(feature = "proc-macro2")]
            WrappedSpan::PM2(_) => Err(IncompatibleSpanError),
        }
    }
}

#[cfg(feature = "proc-macro2")]
impl From<WrappedSpan> for proc_macro2::Span {
    #[inline]
    fn from(value: WrappedSpan) -> Self {
        match value {
            WrappedSpan::PM2(span) => span,

            #[cfg(feature = "proc-macro")]
            WrappedSpan::PM1(span) => span.into(),
        }
    }
}

pub trait Span: ProcMacro + Copy {}

#[cfg(feature = "proc-macro")]
impl Span for proc_macro::Span {}

#[cfg(feature = "proc-macro2")]
impl Span for proc_macro2::Span {}

pub trait SpanExt: ProcMacroExt + Span + Into<WrappedSpan> + TryFrom<WrappedSpan> {}

#[cfg(feature = "proc-macro")]
impl SpanExt for proc_macro::Span {}

#[cfg(feature = "proc-macro2")]
impl SpanExt for proc_macro2::Span {}
