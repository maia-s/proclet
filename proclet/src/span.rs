use std::error::Error;
use std::fmt::{Debug, Display};

use crate::{ProcMacro, ProcMacroExt, TryFromDebug};

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

/// `Span` API trait. See [`proc_macro::Span`](https://doc.rust-lang.org/stable/proc_macro/struct.Span.html).
///
/// This trait is implemented for `Span` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
///
/// See also [`SpanExt`].
pub trait Span: ProcMacro<Span = Self> + Copy {
    /// Create a new `Span` with call site hygiene.
    fn call_site() -> Self;

    /// Create a new `Span` with mixed site hygiene.
    fn mixed_site() -> Self;

    /// Create a new `Span` with the same position as `self` but that resolves as if it was `other`.
    fn resolved_at(&self, other: Self) -> Self;

    /// Create a new `Span` that resolves like `self` but with the position of `other`.
    fn located_at(&self, other: Self) -> Self;

    /// Get the source text of the span, if any. Observable behaviour should not rely on this.
    fn source_text(&self) -> Option<String>;
}

/// Extensions for [`Span`].
///
/// This trait is implemented for `Span` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
pub trait SpanExt:
    ProcMacroExt<SpanExt = Self> + Span + Into<WrappedSpan> + TryFromDebug<WrappedSpan>
{
}

macro_rules! impl_span {
    ($($pm:ident: $feature:literal),*) => { $(
        #[cfg(feature = $feature)]
        impl Span for $pm::Span {
            #[inline]
            fn call_site() -> Self {
                Self::call_site()
            }

            #[inline]
            fn mixed_site() -> Self {
                Self::mixed_site()
            }

            #[inline]
            fn resolved_at(&self, other: Self) -> Self {
                self.resolved_at(other)
            }

            #[inline]
            fn located_at(&self, other: Self) -> Self {
                self.located_at(other)
            }

            #[inline]
            fn source_text(&self) -> Option<String> {
                self.source_text()
            }
        }

        #[cfg(feature = $feature)]
        impl SpanExt for $pm::Span {}
    )* };
}

impl_span!(proc_macro: "proc-macro", proc_macro2: "proc-macro2");