use crate::{ProcMacro, ProcMacroExt};

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
pub trait SpanExt: ProcMacroExt<SpanExt = Self> + Span {}

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
