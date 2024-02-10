use crate::{TokenStream, PM};
use std::{any::Any, fmt::Debug, rc::Rc};

/// Owned trait object of [`Token`]
pub type TokenObject<T> = Rc<dyn Token<T>>;

/// A token.
pub trait Token<T: PM>:
    TokenAuto<T> + Any + Debug + ToTokens<T> + ToTokenStream<T::TokenStream>
{
    /// Check two tokens for equality, not including their spans.
    fn eq_except_span(&self, other: &dyn Token<T>) -> bool;

    /// Turn this token into a trait object.
    #[inline]
    fn into_token_object(self) -> TokenObject<T>
    where
        Self: Sized,
    {
        Rc::new(self)
    }
}

impl<T: PM> dyn Token<T> {
    /// Check if the token is the provided type.
    #[inline]
    pub fn is<X: Token<T>>(&self) -> bool {
        self.as_any().is::<X>()
    }

    /// Get a reference to the underlying type of the token, if it is the provided type.
    #[inline]
    pub fn downcast_ref<X: Token<T>>(&self) -> Option<&X> {
        self.as_any().downcast_ref::<X>()
    }

    /// Get a mutable reference to the underlying type of the token, if it is the provided type.
    #[inline]
    pub fn downcast_mut<X: Token<T>>(&mut self) -> Option<&mut X> {
        self.as_any_mut().downcast_mut::<X>()
    }
}

/// Automatically implemented methods for [`Token`]
pub trait TokenAuto<T: PM> {
    /// Get the token as `&dyn Any`.
    fn as_any(&self) -> &dyn Any;

    /// Get the token as `&mut dyn Any`.
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<T: PM, X: Clone + Token<T>> TokenAuto<T> for X {
    #[inline]
    fn as_any(&self) -> &dyn Any {
        self
    }

    #[inline]
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

/// Trait for converting an object into its token representation.
pub trait ToTokens<T: PM> {
    /// Convert this object into an iterator of tokens representing the object.
    fn into_tokens(self) -> impl Iterator<Item = TokenObject<T>>
    where
        Self: Sized;

    /// Get an iterator of tokens representing this object.
    #[inline]
    fn to_tokens(&self) -> impl Iterator<Item = TokenObject<T>>
    where
        Self: Clone,
    {
        self.clone().into_tokens()
    }
}

/// Methods for making or extending a `TokenStream` with a representation of this object.
pub trait ToTokenStream<T: TokenStream> {
    /// Extend the given `TokenStream` with a representation of this object.
    fn extend_token_stream(&self, token_stream: &mut T);

    /// Make a new `TokenStream` with a representation of this object.
    #[inline]
    fn to_token_stream(&self) -> T {
        let mut ts = T::new();
        self.extend_token_stream(&mut ts);
        ts
    }
}

impl<T: TokenStream, X: ToTokenStream<T>> ToTokenStream<T> for [X] {
    #[inline]
    fn extend_token_stream(&self, token_stream: &mut T) {
        for i in self {
            i.extend_token_stream(token_stream);
        }
    }
}

impl<T: TokenStream, X: ToTokenStream<T>> ToTokenStream<T> for Option<X> {
    #[inline]
    fn extend_token_stream(&self, token_stream: &mut T) {
        if let Some(x) = self {
            x.extend_token_stream(token_stream);
        }
    }
}

macro_rules! def_tokens_ {
    ($($([$feature:literal])? $(#[$attr:meta])* $ident:ident: $t:ty),* $(,)?) => { $(
        $( #[cfg(feature = $feature)] )?
        $( #[$attr] )*
        #[derive(Clone, Debug)]
        pub struct $ident<S: crate::Span> {
            value: $t,
            span: S,
        }

        $( #[cfg(feature = $feature)] )?
        impl<S: crate::Span> $ident<S> {
            #[doc = concat!("Create a new `", stringify!($ident), "`.")]
            #[inline]
            pub fn new(value: $t) -> Self {
                Self {
                    value,
                    span: S::call_site(),
                }
            }

            #[doc = concat!("Create a new `", stringify!($ident), "` with a custom span.")]
            #[inline]
            pub const fn with_span(value: $t, span: S) -> Self {
                Self { value, span }
            }

            /// Get a reference to the value of this token.
            #[inline]
            pub const fn value(&self) -> &$t {
                &self.value
            }

            /// Get a mutable reference to the value of this token.
            #[inline]
            pub fn value_mut(&mut self) -> &mut $t {
                &mut self.value
            }

            /// Get the value of this token.
            pub fn into_value(self) -> $t {
                self.value
            }

            /// Get the span of this token.
            #[inline]
            pub const fn span(&self) -> S {
                self.span
            }

            /// Set the span of this token.
            #[inline]
            pub fn set_span(&mut self, span: S) {
                self.span = span;
            }
        }
    )* };
}
pub(crate) use def_tokens_ as def_tokens;
