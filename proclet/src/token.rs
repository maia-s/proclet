use crate::{TokenStream, PM};
use std::{any::Any, fmt::Debug};

/// A token.
pub trait Token<T: PM>: TokenAuto<T> + Any + Debug + ToTokenStream<T::TokenStream> {
    /// Check two tokens for equality, not including their spans.
    fn eq_except_span(&self, other: &dyn Token<T>) -> bool;
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

    /// Clone the token into a new `Box<dyn Token<T>>`.
    fn clone_boxed(&self) -> Box<dyn Token<T>>;
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

    #[inline]
    fn clone_boxed(&self) -> Box<dyn Token<T>> {
        Box::new(self.clone())
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
