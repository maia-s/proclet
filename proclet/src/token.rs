use crate::{TokenStream, PM};

/// Owned trait object of [`Token`]
pub type TokenObject<T> = <T as crate::ProcMacro>::TokenTree;

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
