use crate::{TokenStream, TokenTree};

/// Token object.
pub type TokenObject<T> = <T as crate::ProcMacro>::TokenTree;

/// Trait for converting an object into its token representation.
pub trait IntoTokens<T: TokenTree> {
    /// Convert this object into an iterator of tokens representing the object.
    fn into_tokens(self) -> impl Iterator<Item = TokenObject<T>>;
}

/// Trait for getting the token representation of an object.
pub trait ToTokens<T: TokenTree> {
    /// Get an iterator of tokens representing this object.
    fn to_tokens(&self) -> impl Iterator<Item = TokenObject<T>> + '_;
}

impl<T: TokenTree, X: IntoTokens<T> + Clone> ToTokens<T> for X {
    #[inline]
    fn to_tokens(&self) -> impl Iterator<Item = TokenObject<T>> {
        self.clone().into_tokens()
    }
}

impl<T: TokenTree, X: IntoTokens<T>> IntoTokens<T> for Option<X> {
    #[inline]
    fn into_tokens(self) -> impl Iterator<Item = TokenObject<T>> {
        self.into_iter().flat_map(|x| x.into_tokens())
    }
}

/// Methods for making or extending a `TokenStream` with a representation of this object.
pub trait ToTokenStream<T: TokenStream> {
    /// Extend the given `TokenStream` with a representation of this object.
    fn extend_token_stream(&self, token_stream: &mut T);

    /// Make a new `TokenStream` with a representation of this object.
    #[inline]
    fn into_token_stream(self) -> T
    where
        Self: Sized,
    {
        let mut ts = T::new();
        self.extend_token_stream(&mut ts);
        ts
    }

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

impl<T: TokenTree, T0: IntoTokens<T>, T1: IntoTokens<T>> IntoTokens<T> for (T0, T1) {
    #[inline]
    fn into_tokens(self) -> impl Iterator<Item = TokenObject<T>>
    where
        Self: Sized,
    {
        self.0.into_tokens().chain(self.1.into_tokens())
    }
}
