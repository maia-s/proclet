use crate::TokenTree;
use std::{any::Any, iter};

pub trait Token<T: TokenTree>: Any + ToTokens<T> {}

pub trait ToTokens<T: TokenTree> {
    fn to_token_trees(&self) -> TokenTrees<T>;

    #[inline]
    fn to_token_stream(&self) -> T::TokenStream {
        self.to_token_trees().collect()
    }
}

pub struct TokenTrees<'a, T: TokenTree>(Box<dyn Iterator<Item = T> + 'a>);

impl<'a, T: TokenTree> TokenTrees<'a, T> {
    #[inline]
    pub fn new(value: impl Iterator<Item = T> + 'a) -> Self {
        Self(Box::new(value))
    }
}

impl<T: TokenTree> From<T> for TokenTrees<'static, T> {
    #[inline]
    fn from(value: T) -> Self {
        Self::new(iter::once(value))
    }
}

impl<'a, T: TokenTree> From<&'a [T]> for TokenTrees<'a, T> {
    #[inline]
    fn from(value: &'a [T]) -> Self {
        Self::new(value.iter().cloned())
    }
}

impl<T: TokenTree> From<Vec<T>> for TokenTrees<'static, T> {
    #[inline]
    fn from(value: Vec<T>) -> Self {
        Self::new(value.into_iter())
    }
}

impl<T: TokenTree> From<Box<[T]>> for TokenTrees<'static, T> {
    #[inline]
    fn from(value: Box<[T]>) -> Self {
        value.into_vec().into()
    }
}

impl<T: TokenTree> Iterator for TokenTrees<'_, T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}
