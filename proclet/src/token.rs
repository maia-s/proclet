use crate::{TokenTree, PM};
use std::{any::Any, fmt::Debug, iter};

pub trait Token<T: PM>:
    TokenAuto<T> + AsToken<T> + Any + Debug + ToTokenTrees<T::TokenTree>
{
    fn eq_except_span(&self, other: &dyn Token<T>) -> bool;
}

impl<T: PM> dyn Token<T> {
    #[inline]
    pub fn is<X: Token<T>>(&self) -> bool {
        self.as_any().is::<X>()
    }

    #[inline]
    pub fn downcast_ref<X: Token<T>>(&self) -> Option<&X> {
        self.as_any().downcast_ref::<X>()
    }

    #[inline]
    pub fn downcast_mut<X: Token<T>>(&mut self) -> Option<&mut X> {
        self.as_any_mut().downcast_mut::<X>()
    }
}

/// Automatically implemented methods for [`Token`]
pub trait TokenAuto<T: PM> {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
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

pub trait AsToken<T: PM>: AsTokenAuto<T> + 'static + Debug {
    fn as_token(&self) -> &dyn Token<T>;
    fn as_token_mut(&mut self) -> &mut dyn Token<T>;
}

impl<T: PM, X: Clone + Token<T>> AsToken<T> for X {
    #[inline]
    fn as_token(&self) -> &dyn Token<T> {
        self
    }

    #[inline]
    fn as_token_mut(&mut self) -> &mut dyn Token<T> {
        self
    }
}

/// Automatically implemented methods for [`AsToken`]
pub trait AsTokenAuto<T: PM> {
    fn clone_boxed(&self) -> Box<dyn AsToken<T>>;
}

impl<T: PM, X: Clone + AsToken<T>> AsTokenAuto<T> for X {
    #[inline]
    fn clone_boxed(&self) -> Box<dyn AsToken<T>> {
        Box::new(self.clone())
    }
}

pub trait ToTokenTrees<T: TokenTree> {
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
