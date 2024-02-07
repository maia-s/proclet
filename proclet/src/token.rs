use crate::{TokenStream, PM};
use std::{any::Any, fmt::Debug};

pub trait Token<T: PM>: TokenAuto<T> + Any + Debug + ToTokenStream<T::TokenStream> {
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

pub trait ToTokenStream<T: TokenStream> {
    fn extend_token_stream(&self, token_stream: &mut T);

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
