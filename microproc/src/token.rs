pub trait Token {}

macro_rules! define_tokens {
    ($($token:ident[$($tt:tt)*]),* $(,)?) => {
        $(
            #[derive(Clone, Copy, Default)]
            pub struct $token;

            impl Token for $token {}

            #[cfg(feature = "proc-macro")]
            impl From<$token> for proc_macro::TokenStream {
                #[inline]
                fn from(_: $token) -> Self {
                    stringify!($($tt)*).parse().unwrap()
                }
            }

            #[cfg(feature = "proc-macro2")]
            impl From<$token> for proc_macro2::TokenStream {
                #[inline]
                fn from(_: $token) -> Self {
                    stringify!($($tt)*).parse().unwrap()
                }
            }

            #[cfg(feature = "proc-macro")]
            impl From<$token> for proc_macro::token_stream::IntoIter {
                #[inline]
                fn from(value: $token) -> Self {
                    let ts: proc_macro::TokenStream = value.into();
                    ts.into_iter()
                }
            }

            #[cfg(feature = "proc-macro2")]
            impl From<$token> for proc_macro2::token_stream::IntoIter {
                #[inline]
                fn from(value: $token) -> Self {
                    let ts: proc_macro2::TokenStream = value.into();
                    ts.into_iter()
                }
            }

            #[cfg(feature = "quote")]
            impl quote::ToTokens for $token {
                #[inline]
                fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                    let ts: proc_macro2::TokenStream = (*self).into();
                    tokens.extend(ts)
                }
            }
        )*

        #[macro_export]
        macro_rules! Token {
            $( ($($tt)*) => { $crate::token::$token }; )*
        }
    };
}

define_tokens! {
    Comma[,],
    Semi[;],
}
