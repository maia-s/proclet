#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
use microproc::{LiteralExt, TokenTreeExt};

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
use std::iter;

#[cfg(all(feature = "proc-macro", not(feature = "proc-macro2")))]
use proc_macro::{TokenStream, TokenTree};
#[cfg(all(
    feature = "proc-macro",
    feature = "proc-macro2",
    feature = "prefer-pm1"
))]
use proc_macro::{TokenStream, TokenTree};
#[cfg(all(
    feature = "proc-macro",
    feature = "proc-macro2",
    not(feature = "prefer-pm1")
))]
use proc_macro2::{TokenStream, TokenTree};
#[cfg(all(feature = "proc-macro2", not(feature = "proc-macro")))]
use proc_macro2::{TokenStream, TokenTree};

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
#[proc_macro]
#[allow(clippy::useless_conversion)]
pub fn literal_roundtrip(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: TokenStream = input.into();
    let mut output = TokenStream::new();
    for mut token in input {
        token.flatten_group();
        if let TokenTree::Literal(mut lit) = token {
            lit.set_value(lit.value());
            let tokens: TokenTree = lit.into();
            output.extend(iter::once(tokens));
        }
    }
    output.into()
}
