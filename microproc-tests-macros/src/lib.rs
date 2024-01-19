#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
use microproc::Literal;

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
use std::iter;

#[cfg(all(feature = "proc-macro", not(feature = "proc-macro2")))]
use proc_macro::{Delimiter, TokenStream, TokenTree};
#[cfg(all(
    feature = "proc-macro",
    feature = "proc-macro2",
    feature = "prefer-pm1"
))]
use proc_macro::{Delimiter, TokenStream, TokenTree};
#[cfg(all(
    feature = "proc-macro",
    feature = "proc-macro2",
    not(feature = "prefer-pm1")
))]
use proc_macro2::{Delimiter, TokenStream, TokenTree};
#[cfg(all(feature = "proc-macro2", not(feature = "proc-macro")))]
use proc_macro2::{Delimiter, TokenStream, TokenTree};

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
#[proc_macro]
#[allow(clippy::useless_conversion)]
pub fn literal_roundtrip(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: TokenStream = input.into();
    let mut output = TokenStream::new();
    for token in input {
        handle_token(token, &mut output);
    }
    output.into()
}

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
fn handle_token(token: TokenTree, output: &mut TokenStream) {
    match token {
        TokenTree::Literal(lit) => {
            let lit: Literal = lit.into();
            let token: TokenTree = lit.try_into().unwrap();
            output.extend(iter::once(token));
        }

        TokenTree::Group(group) => {
            assert_eq!(group.delimiter(), Delimiter::None);
            for token in group.stream() {
                handle_token(token, output);
            }
        }

        _ => panic!("this macro should only be used with literals"),
    }
}
