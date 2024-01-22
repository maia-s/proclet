#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
use microproc::prelude::*;

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
use std::iter;

#[cfg(all(feature = "proc-macro", not(feature = "proc-macro2")))]
use proc_macro::TokenStream;
#[cfg(all(
    feature = "proc-macro",
    feature = "proc-macro2",
    feature = "prefer-pm1"
))]
use proc_macro::TokenStream;
#[cfg(all(
    feature = "proc-macro",
    feature = "proc-macro2",
    not(feature = "prefer-pm1")
))]
use proc_macro2::TokenStream;
#[cfg(all(feature = "proc-macro2", not(feature = "proc-macro")))]
use proc_macro2::TokenStream;

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
#[proc_macro]
#[allow(clippy::useless_conversion)]
pub fn literal_roundtrip(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: TokenStream = input.into();
    let mut output = TokenStream::new();
    for mut token in input {
        token.flatten_group();
        if let Some(lit) = token.literal_mut() {
            lit.set_value(lit.value());
            output.extend(iter::once(lit.to_token_tree()));
        } else {
            panic!("literal_roundtrip only accepts literals");
        }
    }
    output.into()
}
