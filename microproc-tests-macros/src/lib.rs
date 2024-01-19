use microproc::Literal;
use proc_macro::{Delimiter, TokenStream, TokenTree};
use std::iter;

#[proc_macro]
pub fn literal_roundtrip(input: TokenStream) -> TokenStream {
    let mut output = TokenStream::new();
    for token in input {
        handle_token(token, &mut output);
    }
    output
}

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
