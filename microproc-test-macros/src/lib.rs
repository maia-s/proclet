#[cfg(not(any(feature = "proc-macro", feature = "proc-macro2")))]
compile_error!("at least one of `proc-macro` or `proc-macro2` must be enabled");

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
use microproc::prelude::*;

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
    use std::iter;

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

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
#[proc_macro]
#[allow(clippy::useless_conversion)]
pub fn parse_rust_ops(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use microproc::ops::rust_op_parser;
    let input: TokenStream = input.into();
    let mut parser = rust_op_parser().create();
    let mut ops = String::new();
    ops.push('[');
    for mut token in input {
        token.flatten_group();
        if let Some(punct) = token.into_punct() {
            match parser.apply(punct) {
                Some(Ok(op)) => {
                    ops.push_str(&format!("{:?}", op.as_str()));
                    ops.push(',');
                }
                Some(Err(e)) => panic!("{e}"),
                None => (),
            }
        } else {
            panic!("parse_ops only accepts puncts");
        }
    }
    match parser.finish() {
        Some(Ok(op)) => {
            ops.push_str(&format!("{:?}", op.as_str()));
            ops.push(',');
        }
        Some(Err(e)) => panic!("{e}"),
        None => (),
    }
    ops.push(']');
    let output: TokenStream = ops.parse().unwrap();
    output.into()
}

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
#[proc_macro]
#[allow(clippy::useless_conversion)]
pub fn parse_rust_ops_with_buffer(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use microproc::{ops::rust_op_parser, ProcMacro, TokenBuffer};
    let input: TokenStream = input.into();
    let input: TokenBuffer<_> = input.into();
    let parser = rust_op_parser::<<TokenStream as ProcMacro>::Punct>();
    let mut ops = String::new();
    ops.push('[');
    let mut buf = input.as_buf();
    while !buf.is_empty() {
        if let Some(op) = parser.parse(&mut buf) {
            ops.push_str(&format!("{:?}", op.as_str()));
            ops.push(',');
        } else {
            dbg!(&buf);
            panic!("invalid op");
        }
    }
    ops.push(']');
    let output: TokenStream = ops.parse().unwrap();
    output.into()
}
