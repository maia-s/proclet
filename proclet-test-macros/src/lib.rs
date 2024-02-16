#[cfg(not(any(feature = "proc-macro", feature = "proc-macro2")))]
compile_error!("at least one of `proc-macro` or `proc-macro2` must be enabled");

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
use proclet::prelude::*;

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
    use proclet::LiteralValue;
    let input: TokenStream = input.into();
    let mut output = TokenStream::new();
    for mut token in input {
        token.flatten_group();
        if let Some(lit) = token.literal_mut() {
            *lit = LiteralValue::from(lit.clone()).into();
            output.extend(lit.to_token_stream());
        } else {
            panic!("literal_roundtrip only accepts literals");
        }
    }
    let output: TokenStream = output.into();
    output.into()
}

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
#[proc_macro]
#[allow(clippy::useless_conversion)]
pub fn literal_roundtrip_with_parse(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use proclet::{LiteralValue, ToTokenStream, TokenBuffer};
    let input: TokenStream = input.into();
    let input: TokenBuffer<_> = input.into();
    let mut input = input.as_buf();
    let mut output = TokenBuffer::new();
    while !input.is_empty() {
        if let Ok(lit) = LiteralValue::parse(&mut input) {
            let ts: TokenStream = lit.into_token_stream();
            output.extend(ts);
        } else {
            panic!("invalid literal");
        }
    }
    let output: TokenStream = output.into();
    output.into()
}

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
#[proc_macro]
#[allow(clippy::useless_conversion)]
pub fn parse_rust_ops(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use proclet_utils::rust_op_parser;
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
    use proclet::{ProcMacro, TokenBuffer};
    use proclet_utils::rust_op_parser;
    let input: TokenStream = input.into();
    let input: TokenBuffer<_> = input.into();
    let parser = rust_op_parser::<<TokenStream as ProcMacro>::Punct>();
    let mut ops = String::new();
    ops.push('[');
    let mut buf = input.as_buf();
    while !buf.is_empty() {
        if let Ok(op) = parser.parse(&mut buf) {
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

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
#[proc_macro]
#[allow(clippy::useless_conversion)]
pub fn parse_punctuated(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use proclet::{op, punctuated, DelimiterKind, LiteralValue, ProcMacro, TokenBuffer};
    let input: TokenStream = input.into();
    let input: TokenBuffer<_> = input.into();
    let mut input = input.as_buf();
    let args = punctuated(LiteralValue::parser(), op(","))
        .parse(&mut input)
        .unwrap();
    let mut output = TokenStream::new();
    for (lit, comma) in args.iter() {
        let ts: TokenStream = lit.to_token_stream();
        output.extend(ts);
        let ts: TokenStream = comma.to_token_stream();
        output.extend(ts);
    }
    let output: TokenStream = <TokenStream as ProcMacro>::TokenTree::from(
        <TokenStream as ProcMacro>::Group::new(DelimiterKind::Parenthesis.into(), output),
    )
    .into();
    output.into()
}
