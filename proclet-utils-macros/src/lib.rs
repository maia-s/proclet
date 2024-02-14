use proc_macro::TokenStream;
use proclet::{delimited, op, prelude::*, proclet, Error, StringLiteral, TokenBuf};
use std::collections::{hash_map::Entry, HashMap, HashSet};

#[proc_macro]
pub fn _define_ops(input: TokenStream) -> TokenStream {
    proclet(input, _define_ops_)
}

fn _define_ops_(
    input: &mut &TokenBuf<proc_macro::TokenTree>,
) -> Result<TokenStream, Error<proc_macro::Span>> {
    let args = delimited(StringLiteral::parser(), op(",")).parse_all(input)?;

    let mut map = HashMap::<String, (bool, HashSet<char>)>::new();

    for (op, _) in args {
        let str = op.value();
        let clen = str.chars().count();
        if clen == 0 {
            return Err(Error::with_span(op.span(), "empty operator"));
        } else if clen > 1 {
            let mut chars = op.value().chars();
            let mut ci = chars.next().unwrap().len_utf8();
            for ch in chars {
                let s = str[..ci].to_string();
                ci += ch.len_utf8();
                map.entry(s).or_insert((false, HashSet::new())).1.insert(ch);
            }
        }
        match map.entry(op.into_value()) {
            Entry::Occupied(e) => e.into_mut().0 = true,
            Entry::Vacant(e) => {
                e.insert((true, HashSet::new()));
            }
        }
    }

    let mut output = String::from(concat!(
        "const fn __proclet_define_ops",
        "(str: &::core::primitive::str, next: ::core::option::Option<::core::primitive::char>)",
        "-> ::proclet::Match<::std::borrow::Cow<'static, ::core::primitive::str>>",
        "{ use ::proclet::Match; use ::core::option::Option; use ::std::borrow::Cow;",
        "match (str.as_bytes(), next) {"
    ));
    for (str, (valid, follow)) in map {
        let bss = to_byte_string_string(&str);
        if !follow.is_empty() {
            output.push('(');
            output.push_str(&bss);
            output.push_str(", Option::Some(");
            let mut it = follow.into_iter().peekable();
            while let Some(ch) = it.next() {
                output.push_str(&format!("\'\\u{{{:x}}}\'", u32::from(ch)));
                if it.peek().is_some() {
                    output.push('|');
                }
            }
            output.push_str(")) => ");
            if valid {
                output.push_str(&format!("Match::Partial(Cow::Borrowed({:?})),", &str));
            } else {
                output.push_str("Match::NeedMore,")
            }
        }
        if valid {
            output.push('(');
            output.push_str(&bss);
            output.push_str(&format!(
                ", _) => Match::Complete(Cow::Borrowed({:?})),",
                &str
            ));
        }
    }
    output.push_str("_ => Match::NoMatch }}");
    Ok(output
        .parse()
        .expect("internal error: generated invalid code"))
}

fn to_byte_string_string(str: &str) -> String {
    let mut output = String::from("b\"");
    let mut chi = 0;
    for ch in str.chars() {
        let clen = ch.len_utf8();
        for &b in str[chi..chi + clen].as_bytes() {
            output.push_str(&format!("\\x{:02x}", b));
        }
        chi += clen;
    }
    output.push('\"');
    output
}
