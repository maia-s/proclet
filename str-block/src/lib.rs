use proc_macro::TokenStream;
use proclet::{delimited, op, prelude::*, Error, StringToken, TokenBuffer};

/// Remove the first line if it's empty except for whitespace, and remove the common whitespace prefix from
/// all lines, if any. Empty lines are treated as if they have the common prefix.
///
/// Call with `{}` like `str_block!{"string"}` to stop rustfmt from modifying your string.
#[proc_macro]
pub fn str_block(input: TokenStream) -> TokenStream {
    let input: TokenBuffer<_> = input.into();
    let mut input = input.as_buf();
    let Ok(strings) = delimited(StringToken::parser(), Some(op(","))).parse_all(&mut input) else {
        return Error::new("str_block takes one or more string or raw string literals as input")
            .to_compile_error();
    };

    // concat input
    let str = strings.into_iter().fold(String::new(), |mut s, item| {
        s.push_str(item.0.value());
        s
    });

    let mut lines = str.lines();
    let mut lines2 = lines.clone();
    let Some(first) = lines.next() else {
        // input was an empty string
        return StringToken::new(String::new()).to_token_stream();
    };
    // skip first line if it's empty
    let first = if first.trim().is_empty() {
        lines2 = lines.clone();
        if let Some(second) = lines.next() {
            second
        } else {
            // input was one line of only whitespace
            return StringToken::new(String::new()).to_token_stream();
        }
    } else {
        first
    };

    let first_trimmed = first.trim_start();
    let mut prefix = &first[..first.len() - first_trimmed.len()];
    if !prefix.is_empty() {
        for line in lines {
            if !line.is_empty() {
                let ci = prefix
                    .chars()
                    .zip(line.chars())
                    .take_while(|(p, l)| p == l)
                    .fold(0, |ci, (p, _)| ci + p.len_utf8());
                if ci < prefix.len() {
                    prefix = &prefix[..ci];
                    if prefix.is_empty() {
                        break;
                    }
                }
            }
        }
    }

    let mut output = String::from(lines2.next().unwrap().strip_prefix(prefix).unwrap_or(""));
    for line in lines2 {
        output.push('\n');
        output.push_str(line.strip_prefix(prefix).unwrap_or(""));
    }
    StringToken::new(output).to_token_stream()
}
