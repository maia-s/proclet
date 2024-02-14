use proc_macro::TokenStream;
use proclet::{delimited, op, prelude::*, proclet, Error, Optional, StringLiteral, TokenBuf};

/// Remove the first line if it's empty except for whitespace, and remove the common whitespace prefix from
/// all lines, if any. Empty lines are treated as if they have the common prefix.
///
/// Call with `{}` like `str_block!{"string"}` to stop rustfmt from modifying your string.
#[proc_macro]
pub fn str_block(input: TokenStream) -> TokenStream {
    proclet(input, str_block_)
}

fn str_block_(
    input: &mut &TokenBuf<proc_macro::TokenTree>,
) -> Result<StringLiteral<proc_macro::Span>, Error<proc_macro::Span>> {
    let strings = delimited(StringLiteral::parser(), Optional(op(","))).parse_all(input)?;

    // concat input
    let str = strings.into_iter().fold(String::new(), |mut s, item| {
        s.push_str(item.0.value());
        s
    });

    let mut lines = str.lines();
    let mut lines2 = lines.clone();
    let Some(first) = lines.next() else {
        // input was an empty string
        return Ok(StringLiteral::new(String::new()));
    };
    // skip first line if it's empty
    let first = if first.trim().is_empty() {
        lines2 = lines.clone();
        if let Some(second) = lines.next() {
            second
        } else {
            // input was one line of only whitespace
            return Ok(StringLiteral::new(String::new()));
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
    Ok(StringLiteral::new(output))
}
