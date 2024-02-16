use proc_macro::TokenStream;
use proclet::{op, pm1::StringLiteral, prelude::*, proclet, punctuated, Optional};

/// Remove the first line if it's empty except for whitespace, and remove the common whitespace prefix from
/// all lines, if any. Empty lines are treated as if they have the common prefix.
///
/// Call with `{}` like `str_block!{"string"}` to stop rustfmt from modifying your string.
#[proc_macro]
pub fn str_block(input: TokenStream) -> TokenStream {
    proclet(input, |input| {
        let strings = punctuated(StringLiteral::parser(), Optional(op(","))).parse_all(input)?;

        // concat input
        let str: String = strings.into_iter().map(|(s, _)| s.into_value()).collect();

        let mut lines = str.lines();
        let mut lines2 = lines.clone();
        let Some(first) = lines.next() else {
            // input was an empty string
            return Ok(StringLiteral::new(String::new()));
        };
        // skip first line if it's empty
        let first = if first.trim().is_empty() {
            let _ = lines2.next();
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
    })
}
