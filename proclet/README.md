# proclet: proc macros made easy

> ⚠️ `proclet` is still in early development. It's missing some basic features and may
> get major design changes, and documentation is a work in progress.

`proclet` can be used with either proc-macro or proc-macro2, or both. Most of the types
of the proc-macro crates are abstracted into traits, and `proclet`'s types are generic
over these traits. If you run into type inference issues, there's proc-macro specific
aliases for the proclet types in the `pm1` and `pm2` modules.

Here's how you'd make a proc macro that takes a set of comma separated strings as arguments
(last comma optional):

```rust
#[proc_macro]
pub fn my_proc_macro(input: TokenStream) -> TokenStream {
    proclet(input, |input| {
        let args = punctuated(StringLiteral::parser(), op(",")).parse_all(input)?;
        // ...
    })
}
```

The `proclet` function is an optional wrapper that converts the input to a `TokenBuf` ready
for parsing, converts the output back into a `TokenStream`, and handles errors by making them
nice spanned compiler errors instead of panics.

`parse_all` returns an error if there's tokens left in the buffer after parsing. To leave
the rest of the buffer for the next parser to parse, use the `parse` method instead.

You can combine parsers to parse more complex objects like `punctuated` does in the example
above. Types that implement the `Parse` trait can be parsed directly:

```rust
let string = StringLiteral::parse(input)?;
```

The `input` is automatically advanced to point past the parsed object on success.
