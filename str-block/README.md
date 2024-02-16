# str-block

`str_block` is a proc macro for processing string literals. It removes the first line from
its input if it's empty except for whitespace, and removes common indentation from the rest
of the lines. Lines that are empty except for whitespace are treated as if they have the
common indentation.

```rust
# use str_block::str_block;
assert_eq!(str_block! {"
    Hello
    World
"}, "Hello\nWorld\n");
```

Use with `{}` to stop rustfmt from moving around your string.

You can pass multiple string literals to `str_block`, and it'll concatenate them for you
before processing the result. You can also pass raw string literals.
