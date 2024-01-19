#[cfg(test)]
mod tests {
    use microproc_tests_macros::literal_roundtrip;

    macro_rules! check {
        ($($lit:literal),*) => { $(
            let lit2 = literal_roundtrip!($lit);
            assert_eq!($lit, lit2);
        )* };
    }

    #[test]
    fn test_parse() {
        check!('a');
        check!('\'');
        check!('\"');
        check!('\\');
        check!('\0');
        check!('\n');
        check!('\r');
        check!('\t');
        check!('\x7f');
        check!("a string");
        check!("a string with escapes: \' \" \\ \0 \n \r \t \x7f");
        check!(
            "a string with an escaped newline\


            !"
        );
    }
}
