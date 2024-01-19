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
        check!("a string");
        check!("a string with escapes: \' \" \\ \0 \n \r \t");
        check!(
            "a string with an escaped newline\


            !"
        );
    }
}
