#[cfg(not(any(feature = "proc-macro", feature = "proc-macro2")))]
compile_error!("at least one of `proc-macro` or `proc-macro2` must be enabled");

#[cfg(all(test, any(feature = "proc-macro", feature = "proc-macro2")))]
mod tests {
    use microproc_test_macros::{literal_roundtrip, parse_rust_ops, parse_rust_ops_with_buffer};

    macro_rules! test_literal {
        ($($lit:literal),*) => { $(
            let lit2 = literal_roundtrip!($lit);
            assert_eq!(lit2, $lit);
        )* };
    }

    macro_rules! test_ops {
        ([$($punct:tt)*] $expected:tt) => {
            let parsed = parse_rust_ops!($($punct)*);
            assert_eq!(parsed, $expected);

            let parsed_buf = parse_rust_ops_with_buffer!($($punct)*);
            assert_eq!(parsed_buf, $expected);
        };
    }

    #[cfg(all(feature = "proc-macro", not(feature = "proc-macro2")))]
    macro_rules! tests { ($($tt:tt)*) => { mod with_proc_macro { use super::*; $($tt)* } }; }

    #[cfg(all(feature = "proc-macro2", not(feature = "proc-macro")))]
    macro_rules! tests { ($($tt:tt)*) => { mod with_proc_macro2 { use super::*; $($tt)* } }; }

    #[cfg(all(
        feature = "proc-macro",
        feature = "proc-macro2",
        feature = "prefer-pm1"
    ))]
    macro_rules! tests { ($($tt:tt)*) => { mod with_proc_macro_over_proc_macro2 { use super::*; $($tt)* } }; }

    #[cfg(all(
        feature = "proc-macro",
        feature = "proc-macro2",
        not(feature = "prefer-pm1")
    ))]
    macro_rules! tests { ($($tt:tt)*) => { mod with_proc_macro2_over_proc_macro { use super::*; $($tt)* } }; }

    tests! {
        #[test]
        fn test_parse_literals() {
            test_literal!('a');
            test_literal!('æ');
            test_literal!('✨');
            test_literal!('\'');
            test_literal!('\"');
            test_literal!('\\');
            test_literal!('\0');
            test_literal!('\n');
            test_literal!('\r');
            test_literal!('\t');
            test_literal!('\x7f');

            test_literal!("a string");
            test_literal!("a string with escapes: \' \" \\ \0 \n \r \t \x7f \u{0} \u{2728} \u{10ffff}");
            test_literal!(
                "a string with an escaped newline\


                !"
            );
            test_literal!(r"raw string");
            test_literal!(r#""raw string\n""#);
            test_literal!("✨🧚‍♀️✨");

            test_literal!(b'a');
            test_literal!(b'\'');
            test_literal!(b'\"');
            test_literal!(b'\\');
            test_literal!(b'\0');
            test_literal!(b'\n');
            test_literal!(b'\r');
            test_literal!(b'\t');
            test_literal!(b'\xff');

            test_literal!(b"a byte string");
            test_literal!(b"a byte string with escapes: \' \" \\ \0 \n \r \t \xff");
            test_literal!(
                b"a byte string with an escaped newline\


                !"
            );
            test_literal!(br"raw byte string");
            test_literal!(br#""raw byte string\n""#);

            test_literal!(127_i8);
            test_literal!(32767_i16);
            test_literal!(2147483647_i32);
            test_literal!(9223372036854775807_i64);
            test_literal!(170141183460469231731687303715884105727_i128);

            test_literal!(255_u8);
            test_literal!(65535_u16);
            test_literal!(4294967295_u32);
            test_literal!(18446744073709551615_u64);
            test_literal!(340282366920938463463374607431768211455_u128);

            test_literal!(0.5_f32);
            test_literal!(0.5_f64);

            test_literal!(0b1010);
            test_literal!(0o1234);
            test_literal!(0x1234);
            test_literal!(2147483647);

            test_literal!(0b1010_i8);
            test_literal!(0o1234_u16);
            test_literal!(0x1234_usize);
            test_literal!(2147483647_f64);
        }

        #[test]
        fn test_parse_ops() {
            test_ops!([+] ["+"]);
            test_ops!([++] ["+", "+"]);
            test_ops!([+ +] ["+", "+"]);
            test_ops!([+++] ["+", "+", "+"]);
            test_ops!([++ +] ["+", "+", "+"]);
            test_ops!([+ ++] ["+", "+", "+"]);
            test_ops!([++++] ["+", "+", "+", "+"]);
            test_ops!([+++ +] ["+", "+", "+", "+"]);
            test_ops!([++ ++] ["+", "+", "+", "+"]);
            test_ops!([+ +++] ["+", "+", "+", "+"]);

            test_ops!([=] ["="]);
            test_ops!([==] ["=="]);
            test_ops!([= =] ["=", "="]);
            test_ops!([===] ["==", "="]);
            test_ops!([== =] ["==", "="]);
            test_ops!([= ==] ["=", "=="]);
            test_ops!([====] ["==", "=="]);
            test_ops!([=== =] ["==", "=", "="]);
            test_ops!([== ==] ["==", "=="]);
            test_ops!([= ===] ["=", "==", "="]);

            test_ops!([.] ["."]);
            test_ops!([..] [".."]);
            test_ops!([. .] [".", "."]);
            test_ops!([...] ["..."]);
            test_ops!([.. .] ["..", "."]);
            test_ops!([. ..] [".", ".."]);
            test_ops!([....] ["...", "."]);
            test_ops!([... .] ["...", "."]);
            test_ops!([.. ..] ["..", ".."]);
            test_ops!([. ...] [".", "..."]);

            test_ops!([=+==+===+] ["=", "+=", "=", "+=", "==", "+"]);

            test_ops!(
                [!!=#$%%=&&&=&**=++=,--=->..=...../=/.:::;<<=<<<=<-<>>=>>>=>?===>=@^^=|||=|~]
                [
                    "!", "!=", "#", "$", "%", "%=", "&&", "&=", "&", "*", "*=", "+", "+=", ",",
                    "-", "-=", "->", "..=", "...", "..", "/=", "/", ".", "::", ":", ";", "<<=",
                    "<<", "<=", "<-", "<", ">>=", ">>", ">=", ">", "?", "==", "=>", "=", "@",
                    "^", "^=", "||", "|=", "|", "~"
                ]
            );
        }
    }
}
