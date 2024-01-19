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

        check!(b'a');
        check!(b'\'');
        check!(b'\"');
        check!(b'\\');
        check!(b'\0');
        check!(b'\n');
        check!(b'\r');
        check!(b'\t');
        check!(b'\xff');

        check!(b"a byte string");
        check!(b"a byte string with escapes: \' \" \\ \0 \n \r \t \xff");
        check!(
            b"a byte string with an escaped newline\


            !"
        );

        check!(127_i8);
        check!(32767_i16);
        check!(2147483647_i32);
        check!(9223372036854775807_i64);
        check!(170141183460469231731687303715884105727_i128);

        check!(255_u8);
        check!(65535_u16);
        check!(4294967295_u32);
        check!(18446744073709551615_u64);
        check!(340282366920938463463374607431768211455_u128);

        check!(0.5_f32);
        check!(0.5_f64);

        check!(0b1010);
        check!(0o1234);
        check!(0x1234);
        check!(2147483647);

        check!(0b1010_i8);
        check!(0o1234_u16);
        check!(0x1234_usize);
        check!(2147483647_f64);
    }
}
