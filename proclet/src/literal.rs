use crate::{IntoTokens, Parse, ProcMacro, ProcMacroExt, ToTokenStream};
use std::{fmt::Display, str::FromStr};

#[cfg(feature = "literal-value")]
/// A literal token. This is like `Literal` from `proc-macro*`, except that the value has
/// already been parsed and is available at no cost. You can convert it to and from `Literal`
/// with `into`.
#[derive(Clone, Debug)]
pub enum LiteralValue<S: crate::Span> {
    /// String literal.
    String(StringLiteral<S>),

    /// Byte string literal.
    ByteString(ByteStringLiteral<S>),

    /// Character literal.
    Character(CharacterLiteral<S>),

    /// Byte character literal.
    ByteCharacter(ByteCharacterLiteral<S>),

    /// Unsuffixed integer literal.
    Int(IntLiteral<S>),

    /// Unsuffixed floating point literal.
    Float(FloatLiteral<S>),

    /// `i8` suffixed integer literal.
    I8(I8Literal<S>),

    /// `i16` suffixed integer literal.
    I16(I16Literal<S>),

    /// `i32` suffixed integer literal.
    I32(I32Literal<S>),

    /// `i64` suffixed integer literal.
    I64(I64Literal<S>),

    /// `i128` suffixed integer literal.
    I128(I128Literal<S>),

    /// `isize` suffixed integer literal.
    Isize(IsizeLiteral<S>),

    /// `u8` suffixed integer literal.
    U8(U8Literal<S>),

    /// `u16` suffixed integer literal.
    U16(U16Literal<S>),

    /// `u32` suffixed integer literal.
    U32(U32Literal<S>),

    /// `u64` suffixed integer literal.
    U64(U64Literal<S>),

    /// `u128` suffixed integer literal.
    U128(U128Literal<S>),

    /// `usize` suffixed integer literal.
    Usize(UsizeLiteral<S>),

    /// `f32` suffixed floating point literal.
    F32(F32Literal<S>),

    /// `f64` suffixed floating point literal.
    F64(F64Literal<S>),
}

#[cfg(feature = "literal-value")]
impl<S: crate::Span> FromStr for LiteralValue<S> {
    type Err = crate::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut input = s.as_bytes();

        #[derive(Clone, Copy)]
        enum Escapes {
            Char,
            String,
        }

        fn bin_digit(b: u8) -> Result<u8, crate::Error> {
            match b {
                b'0'..=b'1' => Ok(b - b'0'),
                _ => Err(crate::Error::new("expected binary digit")),
            }
        }

        fn oct_digit(b: u8) -> Result<u8, crate::Error> {
            match b {
                b'0'..=b'7' => Ok(b - b'0'),
                _ => Err(crate::Error::new("expected octal digit")),
            }
        }

        fn hex_digit(b: u8) -> Result<u8, crate::Error> {
            match b {
                b'0'..=b'9' => Ok(b - b'0'),
                b'a'..=b'f' => Ok(b - b'a' + 10),
                b'A'..=b'F' => Ok(b - b'A' + 10),
                _ => Err(crate::Error::new("expected hexadecimal digit")),
            }
        }

        fn from_int<S: crate::Span>(
            value: u128,
            suffix: &[u8],
        ) -> Result<LiteralValue<S>, crate::Error> {
            macro_rules! make {
                ($($s:literal => $t:ident: $token:ident $(: $as:ident)?),* $(,)?) => {
                    match suffix {
                        $(
                            $s => Ok(LiteralValue::$t($token::new(
                                (value $(as $as)?)
                                    .try_into()
                                    .map_err(|_| crate::Error::new("suffixed literal out of range"))?,
                            ))),
                        )*

                        b"" => Ok(LiteralValue::Int(IntLiteral::new(value))),

                        _ => unreachable!(),
                    }
                };
            }
            make! {
                b"u8" => U8: U8Literal,
                b"u16" => U16: U16Literal,
                b"u32" => U32: U32Literal,
                b"u64" => U64: U64Literal,
                b"u128" => U128: U128Literal,
                b"usize" => Usize: UsizeLiteral,
                b"i8" => I8: I8Literal,
                b"i16" => I16: I16Literal,
                b"i32" => I32: I32Literal,
                b"i64" => I64: I64Literal,
                b"i128" => I128: I128Literal,
                b"isize" => Isize: IsizeLiteral,
                b"f32" => F32: F32Literal: f32,
                b"f64" => F64: F64Literal: f64,
            }
        }

        fn parse_suffix<'a>(input: &mut &'a [u8], include_float: bool) -> &'a [u8] {
            if input.len() >= 2 {
                let suffix = &input[input.len() - 2..];
                match suffix {
                    b"u8" | b"i8" => {
                        *input = &input[..input.len() - 2];
                        return suffix;
                    }
                    _ => (),
                }
                if input.len() >= 3 {
                    let suffix = &input[input.len() - 3..];
                    match suffix {
                        b"u16" | b"u32" | b"u64" | b"i16" | b"i32" | b"i64" => {
                            *input = &input[..input.len() - 3];
                            return suffix;
                        }
                        b"f32" | b"f64" if include_float => {
                            *input = &input[..input.len() - 3];
                            return suffix;
                        }
                        _ => (),
                    }
                    if input.len() >= 4 {
                        let suffix = &input[input.len() - 4..];
                        match suffix {
                            b"u128" | b"i128" => {
                                *input = &input[..input.len() - 4];
                                return suffix;
                            }
                            _ => (),
                        }
                        if input.len() >= 5 {
                            let suffix = &input[input.len() - 5..];
                            match suffix {
                                b"usize" | b"isize" => {
                                    *input = &input[..input.len() - 5];
                                    return suffix;
                                }
                                _ => (),
                            }
                        }
                    }
                }
            }
            &input[0..0]
        }

        fn parse_byte_escape(
            input: &mut &[u8],
            escapes: Escapes,
        ) -> Result<Option<u8>, crate::Error> {
            assert_eq!(input[0], b'\\');
            if input.len() >= 2 {
                let escape = input[1];
                *input = &input[2..];
                match escape {
                    b'\'' => Ok(Some(b'\'')),
                    b'\"' => Ok(Some(b'\"')),
                    b'\\' => Ok(Some(b'\\')),
                    b'0' => Ok(Some(b'\0')),
                    b'n' => Ok(Some(b'\n')),
                    b'r' => Ok(Some(b'\r')),
                    b't' => Ok(Some(b'\t')),
                    b'\n' if matches!(escapes, Escapes::String) => Ok(None),
                    b'x' if input.len() >= 2 => {
                        let value = hex_digit(input[0])? << 4 | hex_digit(input[1])?;
                        *input = &input[2..];
                        Ok(Some(value))
                    }
                    _ => Err(crate::Error::new("unrecognized byte escape")),
                }
            } else {
                Err(crate::Error::new("unrecognized byte escape"))
            }
        }

        fn parse_byte(input: &mut &[u8], escapes: Escapes) -> Result<Option<u8>, crate::Error> {
            if let Some(&value) = input.first() {
                if value == b'\\' {
                    Ok(parse_byte_escape(input, escapes)?)
                } else {
                    *input = &input[1..];
                    Ok(Some(value))
                }
            } else {
                Err(crate::Error::new("expected byte"))
            }
        }

        fn parse_char_escape(
            input: &mut &[u8],
            escapes: Escapes,
        ) -> Result<Option<char>, crate::Error> {
            assert_eq!(input[0], b'\\');
            if input.len() >= 2 {
                let escape = input[1];
                *input = &input[2..];
                match escape {
                    b'\'' => Ok(Some('\'')),
                    b'\"' => Ok(Some('\"')),
                    b'\\' => Ok(Some('\\')),
                    b'0' => Ok(Some('\0')),
                    b'n' => Ok(Some('\n')),
                    b'r' => Ok(Some('\r')),
                    b't' => Ok(Some('\t')),
                    b'\n' if matches!(escapes, Escapes::String) => Ok(None),
                    b'x' if input.len() >= 2 => {
                        let value = oct_digit(input[0])? << 4 | hex_digit(input[1])?;
                        *input = &input[2..];
                        Ok(Some(char::from(value)))
                    }
                    b'u' if input.len() > 2 && input[0] == b'{' => {
                        *input = &input[1..];
                        let mut value: u32 = 0;
                        while !input.is_empty() && input[0] != b'}' {
                            value = value
                                .checked_shl(4)
                                .ok_or(crate::Error::new("invalid unicode escape"))?
                                | hex_digit(input[0])? as u32;
                            *input = &input[1..];
                        }
                        if input[0] == b'}' {
                            *input = &input[1..];
                            Ok(Some(
                                char::from_u32(value)
                                    .ok_or(crate::Error::new("invalid unicode escape"))?,
                            ))
                        } else {
                            Err(crate::Error::new("expected `}`"))
                        }
                    }
                    _ => Err(crate::Error::new("unrecognized character escape")),
                }
            } else {
                Err(crate::Error::new("unexpected end of input after escape"))
            }
        }

        fn parse_char(input: &mut &[u8], escapes: Escapes) -> Result<Option<char>, crate::Error> {
            if !input.is_empty() && input[0] == b'\\' {
                if let Some(c) = parse_char_escape(input, escapes)? {
                    Ok(Some(c))
                } else if matches!(escapes, Escapes::String) {
                    Ok(None)
                } else {
                    Err(crate::Error::new("unrecognized character escape"))
                }
            } else if !input.is_empty() {
                // the input is known valid utf-8 so we can skip some checks
                match input[0] {
                    value @ 0x00..=0x7f => {
                        *input = &input[1..];
                        Ok(Some(char::from(value)))
                    }
                    0xc0..=0xdf => {
                        let value = ((input[0] & 0x1f) as u32) << 6 | (input[1] & 0x3f) as u32;
                        *input = &input[2..];
                        Ok(Some(char::from_u32(value).unwrap()))
                    }
                    0xe0..=0xef => {
                        let value = (((input[0] & 0x0f) as u32) << 6 | (input[1] & 0x3f) as u32)
                            << 6
                            | (input[2] & 0x3f) as u32;
                        *input = &input[3..];
                        Ok(Some(char::from_u32(value).unwrap()))
                    }
                    0xf0..=0xf7 => {
                        let value = ((((input[0] & 0x07) as u32) << 6 | (input[1] & 0x3f) as u32)
                            << 6
                            | (input[2] & 0x3f) as u32)
                            << 6
                            | (input[3] & 0x3f) as u32;
                        *input = &input[4..];
                        Ok(Some(char::from_u32(value).unwrap()))
                    }
                    _ => unreachable!(),
                }
            } else {
                Err(crate::Error::new("expected character"))
            }
        }

        match input[0] {
            b'\'' => {
                if input[input.len() - 1] == b'\'' {
                    Ok(LiteralValue::Character(CharacterLiteral::new(
                        parse_char(&mut &input[1..input.len() - 1], Escapes::Char)?.unwrap(),
                    )))
                } else {
                    Err(crate::Error::new("unterminated character literal"))
                }
            }

            b'\"' => {
                if input[input.len() - 1] == b'\"' {
                    input = &input[1..input.len() - 1];
                    let mut s = String::new();
                    while !input.is_empty() {
                        if let Some(c) = parse_char(&mut input, Escapes::String)? {
                            s.push(c);
                        } else {
                            while !input.is_empty() && input[0].is_ascii_whitespace() {
                                input = &input[1..];
                            }
                        }
                    }
                    Ok(LiteralValue::String(StringLiteral::new(s)))
                } else {
                    Err(crate::Error::new("unterminated string literal"))
                }
            }

            b'r' => {
                let mut s = &s[1..];
                while let Some(ss) = s.strip_prefix('#') {
                    s = ss
                        .strip_suffix('#')
                        .ok_or(crate::Error::new("unmatched `#` in raw string literal"))?;
                }
                let s = s
                    .strip_prefix('"')
                    .and_then(|s| s.strip_suffix('"'))
                    .ok_or(crate::Error::new("unterminated raw string literal"))?;
                Ok(LiteralValue::String(StringLiteral::new(s.to_owned())))
            }

            b'b' => {
                input = &input[1..];
                if !input.is_empty() {
                    match input[0] {
                        b'\'' => {
                            if input.len() > 1 && input[input.len() - 1] == b'\'' {
                                Ok(LiteralValue::ByteCharacter(ByteCharacterLiteral::new(
                                    parse_byte(&mut &input[1..input.len() - 1], Escapes::Char)?
                                        .unwrap(),
                                )))
                            } else {
                                Err(crate::Error::new("unterminated byte character literal"))
                            }
                        }

                        b'\"' => {
                            if input.len() > 1 && input[input.len() - 1] == b'\"' {
                                input = &input[1..input.len() - 1];
                                let mut s = Vec::new();
                                while !input.is_empty() {
                                    if let Some(c) = parse_byte(&mut input, Escapes::String)? {
                                        s.push(c);
                                    } else {
                                        while !input.is_empty() && input[0].is_ascii_whitespace() {
                                            input = &input[1..];
                                        }
                                    }
                                }
                                Ok(LiteralValue::ByteString(ByteStringLiteral::new(s)))
                            } else {
                                Err(crate::Error::new("unterminated byte string literal"))
                            }
                        }

                        b'r' => {
                            input = &input[1..];
                            while input.len() > 1
                                && input[0] == b'#'
                                && input[input.len() - 1] == b'#'
                            {
                                input = &input[1..input.len() - 1];
                            }
                            if input.len() > 1 && input[0] == b'"' && input[input.len() - 1] == b'"'
                            {
                                Ok(LiteralValue::ByteString(ByteStringLiteral::new(
                                    input[1..input.len() - 1].to_owned(),
                                )))
                            } else {
                                Err(crate::Error::new("unterminated raw byte string literal"))
                            }
                        }

                        _ => Err(crate::Error::new("unrecognized literal prefix")),
                    }
                } else {
                    Err(crate::Error::new("unexpected end of input after `b`"))
                }
            }

            b'0'..=b'9' => {
                if input[0] == b'0' && input.len() > 1 {
                    match input[1] {
                        b'b' | b'B' => {
                            let mut input = &input[2..];
                            let suffix = parse_suffix(&mut input, true);
                            if !input.is_empty() {
                                let mut value: u128 = bin_digit(input[0])?.into();
                                for &digit in &input[1..] {
                                    if digit != b'_' {
                                        value = value
                                            .checked_shl(1)
                                            .ok_or(crate::Error::new("literal value overflow"))?
                                            | bin_digit(digit)? as u128;
                                    }
                                }
                                return from_int(value, suffix);
                            } else {
                                return Err(crate::Error::new("missing value after `0b` prefix"));
                            }
                        }

                        b'o' | b'O' => {
                            let mut input = &input[2..];
                            let suffix = parse_suffix(&mut input, true);
                            if !input.is_empty() {
                                let mut value: u128 = oct_digit(input[0])?.into();
                                for &digit in &input[1..] {
                                    if digit != b'_' {
                                        value = value
                                            .checked_shl(3)
                                            .ok_or(crate::Error::new("literal value overflow"))?
                                            | oct_digit(digit)? as u128;
                                    }
                                }
                                return from_int(value, suffix);
                            } else {
                                return Err(crate::Error::new("missing value after `0o` prefix"));
                            }
                        }

                        b'x' | b'X' => {
                            let mut input = &input[2..];
                            let suffix = parse_suffix(&mut input, false);
                            if !input.is_empty() {
                                let mut value: u128 = hex_digit(input[0])?.into();
                                for &digit in &input[1..] {
                                    if digit != b'_' {
                                        value = value
                                            .checked_shl(4)
                                            .ok_or(crate::Error::new("literal value overflow"))?
                                            | hex_digit(digit)? as u128;
                                    }
                                }
                                return from_int(value, suffix);
                            } else {
                                return Err(crate::Error::new("missing value after `0x` prefix"));
                            }
                        }

                        _ => (),
                    }
                }

                let suffix = parse_suffix(&mut input, true);
                let mut is_float = false;

                // parse doesn't accept '_'s
                let s: String = input
                    .iter()
                    .filter_map(|&b| {
                        if matches!(b, b'.' | b'e' | b'E' | b'+' | b'-') {
                            is_float = true;
                        }
                        if b != b'_' {
                            Some(char::from(b))
                        } else {
                            None
                        }
                    })
                    .collect();

                if is_float {
                    if suffix == b"f32" {
                        let value: f32 = s
                            .parse()
                            .map_err(|_| crate::Error::new("failed to parse f32"))?;
                        Ok(LiteralValue::F32(F32Literal::new(value)))
                    } else {
                        let value: f64 = s
                            .parse()
                            .map_err(|_| crate::Error::new("failed to parse f64"))?;
                        if suffix == b"f64" {
                            Ok(LiteralValue::F64(F64Literal::new(value)))
                        } else {
                            Ok(LiteralValue::Float(FloatLiteral::new(value)))
                        }
                    }
                } else {
                    from_int(
                        s.parse()
                            .map_err(|_| crate::Error::new("failed to parse u128"))?,
                        suffix,
                    )
                }
            }

            _ => Err(crate::Error::new("unrecognized literal")),
        }
    }
}

#[cfg(feature = "literal-value")]
impl<T: crate::TokenTreeExt> Parse<T> for LiteralValue<T::Span> {
    #[inline]
    fn parse(buf: &mut &crate::TokenBuf<T>) -> Option<Self> {
        T::Literal::parse(buf).map(|x| x.into())
    }
}

#[cfg(feature = "literal-value")]
impl<T: crate::TokenTreeExt> crate::IntoTokens<T> for LiteralValue<T::Span> {
    #[inline]
    fn into_tokens(self) -> impl Iterator<Item = crate::TokenObject<T>> {
        T::Literal::from(self).into_tokens()
    }
}

#[cfg(feature = "literal-value")]
impl<T: crate::TokenStreamExt> crate::ToTokenStream<T> for LiteralValue<T::Span> {
    #[inline]
    fn extend_token_stream(&self, token_stream: &mut T) {
        token_stream.extend([T::TokenTree::from(T::Literal::from(self.clone()))])
    }
}

macro_rules! def_literal_tokens {
    ($($(#[$attr:meta])* $ident:ident: $variant:ident: $t:ty),* $(,)?) => { $(
        #[cfg(feature = "literal-value")]
        $( #[$attr] )*
        #[derive(Clone, Debug)]
        pub struct $ident<S: crate::Span> {
            value: $t,
            span: S,
        }

        #[cfg(feature = "literal-value")]
        impl<S: crate::Span> $ident<S> {
            #[doc = concat!("Create a new `", stringify!($ident), "`.")]
            #[inline]
            pub fn new(value: $t) -> Self {
                Self {
                    value,
                    span: S::call_site(),
                }
            }

            #[doc = concat!("Create a new `", stringify!($ident), "` with a custom span.")]
            #[inline]
            pub const fn with_span(value: $t, span: S) -> Self {
                Self { value, span }
            }

            /// Get a reference to the value of this token.
            #[inline]
            pub const fn value(&self) -> &$t {
                &self.value
            }

            /// Get a mutable reference to the value of this token.
            #[inline]
            pub fn value_mut(&mut self) -> &mut $t {
                &mut self.value
            }

            /// Get the value of this token.
            pub fn into_value(self) -> $t {
                self.value
            }

            /// Get the span of this token.
            #[inline]
            pub const fn span(&self) -> S {
                self.span
            }

            /// Set the span of this token.
            #[inline]
            pub fn set_span(&mut self, span: S) {
                self.span = span;
            }
        }

        #[cfg(feature = "literal-value")]
        impl<S: crate::SpanExt> TryFrom<LiteralValue<S>> for $ident<S> {
            type Error = crate::Error;

            #[inline]
            fn try_from(value: LiteralValue<S>) -> Result<Self, Self::Error> {
                if let LiteralValue::$variant(value) = value {
                    Ok(value)
                } else {
                    Err(crate::Error::new("type mismatch"))
                }
            }
        }

        #[cfg(feature = "literal-value")]
        impl<S: crate::SpanExt> From<$ident<S>> for LiteralValue<S> {
            #[inline]
            fn from(value: $ident<S>) -> Self {
                LiteralValue::$variant(value)
            }
        }

        #[cfg(all(feature = "proc-macro", feature = "literal-value"))]
        impl TryFrom<proc_macro::Literal> for $ident<proc_macro::Span> {
            type Error = crate::Error;

            #[inline]
            fn try_from(value: proc_macro::Literal) -> Result<Self, Self::Error> {
                LiteralValue::from(value).try_into()
            }
        }

        #[cfg(all(feature = "proc-macro2", feature = "literal-value"))]
        impl TryFrom<proc_macro2::Literal> for $ident<proc_macro2::Span> {
            type Error = crate::Error;

            #[inline]
            fn try_from(value: proc_macro2::Literal) -> Result<Self, Self::Error> {
                LiteralValue::from(value).try_into()
            }
        }

        #[cfg(all(feature = "proc-macro", feature = "literal-value"))]
        impl From<$ident<proc_macro::Span>> for proc_macro::Literal {
            #[inline]
            fn from(value: $ident<proc_macro::Span>) -> Self {
                LiteralValue::from(value).into()
            }
        }

        #[cfg(all(feature = "proc-macro2", feature = "literal-value"))]
        impl From<$ident<proc_macro2::Span>> for proc_macro2::Literal {
            #[inline]
            fn from(value: $ident<proc_macro2::Span>) -> Self {
                LiteralValue::from(value).into()
            }
        }

        #[cfg(feature = "literal-value")]
        impl<T: crate::TokenTreeExt> Parse<T> for $ident<T::Span> {
            #[inline]
            fn parse(buf: &mut &crate::TokenBuf<T>) -> Option<Self> {
                let mut buf2 = *buf;
                if let Ok(token) = LiteralValue::parse(&mut buf2)?.try_into() {
                    *buf = buf2;
                    return Some(token);
                } else {
                    None
                }
            }
        }

        #[cfg(feature = "literal-value")]
        impl<T: crate::TokenTreeExt> crate::IntoTokens<T> for $ident<T::Span>
            where LiteralValue<T::Span>: crate::IntoTokens<T> // always true, but rust is stupid
        {
            #[inline]
            fn into_tokens(self) -> impl Iterator<Item = crate::TokenObject<T>> {
                LiteralValue::$variant(self).into_tokens()
            }
        }

        #[cfg(feature = "literal-value")]
        impl<T: crate::TokenStreamExt> crate::ToTokenStream<T> for $ident<T::Span> {
            #[inline]
            fn extend_token_stream(&self, token_stream: &mut T) {
                LiteralValue::$variant(self.clone()).extend_token_stream(token_stream);
            }
        }
    )* };
}

def_literal_tokens! {
    /// A string literal. This can be converted to and from `LiteralValue`.
    StringLiteral: String: String,

    /// A byte string literal. This can be converted to and from `LiteralValue`.
    ByteStringLiteral: ByteString: Vec<u8>,

    /// A character literal. This can be converted to and from `LiteralValue`.
    CharacterLiteral: Character: char,

    /// A byte character literal. This can be converted to and from `LiteralValue`.
    ByteCharacterLiteral: ByteCharacter: u8,

    /// Unsuffixed integer literal. This can be converted to and from `LiteralValue`.
    IntLiteral: Int: u128,

    /// Unsuffixed floating point literal. This can be converted to and from `LiteralValue`.
    FloatLiteral: Float: f64,

    /// `i8` suffixed integer literal. This can be converted to and from `LiteralValue`.
    I8Literal: I8: i8,

    /// `i16` suffixed integer literal. This can be converted to and from `LiteralValue`.
    I16Literal: I16: i16,

    /// `i32` suffixed integer literal. This can be converted to and from `LiteralValue`.
    I32Literal: I32: i32,

    /// `i64` suffixed integer literal. This can be converted to and from `LiteralValue`.
    I64Literal: I64: i64,

    /// `i128` suffixed integer literal. This can be converted to and from `LiteralValue`.
    I128Literal: I128: i128,

    /// `isize` suffixed integer literal. This can be converted to and from `LiteralValue`.
    IsizeLiteral: Isize: isize,

    /// `u8` suffixed integer literal. This can be converted to and from `LiteralValue`.
    U8Literal: U8: u8,

    /// `u16` suffixed integer literal. This can be converted to and from `LiteralValue`.
    U16Literal: U16: u16,

    /// `u32` suffixed integer literal. This can be converted to and from `LiteralValue`.
    U32Literal: U32: u32,

    /// `u64` suffixed integer literal. This can be converted to and from `LiteralValue`.
    U64Literal: U64: u64,

    /// `u128` suffixed integer literal. This can be converted to and from `LiteralValue`.
    U128Literal: U128: u128,

    /// `usize` suffixed integer literal. This can be converted to and from `LiteralValue`.
    UsizeLiteral: Usize: usize,

    /// `f32` suffixed floating point literal. This can be converted to and from `LiteralValue`.
    F32Literal: F32: f32,

    /// `f64` suffixed floating point literal. This can be converted to and from `LiteralValue`.
    F64Literal: F64: f64,
}

macro_rules! def {
    ($([$what:tt] $($id:ident: $t:ty),* $(,)?)*) => { $(
        $( def!(@ $what $id: $t); )*
    )* };

    (@ suffixed_int $ident:ident: $t:ty) => {
        /// Suffixed integer literal.
        fn $ident(n: $t) -> Self;
    };

    (@ unsuffixed_int $ident:ident: $t:ty) => {
        /// Unsuffixed integer literal.
        fn $ident(n: $t) -> Self;
    };

    (@ suffixed_float $ident:ident: $t:ty) => {
        /// Suffixed floating-point literal.
        fn $ident(n: $t) -> Self;
    };

    (@ unsuffixed_float $ident:ident: $t:ty) => {
        /// Unsuffixed floating-point literal.
        fn $ident(n: $t) -> Self;
    };
}

/// `Literal` API trait. See [`proc_macro::Literal`](https://doc.rust-lang.org/stable/proc_macro/struct.Literal.html).
///
/// This trait is implemented for `Literal` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
///
/// See also [`LiteralExt`].
pub trait Literal: ProcMacro<Literal = Self> + Display + FromStr {
    def! {
        [suffixed_int]
        i8_suffixed: i8,
        i16_suffixed: i16,
        i32_suffixed: i32,
        i64_suffixed: i64,
        i128_suffixed: i128,
        isize_suffixed: isize,
        u8_suffixed: u8,
        u16_suffixed: u16,
        u32_suffixed: u32,
        u64_suffixed: u64,
        u128_suffixed: u128,
        usize_suffixed: usize,

        [unsuffixed_int]
        i8_unsuffixed: i8,
        i16_unsuffixed: i16,
        i32_unsuffixed: i32,
        i64_unsuffixed: i64,
        i128_unsuffixed: i128,
        isize_unsuffixed: isize,
        u8_unsuffixed: u8,
        u16_unsuffixed: u16,
        u32_unsuffixed: u32,
        u64_unsuffixed: u64,
        u128_unsuffixed: u128,
        usize_unsuffixed: usize,

        [suffixed_float]
        f32_suffixed: f32,
        f64_suffixed: f64,

        [unsuffixed_float]
        f32_unsuffixed: f32,
        f64_unsuffixed: f64,
    }

    /// String literal.
    fn string(str: &str) -> Self;

    /// Character literal.
    fn character(c: char) -> Self;

    /// Byte character literal.
    ///
    /// This method is currently unstable in `proc-macro` and is missing from `proc-macro2`,
    /// but this crate implements it in a way that works with both on stable.
    fn byte_character(b: u8) -> Self;

    /// Byte string literal.
    fn byte_string(bytes: &[u8]) -> Self;

    /// The span of this literal.
    fn span(&self) -> Self::Span;

    /// Set the span of this literal.
    fn set_span(&mut self, span: Self::Span);
}

#[cfg(not(feature = "literal-value"))]
/// Extensions for [`Literal`].
///
/// This trait is implemented for `Literal` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
pub trait LiteralExt:
    ProcMacroExt<LiteralExt = Self>
    + Literal
    + Parse<Self::TokenTree>
    + IntoTokens<Self::TokenTree>
    + crate::ToTokens<Self::TokenTree>
    + ToTokenStream<Self::TokenStream>
{
}

#[cfg(feature = "literal-value")]
/// Extensions for [`Literal`].
///
/// This trait is implemented for `Literal` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
pub trait LiteralExt:
    ProcMacroExt<LiteralExt = Self>
    + Literal
    + From<LiteralValue<Self::Span>>
    + From<StringLiteral<Self::Span>>
    + From<ByteStringLiteral<Self::Span>>
    + From<CharacterLiteral<Self::Span>>
    + From<ByteCharacterLiteral<Self::Span>>
    + From<IntLiteral<Self::Span>>
    + From<FloatLiteral<Self::Span>>
    + From<I8Literal<Self::Span>>
    + From<I16Literal<Self::Span>>
    + From<I32Literal<Self::Span>>
    + From<I64Literal<Self::Span>>
    + From<I128Literal<Self::Span>>
    + From<IsizeLiteral<Self::Span>>
    + From<U8Literal<Self::Span>>
    + From<U16Literal<Self::Span>>
    + From<U32Literal<Self::Span>>
    + From<U64Literal<Self::Span>>
    + From<U128Literal<Self::Span>>
    + From<UsizeLiteral<Self::Span>>
    + From<F32Literal<Self::Span>>
    + From<F64Literal<Self::Span>>
    + Into<LiteralValue<Self::Span>>
    + TryInto<StringLiteral<Self::Span>>
    + TryInto<ByteStringLiteral<Self::Span>>
    + TryInto<CharacterLiteral<Self::Span>>
    + TryInto<ByteCharacterLiteral<Self::Span>>
    + TryInto<IntLiteral<Self::Span>>
    + TryInto<FloatLiteral<Self::Span>>
    + TryInto<I8Literal<Self::Span>>
    + TryInto<I16Literal<Self::Span>>
    + TryInto<I32Literal<Self::Span>>
    + TryInto<I64Literal<Self::Span>>
    + TryInto<I128Literal<Self::Span>>
    + TryInto<IsizeLiteral<Self::Span>>
    + TryInto<U8Literal<Self::Span>>
    + TryInto<U16Literal<Self::Span>>
    + TryInto<U32Literal<Self::Span>>
    + TryInto<U64Literal<Self::Span>>
    + TryInto<U128Literal<Self::Span>>
    + TryInto<UsizeLiteral<Self::Span>>
    + TryInto<F32Literal<Self::Span>>
    + TryInto<F64Literal<Self::Span>>
    + Parse<Self::TokenTree>
    + IntoTokens<Self::TokenTree>
    + crate::ToTokens<Self::TokenTree>
    + ToTokenStream<Self::TokenStream>
{
    /// Convert this `Literal` to a `LiteralValue`.
    fn to_value(&self) -> LiteralValue<Self::Span>;
}

macro_rules! impl_literal {
    ($($pm:ident: $feature:literal),*) => { $(
        #[cfg(all(feature = $feature, feature = "literal-value"))]
        impl From<$pm::Literal> for LiteralValue<$pm::Span> {
            #[inline]
            fn from(value: $pm::Literal) -> Self {
                value.to_value()
            }
        }

        #[cfg(all(feature = $feature, feature = "literal-value"))]
        impl From<LiteralValue<$pm::Span>> for $pm::Literal {
            #[inline]
            fn from(value: LiteralValue<$pm::Span>) -> Self {
                let (mut lit, span) = match value {
                    LiteralValue::String(t) => ($pm::Literal::string(t.value()), t.span()),
                    LiteralValue::ByteString(t) => ($pm::Literal::byte_string(t.value()), t.span()),
                    LiteralValue::Character(t) => ($pm::Literal::character(*t.value()), t.span()),
                    LiteralValue::ByteCharacter(t) => (<$pm::Literal as Literal>::byte_character(*t.value()), t.span()),
                    LiteralValue::Int(t) => ($pm::Literal::u128_unsuffixed(*t.value()), t.span()),
                    LiteralValue::Float(t) => ($pm::Literal::f64_unsuffixed(*t.value()), t.span()),
                    LiteralValue::I8(t) => ($pm::Literal::i8_suffixed(*t.value()), t.span()),
                    LiteralValue::I16(t) => ($pm::Literal::i16_suffixed(*t.value()), t.span()),
                    LiteralValue::I32(t) => ($pm::Literal::i32_suffixed(*t.value()), t.span()),
                    LiteralValue::I64(t) => ($pm::Literal::i64_suffixed(*t.value()), t.span()),
                    LiteralValue::I128(t) => ($pm::Literal::i128_suffixed(*t.value()), t.span()),
                    LiteralValue::Isize(t) => ($pm::Literal::isize_suffixed(*t.value()), t.span()),
                    LiteralValue::U8(t) => ($pm::Literal::u8_suffixed(*t.value()), t.span()),
                    LiteralValue::U16(t) => ($pm::Literal::u16_suffixed(*t.value()), t.span()),
                    LiteralValue::U32(t) => ($pm::Literal::u32_suffixed(*t.value()), t.span()),
                    LiteralValue::U64(t) => ($pm::Literal::u64_suffixed(*t.value()), t.span()),
                    LiteralValue::U128(t) => ($pm::Literal::u128_suffixed(*t.value()), t.span()),
                    LiteralValue::Usize(t) => ($pm::Literal::usize_suffixed(*t.value()), t.span()),
                    LiteralValue::F32(t) => ($pm::Literal::f32_suffixed(*t.value()), t.span()),
                    LiteralValue::F64(t) => ($pm::Literal::f64_suffixed(*t.value()), t.span()),
                };
                lit.set_span(span);
                lit
            }
        }

        #[cfg(feature = $feature)]
        impl Literal for $pm::Literal {
            impl_literal! { @ $pm
                i8_suffixed: n: i8,
                i16_suffixed: n: i16,
                i32_suffixed: n: i32,
                i64_suffixed: n: i64,
                i128_suffixed: n: i128,
                isize_suffixed: n: isize,
                u8_suffixed: n: u8,
                u16_suffixed: n: u16,
                u32_suffixed: n: u32,
                u64_suffixed: n: u64,
                u128_suffixed: n: u128,
                usize_suffixed: n: usize,
                i8_unsuffixed: n: i8,
                i16_unsuffixed: n: i16,
                i32_unsuffixed: n: i32,
                i64_unsuffixed: n: i64,
                i128_unsuffixed: n: i128,
                isize_unsuffixed: n: isize,
                u8_unsuffixed: n: u8,
                u16_unsuffixed: n: u16,
                u32_unsuffixed: n: u32,
                u64_unsuffixed: n: u64,
                u128_unsuffixed: n: u128,
                usize_unsuffixed: n: usize,
                f32_unsuffixed: n: f32,
                f32_suffixed: n: f32,
                f64_unsuffixed: n: f64,
                f64_suffixed: n: f64,
                string: str: &str,
                character: c: char,
                byte_string: bytes: &[u8],
            }

            #[inline]
            fn byte_character(b: u8) -> Self {
                format!("b'\\x{b:02x}'").parse().unwrap()
            }

            #[inline]
            fn span(&self) -> Self::Span {
                self.span()
            }

            #[inline]
            fn set_span(&mut self, span: Self::Span) {
                self.set_span(span)
            }
        }

        #[cfg(feature = $feature)]
        impl LiteralExt for $pm::Literal {
            #[cfg(feature = "literal-value")]
            #[inline]
            fn to_value(&self) -> LiteralValue<$pm::Span> {
                self.to_string().parse().unwrap()
            }
        }

        #[cfg(feature = $feature)]
        impl Parse<$pm::TokenTree> for $pm::Literal {
            #[inline]
            fn parse(buf: &mut &crate::TokenBuf<$pm::TokenTree>) -> Option<Self> {
                buf.parse_prefix(|token| {
                    if let $pm::TokenTree::Literal(t) = token {
                        crate::Match::Complete(t.clone())
                    } else {
                        crate::Match::NoMatch
                    }
                })
            }
        }

        #[cfg(feature = $feature)]
        impl crate::IntoTokens<$pm::TokenTree> for $pm::Literal {
            #[inline]
            fn into_tokens(self) -> impl Iterator<Item = crate::TokenObject<$pm::TokenTree>> {
                std::iter::once($pm::TokenTree::Literal(self))
            }
        }

        #[cfg(feature = $feature)]
        impl crate::ToTokenStream<$pm::TokenStream> for $pm::Literal {
            #[inline]
            fn extend_token_stream(&self, token_stream: &mut $pm::TokenStream)  {
                token_stream.extend([$pm::TokenTree::from(self.clone())]);
            }
        }
    )* };

    (@ $pm:ident $($id:ident: $arg:ident: $t:ty),* $(,)?) => { $(
        #[inline]
        fn $id($arg: $t) -> Self {
            $pm::Literal::$id($arg)
        }
    )* };

    (@ to_literal($value:expr)) => {
        impl_literal!(@ to_literal($value) for
            I8: i8_suffixed,
            I16: i16_suffixed,
            I32: i32_suffixed,
            I64: i64_suffixed,
            I128: i128_suffixed,
            Isize: isize_suffixed,
            U8: u8_suffixed,
            U16: u16_suffixed,
            U32: u32_suffixed,
            U64: u64_suffixed,
            U128: u128_suffixed,
            Usize: usize_suffixed,
            F32: f32_suffixed,
            F64: f64_suffixed,
        )
    };

    (@ to_literal($value:expr) for $($id:ident: $suffixed:ident),* $(,)?) => {
        match $value {
            LiteralValue::String(s) => Self::string(&s),
            LiteralValue::ByteString(bytes) => Self::byte_string(&bytes),
            LiteralValue::Character(c) => Self::character(c),
            LiteralValue::ByteCharacter(b) => <Self as Literal>::byte_character(b),
            LiteralValue::Int(value) => Self::u128_unsuffixed(value),
            LiteralValue::Float(value) => Self::f64_unsuffixed(value),
            $( LiteralValue::Suffixed(Suffixed::$id(value)) => Self::$suffixed(value), )*
        }
    };
}

impl_literal!(proc_macro: "proc-macro", proc_macro2: "proc-macro2");
