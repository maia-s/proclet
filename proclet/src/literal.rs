use crate::{ProcMacro, ProcMacroExt, Token};
use std::{fmt::Display, str::FromStr};

#[cfg(feature = "literal-value")]
use std::{error::Error, fmt};

#[cfg(feature = "literal-value")]
#[derive(Clone, Copy, Debug)]
pub struct OutOfRangeError;

#[cfg(feature = "literal-value")]
impl Error for OutOfRangeError {}

#[cfg(feature = "literal-value")]
impl Display for OutOfRangeError {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "out of range")
    }
}

#[cfg(feature = "literal-value")]
#[derive(Clone, Copy, Debug)]
pub enum LiteralValueParseError {
    UnrecognizedByteEscape,
    UnrecognizedCharEscape,
    InvalidUnicodeEscape,
    InvalidBinDigit,
    InvalidOctDigit,
    InvalidDecDigit,
    InvalidHexDigit,
    InvalidInput,
    ValueOutOfRange,
}

#[cfg(feature = "literal-value")]
impl Error for LiteralValueParseError {}

#[cfg(feature = "literal-value")]
impl Display for LiteralValueParseError {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::UnrecognizedByteEscape => "unrecognized byte escape",
                Self::UnrecognizedCharEscape => "unrecognized character escape",
                Self::InvalidUnicodeEscape => "invalid unicode escape",
                Self::InvalidBinDigit => "invalid binary digit",
                Self::InvalidOctDigit => "invalid octal digit",
                Self::InvalidDecDigit => "invalid decimal digit",
                Self::InvalidHexDigit => "invalid hex digit",
                Self::InvalidInput => "invalid input",
                Self::ValueOutOfRange => "value out of range",
            }
        )
    }
}

/// Suffixed value.
#[cfg(feature = "literal-value")]
#[derive(Clone, Debug, PartialEq)]
pub enum Suffixed {
    /// `i8`
    I8(i8),

    /// `i16`
    I16(i16),

    /// `i32`
    I32(i32),

    /// `i64`
    I64(i64),

    /// `i128`
    I128(i128),

    /// `isize`
    Isize(isize),

    /// `u8`
    U8(u8),

    /// `u16`
    U16(u16),

    /// `u32`
    U32(u32),

    /// `u64`
    U64(u64),

    /// `u128`
    U128(u128),

    /// `usize`
    Usize(usize),

    /// `f32`
    F32(f32),

    /// `f64`
    F64(f64),
}

/// Literal value.
#[cfg(feature = "literal-value")]
#[derive(Clone, Debug, PartialEq)]
pub enum LiteralValue {
    /// String.
    String(String),

    /// Byte string.
    ByteString(Vec<u8>),

    /// Character.
    Character(char),

    /// Byte character.
    ByteCharacter(u8),

    /// Integer.
    Int(u128),

    /// Floating-point value.
    Float(f64),

    /// Suffixed value.
    Suffixed(Suffixed),
}

#[cfg(feature = "literal-value")]
impl LiteralValue {
    /// Returns true if the contained value is `Suffixed`.
    #[inline]
    pub const fn is_suffixed(&self) -> bool {
        matches!(self, Self::Suffixed(_))
    }

    /// Convert a `Suffixed` value to `Int` or `Float` if possible.
    #[inline]
    pub fn remove_suffix(&mut self) -> Result<(), OutOfRangeError> {
        if let Self::Suffixed(s) = self {
            match *s {
                Suffixed::I8(value) => {
                    *self = Self::Int(value.try_into().map_err(|_| OutOfRangeError)?)
                }
                Suffixed::I16(value) => {
                    *self = Self::Int(value.try_into().map_err(|_| OutOfRangeError)?)
                }
                Suffixed::I32(value) => {
                    *self = Self::Int(value.try_into().map_err(|_| OutOfRangeError)?)
                }
                Suffixed::I64(value) => {
                    *self = Self::Int(value.try_into().map_err(|_| OutOfRangeError)?)
                }
                Suffixed::I128(value) => {
                    *self = Self::Int(value.try_into().map_err(|_| OutOfRangeError)?)
                }
                Suffixed::Isize(value) => {
                    *self = Self::Int(value.try_into().map_err(|_| OutOfRangeError)?)
                }
                Suffixed::U8(value) => *self = Self::Int(value as _),
                Suffixed::U16(value) => *self = Self::Int(value as _),
                Suffixed::U32(value) => *self = Self::Int(value as _),
                Suffixed::U64(value) => *self = Self::Int(value as _),
                Suffixed::U128(value) => *self = Self::Int(value),
                Suffixed::Usize(value) => *self = Self::Int(value as _),
                Suffixed::F32(value) => *self = Self::Float(value as _),
                Suffixed::F64(value) => *self = Self::Float(value),
            }
        }
        Ok(())
    }
}

#[cfg(feature = "literal-value")]
impl FromStr for LiteralValue {
    type Err = LiteralValueParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut input = s.as_bytes();

        #[derive(Clone, Copy)]
        enum Escapes {
            Char,
            String,
        }

        fn bin_digit(b: u8) -> Result<u8, LiteralValueParseError> {
            match b {
                b'0'..=b'1' => Ok(b - b'0'),
                _ => Err(LiteralValueParseError::InvalidBinDigit),
            }
        }

        fn oct_digit(b: u8) -> Result<u8, LiteralValueParseError> {
            match b {
                b'0'..=b'7' => Ok(b - b'0'),
                _ => Err(LiteralValueParseError::InvalidHexDigit),
            }
        }

        fn hex_digit(b: u8) -> Result<u8, LiteralValueParseError> {
            match b {
                b'0'..=b'9' => Ok(b - b'0'),
                b'a'..=b'f' => Ok(b - b'a' + 10),
                b'A'..=b'F' => Ok(b - b'A' + 10),
                _ => Err(LiteralValueParseError::InvalidHexDigit),
            }
        }

        fn from_int(value: u128, suffix: &[u8]) -> Result<LiteralValue, LiteralValueParseError> {
            macro_rules! make {
                ($($s:literal => $t:ident $(as $as:ident)?),* $(,)?) => {
                    match suffix {
                        $(
                            $s => Ok(LiteralValue::Suffixed(Suffixed::$t(
                                (value $(as $as)?)
                                    .try_into()
                                    .map_err(|_| LiteralValueParseError::ValueOutOfRange)?,
                            ))),
                        )*

                        b"" => Ok(LiteralValue::Int(value)),

                        _ => unreachable!(),
                    }
                };
            }
            make! {
                b"u8" => U8,
                b"u16" => U16,
                b"u32" => U32,
                b"u64" => U64,
                b"u128" => U128,
                b"usize" => Usize,
                b"i8" => I8,
                b"i16" => I16,
                b"i32" => I32,
                b"i64" => I64,
                b"i128" => I128,
                b"isize" => Isize,
                b"f32" => F32 as f32,
                b"f64" => F64 as f64,
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
        ) -> Result<Option<u8>, LiteralValueParseError> {
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
                    _ => Err(LiteralValueParseError::UnrecognizedByteEscape),
                }
            } else {
                Err(LiteralValueParseError::UnrecognizedByteEscape)
            }
        }

        fn parse_byte(
            input: &mut &[u8],
            escapes: Escapes,
        ) -> Result<Option<u8>, LiteralValueParseError> {
            if let Some(&value) = input.first() {
                if value == b'\\' {
                    Ok(parse_byte_escape(input, escapes)?)
                } else {
                    *input = &input[1..];
                    Ok(Some(value))
                }
            } else {
                Err(LiteralValueParseError::InvalidInput)
            }
        }

        fn parse_char_escape(
            input: &mut &[u8],
            escapes: Escapes,
        ) -> Result<Option<char>, LiteralValueParseError> {
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
                                .ok_or(LiteralValueParseError::InvalidUnicodeEscape)?
                                | hex_digit(input[0])? as u32;
                            *input = &input[1..];
                        }
                        if input[0] == b'}' {
                            *input = &input[1..];
                            Ok(Some(
                                char::from_u32(value)
                                    .ok_or(LiteralValueParseError::InvalidUnicodeEscape)?,
                            ))
                        } else {
                            Err(LiteralValueParseError::InvalidInput)
                        }
                    }
                    _ => Err(LiteralValueParseError::UnrecognizedCharEscape),
                }
            } else {
                Err(LiteralValueParseError::UnrecognizedCharEscape)
            }
        }

        fn parse_char(
            input: &mut &[u8],
            escapes: Escapes,
        ) -> Result<Option<char>, LiteralValueParseError> {
            if !input.is_empty() && input[0] == b'\\' {
                if let Some(c) = parse_char_escape(input, escapes)? {
                    Ok(Some(c))
                } else if matches!(escapes, Escapes::String) {
                    Ok(None)
                } else {
                    Err(LiteralValueParseError::UnrecognizedCharEscape)
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
                Err(LiteralValueParseError::InvalidInput)
            }
        }

        match input[0] {
            b'\'' => {
                if input[input.len() - 1] == b'\'' {
                    Ok(LiteralValue::Character(
                        parse_char(&mut &input[1..input.len() - 1], Escapes::Char)?.unwrap(),
                    ))
                } else {
                    Err(LiteralValueParseError::InvalidInput)
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
                    Ok(LiteralValue::String(s))
                } else {
                    Err(LiteralValueParseError::InvalidInput)
                }
            }

            b'r' => {
                let mut s = &s[1..];
                while let Some(ss) = s.strip_prefix('#') {
                    s = ss
                        .strip_suffix('#')
                        .ok_or(LiteralValueParseError::InvalidInput)?;
                }
                let s = s
                    .strip_prefix('"')
                    .and_then(|s| s.strip_suffix('"'))
                    .ok_or(LiteralValueParseError::InvalidInput)?;
                Ok(LiteralValue::String(s.to_owned()))
            }

            b'b' => {
                input = &input[1..];
                if !input.is_empty() {
                    match input[0] {
                        b'\'' => {
                            if input.len() > 1 && input[input.len() - 1] == b'\'' {
                                Ok(LiteralValue::ByteCharacter(
                                    parse_byte(&mut &input[1..input.len() - 1], Escapes::Char)?
                                        .unwrap(),
                                ))
                            } else {
                                Err(LiteralValueParseError::InvalidInput)
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
                                Ok(LiteralValue::ByteString(s))
                            } else {
                                Err(LiteralValueParseError::InvalidInput)
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
                                Ok(LiteralValue::ByteString(
                                    input[1..input.len() - 1].to_owned(),
                                ))
                            } else {
                                Err(LiteralValueParseError::InvalidInput)
                            }
                        }

                        _ => Err(LiteralValueParseError::InvalidInput),
                    }
                } else {
                    Err(LiteralValueParseError::InvalidInput)
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
                                            .ok_or(LiteralValueParseError::ValueOutOfRange)?
                                            | bin_digit(digit)? as u128;
                                    }
                                }
                                return from_int(value, suffix);
                            } else {
                                return Err(LiteralValueParseError::InvalidInput);
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
                                            .ok_or(LiteralValueParseError::ValueOutOfRange)?
                                            | oct_digit(digit)? as u128;
                                    }
                                }
                                return from_int(value, suffix);
                            } else {
                                return Err(LiteralValueParseError::InvalidInput);
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
                                            .ok_or(LiteralValueParseError::ValueOutOfRange)?
                                            | hex_digit(digit)? as u128;
                                    }
                                }
                                return from_int(value, suffix);
                            } else {
                                return Err(LiteralValueParseError::InvalidInput);
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
                            .map_err(|_| LiteralValueParseError::InvalidInput)?;
                        Ok(LiteralValue::Suffixed(Suffixed::F32(value)))
                    } else {
                        let value: f64 = s
                            .parse()
                            .map_err(|_| LiteralValueParseError::InvalidInput)?;
                        if suffix == b"f64" {
                            Ok(LiteralValue::Suffixed(Suffixed::F64(value)))
                        } else {
                            Ok(LiteralValue::Float(value))
                        }
                    }
                } else {
                    from_int(
                        s.parse()
                            .map_err(|_| LiteralValueParseError::InvalidInput)?,
                        suffix,
                    )
                }
            }

            _ => Err(LiteralValueParseError::InvalidInput),
        }
    }
}

/// A literal token. This is like `Literal` from `proc-macro*`, except that the value has
/// already been parsed and is available at no cost. You can convert it to and from `Literal`
/// with `into`.
#[cfg(feature = "literal-value")]
#[derive(Clone, Debug)]
pub struct LiteralToken<S: crate::Span> {
    value: LiteralValue,
    span: S,
}

#[cfg(feature = "literal-value")]
impl<S: crate::Span> LiteralToken<S> {
    /// Create a new `LiteralToken`.
    #[inline]
    pub fn new(value: LiteralValue) -> Self {
        Self {
            value,
            span: S::call_site(),
        }
    }

    /// Create a new `LiteralToken` with a custom span.
    #[inline]
    pub const fn with_span(value: LiteralValue, span: S) -> Self {
        Self { value, span }
    }

    /// Get the value of this literal.
    #[inline]
    pub const fn value(&self) -> &LiteralValue {
        &self.value
    }

    /// Get a mutable reference to the value of this literal.
    #[inline]
    pub fn value_mut(&mut self) -> &mut LiteralValue {
        &mut self.value
    }

    /// Get the span of this literal.
    #[inline]
    pub const fn span(&self) -> S {
        self.span
    }

    /// Set the span of this literal.
    #[inline]
    pub fn set_span(&mut self, span: S) {
        self.span = span;
    }
}

#[cfg(all(feature = "literal-value", feature = "token-buffer"))]
impl<T: crate::PMExt> crate::Parse<T> for LiteralToken<T::Span> {
    #[inline]
    fn parse(buf: &mut &crate::TokenBuf<T>) -> Option<Self> {
        buf.parse_prefix(|token| {
            if let Some(token) = token.downcast_ref::<Self>() {
                crate::Match::Complete(token.clone())
            } else if let Some(token) = token.downcast_ref::<T::Literal>() {
                crate::Match::Complete(token.clone().into())
            } else {
                crate::Match::NoMatch
            }
        })
    }
}

#[cfg(feature = "literal-value")]
impl<T: crate::PMExt> Token<T> for LiteralToken<T::Span> {
    #[inline]
    fn eq_except_span(&self, other: &dyn Token<T>) -> bool {
        if let Some(other) = other.downcast_ref::<Self>() {
            self.value() == other.value()
        } else if let Some(other) = other.downcast_ref::<T::Literal>() {
            self.value() == &other.to_value()
        } else {
            false
        }
    }
}

#[cfg(feature = "literal-value")]
impl<T: crate::TokenStreamExt> crate::ToTokenStream<T> for LiteralToken<T::Span> {
    #[inline]
    fn extend_token_stream(&self, token_stream: &mut T) {
        token_stream.extend([T::TokenTree::from(T::Literal::from(self.clone()))])
    }
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
pub trait LiteralExt: ProcMacroExt<LiteralExt = Self> + Literal + Token<Self::PM> {}

#[cfg(feature = "literal-value")]
/// Extensions for [`Literal`].
///
/// This trait is implemented for `Literal` in `proc_macro` and `proc_macro2` if the
/// corresponding feature is enabled.
pub trait LiteralExt:
    ProcMacroExt<LiteralExt = Self>
    + Literal
    + Token<Self::PM>
    + From<LiteralToken<Self::Span>>
    + Into<LiteralToken<Self::Span>>
{
    /// Parse this literal and get its value. If you're going to do this more than once
    /// it's better to convert the literal into a [`LiteralToken`].
    ///
    /// This is only available if the `literal-value` feature is enabled.
    fn to_value(&self) -> LiteralValue;

    /// Set the literal's value.
    ///
    /// This is only available if the `literal-value` feature is enabled.
    fn set_value(&mut self, value: LiteralValue);
}

macro_rules! impl_literal {
    ($($pm:ident: $feature:literal),*) => { $(
        #[cfg(all(feature = $feature, feature = "literal-value"))]
        impl From<$pm::Literal> for LiteralToken<$pm::Span> {
            #[inline]
            fn from(value: $pm::Literal) -> Self {
                Self::with_span(value.to_value(), value.span())
            }
        }

        #[cfg(all(feature = $feature, feature = "literal-value"))]
        impl From<LiteralToken<$pm::Span>> for $pm::Literal {
            #[inline]
            fn from(value: LiteralToken<$pm::Span>) -> Self {
                let mut lit = $pm::Literal::u8_unsuffixed(0);
                lit.set_span(value.span());
                lit.set_value(value.value);
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
            fn to_value(&self) -> LiteralValue {
                self.to_string().parse().unwrap()
            }

            #[cfg(feature = "literal-value")]
            #[inline]
            fn set_value(&mut self, value: LiteralValue) {
                let mut lit = impl_literal!(@ to_literal(value));
                lit.set_span(self.span());
                *self = lit;
            }
        }

        #[cfg(all(feature = $feature, feature = "token-buffer"))]
        impl crate::Parse<crate::base::$pm::PM> for $pm::Literal {
            #[inline]
            fn parse(buf: &mut &crate::TokenBuf<crate::base::$pm::PM>) -> Option<Self> {
                buf.parse_prefix(|token| {
                    if let Some(token) = token.downcast_ref::<Self>() {
                        return crate::Match::Complete(token.clone());
                    }
                    #[cfg(feature = "literal-value")]
                    if let Some(token) = token.downcast_ref::<LiteralToken<$pm::Span>>() {
                        return crate::Match::Complete(token.clone().into())
                    }
                    crate::Match::NoMatch
                })
            }
        }

        #[cfg(feature = $feature)]
        impl Token<crate::base::$pm::PM> for $pm::Literal {
            #[cfg(feature = "literal-value")]
            #[inline]
            fn eq_except_span(&self, other: &dyn Token<crate::base::$pm::PM>) -> bool {
                if let Some(other) = other.downcast_ref::<LiteralToken<$pm::Span>>() {
                    return &self.to_value() == other.value();
                } else if let Some(other) = other.downcast_ref::<Self>() {
                    self.to_value() == other.to_value()
                } else {
                    false
                }
            }

            #[cfg(not(feature = "literal-value"))]
            #[inline]
            fn eq_except_span(&self, other: &dyn Token<crate::base::$pm::PM>) -> bool {
                if let Some(other) = other.downcast_ref::<Self>() {
                    self.to_string() == other.to_string()
                } else {
                    false
                }
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
