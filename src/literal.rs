use crate::span::{IncompatibleSpanError, Span, WrappedSpan};
use std::{
    error::Error,
    fmt::{self, Display},
    str::{self, FromStr},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IsSuffixed {
    No,
    Yes,
}

impl From<IsSuffixed> for bool {
    #[inline]
    fn from(value: IsSuffixed) -> Self {
        value == IsSuffixed::Yes
    }
}

#[derive(Clone, Copy, Debug)]
pub enum LiteralValueIntError {
    WrongType,
    OutOfRange,
}

impl Error for LiteralValueIntError {}

impl Display for LiteralValueIntError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::WrongType => "wrong type",
                Self::OutOfRange => "out of range",
            }
        )
    }
}

#[derive(Clone, Copy, Debug)]
pub struct WrongTypeError;

impl Error for WrongTypeError {}

impl Display for WrongTypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "wrong type")
    }
}

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

impl Error for LiteralValueParseError {}

impl Display for LiteralValueParseError {
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

#[derive(Clone, PartialEq)]
pub enum LiteralValue {
    String(String),
    ByteString(Vec<u8>),
    Character(char),
    ByteCharacter(u8),
    U8(u8, IsSuffixed),
    U16(u16, IsSuffixed),
    U32(u32, IsSuffixed),
    U64(u64, IsSuffixed),
    U128(u128, IsSuffixed),
    Usize(usize, IsSuffixed),
    I8(i8, IsSuffixed),
    I16(i16, IsSuffixed),
    I32(i32, IsSuffixed),
    I64(i64, IsSuffixed),
    I128(i128, IsSuffixed),
    Isize(isize, IsSuffixed),
    F32(f32, IsSuffixed),
    F64(f64, IsSuffixed),
}

impl LiteralValue {
    #[inline]
    pub const fn is_suffixed(&self) -> Option<bool> {
        match self {
            Self::U8(_, is)
            | Self::U16(_, is)
            | Self::U32(_, is)
            | Self::U64(_, is)
            | Self::U128(_, is)
            | Self::Usize(_, is)
            | Self::I8(_, is)
            | Self::I16(_, is)
            | Self::I32(_, is)
            | Self::I64(_, is)
            | Self::I128(_, is)
            | Self::Isize(_, is)
            | Self::F32(_, is)
            | Self::F64(_, is) => Some(matches!(is, IsSuffixed::Yes)),
            _ => None,
        }
    }

    #[inline]
    pub fn uint_value(&self) -> Result<u128, LiteralValueIntError> {
        match self {
            Self::U8(value, _) => Ok((*value) as _),
            Self::U16(value, _) => Ok((*value) as _),
            Self::U32(value, _) => Ok((*value) as _),
            Self::U64(value, _) => Ok((*value) as _),
            Self::U128(value, _) => Ok(*value),
            Self::Usize(value, _) => (*value)
                .try_into()
                .map_err(|_| LiteralValueIntError::OutOfRange),
            Self::I8(value, _) => (*value)
                .try_into()
                .map_err(|_| LiteralValueIntError::OutOfRange),
            Self::I16(value, _) => (*value)
                .try_into()
                .map_err(|_| LiteralValueIntError::OutOfRange),
            Self::I32(value, _) => (*value)
                .try_into()
                .map_err(|_| LiteralValueIntError::OutOfRange),
            Self::I64(value, _) => (*value)
                .try_into()
                .map_err(|_| LiteralValueIntError::OutOfRange),
            Self::I128(value, _) => (*value)
                .try_into()
                .map_err(|_| LiteralValueIntError::OutOfRange),
            Self::Isize(value, _) => (*value)
                .try_into()
                .map_err(|_| LiteralValueIntError::OutOfRange),
            _ => Err(LiteralValueIntError::WrongType),
        }
    }

    #[inline]
    pub fn int_value(&self) -> Result<i128, LiteralValueIntError> {
        match self {
            Self::U8(value, _) => Ok((*value) as _),
            Self::U16(value, _) => Ok((*value) as _),
            Self::U32(value, _) => Ok((*value) as _),
            Self::U64(value, _) => Ok((*value) as _),
            Self::U128(value, _) => (*value)
                .try_into()
                .map_err(|_| LiteralValueIntError::OutOfRange),
            Self::Usize(value, _) => (*value)
                .try_into()
                .map_err(|_| LiteralValueIntError::OutOfRange),
            Self::I8(value, _) => Ok((*value) as _),
            Self::I16(value, _) => Ok((*value) as _),
            Self::I32(value, _) => Ok((*value) as _),
            Self::I64(value, _) => Ok((*value) as _),
            Self::I128(value, _) => Ok(*value),
            Self::Isize(value, _) => (*value)
                .try_into()
                .map_err(|_| LiteralValueIntError::OutOfRange),
            _ => Err(LiteralValueIntError::WrongType),
        }
    }

    #[inline]
    pub fn float_value(&self) -> Result<f64, WrongTypeError> {
        match self {
            Self::F32(value, _) => Ok((*value) as _),
            Self::F64(value, _) => Ok(*value),
            _ => Err(WrongTypeError),
        }
    }
}

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

        fn dec_digit(b: u8) -> Result<u8, LiteralValueParseError> {
            match b {
                b'0'..=b'9' => Ok(b - b'0'),
                _ => Err(LiteralValueParseError::InvalidDecDigit),
            }
        }

        fn from_int(value: u128, suffix: &[u8]) -> Result<LiteralValue, LiteralValueParseError> {
            macro_rules! make {
                ($($s:literal => $t:ident $(as $as:ident)?),* $(,)?) => {
                    match suffix {
                        $(
                            $s => Ok(LiteralValue::$t(
                                (value $(as $as)?)
                                    .try_into()
                                    .map_err(|_| LiteralValueParseError::ValueOutOfRange)?,
                                IsSuffixed::Yes,
                            )),
                        )*

                        b"" => {
                            if value <= u32::MAX as u128 {
                                if value <= u8::MAX as u128 {
                                    Ok(LiteralValue::U8(value as u8, IsSuffixed::No))
                                } else if value <= u16::MAX as u128 {
                                    Ok(LiteralValue::U16(value as u16, IsSuffixed::No))
                                } else {
                                    Ok(LiteralValue::U32(value as u32, IsSuffixed::No))
                                }
                            } else {
                                if value <= u64::MAX as u128 {
                                    Ok(LiteralValue::U64(value as u64, IsSuffixed::No))
                                } else {
                                    Ok(LiteralValue::U128(value, IsSuffixed::No))
                                }
                            }
                        }

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

        fn parse_byte_escape(input: &mut &[u8]) -> Result<u8, LiteralValueParseError> {
            assert_eq!(input[0], b'\\');
            if input.len() >= 2 {
                let escape = input[1];
                *input = &input[2..];
                match escape {
                    b'\'' => Ok(b'\''),
                    b'\"' => Ok(b'\"'),
                    b'\\' => Ok(b'\\'),
                    b'\0' => Ok(b'\0'),
                    b'n' => Ok(b'\n'),
                    b'r' => Ok(b'\r'),
                    b't' => Ok(b'\t'),
                    _ => Err(LiteralValueParseError::UnrecognizedCharEscape),
                }
            } else {
                Err(LiteralValueParseError::UnrecognizedCharEscape)
            }
        }

        fn parse_byte(input: &mut &[u8]) -> Result<u8, LiteralValueParseError> {
            if !input.is_empty() {
                if input[0] == b'\\' {
                    Ok(parse_byte_escape(&mut &input[1..input.len() - 1])?)
                } else {
                    Ok(input[0])
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
                    b'\0' => Ok(Some('\0')),
                    b'n' => Ok(Some('\n')),
                    b'r' => Ok(Some('\r')),
                    b't' => Ok(Some('\t')),
                    b'\n' if matches!(escapes, Escapes::String) => Ok(None),
                    _ => {
                        if input.len() >= 2 && escape == b'x' {
                            // \x..
                            *input = &input[2..];
                            Ok(Some(char::from(
                                oct_digit(input[2])? << 4 | hex_digit(input[3])?,
                            )))
                        } else if input.len() > 2 && escape == b'u' && input[0] == b'{' {
                            // \u{...}
                            *input = &input[1..];
                            let mut value = 0;
                            while !input.is_empty() && input[0] != b'}' {
                                *input = &input[1..];
                                value = value << 8 | hex_digit(input[0])? as u32;
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
                        } else {
                            Err(LiteralValueParseError::UnrecognizedCharEscape)
                        }
                    }
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
                    0x00..=0x7f => {
                        *input = &input[1..];
                        Ok(Some(char::from(input[0])))
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
                                parse_byte(&mut &input[1..input.len() - 1])
                                    .map(LiteralValue::ByteCharacter)
                            } else {
                                Err(LiteralValueParseError::InvalidInput)
                            }
                        }

                        b'\"' => {
                            if input.len() > 1 && input[input.len() - 1] == b'\"' {
                                input = &input[1..input.len() - 1];
                                let mut s = Vec::new();
                                while !input.is_empty() {
                                    s.push(parse_byte(&mut input)?);
                                }
                                Ok(LiteralValue::ByteString(s))
                            } else {
                                Err(LiteralValueParseError::InvalidInput)
                            }
                        }

                        b'r' => {
                            input = &input[2..];
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
                            let suffix = parse_suffix(&mut &input[2..], true);
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
                            let suffix = parse_suffix(&mut &input[2..], true);
                            if !input.is_empty() {
                                let mut value: u128 = oct_digit(input[2])?.into();
                                for &digit in &input[3..] {
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
                            let suffix = parse_suffix(&mut &input[2..], false);
                            if !input.is_empty() {
                                let mut value: u128 = hex_digit(input[2])?.into();
                                for &digit in &input[3..] {
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
                todo!("i*/u*/f32/f64")
            }

            _ => Err(LiteralValueParseError::InvalidInput),
        }
    }
}

pub struct Literal {
    value: LiteralValue,
    span: WrappedSpan,
}

impl Literal {
    #[inline]
    pub fn new(value: LiteralValue, span: impl Span) -> Self {
        Self {
            value,
            span: span.into(),
        }
    }

    #[inline]
    pub const fn value(&self) -> &LiteralValue {
        &self.value
    }

    /// Get the associated span of this literal. You can get either a `proc_macro::Span`
    /// or a `proc_macro2::Span` if the corresponding crate features are enabled.
    /// This may fail if the span originates from a `proc_macro2::Span` and you're trying
    /// to get a `proc_macro::Span`.
    #[inline]
    pub fn span<S: Span>(&self) -> Result<S, S::Error> {
        self.span.try_into()
    }

    /// Set the associated span of this literal. You can set either a `proc_macro::Span`
    /// or a `proc_macro2::Span` if the corresponding crate features are enabled.
    /// `proc_macro::Span` is compatible with both `proc-macro` and `proc-macro2`, but
    /// `proc_macro2::Span` can't be used with types from `proc-macro`.
    #[inline]
    pub fn set_span(&mut self, span: impl Span) {
        self.span = span.into();
    }
}

#[cfg(feature = "proc-macro")]
impl From<proc_macro::Literal> for Literal {
    #[inline]
    fn from(value: proc_macro::Literal) -> Self {
        Self::from(&value)
    }
}

#[cfg(feature = "proc-macro")]
impl From<&proc_macro::Literal> for Literal {
    #[inline]
    fn from(value: &proc_macro::Literal) -> Self {
        Literal::new(
            LiteralValue::from_str(&value.to_string()).unwrap(),
            value.span(),
        )
    }
}

#[cfg(feature = "proc-macro2")]
impl From<proc_macro2::Literal> for Literal {
    #[inline]
    fn from(value: proc_macro2::Literal) -> Self {
        Self::from(&value)
    }
}

#[cfg(feature = "proc-macro2")]
impl From<&proc_macro2::Literal> for Literal {
    #[inline]
    fn from(value: &proc_macro2::Literal) -> Self {
        Literal::new(
            LiteralValue::from_str(&value.to_string()).unwrap(),
            value.span(),
        )
    }
}

macro_rules! from_literal_impl {
    ([$($expr:tt)*][fallible]) => { from_literal_impl!(@ [$($expr)*][fallible:1]) };
    ([$($expr:tt)*][infallible]) => { from_literal_impl!(@ [$($expr)*][infallible:1]) };

    (@ [$($expr:tt)*]$([fallible:$fallible:literal])?$([infallible:$infallible:literal])?) => {
        from_literal_impl! {
            @ [$($expr)*]$([fallible:$fallible])?$([infallible:$infallible])?
            U8, u8_suffixed, u8_unsuffixed,
            U16, u16_suffixed, u16_unsuffixed,
            U32, u32_suffixed, u32_unsuffixed,
            U64, u64_suffixed, u64_unsuffixed,
            U128, u128_suffixed, u128_unsuffixed,
            Usize, usize_suffixed, usize_unsuffixed,
            I8, i8_suffixed, i8_unsuffixed,
            I16, i16_suffixed, i16_unsuffixed,
            I32, i32_suffixed, i32_unsuffixed,
            I64, i64_suffixed, i64_unsuffixed,
            I128, i128_suffixed, i128_unsuffixed,
            Isize, isize_suffixed, isize_unsuffixed,
            F32, f32_suffixed, f32_unsuffixed,
            F64, f64_suffixed, f64_unsuffixed,
        }
    };

    (@ [$expr:expr]$([fallible:$fallible:literal])?$([infallible:$infallible:literal])? $($ident:ident, $suffixed:ident, $unsuffixed:ident),* $(,)?) => {{
        let expr = $expr;
        let mut output = match &expr.value {
            LiteralValue::String(s) => Self::string(s),
            LiteralValue::ByteString(s) => Self::byte_string(s),
            LiteralValue::Character(c) => Self::character(*c),
            LiteralValue::ByteCharacter(b) => format!("b'\\x{:02x}'", b).parse().unwrap(),
            $(
                LiteralValue::$ident(value, suffixed) => {
                    match suffixed {
                        IsSuffixed::Yes => Self::$suffixed(*value),
                        IsSuffixed::No => Self::$unsuffixed(*value),
                    }
                }
            )*
        };
        $(
            let _ = $fallible;
            output.set_span(expr.span()?);
            Ok(output)
        )?
        $(
            let _ = $infallible;
            output.set_span(expr.span().unwrap());
            output
        )?
    }};
}

#[cfg(feature = "proc-macro")]
impl TryFrom<Literal> for proc_macro::Literal {
    type Error = IncompatibleSpanError;

    #[inline]
    fn try_from(value: Literal) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

#[cfg(feature = "proc-macro")]
impl TryFrom<&Literal> for proc_macro::Literal {
    type Error = IncompatibleSpanError;

    #[inline]
    fn try_from(value: &Literal) -> Result<Self, Self::Error> {
        from_literal_impl!([value][fallible])
    }
}

#[cfg(feature = "proc-macro2")]
impl From<Literal> for proc_macro2::Literal {
    #[inline]
    fn from(value: Literal) -> Self {
        Self::from(&value)
    }
}

#[cfg(feature = "proc-macro2")]
impl From<&Literal> for proc_macro2::Literal {
    #[inline]
    fn from(value: &Literal) -> Self {
        from_literal_impl!([value][infallible])
    }
}

#[cfg(feature = "proc-macro")]
impl TryFrom<Literal> for proc_macro::TokenTree {
    type Error = IncompatibleSpanError;

    #[inline]
    fn try_from(value: Literal) -> Result<Self, IncompatibleSpanError> {
        proc_macro::Literal::try_from(value).map(|x| x.into())
    }
}

#[cfg(feature = "proc-macro")]
impl TryFrom<&Literal> for proc_macro::TokenTree {
    type Error = IncompatibleSpanError;

    #[inline]
    fn try_from(value: &Literal) -> Result<Self, IncompatibleSpanError> {
        proc_macro::Literal::try_from(value).map(|x| x.into())
    }
}

#[cfg(feature = "proc-macro2")]
impl From<Literal> for proc_macro2::TokenTree {
    #[inline]
    fn from(value: Literal) -> Self {
        proc_macro2::Literal::from(value).into()
    }
}

#[cfg(feature = "proc-macro2")]
impl From<&Literal> for proc_macro2::TokenTree {
    #[inline]
    fn from(value: &Literal) -> Self {
        proc_macro2::Literal::from(value).into()
    }
}

#[cfg(feature = "quote")]
impl quote::ToTokens for Literal {
    #[inline]
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let tt: proc_macro2::TokenTree = self.into();
        tt.to_tokens(tokens);
    }
}
