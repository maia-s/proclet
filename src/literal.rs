use crate::span::{IncompatibleSpanError, Span, WrappedSpan};
use std::str::FromStr;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Suffixed {
    No,
    Yes,
}

#[derive(Clone, PartialEq)]
pub enum LiteralValue {
    String(String),
    ByteString(Vec<u8>),
    Character(char),
    ByteCharacter(u8),
    U8(u8, Suffixed),
    U16(u16, Suffixed),
    U32(u32, Suffixed),
    U64(u64, Suffixed),
    U128(u128, Suffixed),
    Usize(usize, Suffixed),
    I8(i8, Suffixed),
    I16(i16, Suffixed),
    I32(i32, Suffixed),
    I64(i64, Suffixed),
    I128(i128, Suffixed),
    Isize(isize, Suffixed),
    F32(f32, Suffixed),
    F64(f64, Suffixed),
}

pub struct Literal {
    value: LiteralValue,
    span: WrappedSpan,
}

impl Literal {
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

impl FromStr for Literal {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        todo!()
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
        let mut lit = Self::from_str(&value.to_string()).unwrap();
        lit.span = value.span().into();
        lit
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
        let mut lit = Self::from_str(&value.to_string()).unwrap();
        lit.span = value.span().into();
        lit
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
                        Suffixed::Yes => Self::$suffixed(*value),
                        Suffixed::No => Self::$unsuffixed(*value),
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
