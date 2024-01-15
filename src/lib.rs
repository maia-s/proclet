#![cfg_attr(feature = "nightly", feature(doc_auto_cfg))]
#![doc = include_str!("../README.md")]

use std::str::FromStr;

#[cfg(feature = "proc-macro")]
extern crate proc_macro;

mod alias {
    // Span can be converted from proc-macro to proc-macro2, but not the other way,
    // so prefer proc-macro's
    #[cfg(feature = "proc-macro")]
    pub type Span = proc_macro::Span;
    #[cfg(all(feature = "proc-macro2", not(feature = "proc-macro")))]
    pub type Span = proc_macro2::Span;
}

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
    span: alias::Span,
}

impl Literal {
    #[inline]
    pub const fn value(&self) -> &LiteralValue {
        &self.value
    }

    #[cfg(feature = "proc-macro")]
    #[inline]
    pub fn span(&self) -> proc_macro::Span {
        self.span
    }

    #[cfg(feature = "proc-macro2")]
    #[inline]
    pub fn span2(&self) -> proc_macro2::Span {
        self.span.into()
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
        Self::from_str(&value.to_string()).unwrap()
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
        Self::from_str(&value.to_string()).unwrap()
    }
}

macro_rules! from_literal_impl {
    ([$($expr:tt)*]) => {
        from_literal_impl! {
            @ [$($expr)*]
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
    (@ [$expr:expr] $($ident:ident, $suffixed:ident, $unsuffixed:ident),* $(,)?) => {{
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
        output.set_span(expr.span.into());
        output
    }};
}

#[cfg(feature = "proc-macro")]
impl From<Literal> for proc_macro::Literal {
    #[inline]
    fn from(value: Literal) -> Self {
        Self::from(&value)
    }
}

#[cfg(feature = "proc-macro")]
impl From<&Literal> for proc_macro::Literal {
    #[inline]
    fn from(value: &Literal) -> Self {
        from_literal_impl!([value])
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
        from_literal_impl!([value])
    }
}
