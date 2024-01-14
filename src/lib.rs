#![doc = include_str!("../README.md")]

use std::str::FromStr;

extern crate proc_macro;

pub enum Literal {
    String(String),
    ByteString(Vec<u8>),
    Character(char),
    ByteCharacter(u8),
    U8 { value: u8, suffixed: bool },
    U16 { value: u16, suffixed: bool },
    U32 { value: u32, suffixed: bool },
    U64 { value: u64, suffixed: bool },
    U128 { value: u128, suffixed: bool },
    Usize { value: usize, suffixed: bool },
    I8 { value: i8, suffixed: bool },
    I16 { value: i16, suffixed: bool },
    I32 { value: i32, suffixed: bool },
    I64 { value: i64, suffixed: bool },
    I128 { value: i128, suffixed: bool },
    Isize { value: isize, suffixed: bool },
    F32 { value: f32, suffixed: bool },
    F64 { value: f64, suffixed: bool },
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
    (@ [$expr:expr] $($ident:ident, $suffixed:ident, $unsuffixed:ident),* $(,)?) => {
        match $expr {
            Literal::String(s) => Self::string(s),
            Literal::ByteString(s) => Self::byte_string(s),
            Literal::Character(c) => Self::character(*c),
            Literal::ByteCharacter(_) => {
                //Self::byte_character(b), // nightly
                panic!("ByteCharacter is a nightly feature");
            }
            $(
                Literal::$ident { value, suffixed } => {
                    if *suffixed {
                        Self::$suffixed(*value)
                    } else {
                        Self::$unsuffixed(*value)
                    }
                }
            )*
        }
    };
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
