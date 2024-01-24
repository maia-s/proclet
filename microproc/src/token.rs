use crate::{op::OpParser, PunctExt};

pub trait Token {}

macro_rules! define_tokens {
    ($($token:ident[$($tt:tt)*]),* $(,)?) => {
        $(
            #[derive(Clone, Copy, Default)]
            pub struct $token;

            impl Token for $token {}

            #[cfg(feature = "proc-macro")]
            impl From<$token> for proc_macro::TokenStream {
                #[inline]
                fn from(_: $token) -> Self {
                    stringify!($($tt)*).parse().unwrap()
                }
            }

            #[cfg(feature = "proc-macro2")]
            impl From<$token> for proc_macro2::TokenStream {
                #[inline]
                fn from(_: $token) -> Self {
                    stringify!($($tt)*).parse().unwrap()
                }
            }

            #[cfg(feature = "proc-macro")]
            impl From<$token> for proc_macro::token_stream::IntoIter {
                #[inline]
                fn from(value: $token) -> Self {
                    let ts: proc_macro::TokenStream = value.into();
                    ts.into_iter()
                }
            }

            #[cfg(feature = "proc-macro2")]
            impl From<$token> for proc_macro2::token_stream::IntoIter {
                #[inline]
                fn from(value: $token) -> Self {
                    let ts: proc_macro2::TokenStream = value.into();
                    ts.into_iter()
                }
            }

            #[cfg(feature = "quote")]
            impl quote::ToTokens for $token {
                #[inline]
                fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                    let ts: proc_macro2::TokenStream = (*self).into();
                    tokens.extend(ts)
                }
            }
        )*

        #[macro_export]
        macro_rules! Token {
            $( ($($tt)*) => { $crate::token::$token }; )*
        }
    };
}

define_tokens! {
    And[&],
    AndAnd[&&],
    AndEq[&=],
    At[@],
    Caret[^],
    CaretEq[^=],
    Colon[:],
    Comma[,],
    Dollar[$],
    Dot[.],
    DotDot[..],
    DotDotDot[...],
    DotDotEq[..=],
    Eq[=],
    EqEq[==],
    FatArrow[=>],
    Ge[>=],
    Gt[>],
    LArrow[<-],
    Le[<=],
    Lt[<],
    Minus[-],
    MinusEq[-=],
    Ne[!=],
    Not[!],
    Or[|],
    OrEq[|=],
    OrOr[||],
    PathSep[::],
    Percent[%],
    PercentEq[%=],
    Plus[+],
    PlusEq[+=],
    Pound[#],
    Question[?],
    RArrow[->],
    Semi[;],
    Shl[<<],
    ShlEq[<<=],
    Shr[>>],
    ShrEq[>>=],
    Slash[/],
    SlashEq[/=],
    Star[*],
    StarEq[*=],
    Tilde[~],
}

pub struct InvalidOpError;

#[inline]
#[allow(clippy::type_complexity)]
pub fn rust_op_token_parser<P: PunctExt>(
) -> OpParser<P, fn(&str, Option<char>, &[P]) -> Option<Result<Box<dyn Token>, InvalidOpError>>> {
    OpParser::new(parse_rust_op_token)
}

pub fn parse_rust_op_token<P: PunctExt>(
    str: &str,
    next: Option<char>,
    _puncts: &[P],
) -> Option<Result<Box<dyn Token>, InvalidOpError>> {
    let token: Box<dyn Token> = match (str, next) {
        ("!", Some('=')) => return None,
        ("!", _) => Box::<Token![!]>::default(),
        ("!=", _) => Box::<Token![!=]>::default(),
        ("#", _) => Box::<Token![#]>::default(),
        ("$", _) => Box::<Token![$]>::default(),
        ("%", Some('=')) => return None,
        ("%", _) => Box::<Token![%]>::default(),
        ("%=", _) => Box::<Token![%=]>::default(),
        ("&", Some('&' | '=')) => return None,
        ("&", _) => Box::<Token![&]>::default(),
        ("&&", _) => Box::<Token![&&]>::default(),
        ("&=", _) => Box::<Token![&=]>::default(),
        ("*", Some('=')) => return None,
        ("*", _) => Box::<Token![*]>::default(),
        ("*=", _) => Box::<Token![*=]>::default(),
        ("+", Some('=')) => return None,
        ("+", _) => Box::<Token![+]>::default(),
        ("+=", _) => Box::<Token![+=]>::default(),
        (",", _) => Box::<Token![,]>::default(),
        ("-", Some('=' | '>')) => return None,
        ("-", _) => Box::<Token![-]>::default(),
        ("-=", _) => Box::<Token![-=]>::default(),
        ("->", _) => Box::<Token![->]>::default(),
        (".", Some('.')) => return None,
        (".", _) => Box::<Token![.]>::default(),
        ("..", Some('.' | '=')) => return None,
        ("..", _) => Box::<Token![..]>::default(),
        ("...", _) => Box::<Token![...]>::default(),
        ("..=", _) => Box::<Token![..=]>::default(),
        ("/", Some('=')) => return None,
        ("/", _) => Box::<Token![/]>::default(),
        ("/=", _) => Box::<Token![/=]>::default(),
        (":", Some(':')) => return None,
        (":", _) => Box::<Token![:]>::default(),
        ("::", _) => Box::<Token![::]>::default(),
        (";", _) => Box::<Token![;]>::default(),
        ("<", Some('-' | '<' | '=')) => return None,
        ("<", _) => Box::<Token![<]>::default(),
        ("<-", _) => Box::<Token![<-]>::default(),
        ("<<", Some('=')) => return None,
        ("<<", _) => Box::<Token![<<]>::default(),
        ("<<=", _) => Box::<Token![<<=]>::default(),
        ("<=", _) => Box::<Token![<=]>::default(),
        ("=", Some('=' | '>')) => return None,
        ("=", _) => Box::<Token![=]>::default(),
        ("==", _) => Box::<Token![==]>::default(),
        ("=>", _) => Box::<Token![=>]>::default(),
        (">", Some('=' | '>')) => return None,
        (">", _) => Box::<Token![>]>::default(),
        (">=", _) => Box::<Token![>=]>::default(),
        (">>", Some('=')) => return None,
        (">>", _) => Box::<Token![>>]>::default(),
        (">>=", _) => Box::<Token![>>=]>::default(),
        ("?", _) => Box::<Token![?]>::default(),
        ("@", _) => Box::<Token![@]>::default(),
        ("^", Some('=')) => return None,
        ("^", _) => Box::<Token![^]>::default(),
        ("^=", _) => Box::<Token![^=]>::default(),
        ("|", Some('=' | '|')) => return None,
        ("|", _) => Box::<Token![|]>::default(),
        ("|=", _) => Box::<Token![|=]>::default(),
        ("||", _) => Box::<Token![||]>::default(),
        ("~", _) => Box::<Token![~]>::default(),
        (_, None) => return Some(Err(InvalidOpError)),
        _ => return None,
    };
    Some(Ok(token))
}
