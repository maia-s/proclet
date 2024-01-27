use crate::{prelude::*, Punct, PunctExt};
use std::{fmt::Display, mem};

#[allow(unused_imports)]
use crate::TokenStream; // used by docs

#[derive(Clone, Debug)]
pub struct Op<P: Punct>(String, Vec<P>);

impl<P: PunctExt> Op<P> {
    /// Create a new operator from a non-empty string, using the default span.
    /// Use [`Op::from_iter`] if you have an array or iterator of [`Punct`]s.
    #[inline]
    pub fn new(str: &str) -> Self {
        Self::from_iter(str.chars().map(|c| P::new(c, P::Spacing::Joint)))
    }
}

impl<P: Punct> Op<P> {
    /// Get this operator as a string.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Create a [`TokenStream`] from this operator.
    #[inline]
    pub fn to_token_stream(&self) -> P::TokenStream {
        P::TokenStream::from_iter(self.1.iter().cloned().map(P::TokenTree::from))
    }

    /// Turn this operator into a [`TokenStream`].
    #[inline]
    pub fn into_token_stream(self) -> P::TokenStream {
        P::TokenStream::from_iter(self.1.into_iter().map(P::TokenTree::from))
    }

    /// Get the span of this operator.
    ///
    /// For ops encompassing more than one [`Punct`], this will currently only get the span of the
    /// first `Punct`. This will change in the future if stable rust gets the ability to merge spans.
    #[inline]
    pub fn span(&self) -> P::Span {
        self.1.first().unwrap().span()
    }

    /// Set the span of this operator.
    #[inline]
    pub fn set_span(&mut self, span: P::Span) {
        for punct in self.1.iter_mut() {
            punct.set_span(span);
        }
    }
}

impl<P: Punct> AsRef<str> for Op<P> {
    #[inline]
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl<P: PunctExt> FromIterator<P> for Op<P> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = P>>(iter: I) -> Self {
        let mut str = String::new();
        let mut puncts: Vec<P> = iter
            .into_iter()
            .map(|mut punct| {
                str.push(punct.as_char());
                punct.set_spacing(P::Spacing::Joint);
                punct
            })
            .collect();
        if let Some(last) = puncts.last_mut() {
            last.set_spacing(P::Spacing::Alone);
        } else {
            panic!("empty operator");
        }
        Self(str, puncts)
    }
}

impl<P: Punct> IntoIterator for Op<P> {
    type IntoIter = std::vec::IntoIter<P>;
    type Item = P::Punct;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.1.into_iter()
    }
}

#[derive(Debug)]
pub struct InvalidOpError<P: Punct>(pub Vec<P>);

impl<P: Punct> std::error::Error for InvalidOpError<P> {}

impl<P: Punct> Display for InvalidOpError<P> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid op: `")?;
        for punct in self.0.iter() {
            write!(f, "{}", punct.as_char())?;
        }
        write!(f, "`")
    }
}

#[derive(Clone)]
pub struct OpParser<P: PunctExt, F> {
    str: String,
    puncts: Vec<P>,
    next: Option<P>,
    accept: F,
}

impl<P: PunctExt, F: Fn(&str, Option<char>) -> bool> OpParser<P, F> {
    #[inline]
    pub const fn new(accept: F) -> Self {
        Self {
            str: String::new(),
            next: None,
            puncts: Vec::new(),
            accept,
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.str.clear();
        self.puncts.clear();
        self.next = None;
    }

    #[inline]
    pub fn is_valid(&self, str: &str, next: Option<char>) -> bool {
        (self.accept)(str, next)
    }

    /// Add a `Punct` to the currently accumulating op. This may return a new [`Op`], or `None`
    /// if more puncts are needed. If `punct` has alone spacing and the accumulated op isn't
    /// valid, it returns an error. Call [`OpParser::finish`] to finish parsing.
    #[inline]
    pub fn apply(&mut self, punct: P) -> Option<Result<Op<P>, InvalidOpError<P>>> {
        let (mut punct, mut next_ch) = if let Some(next) = mem::take(&mut self.next) {
            let next_ch = next.spacing().is_joint().then_some(punct.as_char());
            self.next = Some(punct);
            (next, next_ch)
        } else if punct.spacing().is_alone() {
            (punct, None)
        } else {
            self.next = Some(punct);
            return None;
        };

        loop {
            self.str.push(punct.as_char());
            self.puncts.push(punct);

            if self.is_valid(&self.str, next_ch) {
                self.str.clear();
                return Some(Ok(Op::from_iter(mem::take(&mut self.puncts))));
            } else if let Some(next) = self.next.as_ref() {
                if next.spacing().is_alone() {
                    punct = mem::take(&mut self.next).unwrap();
                    next_ch = None;
                    continue;
                } else {
                    return None;
                }
            } else {
                self.str.clear();
                return Some(Err(InvalidOpError(mem::take(&mut self.puncts))));
            }
        }
    }

    /// Finish parsing the currently accumulated op.
    #[inline]
    pub fn finish(&mut self) -> Option<Result<Op<P>, InvalidOpError<P>>> {
        mem::take(&mut self.next).map(|punct| {
            self.str.push(punct.as_char());
            let is_valid = self.is_valid(&self.str, None);
            self.str.clear();
            self.puncts.push(punct);
            let puncts = mem::take(&mut self.puncts);
            if is_valid {
                Ok(Op::from_iter(puncts))
            } else {
                Err(InvalidOpError(puncts))
            }
        })
    }
}

impl<P: PunctExt> OpParser<P, fn(&str, Option<char>) -> bool> {
    #[inline]
    pub const fn new_rust() -> Self {
        Self::new(is_valid_rust_op)
    }
}

/// Returns `true` if `str` is a valid operator in Rust, unless, if `next` is `Some`,
/// appending `next` to `str` would also be a valid op.
#[inline]
pub fn is_valid_rust_op(str: &str, next: Option<char>) -> bool {
    match (str, next) {
        ("!", Some('=')) => false,
        ("!", _) => true,
        ("!=", _) => true,
        ("#", _) => true,
        ("$", _) => true,
        ("%", Some('=')) => false,
        ("%", _) => true,
        ("%=", _) => true,
        ("&", Some('&' | '=')) => false,
        ("&", _) => true,
        ("&&", _) => true,
        ("&=", _) => true,
        ("*", Some('=')) => false,
        ("*", _) => true,
        ("*=", _) => true,
        ("+", Some('=')) => false,
        ("+", _) => true,
        ("+=", _) => true,
        (",", _) => true,
        ("-", Some('=' | '>')) => false,
        ("-", _) => true,
        ("-=", _) => true,
        ("->", _) => true,
        (".", Some('.')) => false,
        (".", _) => true,
        ("..", Some('.' | '=')) => false,
        ("..", _) => true,
        ("...", _) => true,
        ("..=", _) => true,
        ("/", Some('=')) => false,
        ("/", _) => true,
        ("/=", _) => true,
        (":", Some(':')) => false,
        (":", _) => true,
        ("::", _) => true,
        (";", _) => true,
        ("<", Some('-' | '<' | '=')) => false,
        ("<", _) => true,
        ("<-", _) => true,
        ("<<", Some('=')) => false,
        ("<<", _) => true,
        ("<<=", _) => true,
        ("<=", _) => true,
        ("=", Some('=' | '>')) => false,
        ("=", _) => true,
        ("==", _) => true,
        ("=>", _) => true,
        (">", Some('=' | '>')) => false,
        (">", _) => true,
        (">=", _) => true,
        (">>", Some('=')) => false,
        (">>", _) => true,
        (">>=", _) => true,
        ("?", _) => true,
        ("@", _) => true,
        ("^", Some('=')) => false,
        ("^", _) => true,
        ("^=", _) => true,
        ("|", Some('=' | '|')) => false,
        ("|", _) => true,
        ("|=", _) => true,
        ("||", _) => true,
        ("~", _) => true,
        _ => false,
    }
}
