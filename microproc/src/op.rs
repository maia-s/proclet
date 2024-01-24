use crate::{prelude::*, PunctExt};
use std::mem;

#[derive(Clone)]
pub struct OpParser<P: PunctExt, F> {
    str: String,
    next: Option<char>,
    puncts: Vec<P::PunctExt>,
    accept: F,
}

impl<P: PunctExt, F: Fn(&str, Option<char>, &[P]) -> Option<T>, T> OpParser<P, F> {
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

    #[must_use]
    pub fn apply(&mut self, punct: P) -> Option<T> {
        if let Some(last) = self.puncts.last() {
            if last.spacing().is_alone() {
                self.clear();
            }
        }

        let terminating = punct.spacing().is_alone();
        let ch = mem::replace(&mut self.next, Some(punct.as_char()));
        if let Some(ch) = ch {
            self.str.push(ch);
        }
        self.puncts.push(punct);

        if terminating {
            self.str.push(self.next.unwrap());
            (self.accept)(&self.str, None, &self.puncts)
        } else if ch.is_some() {
            (self.accept)(&self.str, self.next, &self.puncts[..self.puncts.len() - 1])
        } else {
            None
        }
    }
}
