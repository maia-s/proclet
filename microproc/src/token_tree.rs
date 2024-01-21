pub trait TokenTreeExt {
    /// If the TokenTree is a group with delimiter None containing a single item,
    /// replace the group with that item, recursively.
    fn flatten_group(&mut self);
}

macro_rules! impl_token_tree_ext {
    ($($pm:ident: $feature:literal),*) => { $(
        #[cfg(feature = $feature)]
        impl TokenTreeExt for $pm::TokenTree {
            #[inline]
            fn flatten_group(&mut self) {
                while let Self::Group(group) = self {
                    if matches!(group.delimiter(), $pm::Delimiter::None) {
                        let mut stream = group.stream().into_iter();
                        if let Some(tt) = stream.next() {
                            if stream.next().is_none() {
                                *self = tt;
                                continue;
                            }
                        }
                    }
                    break;
                }
            }
        }
    )* };
}

impl_token_tree_ext!(proc_macro: "proc-macro", proc_macro2: "proc-macro2");
