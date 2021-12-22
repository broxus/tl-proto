pub const TL: Symbol = Symbol("tl");
pub const ID: Symbol = Symbol("id");
pub const FLAGS: Symbol = Symbol("flags");
pub const WHEN: Symbol = Symbol("when");
pub const SIZE_HINT: Symbol = Symbol("size_hint");
pub const BOXED: Symbol = Symbol("boxed");

#[derive(Copy, Clone)]
pub struct Symbol(&'static str);

impl PartialEq<Symbol> for syn::Ident {
    fn eq(&self, other: &Symbol) -> bool {
        self == other.0
    }
}

impl PartialEq<Symbol> for &syn::Ident {
    fn eq(&self, other: &Symbol) -> bool {
        *self == other.0
    }
}

impl PartialEq<Symbol> for syn::Path {
    fn eq(&self, other: &Symbol) -> bool {
        self.is_ident(other.0)
    }
}

impl PartialEq<Symbol> for &syn::Path {
    fn eq(&self, other: &Symbol) -> bool {
        self.is_ident(other.0)
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.0)
    }
}
