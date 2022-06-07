pub const TL: Symbol = Symbol("tl");

pub const BOXED: Symbol = Symbol("boxed");
pub const ID: Symbol = Symbol("id");

pub const READ_WITH: Symbol = Symbol("read_with");
pub const WRITE_WITH: Symbol = Symbol("write_with");
pub const WITH: Symbol = Symbol("with");

pub const SIZE_HINT: Symbol = Symbol("size_hint");

pub const FLAGS: Symbol = Symbol("flags");
pub const FLAGS_BIT: Symbol = Symbol("flags_bit");
pub const DEFAULT_FLAGS: Symbol = Symbol("default_flags");

pub const SKIP: Symbol = Symbol("skip");
pub const SKIP_WRITE: Symbol = Symbol("skip_write");
pub const SKIP_READ: Symbol = Symbol("skip_read");
pub const SIGNATURE: Symbol = Symbol("signature");

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
