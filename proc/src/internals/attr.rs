use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::Meta::{List, NameValue, Path};
use syn::NestedMeta::{Lit, Meta};

use super::ctxt::*;
use super::symbol::*;

pub struct Container {
    pub boxed: bool,
    pub id: Option<u32>,
    pub size_hint: Option<SizeHint>,
}

impl Container {
    pub fn from_ast(cx: &Ctxt, item: &syn::DeriveInput) -> Self {
        let mut boxed = BoolAttr::none(cx, BOXED);
        let mut id = Attr::none(cx, ID);
        let mut size_hint = Attr::none(cx, SIZE_HINT);

        for meta_item in item
            .attrs
            .iter()
            .flat_map(|attr| get_meta_items(cx, attr))
            .flatten()
        {
            match &meta_item {
                // Parse `#[tl(boxed)]`
                Meta(Path(word)) if word == BOXED => {
                    boxed.set_true(word);
                }
                // Parse `#[tl(id = 0x123456)]`
                Meta(NameValue(m)) if m.path == ID => {
                    if let Ok(n) = get_lit_number(cx, ID, &m.lit) {
                        id.set(&m.path, n);
                    }
                }
                // Parse `#[tl(size_hint = 10)]` or `#[tl(size_hint = "get_some_value()")]`
                Meta(NameValue(m)) if m.path == SIZE_HINT => {
                    if let Ok(h) = get_size_hint(cx, SIZE_HINT, &m.lit) {
                        size_hint.set(&m.path, h);
                    }
                }
                Meta(meta_item) => {
                    let path = meta_item
                        .path()
                        .into_token_stream()
                        .to_string()
                        .replace(' ', "");
                    cx.error_spanned_by(
                        meta_item.path(),
                        format!("unknown tl container attribute `{}`", path),
                    );
                }
                Lit(lit) => {
                    cx.error_spanned_by(lit, "unexpected literal in tl container attribute");
                }
            }
        }

        Self {
            boxed: boxed.get(),
            id: id.get(),
            size_hint: size_hint.get(),
        }
    }
}

pub struct Variant {
    pub id: Option<u32>,
    pub size_hint: Option<SizeHint>,
}

impl Variant {
    pub fn from_ast(cx: &Ctxt, item: &syn::Variant) -> Self {
        let mut id = Attr::none(cx, ID);
        let mut size_hint = Attr::none(cx, SIZE_HINT);

        for meta_item in item
            .attrs
            .iter()
            .flat_map(|attr| get_meta_items(cx, attr))
            .flatten()
        {
            match &meta_item {
                // Parse `#[tl(id = 0x123456)]`
                Meta(NameValue(m)) if m.path == ID => {
                    if let Ok(n) = get_lit_number(cx, ID, &m.lit) {
                        id.set(&m.path, n);
                    }
                }
                // Parse `#[tl(size_hint = 10)]` or `#[tl(size_hint = "get_some_value()")]`
                Meta(NameValue(m)) if m.path == SIZE_HINT => {
                    if let Ok(h) = get_size_hint(cx, SIZE_HINT, &m.lit) {
                        size_hint.set(&m.path, h);
                    }
                }
                Meta(meta_item) => {
                    let path = meta_item
                        .path()
                        .into_token_stream()
                        .to_string()
                        .replace(' ', "");
                    cx.error_spanned_by(
                        meta_item.path(),
                        format!("unknown tl container attribute `{}`", path),
                    );
                }
                Lit(lit) => {
                    cx.error_spanned_by(lit, "unexpected literal in tl container attribute");
                }
            }
        }

        Self {
            id: id.get(),
            size_hint: size_hint.get(),
        }
    }
}

pub struct Field {
    pub size_hint: Option<SizeHint>,
    pub flags: bool,
    pub flags_bit: Option<u8>,
    pub default_flags: Option<u32>,
    pub skip_write: bool,
    pub skip_read: bool,
    pub signature: bool,
}

impl Field {
    pub fn from_ast(cx: &Ctxt, field: &syn::Field) -> Self {
        let mut size_hint = Attr::none(cx, SIZE_HINT);
        let mut flags = BoolAttr::none(cx, FLAGS);
        let mut flags_bit = Attr::none(cx, FLAGS_BIT);
        let mut default_flags = Attr::none(cx, DEFAULT_FLAGS);
        let mut skip = BoolAttr::none(cx, SKIP);
        let mut skip_write = BoolAttr::none(cx, SKIP_WRITE);
        let mut skip_read = BoolAttr::none(cx, SKIP_READ);
        let mut signature = BoolAttr::none(cx, SIGNATURE);

        for meta_item in field
            .attrs
            .iter()
            .flat_map(|attr| get_meta_items(cx, attr))
            .flatten()
        {
            match &meta_item {
                // Parse `#[tl(size_hint = 10)]` or `#[tl(size_hint = "get_some_value()")]`
                Meta(NameValue(m)) if m.path == SIZE_HINT => {
                    if let Ok(h) = get_size_hint(cx, SIZE_HINT, &m.lit) {
                        size_hint.set(&m.path, h);
                    }
                }
                // Parse `#[tl(flags)]`
                Meta(Path(word)) if word == FLAGS => {
                    flags.set_true(word);
                }
                // Parse `#[tl(flags_bit = 0x123456)]`
                Meta(NameValue(m)) if m.path == FLAGS_BIT => {
                    if let Ok(n) = get_lit_number(cx, FLAGS_BIT, &m.lit) {
                        flags_bit.set(&m.path, n);
                    }
                }
                Meta(NameValue(m)) if m.path == DEFAULT_FLAGS => {
                    if let Ok(n) = get_lit_number(cx, DEFAULT_FLAGS, &m.lit) {
                        default_flags.set(&m.path, n);
                    }
                }
                // Parse `#[tl(skip)]`
                Meta(Path(word)) if word == SKIP => {
                    skip.set_true(word);
                }
                // Parse `#[tl(skip_write)]`
                Meta(Path(word)) if word == SKIP_WRITE => {
                    skip_write.set_true(word);
                }
                // Parse `#[tl(skip_read)]`
                Meta(Path(word)) if word == SKIP_READ => {
                    skip_read.set_true(word);
                }
                // Parse `#[tl(signature)]`
                Meta(Path(word)) if word == SIGNATURE => {
                    signature.set_true(word);
                }
                // Other
                Meta(meta_item) => {
                    let path = meta_item
                        .path()
                        .into_token_stream()
                        .to_string()
                        .replace(' ', "");
                    cx.error_spanned_by(
                        meta_item.path(),
                        format!("unknown tl container attribute `{}`", path),
                    );
                }
                Lit(lit) => {
                    cx.error_spanned_by(lit, "unexpected literal in tl container attribute");
                }
            }
        }

        Self {
            size_hint: size_hint.get(),
            flags: flags.get(),
            flags_bit: flags_bit.get(),
            default_flags: default_flags.get(),
            skip_write: skip.get() || skip_write.get(),
            skip_read: skip.get() || skip_read.get(),
            signature: signature.get(),
        }
    }
}

pub enum SizeHint {
    Explicit { value: usize },
    Expression { expr: syn::Expr },
}

fn get_size_hint(cx: &Ctxt, attr_name: Symbol, lit: &syn::Lit) -> Result<SizeHint, ()> {
    match lit {
        syn::Lit::Int(literal) => match literal.base10_parse::<usize>() {
            Ok(value) => Ok(SizeHint::Explicit { value }),
            Err(e) => {
                cx.syn_error(e);
                Err(())
            }
        },
        syn::Lit::Str(expr) => match expr.parse::<syn::Expr>() {
            Ok(expr) => Ok(SizeHint::Expression { expr }),
            Err(e) => {
                cx.syn_error(e);
                Err(())
            }
        },
        _ => {
            cx.error_spanned_by(
                lit,
                format!(
                    "expected tl {} attribute to be an integer \
                    or a string with expression: `{} = \"\"`",
                    attr_name, attr_name
                ),
            );
            Err(())
        }
    }
}

fn get_lit_number<T>(cx: &Ctxt, attr_name: Symbol, lit: &syn::Lit) -> Result<T, ()>
where
    T: std::str::FromStr,
    T::Err: std::fmt::Display,
{
    if let syn::Lit::Int(lit) = lit {
        lit.base10_parse().map_err(|err| cx.syn_error(err))
    } else {
        cx.error_spanned_by(
            lit,
            format!(
                "expected tl {} attribute to be an integer: `{} = \"...\"`",
                attr_name, attr_name,
            ),
        );
        Err(())
    }
}

fn get_meta_items(cx: &Ctxt, attr: &syn::Attribute) -> Result<Vec<syn::NestedMeta>, ()> {
    if attr.path != TL {
        return Ok(Vec::new());
    }

    match attr.parse_meta() {
        Ok(List(meta)) => Ok(meta.nested.into_iter().collect()),
        Ok(other) => {
            cx.error_spanned_by(other, "expected #[tl(...)]");
            Err(())
        }
        Err(err) => {
            cx.syn_error(err);
            Err(())
        }
    }
}

struct BoolAttr<'c>(Attr<'c, ()>);

impl<'c> BoolAttr<'c> {
    fn none(cx: &'c Ctxt, name: Symbol) -> Self {
        BoolAttr(Attr::none(cx, name))
    }

    fn set_true<O>(&mut self, object: O)
    where
        O: ToTokens,
    {
        self.0.set(object, ());
    }

    fn get(&self) -> bool {
        self.0.value.is_some()
    }
}

struct Attr<'c, T> {
    cx: &'c Ctxt,
    name: Symbol,
    tokens: TokenStream,
    value: Option<T>,
}

impl<'c, T> Attr<'c, T> {
    fn none(cx: &'c Ctxt, name: Symbol) -> Self {
        Self {
            cx,
            name,
            tokens: TokenStream::new(),
            value: None,
        }
    }

    fn set<O>(&mut self, object: O, value: T)
    where
        O: ToTokens,
    {
        let tokens = object.into_token_stream();

        if self.value.is_some() {
            self.cx
                .error_spanned_by(tokens, format!("duplicate tl attribute `{}`", self.name));
        } else {
            self.tokens = tokens;
            self.value = Some(value);
        }
    }

    #[allow(unused)]
    fn set_opt<O>(&mut self, object: O, value: Option<T>)
    where
        O: ToTokens,
    {
        if let Some(value) = value {
            self.set(object, value);
        }
    }

    #[allow(unused)]
    fn set_if_none(&mut self, value: T) {
        if self.value.is_none() {
            self.value = Some(value);
        }
    }

    fn get(self) -> Option<T> {
        self.value
    }

    #[allow(unused)]
    fn get_with_tokens(self) -> Option<(TokenStream, T)> {
        match self.value {
            Some(value) => Some((self.tokens, value)),
            None => None,
        }
    }
}
