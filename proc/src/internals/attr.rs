use std::hash::Hash;

use proc_macro2::{Group, Span, TokenStream, TokenTree};
use quote::ToTokens;
use syn::meta::ParseNestedMeta;

use super::ctxt::*;
use super::symbol::*;

pub enum LegacyMeta {
    Lit(syn::Lit),
    NameValue(syn::MetaNameValue),
}

impl syn::parse::Parse for LegacyMeta {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        Ok(if lookahead.peek(syn::Ident) {
            Self::NameValue(input.parse()?)
        } else {
            Self::Lit(input.parse()?)
        })
    }
}

pub struct IdMacro {
    pub id: TlId,
    pub scheme: Scheme,
}

impl IdMacro {
    pub fn from_ast(cx: &Ctxt, outer: &TokenStream, args: &[LegacyMeta]) -> Option<Self> {
        let mut id = Attr::none(cx, ID);
        let mut scheme = Attr::none(cx, SCHEME);

        for meta in args {
            match meta {
                LegacyMeta::Lit(lit) => {
                    // Parse `0x123456` or "boolTrue"`
                    if let Ok(n) = get_tl_id_inline(cx, lit) {
                        id.set(lit, n);
                    }
                }
                LegacyMeta::NameValue(meta) => {
                    if meta.path == SCHEME {
                        // Parse `#[tl(scheme = "path/to/scheme.tl")]`
                        if let Ok(n) = get_lit_str_from_expr(cx, SCHEME, &meta.value) {
                            scheme.set(
                                &meta.path,
                                Scheme {
                                    original: meta.to_token_stream(),
                                    source: SchemeSource::File { path: n.clone() },
                                },
                            );
                        }
                    } else if meta.path == SCHEME_INLINE {
                        // Parse `#[tl(scheme_inline = "boolTrue = Bool")]`
                        if let Ok(n) = get_lit_str_from_expr(cx, SCHEME_INLINE, &meta.value) {
                            scheme.set(
                                &meta.path,
                                Scheme {
                                    original: meta.to_token_stream(),
                                    source: SchemeSource::Inline { content: n.clone() },
                                },
                            );
                        }
                    } else {
                        let path = meta.path.to_token_stream().to_string().replace(' ', "");
                        cx.error_spanned_by(
                            meta,
                            format_args!("unknown tl id attribute `{}`", path),
                        );
                    }
                }
            }
        }

        let id = match id.get() {
            Some(id) => id,
            None => {
                cx.error_spanned_by(outer, "missing variant name");
                return None;
            }
        };

        let scheme = match scheme.get() {
            Some(scheme) => scheme,
            None => {
                cx.error_spanned_by(
                    outer,
                    "missing scheme attribute. `scheme = \"path/to/scheme.tl\"`",
                );
                return None;
            }
        };

        Some(IdMacro { id, scheme })
    }
}

pub struct Container {
    pub boxed: bool,
    pub id: Option<TlId>,
    pub scheme: Option<Scheme>,
    pub size_hint: Option<SizeHint>,
}

impl Container {
    pub fn from_ast(cx: &Ctxt, item: &syn::DeriveInput) -> Self {
        let mut boxed = BoolAttr::none(cx, BOXED);
        let mut id = Attr::none(cx, ID);
        let mut scheme = Attr::none(cx, SCHEME);
        let mut size_hint = Attr::none(cx, SIZE_HINT);

        for attr in &item.attrs {
            if attr.path() != TL {
                continue;
            }

            if let syn::Meta::List(meta) = &attr.meta {
                if meta.tokens.is_empty() {
                    continue;
                }
            }

            if let Err(e) = attr.parse_nested_meta(|meta| {
                if meta.path == BOXED {
                    // Parse `#[tl(boxed)]`
                    boxed.set_true(meta.path);
                } else if meta.path == ID {
                    // Parse `#[tl(id = 0x123456)]` or `#[tl(id = "boolTrue"]`
                    if let Some(s) = get_tl_id(cx, ID, &meta)? {
                        id.set(&meta.path, s);
                    }
                } else if meta.path == SCHEME {
                    // Parse `#[tl(scheme = "path/to/scheme.tl")]`
                    if let Some(s) = get_lit_str(cx, SCHEME, &meta)? {
                        scheme.set(
                            &meta.path,
                            Scheme {
                                original: s.to_token_stream(),
                                source: SchemeSource::File { path: s },
                            },
                        );
                    }
                } else if meta.path == SCHEME_INLINE {
                    // Parse `#[tl(scheme_inline = "boolTrue = Bool")]`
                    if let Some(s) = get_lit_str(cx, SCHEME_INLINE, &meta)? {
                        scheme.set(
                            &meta.path,
                            Scheme {
                                original: s.to_token_stream(),
                                source: SchemeSource::Inline { content: s.clone() },
                            },
                        );
                    }
                } else if meta.path == SIZE_HINT {
                    // Parse `#[tl(size_hint = 10)]` or `#[tl(size_hint = "get_some_value()")]`
                    if let Some(s) = get_size_hint(cx, SIZE_HINT, &meta)? {
                        size_hint.set(&meta.path, s);
                    }
                } else {
                    let path = meta.path.to_token_stream().to_string().replace(' ', "");
                    return Err(
                        meta.error(format_args!("unknown tl container attribute `{}`", path))
                    );
                }
                Ok(())
            }) {
                cx.syn_error(e);
            }
        }

        Self {
            boxed: boxed.get(),
            id: id.get(),
            scheme: scheme.get(),
            size_hint: size_hint.get(),
        }
    }
}

pub struct Variant {
    pub id: Option<TlId>,
    pub size_hint: Option<SizeHint>,
}

impl Variant {
    pub fn from_ast(cx: &Ctxt, item: &syn::Variant) -> Self {
        let mut id = Attr::none(cx, ID);
        let mut size_hint = Attr::none(cx, SIZE_HINT);

        for attr in &item.attrs {
            if attr.path() != TL {
                continue;
            }

            if let syn::Meta::List(meta) = &attr.meta {
                if meta.tokens.is_empty() {
                    continue;
                }
            }

            if let Err(e) = attr.parse_nested_meta(|meta| {
                if meta.path == ID {
                    // Parse `#[tl(id = 0x123456)]` or `#[tl(id = "boolTrue"]`
                    if let Some(s) = get_tl_id(cx, ID, &meta)? {
                        id.set(&meta.path, s);
                    }
                } else if meta.path == SIZE_HINT {
                    // Parse `#[tl(size_hint = 10)]` or `#[tl(size_hint = "get_some_value()")]`
                    if let Some(s) = get_size_hint(cx, SIZE_HINT, &meta)? {
                        size_hint.set(&meta.path, s);
                    }
                } else {
                    let path = meta.path.to_token_stream().to_string().replace(' ', "");
                    return Err(meta.error(format_args!("unknown tl variant attribute `{}`", path)));
                }
                Ok(())
            }) {
                cx.syn_error(e);
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
    pub read_with: Option<syn::Expr>,
    pub write_with: Option<syn::Expr>,
    pub with: Option<syn::Expr>,
    pub flags: bool,
    pub flags_field: Option<(syn::Lit, FlagsField)>,
    pub flags_bit: Option<u8>,
    pub default_flags: Option<u32>,
    pub skip_write: bool,
    pub skip_read: bool,
    pub signature: bool,
}

impl Field {
    pub fn from_ast(cx: &Ctxt, field: &syn::Field) -> Self {
        let mut size_hint = Attr::none(cx, SIZE_HINT);
        let mut read_with = Attr::none(cx, READ_WITH);
        let mut write_with = Attr::none(cx, WRITE_WITH);
        let mut with = Attr::none(cx, WITH);
        let mut flags = BoolAttr::none(cx, FLAGS);
        let mut flags_field = None;
        let mut flags_bit = Attr::none(cx, FLAGS_BIT);
        let mut default_flags = Attr::none(cx, DEFAULT_FLAGS);
        let mut skip = BoolAttr::none(cx, SKIP);
        let mut skip_write = BoolAttr::none(cx, SKIP_WRITE);
        let mut skip_read = BoolAttr::none(cx, SKIP_READ);
        let mut signature = BoolAttr::none(cx, SIGNATURE);

        for attr in &field.attrs {
            if attr.path() != TL {
                continue;
            }

            if let syn::Meta::List(meta) = &attr.meta {
                if meta.tokens.is_empty() {
                    continue;
                }
            }

            if let Err(e) = attr.parse_nested_meta(|meta| {
                if meta.path == SIZE_HINT {
                    // Parse `#[tl(size_hint = 10)]` or `#[tl(size_hint = "get_some_value()")]`
                    if let Some(s) = get_size_hint(cx, SIZE_HINT, &meta)? {
                        size_hint.set(&meta.path, s);
                    }
                } else if meta.path == READ_WITH {
                    // Parse `#[tl(read_with = "some_function"]`
                    if let Some(expr) = parse_lit_into_expr(cx, READ_WITH, &meta)? {
                        read_with.set(&meta.path, expr);
                    }
                } else if meta.path == WRITE_WITH {
                    // Parse `#[tl(write_with = "some_function"]`
                    if let Some(expr) = parse_lit_into_expr(cx, WRITE_WITH, &meta)? {
                        write_with.set(&meta.path, expr);
                    }
                } else if meta.path == WITH {
                    // Parse `#[tl(with = "some_module"]`
                    if let Some(expr) = parse_lit_into_expr(cx, WITH, &meta)? {
                        with.set(&meta.path, expr);
                    }
                } else if meta.path == FLAGS {
                    // Parse `#[tl(flags)]`
                    if meta.path == FLAGS {
                        flags.set_true(&meta.path);
                    }
                } else if meta.path == FLAGS_BIT {
                    // Parse `#[tl(flags_bit = 0)]` or `#[tl(flags_bit = "field.0")]`
                    if let Some((lit, field, n)) = get_lit_flags_bit(cx, FLAGS_BIT, &meta)? {
                        match (&flags_field, field) {
                            (None, Some(field)) => {
                                flags_field = Some((lit, field));
                            }
                            (Some((_, flags_field)), Some(field)) => cx.error_spanned_by(
                                &lit,
                                format!(
                                    "either #[tl(flags_field = \"{flags_field}\", flags_bit = {n})] \
                                    or #[tl(flags_bit = \"{field}.{n}\"] can be used"
                                ),
                            ),
                            _ => {}
                        }
                        flags_bit.set(&meta.path, n);
                    }
                } else if meta.path == FLAGS_FIELD {
                    // Parse `#[tl(flags_field = "field")]`
                    if let Some(field) = get_lit_str(cx, FLAGS_FIELD, &meta)? {
                        flags_field = Some((
                            syn::Lit::Str(field.clone()),
                            FlagsField::new(&field.value()),
                        ));
                    }
                } else if meta.path == DEFAULT_FLAGS {
                    // Parse `#[tl(default_flags = 0x123123)]`
                    if let Some(n) = get_lit_number(cx, DEFAULT_FLAGS, &meta)? {
                        default_flags.set(&meta.path, n);
                    }
                } else if meta.path == SKIP {
                    // Parse `#[tl(skip)]`
                    skip.set_true(&meta.path);
                } else if meta.path == SKIP_WRITE {
                    // Parse `#[tl(skip_write)]`
                    skip_write.set_true(&meta.path);
                } else if meta.path == SKIP_READ {
                    // Parse `#[tl(skip_read)]`
                    skip_read.set_true(&meta.path);
                } else if meta.path == SIGNATURE {
                    // Parse `#[tl(signature)]`
                    signature.set_true(&meta.path);
                } else {
                    let path = meta.path.to_token_stream().to_string().replace(' ', "");
                    return Err(meta.error(format_args!("unknown tl field attribute `{}`", path)));
                }
                Ok(())
            }) {
                cx.syn_error(e);
            }
        }

        let flags = flags.get();

        let with = with.get();
        let read_with = read_with.get();
        let write_with = write_with.get();
        let size_hint = size_hint.get();

        match (&with, &read_with, &write_with, &size_hint) {
            (Some(_), _, _, _) | (_, Some(_), _, _) | (_, _, Some(_), _) if flags => {
                cx.error_spanned_by(
                    field,
                    "Attribute `flags` can't be used with `with`, `read_with` or `write_with`",
                );
            }
            (Some(_), Some(_), _, _) | (Some(_), _, Some(_), _) | (Some(_), _, _, Some(_)) => {
                cx.error_spanned_by(
                    field,
                    "Attribute `with` can't be used with attibutes `size_hint`, `read_with` or `write_with`",
                );
            }
            (_, _, Some(_), None) => {
                cx.error_spanned_by(
                    field,
                    "Attribute `write_with` requires attibute `size_hint`",
                );
            }
            _ => {}
        };

        Self {
            size_hint,
            read_with,
            write_with,
            with,
            flags,
            flags_field,
            flags_bit: flags_bit.get(),
            default_flags: default_flags.get(),
            skip_write: skip.get() || skip_write.get(),
            skip_read: skip.get() || skip_read.get(),
            signature: signature.get(),
        }
    }
}

pub struct Scheme {
    pub original: TokenStream,
    pub source: SchemeSource,
}

pub enum SchemeSource {
    File { path: syn::LitStr },
    Inline { content: syn::LitStr },
}

pub enum SizeHint {
    Explicit { value: usize },
    Expression { expr: syn::Expr },
}

fn get_size_hint(
    cx: &Ctxt,
    attr_name: Symbol,
    meta: &ParseNestedMeta,
) -> syn::Result<Option<SizeHint>> {
    let expr: syn::Expr = meta.value()?.parse()?;
    let mut value = &expr;
    while let syn::Expr::Group(e) = value {
        value = &e.expr;
    }

    Ok(match value {
        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Int(lit),
            ..
        }) => Some(SizeHint::Explicit {
            value: lit.base10_parse()?,
        }),
        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Str(lit),
            ..
        }) => {
            let suffix = lit.suffix();
            if !suffix.is_empty() {
                cx.error_spanned_by(
                    &expr,
                    format!("unexpected suffix `{}` on string literal", suffix),
                );
            }
            Some(SizeHint::Expression {
                expr: lit.parse::<syn::Expr>()?,
            })
        }
        _ => {
            cx.error_spanned_by(
                expr,
                format!(
                    "expected tl {} attribute to be an integer \
                    or a string with expression: `{} = \"\"`",
                    attr_name, attr_name,
                ),
            );
            None
        }
    })
}

fn parse_lit_into_expr(
    cx: &Ctxt,
    attr_name: Symbol,
    meta: &ParseNestedMeta,
) -> syn::Result<Option<syn::Expr>> {
    let Some(s) = get_lit_str(cx, attr_name, meta)? else {
        return Ok(None);
    };

    let tokens = spanned_tokens(&s)?;
    let expr: syn::Expr = syn::parse2(tokens)?;
    Ok(Some(expr))
}

fn spanned_tokens(s: &syn::LitStr) -> syn::parse::Result<TokenStream> {
    let stream = syn::parse_str(&s.value())?;
    Ok(respan_token_stream(stream, s.span()))
}

fn respan_token_stream(stream: TokenStream, span: Span) -> TokenStream {
    stream
        .into_iter()
        .map(|token| respan_token_tree(token, span))
        .collect()
}

fn respan_token_tree(mut token: TokenTree, span: Span) -> TokenTree {
    if let TokenTree::Group(g) = &mut token {
        *g = Group::new(g.delimiter(), respan_token_stream(g.stream(), span));
    }
    token.set_span(span);
    token
}

fn get_lit_str(
    cx: &Ctxt,
    attr_name: Symbol,
    meta: &ParseNestedMeta,
) -> syn::Result<Option<syn::LitStr>> {
    get_lit_str2(cx, attr_name, attr_name, meta)
}

fn get_lit_str2(
    cx: &Ctxt,
    attr_name: Symbol,
    meta_item_name: Symbol,
    meta: &ParseNestedMeta,
) -> syn::Result<Option<syn::LitStr>> {
    let expr: syn::Expr = meta.value()?.parse()?;
    Ok(get_lit_str_from_expr2(cx, attr_name, meta_item_name, &expr).ok())
}

fn get_lit_str_from_expr(
    cx: &Ctxt,
    attr_name: Symbol,
    expr: &syn::Expr,
) -> Result<syn::LitStr, ()> {
    get_lit_str_from_expr2(cx, attr_name, attr_name, expr)
}

fn get_lit_str_from_expr2(
    cx: &Ctxt,
    attr_name: Symbol,
    meta_item_name: Symbol,
    mut expr: &syn::Expr,
) -> Result<syn::LitStr, ()> {
    while let syn::Expr::Group(e) = expr {
        expr = &e.expr;
    }
    if let syn::Expr::Lit(syn::ExprLit {
        lit: syn::Lit::Str(lit),
        ..
    }) = expr
    {
        let suffix = lit.suffix();
        if !suffix.is_empty() {
            cx.error_spanned_by(
                lit,
                format!("unexpected suffix `{}` on string literal", suffix),
            );
        }
        Ok(lit.clone())
    } else {
        cx.error_spanned_by(
            expr,
            format!(
                "expected {} attribute to be a string: `{} = \"...\"`",
                attr_name, meta_item_name
            ),
        );
        Err(())
    }
}

pub enum TlId {
    Explicit { value: u32, lit: TokenStream },
    FromScheme { value: String, lit: TokenStream },
}

impl TlId {
    pub fn unwrap_explicit(&self) -> u32 {
        match self {
            TlId::Explicit { value, .. } => *value,
            _ => panic!("Expected explicit tl id"),
        }
    }
}

impl Eq for TlId {}
impl PartialEq for TlId {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Explicit { value, .. }, Self::Explicit { value: other, .. }) => value == other,
            (Self::FromScheme { value, .. }, Self::FromScheme { value: other, .. }) => {
                value == other
            }
            _ => false,
        }
    }
}

impl Hash for TlId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Explicit { value, .. } => {
                Hash::hash(&::core::mem::discriminant(self), state);
                Hash::hash(value, state);
            }
            Self::FromScheme { value, .. } => {
                Hash::hash(&::core::mem::discriminant(self), state);
                Hash::hash(value, state);
            }
        }
    }
}

fn get_tl_id(cx: &Ctxt, attr_name: Symbol, meta: &ParseNestedMeta) -> syn::Result<Option<TlId>> {
    let expr: syn::Expr = meta.value()?.parse()?;
    let mut value = &expr;
    while let syn::Expr::Group(e) = value {
        value = &e.expr;
    }

    Ok(match value {
        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Str(lit),
            ..
        }) => {
            let suffix = lit.suffix();
            if !suffix.is_empty() {
                cx.error_spanned_by(
                    &expr,
                    format!("unexpected suffix `{}` on string literal", suffix),
                );
            }
            Some(TlId::FromScheme {
                value: lit.value().trim().to_string(),
                lit: lit.to_token_stream(),
            })
        }
        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Int(lit),
            ..
        }) => Some(TlId::Explicit {
            value: lit.base10_parse()?,
            lit: lit.to_token_stream(),
        }),
        _ => {
            cx.error_spanned_by(
                expr,
                format!(
                    "expected tl {} attribute to be a string or integer: `{} = \"...\"`",
                    attr_name, attr_name,
                ),
            );
            None
        }
    })
}

fn get_tl_id_inline(cx: &Ctxt, lit: &syn::Lit) -> Result<TlId, ()> {
    match lit {
        syn::Lit::Str(literal) => Ok(TlId::FromScheme {
            value: literal.value().trim().to_string(),
            lit: literal.to_token_stream(),
        }),
        syn::Lit::Int(lit) => Ok(TlId::Explicit {
            value: lit.base10_parse().map_err(|err| cx.syn_error(err))?,
            lit: lit.to_token_stream(),
        }),
        lit => {
            cx.error_spanned_by(lit, "expected integer value or string");
            Err(())
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum FlagsField {
    Named(String),
    Unnamed(u32),
}

impl FlagsField {
    fn new(field: &str) -> Self {
        let field = field.trim();
        match field.parse::<u32>() {
            Ok(idx) => Self::Unnamed(idx),
            Err(_) => Self::Named(field.to_string()),
        }
    }
}

impl quote::IdentFragment for FlagsField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::fmt::Display for FlagsField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Named(name) => f.write_str(name),
            Self::Unnamed(idx) => write!(f, "{idx}"),
        }
    }
}

impl From<&syn::Member> for FlagsField {
    fn from(m: &syn::Member) -> Self {
        match m {
            syn::Member::Named(name) => Self::Named(name.to_string()),
            syn::Member::Unnamed(m) => Self::Unnamed(m.index),
        }
    }
}

impl PartialEq<syn::Member> for FlagsField {
    fn eq(&self, rhs: &syn::Member) -> bool {
        match (self, rhs) {
            (Self::Named(name), syn::Member::Named(other)) => other == name,
            (Self::Unnamed(idx), syn::Member::Unnamed(other)) => other.index == *idx,
            _ => false,
        }
    }
}

fn get_lit_flags_bit(
    cx: &Ctxt,
    attr_name: Symbol,
    meta: &ParseNestedMeta,
) -> syn::Result<Option<(syn::Lit, Option<FlagsField>, u8)>> {
    let expr: syn::Expr = meta.value()?.parse()?;
    let mut value = &expr;
    while let syn::Expr::Group(e) = value {
        value = &e.expr;
    }

    match value {
        syn::Expr::Lit(syn::ExprLit {
            lit: lit @ syn::Lit::Int(l),
            ..
        }) => return Ok(Some((lit.clone(), None, l.base10_parse()?))),
        syn::Expr::Lit(syn::ExprLit {
            lit: lit @ syn::Lit::Str(l),
            ..
        }) => {
            let suffix = l.suffix();
            if !suffix.is_empty() {
                cx.error_spanned_by(
                    &expr,
                    format!("unexpected suffix `{}` on string literal", suffix),
                );
            }

            let string = l.value();
            let mut parts = string.split('.');
            if let (Some(field), Some(bit), None) = (parts.next(), parts.next(), parts.next()) {
                let bit = bit.trim();
                let Ok(bit) = (match bit.strip_prefix("0x") {
                    Some(bit) => u8::from_str_radix(bit, 16),
                    None => bit.parse::<u8>(),
                }) else {
                    cx.error_spanned_by(l, "failed to parse flags bit");
                    return Ok(None);
                };

                return Ok(Some((lit.clone(), Some(FlagsField::new(field)), bit)));
            }
        }
        _ => {}
    };

    cx.error_spanned_by(
        expr,
        format!(
            "expected tl {a} attribute to be an integer or a string: `{a} = 0` or `{a} = \"field.0\"`",
            a = attr_name,
        ),
    );
    Ok(None)
}

fn get_lit_number<T>(cx: &Ctxt, attr_name: Symbol, meta: &ParseNestedMeta) -> syn::Result<Option<T>>
where
    T: std::str::FromStr,
    T::Err: std::fmt::Display,
{
    let expr: syn::Expr = meta.value()?.parse()?;
    let mut value = &expr;
    while let syn::Expr::Group(e) = value {
        value = &e.expr;
    }
    Ok(
        if let syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Int(lit),
            ..
        }) = value
        {
            Some(lit.base10_parse()?)
        } else {
            cx.error_spanned_by(
                expr,
                format!(
                    "expected tl {} attribute to be an integer: `{} = \"...\"`",
                    attr_name, attr_name,
                ),
            );
            None
        },
    )
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
