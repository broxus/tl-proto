use proc_macro2::TokenStream;

use super::internals::{attr, ctxt};
use super::scheme_loader;

pub struct IdMacroInput(Vec<attr::LegacyMeta>);

impl syn::parse::Parse for IdMacroInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let items =
            syn::punctuated::Punctuated::<attr::LegacyMeta, syn::Token!(,)>::parse_terminated(
                input,
            )?;
        Ok(Self(items.into_iter().collect()))
    }
}

pub fn impl_id(input: &TokenStream, meta: &IdMacroInput) -> Result<TokenStream, Vec<syn::Error>> {
    let cx = ctxt::Ctxt::new();
    let id = match attr::IdMacro::from_ast(&cx, input, &meta.0)
        .and_then(|attrs| scheme_loader::compute_tl_id(&cx, attrs.id, &attrs.scheme))
    {
        Some(attrs) => attrs,
        None => return Err(cx.check().unwrap_err()),
    };
    cx.check()?;

    Ok(format!("0x{id:08x}").parse().unwrap())
}
