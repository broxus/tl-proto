use proc_macro2::TokenStream;
use quote::quote;

use super::{bound, dummy};
use crate::internals::{ast, attr, ctxt};

pub fn impl_derive_tl_write(input: syn::DeriveInput) -> Result<TokenStream, Vec<syn::Error>> {
    let cx = ctxt::Ctxt::new();
    let container = match ast::Container::from_ast(&cx, &input) {
        Some(container) => container,
        None => return Err(cx.check().unwrap_err()),
    };
    cx.check()?;

    let ident = &container.ident;
    let generics = build_generics(&container);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let body = build_body(&container);

    let result = quote! {
        impl #impl_generics _tl_proto::TlWrite for #ident #ty_generics #where_clause {
            #body
        }
    };

    Ok(dummy::wrap_in_const("TL_WRITE", ident, result))
}

fn build_generics(container: &ast::Container) -> syn::Generics {
    let generics = bound::without_default(container.generics);

    bound::with_bound(
        container,
        &generics,
        |field, _| !field.skip_write,
        &syn::parse_quote!(_tl_proto::TlWrite),
    )
}

fn build_body(container: &ast::Container) -> TokenStream {
    match &container.data {
        ast::Data::Enum(_variants) => todo!(),
        ast::Data::Struct(_, fields) => build_struct(container, fields),
    }
}

fn build_struct(container: &ast::Container, fields: &[ast::Field]) -> TokenStream {
    let max_size_hint = if fields.is_empty() {
        let size: usize = if container.attrs.boxed { 4 } else { 0 };
        quote! { #size }
    } else {
        let tokens = match &container.attrs.size_hint {
            Some(size_hint) => build_size_hint(size_hint),
            None => {
                let fields = fields
                    .iter()
                    .filter(|field| !field.attrs.skip_write)
                    .map(|field| match &field.attrs.size_hint {
                        Some(size_hint) => build_size_hint(size_hint),
                        None => {
                            let member = field.member.clone();
                            quote! { self.#member.max_size_hint() }
                        }
                    });
                quote! { #(#fields)+* }
            }
        };
        if container.attrs.boxed {
            quote! { 4 + #tokens }
        } else {
            tokens
        }
    };

    let write_to = {
        let id = container.attrs.boxed.then(|| container.attrs.id).flatten();
        let prefix = id
            .map(|id| quote! { u32::write_to(&#id, packet); })
            .into_iter();

        let fields = prefix.chain(fields.iter().filter(|field| !field.attrs.skip_write).map(
            |field| {
                let member = field.member.clone();
                quote! { _tl_proto::TlWrite::write_to::<P_>(&self.#member, packet); }
            },
        ));

        quote! {
            #(#fields)*
        }
    };

    quote! {
        fn max_size_hint(&self) -> usize {
            #max_size_hint
        }

        fn write_to<P_>(&self, packet: &mut P_)
        where
            P_: _tl_proto::TlPacket
        {
            #write_to
        }
    }
}

fn build_size_hint(size_hint: &attr::SizeHintType) -> TokenStream {
    match size_hint {
        attr::SizeHintType::Explicit { value } => quote! { #value },
        attr::SizeHintType::Expression { expr } => quote! { #expr },
    }
}
