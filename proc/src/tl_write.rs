use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

use super::{bound, dummy, Derive};
use crate::internals::{ast, attr, ctxt};

pub fn impl_derive_tl_write(input: syn::DeriveInput) -> Result<TokenStream, Vec<syn::Error>> {
    let cx = ctxt::Ctxt::new();
    let container = match ast::Container::from_ast(&cx, &input, Derive::Write) {
        Some(container) => container,
        None => return Err(cx.check().unwrap_err()),
    };
    cx.check()?;

    let ident = &container.ident;
    let generics = build_generics(&container);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let body = match &container.data {
        ast::Data::Enum(variants) => build_enum(&container, variants),
        ast::Data::Struct(_, fields) => build_struct(&container, fields),
    };

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

fn build_enum(container: &ast::Container, variants: &[ast::Variant]) -> TokenStream {
    let build_field = |field: &ast::Field| match &field.member {
        syn::Member::Named(member) => quote! { #member },
        syn::Member::Unnamed(index) => quote::format_ident!("field_{}", index).to_token_stream(),
    };

    let max_size_hint = match &container.attrs.size_hint {
        Some(size_hint) => {
            let size_hint = build_size_hint(size_hint);
            if container.attrs.boxed {
                quote! { 4usize + #size_hint }
            } else {
                size_hint
            }
        }
        None if !variants.is_empty() => {
            let variants = map_variants(container, variants, |destructed, variant| {
                let body = build_max_size_hint(
                    &variant.fields,
                    build_field,
                    &variant.attrs.size_hint,
                    container.attrs.boxed,
                );
                quote! { #destructed => #body }
            });

            quote! {
                match self {
                    #(#variants),*,
                }
            }
        }
        None => quote! { 0usize },
    };

    let write_to = if !variants.is_empty() {
        let variants = map_variants(container, variants, |destructed, variant| {
            let body = build_write_to(
                &variant.fields,
                build_field,
                container.attrs.boxed,
                variant.attrs.id,
            );
            quote! { #destructed => { #body } }
        });

        quote! {
            match self {
                #(#variants),*,
            }
        }
    } else {
        quote! {}
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

fn map_variants<'a, F>(
    container: &'a ast::Container,
    variants: &'a [ast::Variant],
    mut f: F,
) -> impl Iterator<Item = TokenStream> + 'a
where
    F: FnMut(TokenStream, &'a ast::Variant) -> TokenStream + 'a,
{
    variants.iter().map(move |variant| {
        let ident = &container.ident;
        let variant_name = &variant.ident;
        let destructed = match &variant.style {
            ast::Style::Struct => {
                let mut skip_rest = false;
                let fields = variant
                    .fields
                    .iter()
                    .filter_map(|field| {
                        if field.attrs.skip_write {
                            skip_rest = true;
                            return None;
                        }

                        let ident = &field.member;
                        Some(quote! { #ident })
                    })
                    .collect::<Vec<_>>();

                let skip_rest = skip_rest.then(|| quote! { .. });

                quote! {
                    #ident::#variant_name {
                        #(#fields),*,
                        #skip_rest
                    }
                }
            }
            ast::Style::Tuple => {
                let fields = variant.fields.iter().enumerate().map(|(i, field)| {
                    if field.attrs.skip_write {
                        quote! { _ }
                    } else {
                        quote::format_ident!("field_{}", i).to_token_stream()
                    }
                });

                quote! {
                    #ident::#variant_name(#(#fields),*)
                }
            }
            ast::Style::Unit => quote! {
                #ident::#variant_name
            },
        };

        f(destructed, variant)
    })
}

fn build_struct(container: &ast::Container, fields: &[ast::Field]) -> TokenStream {
    let max_size_hint = build_max_size_hint(
        fields,
        |field: &ast::Field| {
            let member = &field.member;
            quote! { self.#member }
        },
        &container.attrs.size_hint,
        container.attrs.boxed,
    );

    let write_to = build_write_to(
        fields,
        |field: &ast::Field| {
            let member = &field.member;
            quote! { &self.#member }
        },
        container.attrs.boxed,
        container.attrs.id,
    );

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

fn build_max_size_hint<F>(
    fields: &[ast::Field],
    mut build_field: F,
    size_hint: &Option<attr::SizeHint>,
    boxed: bool,
) -> TokenStream
where
    F: FnMut(&ast::Field) -> TokenStream,
{
    if fields.is_empty() {
        let size: usize = if boxed { 4 } else { 0 };
        quote! { #size }
    } else {
        let tokens = match &size_hint {
            Some(size_hint) => build_size_hint(size_hint),
            None => {
                let fields = fields
                    .iter()
                    .filter(|field| !field.attrs.skip_write)
                    .map(|field| match &field.attrs.size_hint {
                        Some(size_hint) => build_size_hint(size_hint),
                        None => {
                            let field = build_field(field);
                            quote! { #field.max_size_hint() }
                        }
                    });
                quote! { #(#fields)+* }
            }
        };
        if boxed {
            quote! { 4usize + #tokens }
        } else {
            tokens
        }
    }
}

fn build_write_to<F>(
    fields: &[ast::Field],
    mut build_field: F,
    boxed: bool,
    id: Option<u32>,
) -> TokenStream
where
    F: FnMut(&ast::Field) -> TokenStream,
{
    let id = boxed.then(|| id).flatten();
    let prefix = id
        .map(|id: u32| quote! { _tl_proto::TlWrite::write_to::<P_>(&#id, packet); })
        .into_iter();

    let fields = prefix.chain(
        fields
            .iter()
            .filter(|field| !field.attrs.skip_write)
            .map(|field| {
                let field_name = build_field(field);
                if field.attrs.signature {
                    quote! {
                        if <P_ as _tl_proto::TlPacket>::TARGET == _tl_proto::TlTarget::Packet {
                            _tl_proto::TlWrite::write_to::<P_>(#field_name, packet);
                        } else {
                            <&[u8] as _tl_proto::TlWrite>::write_to::<P_>(&[].as_ref(), packet);
                        }
                    }
                } else {
                    quote! { _tl_proto::TlWrite::write_to::<P_>(#field_name, packet); }
                }
            }),
    );

    quote! {
        #(#fields)*
    }
}

fn build_size_hint(size_hint: &attr::SizeHint) -> TokenStream {
    match size_hint {
        attr::SizeHint::Explicit { value } => quote! { #value },
        attr::SizeHint::Expression { expr } => quote! { #expr },
    }
}
