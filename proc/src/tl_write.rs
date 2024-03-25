use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use rustc_hash::FxHashMap;

use super::{bound, dummy, scheme_loader, Derive};
use crate::internals::{ast, attr, ctxt};

pub fn impl_derive_tl_write(input: syn::DeriveInput) -> Result<TokenStream, Vec<syn::Error>> {
    let cx = ctxt::Ctxt::new();
    let mut container = match ast::Container::from_ast(&cx, &input, Derive::Write) {
        Some(container) => container,
        None => return Err(cx.check().unwrap_err()),
    };
    scheme_loader::compute_tl_ids(&cx, &mut container);
    cx.check()?;

    let ident = &container.ident;
    let generics = build_generics(&container);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let body = match &container.data {
        ast::Data::Enum(variants) => build_enum(&container, variants),
        ast::Data::Struct(_, fields) => build_struct(&container, fields),
    };

    let boxed = if container.attrs.boxed {
        quote! { _tl_proto::Boxed }
    } else {
        quote! { _tl_proto::Bare }
    };

    let result = quote! {
        impl #impl_generics _tl_proto::TlWrite for #ident #ty_generics #where_clause {
            type Repr = #boxed;

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
            let variants = map_variants(
                container,
                variants,
                |variant, field| {
                    !field.attrs.flags
                        && variant.attrs.size_hint.is_none()
                        && field.attrs.size_hint.is_none()
                },
                |destructed, variant| {
                    let body = build_max_size_hint(
                        &variant.fields,
                        build_field,
                        &variant.attrs.size_hint,
                        container.attrs.boxed,
                    );
                    quote! { #destructed => #body }
                },
            );

            quote! {
                match self {
                    #(#variants),*,
                }
            }
        }
        None => quote! { 0usize },
    };

    let write_to = if !variants.is_empty() {
        let variants = map_variants(
            container,
            variants,
            |_, field| !field.attrs.flags,
            |destructed, variant| {
                let body = build_write_to(
                    &variant.fields,
                    build_field,
                    container.attrs.boxed,
                    &variant.attrs.id,
                );
                quote! { #destructed => { #body } }
            },
        );

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

        fn write_to<P_>(&self, __packet: &mut P_)
        where
            P_: _tl_proto::TlPacket
        {
            #write_to
        }
    }
}

fn map_variants<'a, P, F>(
    container: &'a ast::Container,
    variants: &'a [ast::Variant],
    mut filter_fields: P,
    mut f: F,
) -> impl Iterator<Item = TokenStream> + 'a
where
    P: FnMut(&'a ast::Variant, &'a ast::Field) -> bool + 'a,
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
                        if field.attrs.skip_write || !filter_fields(variant, field) {
                            skip_rest = true;
                            return None;
                        }

                        let ident = &field.member;
                        Some(quote! { #ident })
                    })
                    .collect::<Vec<_>>();

                let skip_rest = skip_rest.then(|| quote! { .. });

                match (fields.is_empty(), skip_rest.is_some()) {
                    (false, _) => quote! {
                        #ident::#variant_name {
                            #(#fields),*,
                            #skip_rest
                        }
                    },
                    (true, true) => quote! { #ident::#variant_name { .. } },
                    (true, false) => quote! { #ident::#variant_name },
                }
            }
            ast::Style::Tuple => {
                let fields = variant.fields.iter().enumerate().map(|(i, field)| {
                    if field.attrs.skip_write || !filter_fields(variant, field) {
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
        &container.attrs.id,
    );

    quote! {
        fn max_size_hint(&self) -> usize {
            #max_size_hint
        }

        fn write_to<P_>(&self, __packet: &mut P_)
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
                    .map(|field| {
                        match (field.attrs.flags, &field.attrs.size_hint, &field.attrs.with) {
                            (true, _, _) => quote! { 4usize },
                            (false, Some(size_hint), _) => build_size_hint(size_hint),
                            (false, None, Some(with)) => {
                                let field = build_field(field);
                                quote! { #with::size_hint(&#field) }
                            }
                            (false, None, None) => {
                                let field = build_field(field);
                                quote! { #field.max_size_hint() }
                            }
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
    id: &Option<attr::TlId>,
) -> TokenStream
where
    F: FnMut(&ast::Field) -> TokenStream,
{
    let flag_fields = fields
        .iter()
        .filter_map(|field| {
            if field.attrs.flags {
                let default_flags = field.attrs.default_flags.unwrap_or_default();

                let sum = std::iter::once(quote! { #default_flags })
                    .chain(fields.iter().filter_map(|other_field| {
                        if let Some((_, flags_field)) = &other_field.attrs.flags_field {
                            if flags_field != &field.member {
                                return None;
                            }
                        }

                        other_field.attrs.flags_bit.map(|flags_bit| {
                            let field_name = build_field(other_field);
                            quote! { (((#field_name).is_some() as u32) << #flags_bit) }
                        })
                    }))
                    .collect::<Vec<_>>();
                Some((attr::FlagsField::from(&field.member), sum))
            } else {
                None
            }
        })
        .collect::<FxHashMap<_, _>>();

    let id = boxed.then_some(id.as_ref()).flatten();
    let prefix = id
        .map(|id| {
            let id = id.unwrap_explicit();
            quote! { _tl_proto::TlWrite::write_to::<P_>(&#id, __packet); }
        })
        .into_iter();

    let fields = prefix.chain(
        fields
            .iter()
            .filter(|field| !field.attrs.skip_write)
            .map(|field| {
                let field_name = build_field(field);
                if field.attrs.flags {
                    let sum = flag_fields.get(&attr::FlagsField::from(&field.member)).unwrap();
                    quote! { <u32 as _tl_proto::TlWrite>::write_to::<P_>(&(#(#sum)|*), __packet); }
                } else {
                    let write_to = if let Some(with) = &field.attrs.with {
                        quote! { #with::write(#field_name, __packet); }
                    } else if let Some(write_with) = &field.attrs.write_with {
                        quote! { #write_with(#field_name, __packet); }
                    } else {
                        quote! { _tl_proto::TlWrite::write_to::<P_>(#field_name, __packet); }
                    };

                    if field.attrs.signature {
                        quote! {
                        if <P_ as _tl_proto::TlPacket>::TARGET == _tl_proto::TlTarget::Packet {
                            #write_to
                        } else {
                            <&[u8] as _tl_proto::TlWrite>::write_to::<P_>(&[].as_ref(), __packet);
                        }
                    }
                    } else {
                        write_to
                    }
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
