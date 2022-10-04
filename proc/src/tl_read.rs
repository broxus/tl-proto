use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

use super::{bound, dummy, scheme_loader, Derive};
use crate::internals::{ast, case, ctxt};

pub fn impl_derive_tl_read(input: syn::DeriveInput) -> Result<TokenStream, Vec<syn::Error>> {
    let cx = ctxt::Ctxt::new();
    let mut container = match ast::Container::from_ast(&cx, &input, Derive::Read) {
        Some(container) => container,
        None => return Err(cx.check().unwrap_err()),
    };
    scheme_loader::compute_tl_ids(&cx, &mut container);
    cx.check()?;

    let tl_lifetime: syn::LifetimeDef = syn::parse_quote!('tl);

    let ident = &container.ident;
    let generics = build_generics(&container);
    let (_, ty_generics, where_clause) = generics.split_for_impl();

    let mut alt_generics = generics.clone();
    if !alt_generics.params.iter().any(|param| match param {
        syn::GenericParam::Lifetime(def) => def.lifetime.eq(&tl_lifetime.lifetime),
        _ => false,
    }) {
        alt_generics
            .params
            .push(syn::GenericParam::Lifetime(tl_lifetime));
    }
    let (impl_generics, _, _) = alt_generics.split_for_impl();

    let (ids, body) = match &container.data {
        ast::Data::Enum(variants) => (build_tl_ids_enum(variants), build_enum(variants)),
        ast::Data::Struct(style, fields) => (
            build_tl_ids_struct(&container),
            build_struct(&container, style, fields),
        ),
    };

    let boxed = if container.attrs.boxed {
        quote! { _tl_proto::Boxed }
    } else {
        quote! { _tl_proto::Bare }
    };

    let result = quote! {
        #[allow(clippy::extra_unused_lifetimes)]
        impl #impl_generics #ident #ty_generics #where_clause {
            #ids
        }

        impl #impl_generics _tl_proto::TlRead<'tl> for #ident #ty_generics #where_clause {
            type Repr = #boxed;

            #body
        }
    };

    Ok(dummy::wrap_in_const("TL_READ", ident, result))
}

fn build_generics(container: &ast::Container) -> syn::Generics {
    let generics = bound::without_default(container.generics);

    bound::with_bound(
        container,
        &generics,
        |field, _| !field.skip_read,
        &syn::parse_quote!(_tl_proto::TlRead<'tl>),
    )
}

fn build_tl_ids_enum(variants: &[ast::Variant]) -> TokenStream {
    let ids = variants.iter().filter_map(|variant| {
        let id = variant.attrs.id.as_ref()?.unwrap_explicit();
        let ident = quote::format_ident!(
            "TL_ID_{}",
            case::screaming_snake_case(&variant.ident.to_string())
        );

        Some(quote! { pub const #ident: u32 = #id; })
    });

    quote! {
        #(#ids)*
    }
}

fn build_tl_ids_struct(container: &ast::Container) -> TokenStream {
    let id = container
        .attrs
        .boxed
        .then_some(container.attrs.id.as_ref())
        .flatten();
    let id = id
        .map(|id| {
            let id = id.unwrap_explicit();
            quote! { pub const TL_ID: u32 = #id; }
        })
        .into_iter();

    quote! {
        #(#id)*
    }
}

fn build_enum(variants: &[ast::Variant]) -> TokenStream {
    let variants = variants.iter().filter_map(|variant| {
        let id = variant.attrs.id.as_ref()?.unwrap_explicit();

        let ident = &variant.ident;
        let read_from = build_read_from(quote! { Self::#ident }, &variant.style, &variant.fields);
        Some(quote! {
            #id => {
                #read_from
            }
        })
    });

    quote! {
        fn read_from(__packet: &'tl [u8], __offset: &mut usize) -> _tl_proto::TlResult<Self> {
            match u32::read_from(__packet, __offset) {
                Ok(constructor) => match constructor {
                    #(#variants)*
                    _ => Err(_tl_proto::TlError::UnknownConstructor)
                },
                Err(e) => Err(e),
            }
        }
    }
}

fn build_struct(
    container: &ast::Container,
    style: &ast::Style,
    fields: &[ast::Field],
) -> TokenStream {
    let id = container
        .attrs
        .boxed
        .then_some(container.attrs.id.as_ref())
        .flatten();
    let prefix = id
        .map(|id| {
            let id = id.unwrap_explicit();
            quote! {
                match u32::read_from(__packet, __offset) {
                    Ok(constructor) => {
                        if constructor != #id {
                            return Err(_tl_proto::TlError::UnknownConstructor)
                        }
                    },
                    Err(e) => return Err(e),
                }
            }
        })
        .into_iter();

    let read_from = build_read_from(quote! { Self }, style, fields);

    quote! {
        fn read_from(__packet: &'tl [u8], __offset: &mut usize) -> _tl_proto::TlResult<Self> {
            #(#prefix)*
            #read_from
        }
    }
}

fn build_read_from(ident: TokenStream, style: &ast::Style, fields: &[ast::Field]) -> TokenStream {
    fn default_flags_name() -> TokenStream {
        quote! { __flags }
    }

    let has_multiple_flags_fields = fields.iter().filter(|field| field.attrs.flags).count() > 1;
    let make_flags_field_name = |ident: &dyn quote::IdentFragment| {
        if has_multiple_flags_fields {
            quote::format_ident!("__flags_{}", ident).to_token_stream()
        } else {
            default_flags_name()
        }
    };

    let make_ident_name = |member: &syn::Member, flags_field: bool| match member {
        _ if flags_field => make_flags_field_name(member),
        syn::Member::Named(member) => member.to_token_stream(),
        syn::Member::Unnamed(i) => quote::format_ident!("field_{}", i).to_token_stream(),
    };

    let reads = fields.iter().map(|field| {
        let ty = &field.ty;

        let ident = make_ident_name(&field.member, field.attrs.flags);

        if field.attrs.flags {
            quote! {
                let #ident = match <u32 as _tl_proto::TlRead<'tl>>::read_from(__packet, __offset) {
                    Ok(flags) => flags,
                    Err(e) => return Err(e),
                };
            }
        } else if field.attrs.skip_read {
            quote! { let #ident = Default::default(); }
        } else if let Some(flags_bit) = field.attrs.flags_bit {
            let mask = 0x1u32 << flags_bit;
            let flags_ident = match &field.attrs.flags_field {
                Some((_, field)) => make_flags_field_name(field),
                None => default_flags_name(),
            };

            let read = if let Some(with) = &field.attrs.with {
                quote! {
                    match #with::read(__packet, __offset) {
                        Ok(value) => value,
                        Err(e) => return Err(e),
                    }
                }
            } else if let Some(read_with) = &field.attrs.read_with {
                quote! {
                    match #read_with(__packet, __offset) {
                        Ok(value) => value,
                        Err(e) => return Err(e),
                    }
                }
            } else {
                quote! {
                    match <<#ty as IntoIterator>::Item as _tl_proto::TlRead<'tl>>::read_from(
                        __packet, __offset,
                    ) {
                        Ok(value) => value,
                        Err(e) => return Err(e),
                    }
                }
            };

            quote! {
                let #ident = if #flags_ident & #mask != 0 {
                    Some(#read)
                } else {
                    None
                };
            }
        } else if let Some(with) = &field.attrs.with {
            quote! {
                let #ident = match #with::read(__packet, __offset) {
                    Ok(value) => value,
                    Err(e) => return Err(e),
                };
            }
        } else if let Some(read_with) = &field.attrs.read_with {
            quote! {
                let #ident = match #read_with(__packet, __offset) {
                    Ok(value) => value,
                    Err(e) => return Err(e),
                };
            }
        } else {
            quote! {
                let #ident = match <#ty as _tl_proto::TlRead<'tl>>::read_from(__packet, __offset) {
                    Ok(value) => value,
                    Err(e) => return Err(e),
                };
            }
        }
    });

    let members = fields.iter().map(|field| match &field.member {
        syn::Member::Named(member) if field.attrs.flags => {
            quote! { #member: () }
        }
        syn::Member::Unnamed(_) if field.attrs.flags => {
            quote! { () }
        }
        member => make_ident_name(member, false),
    });

    match style {
        ast::Style::Struct => {
            quote! {
                #(#reads)*
                Ok(#ident {
                    #(#members),*,
                })
            }
        }
        ast::Style::Tuple => {
            quote! {
                #(#reads)*
                Ok(#ident(#(#members),*))
            }
        }
        ast::Style::Unit => quote! {
            Ok(#ident)
        },
    }
}
