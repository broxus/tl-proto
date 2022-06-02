use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

use super::{bound, dummy, Derive};
use crate::internals::{ast, case, ctxt};

pub fn impl_derive_tl_read(input: syn::DeriveInput) -> Result<TokenStream, Vec<syn::Error>> {
    let cx = ctxt::Ctxt::new();
    let container = match ast::Container::from_ast(&cx, &input, Derive::Read) {
        Some(container) => container,
        None => return Err(cx.check().unwrap_err()),
    };
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
        let id = variant.attrs.id?;
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
    let id = container.attrs.boxed.then(|| container.attrs.id).flatten();
    let id = id
        .map(|id| quote! { pub const TL_ID: u32 = #id; })
        .into_iter();

    quote! {
        #(#id)*
    }
}

fn build_enum(variants: &[ast::Variant]) -> TokenStream {
    let variants = variants.iter().filter_map(|variant| {
        let id = variant.attrs.id?;

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
            match u32::read_from(__packet, __offset)? {
                #(#variants)*
                _ => Err(_tl_proto::TlError::UnknownConstructor)
            }
        }
    }
}

fn build_struct(
    container: &ast::Container,
    style: &ast::Style,
    fields: &[ast::Field],
) -> TokenStream {
    let id = container.attrs.boxed.then(|| container.attrs.id).flatten();
    let prefix = id
        .map(|id| {
            quote! {
                if u32::read_from(__packet, __offset)? != #id {
                    return Err(_tl_proto::TlError::UnknownConstructor)
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
    let idents = fields.iter().map(|field| match &field.member {
        syn::Member::Named(member) => {
            let member = member.to_token_stream();
            if field.attrs.flags {
                quote! { #member: () }
            } else {
                member
            }
        }
        syn::Member::Unnamed(i) => {
            if field.attrs.flags {
                quote! { () }
            } else {
                quote::format_ident!("field_{}", i).to_token_stream()
            }
        }
    });

    let reads = idents.clone().zip(fields.iter()).map(|(ident, field)| {
        let ty = &field.ty;

        if field.attrs.flags {
            quote! {
                let __flags = <u32 as _tl_proto::TlRead<'tl>>::read_from(__packet, __offset)?;
            }
        } else if field.attrs.skip_read {
            quote! { let #ident = Default::default(); }
        } else if let Some(flags_bit) = field.attrs.flags_bit {
            let mask = 0x1u32 << flags_bit;
            quote! {
                let #ident = if __flags & #mask != 0 {
                    Some(<<#ty as IntoIterator>::Item as _tl_proto::TlRead<'tl>>::read_from(
                        __packet, __offset,
                    )?)
                } else {
                    None
                };
            }
        } else {
            quote! {
                let #ident = <#ty as _tl_proto::TlRead<'tl>>::read_from(__packet, __offset)?;
            }
        }
    });

    match style {
        ast::Style::Struct => {
            quote! {
                #(#reads)*
                Ok(#ident {
                    #(#idents),*,
                })
            }
        }
        ast::Style::Tuple => {
            quote! {
                #(#reads)*
                Ok(#ident(#(#idents),*))
            }
        }
        ast::Style::Unit => quote! {
            Ok(#ident)
        },
    }
}
