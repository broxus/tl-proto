use proc_macro::TokenStream;
use quote::quote;

use self::id::*;
use self::tl_read::*;
use self::tl_write::*;

mod bound;
mod dummy;
mod id;
mod internals;
mod scheme_loader;
mod tl_read;
mod tl_write;

#[derive(Copy, Clone, Eq, PartialEq)]
enum Derive {
    Write,
    Read,
}

/// Computes TL id at compile time using the given variant name and scheme.
///
/// ### Example
/// ```rust
/// const ID1: u32 = tl_proto::id!("boolTrue", scheme = "my.tl");
///
/// // or with inline scheme
/// const ID2: u32 = tl_proto::id!(
///     "boolTrue",
///     scheme_inline = r##"
///         boolTrue = Bool;
///         boolFalse = Bool;
///     "##
/// );
/// ```
#[proc_macro]
pub fn id(input: TokenStream) -> TokenStream {
    let meta = {
        let input = input.clone();
        syn::parse_macro_input!(input as Vec<syn::NestedMeta>)
    };
    impl_id(&input.into(), &meta)
        .unwrap_or_else(to_compile_errors)
        .into()
}

#[proc_macro_derive(TlWrite, attributes(tl))]
pub fn derive_tl_write(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    impl_derive_tl_write(input)
        .unwrap_or_else(to_compile_errors)
        .into()
}

#[proc_macro_derive(TlRead, attributes(tl))]
pub fn derive_tl_read(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    impl_derive_tl_read(input)
        .unwrap_or_else(to_compile_errors)
        .into()
}

fn to_compile_errors(errors: Vec<syn::Error>) -> proc_macro2::TokenStream {
    let compile_errors = errors.iter().map(syn::Error::to_compile_error);
    quote!(#(#compile_errors)*)
}
