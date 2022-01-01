use proc_macro::TokenStream;
use quote::quote;

use self::tl_write::*;

mod bound;
mod dummy;
mod internals;
mod tl_write;

#[proc_macro_derive(TlWrite, attributes(tl))]
pub fn derive_tl_write(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    impl_derive_tl_write(input)
        .unwrap_or_else(to_compile_errors)
        .into()
}

#[proc_macro_derive(TlRead, attributes(tl))]
pub fn derive_tl_read(input: TokenStream) -> TokenStream {
    let _input = syn::parse_macro_input!(input as syn::DeriveInput);
    todo!()
}

#[proc_macro_derive(TlHash, attributes(tl))]
pub fn derive_tl_hash(input: TokenStream) -> TokenStream {
    let _input = syn::parse_macro_input!(input as syn::DeriveInput);
    todo!()
}

fn to_compile_errors(errors: Vec<syn::Error>) -> proc_macro2::TokenStream {
    let compile_errors = errors.iter().map(syn::Error::to_compile_error);
    quote!(#(#compile_errors)*)
}
