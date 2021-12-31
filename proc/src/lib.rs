use proc_macro::TokenStream;

mod bound;
mod internals;

#[proc_macro_derive(ReadFromPacket, attribute(tl))]
pub fn derive_tl(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    todo!()
}
