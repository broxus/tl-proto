pub fn wrap_in_const(code: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let use_tl_proto = quote::quote! {
        #[allow(rust_2018_idioms, clippy::useless_attribute)]
        extern crate tl_proto as _tl_proto;
    };

    quote::quote! {
        #[doc(hidden)]
        #[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
        const _: () = {
            #use_tl_proto
            #code
        };
    }
}
