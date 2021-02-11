//! # Derive Utilities
//!
//! This crate holds derive macros to ease development of the main zephyr-lang crate.

use proc_macro::TokenStream;
use quote::quote;
use syn;

// ——————————————————————————————— Store IDs ——————————————————————————————— //

#[proc_macro_derive(Identifier)]
pub fn identifier_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_identifier(&ast)
}

fn impl_identifier(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl Identifier for #name {
            fn new(id: Id) -> Self {
                Self(id)
            }
        }
    };
    gen.into()
}
