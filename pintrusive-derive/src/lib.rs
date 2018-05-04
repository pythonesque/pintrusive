extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use syn::DeriveInput;

#[proc_macro_derive(PinFields, attributes(PinDrop_may_dangle))]
#[allow(missing_docs)]
#[doc(hidden)]
pub fn hello_world(input: TokenStream) -> TokenStream {
    // Construct a string representation of the type definition
    // let s = input.to_string();
    
    // Parse the string representation
    // let ast = syn::parse_derive_input(&s).unwrap();
    let ast: DeriveInput = syn::parse(input).unwrap();

    // Build the impl
    let gen = impl_hello_world(&ast);
    
    // Return the generated impl
    // gen.parse().unwrap()
    gen.into()
}

fn impl_hello_world(ast: &syn::DeriveInput) -> quote::Tokens {
    let input_type = &ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let mut attr = ast.attrs.iter().map(syn::Attribute::interpret_meta)
        .filter_map( |x| match x {
            Some(syn::Meta::NameValue(syn::MetaNameValue { ident, lit, .. })) => {
                if ident == "PinDrop_may_dangle" { Some(lit) } else { None }
            },
            _ => None,
        });
    if let Some(lit) = attr.next() {
        quote! {
            /* #[allow(missing_docs)]
            #[doc(hidden)]
            unsafe impl#impl_generics ReflectPinDrop for #input_type#ty_generics #where_clause {} */

            #[allow(missing_docs)]
            unsafe impl#impl_generics PinFields for #input_type#ty_generics #where_clause {}

            /* #[allow(missing_docs)]
            #[doc(hidden)]
            impl#impl_generics PinDropInternal for #input_type#ty_generics #where_clause {
                const_assert!(is_valid; /*::core::intrinsics::needs_drop::<#name>()*/<Self as ReflectPinDrop>::REFLECT_PIN_DROP);
            } */

            impl#impl_generics Drop for #input_type#ty_generics #where_clause {
                fn drop(&mut self) {
                    unsafe {
                        PinDrop::pin_drop(Dropping::new(Pin::new_unchecked(self)))
                    }
                }
            }
        }
        // panic!("#[derive(HelloWorld)] is only defined for non Drop!");
        // println!("{:?}", ast.attrs.iter().map(syn::Attribute::interpret_meta).collect::<Vec<_>>().into::<String>());
    } else {
        quote! {
            #[allow(missing_docs)]
            #[doc(hidden)]
            unsafe impl#impl_generics ReflectDrop for #input_type#ty_generics #where_clause {}

            // #[allow(missing_docs)]
            // #[doc(hidden)]
            // unsafe impl#impl_generics ReflectPinDrop for #input_type#ty_generics #where_clause {}

            // Observation: bounds on the Drop impl for T are always the same as the bounds
            // on the structure itself (or such is my understanding; if this stops being
            // true with impl specialization that makes me kind of sad, but it doesn't
            // really kill this approach, just makes it less convenient as a custom derive).
            // Therefore, if T has Drop impls with any where bounds, it has a Drop impl at all
            // where bounds, so we can be confident that a PinFields impl that exists because
            // there is no Drop bound is always accurate.  The only possible exceptions to this are
            // existentials, because trait objects can hide the existence of the Drop impl;
            // fortunately, bare trait objects can have neither directly have fields nor Drop impls,
            // and both are required in order to produce unsoundness.  Although one could imagine bare
            // trait objects getting associated fields at some point, avoiding giving them direct
            // Drop impls seems pretty reasonable to me (at least, I can't think of a very good use
            // case, but maybe I lack sufficient imagination.  But if we ever wanted to add that
            // functionality [which might become more useful if there were associated fields?] we
            // could always restrict it to Pin<Self> from the getgo).  Similarly, opaque types are
            // never (soundly) allowed to move, so we should be okay there (and in any case they don't
            // provide field projections, by definition).  As for other (non-bare) existentials, like
            // trait objects behind the various pointers, we don't have to worry about them at
            // all--pinning doesn't project through pointer types.
            #[allow(missing_docs)]
            unsafe impl#impl_generics PinFields for #input_type#ty_generics #where_clause {}

            #[allow(missing_docs)]
            #[doc(hidden)]
            impl#impl_generics PinDropInternal for #input_type#ty_generics #where_clause {
                const_assert!(is_valid; /* !/*::core::intrinsics::needs_drop::<#name>()*/<Self as ReflectDrop>::REFLECT_DROP */true);
            }
        }
    }
    // Key idea: we create an alternate version of the pinned type, and provide `Deref` and
    // `DerefMut` implementations to convert from the outer pinned structure to the inner one.  The
    // alternate version is completely identical to the original structure, except that every field
    // is surrounded with a Pin.
    // match ast.data {
    //     syn::Data::Struct(_) => {
    //     },
    // }
    /* let name = &ast.ident;
    // Check if derive(HelloWorld) was specified for a struct
    if let syn::Data::Struct(_) = ast.data {
        // Yes, this is a struct
        quote! {
            /* {
                const fn foo() {
                    /* const_assert!(!::core::intrinsics::needs_drop::<#name>()); */
                }
            } */
            impl#impl_generics ReflectDrop for #input_type#ty_generics #where_clause
            impl ReflectDrop for #name {}

            // Observation: bounds on Drop are always the same as the bounds on the structure
            // itself (or such is my understanding; if this stops being true with impl
            // specialization that makes me kind of sad, but it doesn't really kill this approach,
            // just makes it less convenient as a custom derive).

            unsafe impl PinFields for #name {}
            impl PinDropInternal for #name {
                const_assert!(is_valid; !/*::core::intrinsics::needs_drop::<#name>()*/<Self as ReflectDrop>::IS_DROP);
            }
            impl HelloWorld for #name {
                fn hello_world() {
                    println!("Hello, World! My name is {}", stringify!(#name));
                }
            }
        }
    } else {
        //Nope. This is an Enum. We cannot handle these!
       panic!("#[derive(HelloWorld)] is only defined for structs, not for enums!");
    } */
}

/* #[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
} */
