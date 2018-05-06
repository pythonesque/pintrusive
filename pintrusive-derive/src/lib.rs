extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use quote::Tokens;
use syn::{
    Data,
    DataStruct,
    DeriveInput,
    GenericParam,
    Ident,
    Lifetime,
    LifetimeDef,
    Field,
    Fields,
    FieldsNamed,
    FieldsUnnamed,
    TypeParamBound,
};
use syn::punctuated::{
    Pair,
    Punctuated,
};
use std::iter::{FromIterator};

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

fn pin_field(lt: &Lifetime, field: &Field) -> Field {
    let ty = &field.ty;
    Field {
        // Avoiding attributes on fields for now, since it's not clear we want to carry them along.
        attrs: Vec::new(),
        vis: field.vis.clone(),
        ident: field.ident.clone(),
        colon_token: field.colon_token.clone(),
        ty: /* Type::Verbatim(TypeVerbatim { tts: */ /*ty.clone(), */parse_quote! { Pin<#lt, #ty> } /*})*/,
    }
}

fn pin_fields<P: Clone>(lt: &Lifetime, fields: &Punctuated<Field, P>) -> Punctuated<Field, P> {
    Punctuated::from_iter(fields.pairs().map(|pair| match pair {
        Pair::Punctuated(field, p) => Pair::Punctuated(pin_field(lt, field), p.clone()),
        Pair::End(field) => Pair::End(pin_field(lt, field)),
    }))
}

fn impl_hello_world(ast: &syn::DeriveInput) -> quote::Tokens {
    let input_type = &ast.ident;
    let vis = &ast.vis;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let mut attr = ast.attrs.iter().map(syn::Attribute::interpret_meta)
        .filter_map( |x| match x {
            Some(syn::Meta::NameValue(syn::MetaNameValue { ident, lit, .. })) => {
                if ident == "PinDrop_may_dangle" { Some(lit) } else { None }
            },
            _ => None,
        });
    let pin_type = Ident::from(format!("PinFields{}", input_type));
    let mut pin_generics = ast.generics.clone();
    // TODO: Make sure unique
    let new_lt: Lifetime = parse_quote!('__lifetime_used_for_derive_pinfields);
    for param in &mut pin_generics.params {
        match param {
            GenericParam::Type(ty) => {
                ty.colon_token = Some(parse_quote!(:));
                ty.bounds.push(TypeParamBound::Lifetime(new_lt.clone()));
                // let s : String = parse_quote!(#ty);
                // println!("Found type param {:?}", s);
                // let s = ty.bounds.iter()
                //     .filter_map(|x| if let TypeParamBound::Lifetime(x) = x { Some(x) } else { None })
                //     .map(|x| quote!(#x))
                //     .collect::<Vec<_>>();
                // println!("Found type param {:?}", s);
            },
            GenericParam::Lifetime(lt) => {
                lt.colon_token = Some(parse_quote!(:));
                lt.bounds.push(new_lt.clone());
            },
            // TODO: Figure out whether Const ever needs bounds?  It'll always be `'static`, right?
            GenericParam::Const(_) => {}
        }
        // let param_ = param.clone();
        // println!("Found type param {:?}", quote!(#param_));
    }
    pin_generics.params.push(GenericParam::Lifetime(LifetimeDef::new(new_lt.clone())));
    /* {
        let params_ = &pin_generics.params;
        println!("Found type param {:?}", quote!(#params_));
    } */
    let (pin_impl_generics, pin_ty_generics, pin_where_clause) = pin_generics.split_for_impl();
    let mut pin_impl_generics = &pin_impl_generics;
    let mut pin_ty_generics = &pin_ty_generics;
    let body =  match ast.data {
        Data::Struct(DataStruct { struct_token, ref fields, semi_token}) => {
            let fields = match fields {
                Fields::Named(FieldsNamed { brace_token, ref named }) => Fields::Named(FieldsNamed {
                    brace_token: brace_token.clone(),
                    named: pin_fields(&new_lt, named),
                }),
                Fields::Unnamed(FieldsUnnamed { paren_token, ref unnamed }) => Fields::Unnamed(FieldsUnnamed {
                    paren_token: paren_token.clone(),
                    unnamed: pin_fields(&new_lt, unnamed),
                }),
                Fields::Unit => Fields::Unit,
            };
            if fields.iter().next().is_none() {
                // No fields, so just replicate the structure of the original type.
                pin_impl_generics = &impl_generics;
                pin_ty_generics = &ty_generics;
            }
            let field_pat = match fields {
                Fields::Unnamed(FieldsUnnamed { ref unnamed, .. }) => {
                    let new_field_names: Punctuated<Tokens, _> = Punctuated::from_iter(unnamed.pairs()
                        .enumerate()
                        .map(|(index, pair)| {
                            let field_name = Ident::from(format!("field_{:?}", index));
                            Pair::new(quote!(ref mut #field_name), pair.punct().map(|&x| x.clone()))
                        }));
                    quote!(#input_type(#new_field_names))
                },
                Fields::Named(FieldsNamed { ref named, .. }) => {
                    let new_field_names: Punctuated<Tokens, _> = Punctuated::from_iter(named.pairs()
                        .map(|pair| {
                            let field_name = &pair.value().ident;
                            Pair::new(quote!(ref mut #field_name), pair.punct().map(|&x| x.clone()))
                        }));
                    quote!(#input_type { #new_field_names })
                },
                Fields::Unit => quote!(#input_type)
            };
            let field_constructor = match fields {
                Fields::Unnamed(FieldsUnnamed { ref unnamed, .. }) => {
                    let new_field_names: Punctuated<Tokens, _> = Punctuated::from_iter(unnamed.pairs()
                        .enumerate()
                        .map(|(index, pair)| {
                            let field_name = Ident::from(format!("field_{:?}", index));
                            Pair::new(quote!(Pin::new_unchecked(#field_name)), pair.punct().map(|&x| x.clone()))
                        }));
                    quote!(#pin_type(#new_field_names))
                },
                Fields::Named(FieldsNamed { ref named, .. }) => {
                    let new_field_names: Punctuated<Tokens, _> = Punctuated::from_iter(named.pairs()
                        .map(|pair| {
                            let field_name = &pair.value().ident;
                            Pair::new(quote!(#field_name: Pin::new_unchecked(#field_name)), pair.punct().map(|&x| x.clone()))
                        }));
                    quote!(#pin_type { #new_field_names })
                },
                Fields::Unit => quote!(#pin_type)
            };
            // println!("{:?}", quote!(#field_pat));
            quote! {
                #[allow(missing_docs)]
                #vis #struct_token #pin_type#pin_impl_generics #pin_where_clause #fields#semi_token

                impl#impl_generics #input_type#ty_generics #where_clause {
                    /// Projects a Pin'd reference of this type out to its immediate fields.
                    /// This is guaranteed to be safe by the implementation of the `PinFields` custom
                    /// derive macro.
                    pub fn deref_pin_mut<#new_lt>(self: &#new_lt mut Pin<Self>) -> #pin_type#pin_ty_generics {
                        unsafe {
                            let #field_pat = Pin::get_mut(self);
                            #field_constructor
                        }
                    }
                }
            }
        },/*syn::Data::Struct(quote! {
                #![allow(missing_docs)]
                struct Pin#input_type#ty_generics #where_clause {
                }
            },*/
        _ => panic!("Only handle structs at the moment.")
    };
    if let Some(lit) = attr.next() {
        quote! {
            #body
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
            #body

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
    // alternate version is completely identical to the original structure, except that it has an
    // extra lifetime (the pin lifetime) and every field is surrounded with a Pin of that lifetime.
    /* match ast.data {
        syn::Data::Struct(body) => {
            impl Deref<T> for
        },
    } */
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
