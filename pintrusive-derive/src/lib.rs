extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use quote::Tokens;
use std::iter::FromIterator;
use syn::punctuated::{Pair, Punctuated};
use syn::{
    Data, DataEnum, DataStruct, DeriveInput, Field, Fields, FieldsNamed, FieldsUnnamed,
    GenericParam, Ident, Lifetime, LifetimeDef, TypeParamBound, Variant,
};

#[proc_macro_derive(PinFields, attributes(PinDrop_may_dangle))]
#[allow(missing_docs)]
#[doc(hidden)]
pub fn pin_fields(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    let gen = impl_pin_fields(&ast);
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
        ty: parse_quote! { Pin<#lt, #ty> },
    }
}

fn pin_fields_list<P: Clone>(lt: &Lifetime, fields: &Punctuated<Field, P>) -> Punctuated<Field, P> {
    Punctuated::from_iter(fields.pairs().map(|pair| match pair {
        Pair::Punctuated(field, p) => Pair::Punctuated(pin_field(lt, field), p.clone()),
        Pair::End(field) => Pair::End(pin_field(lt, field)),
    }))
}

fn new_fields(lt: &Lifetime, fields: &Fields) -> Fields {
    match fields {
        Fields::Named(FieldsNamed {
            brace_token,
            ref named,
        }) => Fields::Named(FieldsNamed {
            brace_token: brace_token.clone(),
            named: pin_fields_list(lt, named),
        }),
        Fields::Unnamed(FieldsUnnamed {
            paren_token,
            ref unnamed,
        }) => Fields::Unnamed(FieldsUnnamed {
            paren_token: paren_token.clone(),
            unnamed: pin_fields_list(lt, unnamed),
        }),
        Fields::Unit => Fields::Unit,
    }
}

fn field_pat(fields: &Fields) -> Tokens {
    match fields {
        Fields::Unnamed(FieldsUnnamed { ref unnamed, .. }) => {
            let new_field_names: Punctuated<Tokens, _> =
                Punctuated::from_iter(unnamed.pairs().enumerate().map(|(index, pair)| {
                    let field_name = Ident::from(format!("field_{:?}", index));
                    Pair::new(
                        quote!(ref mut #field_name),
                        pair.punct().map(|&x| x.clone()),
                    )
                }));
            quote!((#new_field_names))
        }
        Fields::Named(FieldsNamed { ref named, .. }) => {
            let new_field_names: Punctuated<Tokens, _> =
                Punctuated::from_iter(named.pairs().map(|pair| {
                    let field_name = &pair.value().ident;
                    Pair::new(
                        quote!(ref mut #field_name),
                        pair.punct().map(|&x| x.clone()),
                    )
                }));
            quote!({ #new_field_names })
        }
        Fields::Unit => quote!(),
    }
}

fn field_constructor(fields: &Fields) -> Tokens {
    match fields {
        Fields::Unnamed(FieldsUnnamed { ref unnamed, .. }) => {
            let new_field_names: Punctuated<Tokens, _> =
                Punctuated::from_iter(unnamed.pairs().enumerate().map(|(index, pair)| {
                    let field_name = Ident::from(format!("field_{:?}", index));
                    Pair::new(
                        quote!(Pin::new_unchecked(#field_name)),
                        pair.punct().map(|&x| x.clone()),
                    )
                }));
            quote!((#new_field_names))
        }
        Fields::Named(FieldsNamed { ref named, .. }) => {
            let new_field_names: Punctuated<Tokens, _> =
                Punctuated::from_iter(named.pairs().map(|pair| {
                    let field_name = &pair.value().ident;
                    Pair::new(
                        quote!(#field_name: Pin::new_unchecked(#field_name)),
                        pair.punct().map(|&x| x.clone()),
                    )
                }));
            quote!({ #new_field_names })
        }
        Fields::Unit => quote!(),
    }
}

fn impl_pin_fields(ast: &syn::DeriveInput) -> quote::Tokens {
    let input_type = &ast.ident;
    let vis = &ast.vis;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let mut attr = ast.attrs
        .iter()
        .map(syn::Attribute::interpret_meta)
        .filter_map(|x| match x {
            Some(syn::Meta::NameValue(syn::MetaNameValue { ident, lit, .. })) => {
                if ident == "PinDrop_may_dangle" {
                    Some(lit)
                } else {
                    None
                }
            }
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
            }
            GenericParam::Lifetime(lt) => {
                lt.colon_token = Some(parse_quote!(:));
                lt.bounds.push(new_lt.clone());
            }
            // TODO: Figure out whether Const ever needs bounds?  It'll always be `'static`, right?
            GenericParam::Const(_) => {}
        }
    }
    pin_generics
        .params
        .push(GenericParam::Lifetime(LifetimeDef::new(new_lt.clone())));
    let (pin_impl_generics, pin_ty_generics, pin_where_clause) = pin_generics.split_for_impl();
    let mut pin_impl_generics = &pin_impl_generics;
    let mut pin_ty_generics = &pin_ty_generics;
    let body = match ast.data {
        Data::Struct(DataStruct {
            struct_token,
            ref fields,
            semi_token,
        }) => {
            if fields.iter().next().is_none() {
                // No fields, so just replicate the structure of the original type.
                pin_impl_generics = &impl_generics;
                pin_ty_generics = &ty_generics;
            }
            let new_fields = new_fields(&new_lt, &fields);
            let pat = field_pat(&fields);
            let constr = field_constructor(&fields);
            let fields = if let Fields::Named(_) = fields {
                quote! { #pin_where_clause #new_fields }
            } else {
                quote! { #new_fields #pin_where_clause }
            };
            quote! {
                #[allow(missing_docs, unused)]
                #vis #struct_token #pin_type#pin_impl_generics #fields#semi_token

                impl#impl_generics #input_type#ty_generics #where_clause {
                    /// Projects a Pin'd reference of this type out to its immediate fields.
                    /// This is guaranteed to be safe by the implementation of the `PinFields` custom
                    /// derive macro.
                    #[allow(unused)]
                    pub fn deref_pin_mut<#new_lt>(self: &#new_lt mut Pin<Self>) ->
                        #pin_type#pin_ty_generics {
                        unsafe {
                            let #input_type #pat = Pin::get_mut(self);
                            #pin_type #constr
                        }
                    }
                }
            }
        }
        Data::Enum(DataEnum {
            enum_token,
            ref variants,
            ..
        }) => {
            if variants.iter().next().is_none() {
                // No variants, so just replicate the structure of the original type.
                pin_impl_generics = &impl_generics;
                pin_ty_generics = &ty_generics;
            }
            let variants =
                Punctuated::<Variant, _>::from_iter(variants.pairs().map(|pair| match pair {
                    Pair::Punctuated(
                        Variant {
                            attrs,
                            ident,
                            fields,
                            discriminant,
                        },
                        p,
                    ) => Pair::Punctuated(
                        Variant {
                            attrs: attrs.clone(),
                            ident: ident.clone(),
                            fields: new_fields(&new_lt, fields),
                            discriminant: discriminant.clone(),
                        },
                        p.clone(),
                    ),
                    Pair::End(Variant {
                        attrs,
                        ident,
                        fields,
                        discriminant,
                    }) => Pair::End(Variant {
                        attrs: attrs.clone(),
                        ident: ident.clone(),
                        fields: new_fields(&new_lt, fields),
                        discriminant: discriminant.clone(),
                    }),
                }));
            let match_arms = Punctuated::<Tokens, _>::from_iter(variants.pairs().map(|pair| {
                let Variant { ident, fields, .. } = pair.value();
                let pat = field_pat(&fields);
                let constr = field_constructor(&fields);
                Pair::new(
                    quote!(#input_type::#ident #pat => #pin_type::#ident #constr),
                    pair.punct().map(|&x| x.clone()),
                )
            }));
            quote! {
                #[allow(missing_docs, unused)]
                #vis #enum_token #pin_type#pin_impl_generics #pin_where_clause { #variants }
                impl#impl_generics #input_type#ty_generics #where_clause {

                    /// Projects a Pin'd reference of this type out to the fields in its
                    /// immediate variants.
                    /// This is guaranteed to be safe by the implementation of the `PinFields` custom
                    /// derive macro.
                    #[allow(unused)]
                    pub fn deref_pin_mut<#new_lt>(self: &#new_lt mut Pin<Self>) ->
                        #pin_type#pin_ty_generics {
                        unsafe {
                            match Pin::get_mut(self) {
                                #match_arms
                            }
                        }
                    }
                }
            }
        }
        _ => {
            panic!("Cannot derive PinFields for union--it's not even safe to access them normally!")
        }
    };
    if let Some(_lit) = attr.next() {
        // FIXME: Want to use attributes to be able to replicate the functionality of may_dangle.
        quote! {
            #body
            /* #[allow(missing_docs)]
            #[doc(hidden)]
            unsafe impl#impl_generics ReflectPinDrop for #input_type#ty_generics #where_clause {} */

            #[allow(missing_docs)]
            unsafe impl#impl_generics PinFields for #input_type#ty_generics #where_clause {}

            /// NOTE: The Drop implementation here is autogenerated by the [PinFields] custom derive.  It calls the
            /// [PinDrop::pin_drop] method on the [PinDrop] trait (which is required for soundness in the presence
            /// of a custom drop definition and pinned field projections).  So if you're looking for this type's
            /// destructor, look there instead.
            impl#impl_generics Drop for #input_type#ty_generics #where_clause {
                fn drop(&mut self) {
                    // XXX: Since I am pretty sure `Drop` isn't supposed to be able to be specialized, even with
                    // specialization enabled, this `Drop` implementation should *always* conflict with any manual
                    // drop implementation on the same type, which guarantees soundness.  The reason we want the
                    // non-Drop case is in order to avoid adding spurious custom drop implementations, since rustc
                    // can't prove that they're equivalent to the default ones and therefore doesn't provide them
                    // with the same benefits as the defaults.
                    unsafe {
                        // Why pinning is okay here: essentially, the (revised) pinning API says
                        // the following about a pinned type's interaction with Drop: once pinned,
                        // in addition to normal Rust requirements (no UB), for !Unpin types we
                        // have:
                        //
                        // (1) the value should remain pinned until its drop is first called.
                        // (2) the value should remain a valid instance of its type at any
                        //     observable point in program execution until its own
                        //     drop function successfully completes (for instance, the memory for
                        //     it can't be deallocated without successfully calling drop first).
                        //
                        // It is true that the Drop implementation for a dyn trait and a type that
                        // it contained would have overlapping fields, which might seem concerning
                        // if we were to add a proposed feature like "associated fields".  Fortunately,
                        // Drop cannot be explicitly implemented for dyn traits, so we don't have to
                        // worry about the dyn trait's drop implementation invalidating assumptions
                        // about the type being pinned.  Thus, we can generally assume that (if the
                        // drop finishes successfully) the only code that will be accessing the
                        // fields are the immediate destructor for the type that the fields are on,
                        // and the fields' own destructors, with nothing in between.  Since the
                        // moment a type's destructor is run, pinning invariant (1) no longer needs
                        // to hold (in general), this greatly simplifies our task.
                        //
                        // Now, consider the cases: if this value was never pinned, then what we do
                        // here doesn't matter.  If it *was* successfully pinned, we want to show
                        // that if the original pin was correct *without* allowing field
                        // projections, then the field projections are correct.  Thanks to this API
                        // only providing Pin<Self> rather than &mut Self, we know that the drop for
                        // this type can't (in safe code) move out of any of its fields while
                        // this drop function is running; adding a field projection API doesn't
                        // change that.  Similarly, the contents of the fields cannot be deallocated
                        // in safe code.
                        //
                        // Next, since code must respect normal Rust invariants
                        // outside of drop, we know that calling drop on a pinned field "early" can
                        // only be safe if the drop is guaranteed to fix all references that were
                        // making assumptions about the pin still being there, regardless of Drop on
                        // the container.  Since you cannot get a &mut reference to these fields,
                        // only a Pin one (unless they are Unpin), this means that anyone trying to
                        // perform this "early drop" would have to be aware that the field was
                        // pinned when performing the drop, and would necessarily need to know it
                        // was safe for some reason.  This implies a shared invariant with the
                        // immediate container, requiring unsafe code to perform the unpin (note
                        // that we do not project through pointers, only direct fields).  While
                        // these cases might sometimes invalidate projections, they would never do
                        // so except in a manner that was contained to the types sharing the
                        // invariant (i.e., they shouldn't be able to cause otherwise safe code using
                        // field projections to become unsafe).
                        //
                        // Now, consider the drop code that runs on each field.
                        // Because the pinning invariants only last until drop is first called, it
                        // suffices to show that fields that have yet to have their destructors
                        // called on them remain pinned (in safe code).  This follows from the
                        // dropck rules; may_dangle asserts that values of some type or lifetime are
                        // only moved or dropped by this drop function; since the drop function
                        // has a pinned type semantically, in general this means the value can only
                        // be dropped (in correct unsafe code).  In all other cases, types with
                        // self-referential lifetimes and custom destructors will fail to compile.
                        //
                        // We are left with cases where fields have raw pointers that are used with
                        // self-referential functionality in Drop; these are not generally safe to
                        // construct, and any Drop implementations that mutate through such pointers
                        // must either ensure that they can be called safely in any order, or must
                        // rely on knowledge of the drop order in the parent container; the latter
                        // implies that the earlier field (at least) is unsafe to construct and its
                        // invariant is shared with the parent structure, and the former implies
                        // that both field destructors share the same invariant (so if one is
                        // `!Unpin`, the other presumably must know about it).  In particular, for
                        // (2) observe that if the later type is *nested* inside the earlier one,
                        // it does not get Pin projections if the earlier one has a regular Drop
                        // implementation--it's only when the precise field order of the immediate
                        // containing type is being relied upon for unsafe code that we encounter
                        // this possibility.
                        //
                        // The last case is the one I'm most uncertain about; I would very much like
                        // to have a clearer invariant to state that explains exactly why cases like
                        // the above would never be able to expose general-purpose safe interfaces
                        // without respecting pinning.  But I'm pretty sure they must; to be honest,
                        // I'm not even sure if relying on destructor order in the way (2) describes
                        // should be allowed at all, but for all I know that's how generators are
                        // supposed to work :P.  For case (1) it's a bit weirder, but it's probably
                        // a positive sign that the main places I've seen such interfaces so far is
                        // use pinning for intrusive collections; it seems possible that without
                        // pinning such APIs can't be made safe.
                        PinDrop::pin_drop(Dropping::new(Pin::new_unchecked(self)))
                    }
                }
            }
        }
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
                // FIXME: Pending fixing of const generics to register the presence of trait bounds.
                const_assert!(is_valid;
                // !/*::core::intrinsics::needs_drop::<#name>()*/<Self as ReflectDrop>::REFLECT_DROP
                true);
            }
        }
    }
}
