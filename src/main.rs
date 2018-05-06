// #![feature(core_intrinsics)]
// #![feature(rustc_const_unstable)]
#![feature(arbitrary_self_types,pin)]
#![feature(generic_associated_types)]
#![feature(more_struct_aliases)]
// NOTE: This is unsound in the presence of specialization.  It could be fixed by the constant
// reflection trick, but unfortunately that breaks at the moment whenever there are trait bounds
// floating around.
// #![feature(specialization)]
// pub extern crate core;
#[macro_use]
pub extern crate pintrusive_derive;
#[macro_use]
pub extern crate static_assertions;

use bear::Bears;
use std::mem::{Pin};
use std::ops::{Deref};

/// Purpose of Dropping is to be a wrapper around Pin<Self> that can't be safely constructed; this
/// is a hacky way to make it unsafe to call `pin_drop` without actually marking `pin_drop` unsafe
/// (which would mean a spurious use of unsafe in client code, and also that the body of the function
/// was trusted, which isn't the intent).
pub struct Dropping<'a, T: ?Sized> where T: 'a { inner: Pin<'a, T> }

/// NOTE: Deref implementation is safe because it only gives immutable access to the pin, which is
/// always fine (for now).  We only need the Deref implementation for the same reason that Pin
/// currently has it (being able to use it as a Self trait), which means that it can go away if it
/// can go away on Pin (which is the only reason we wouldn't want this implementation).
impl<'a, T: ?Sized> Deref for Dropping<'a, T> where T: 'a {
    type Target = T;

    fn deref(&self) -> &T {
        &self.inner
    }
}

impl<'a, T> Dropping<'a, T> where T: ?Sized + 'a {
    #[allow(missing_docs)]
    #[doc(hidden)]
    pub unsafe fn new(inner: Pin<'a, T>) -> Self {
        Dropping { inner }
    }

    /// Turn a `Dropping` into a `Pin`, consuming it in the process.  This is the only thing most
    /// code will ever need to do with `Dropping`, as it is passed to `pin_drop` instead of
    /// `Pin<T>`.
    pub fn as_pin(self) -> Pin<'a, T> {
        self.inner
    }
}

#[allow(missing_docs)]
#[doc(hidden)]
pub unsafe trait ReflectDrop {
    const REFLECT_DROP : bool = false;
}

/* #[allow(missing_docs)]
#[doc(hidden)]
pub unsafe trait ReflectPinDrop {
    const REFLECT_PIN_DROP : bool = false;
} */

/* default impl<T> ReflectDrop for T {
    const REFLECT_DROP : bool = false;
} */

#[allow(missing_docs)]
#[doc(hidden)]
unsafe impl<T> ReflectDrop for T where T : Drop + ?Sized {
    const REFLECT_DROP : bool = true;
}

/* #[allow(missing_docs)]
#[doc(hidden)]
unsafe impl<T> ReflectPinDrop for T where T : PinDrop + ?Sized {
    const REFLECT_PIN_DROP : bool = true;
} */

#[allow(missing_docs)]
#[doc(hidden)]
pub trait PinDropInternal {
    fn is_valid();
}

/// Inheriting from Drop is mostly used as a lint, since people might otherwise accidentally write
/// a PinDrop implementation without remembering to indicate (using attributes) that this was
/// actually supposed to have a drop implementation at all.
pub trait PinDrop : Drop {
    fn pin_drop(self: Dropping<Self>);
}

pub unsafe trait PinFields {}

/* pub trait HelloWorld {
    fn hello_world();
} */

#[derive(PinFields)]
#[PinDrop_may_dangle = "the best French Toast"]
struct FrenchToast;

impl PinDrop for FrenchToast {
    fn pin_drop(self: Dropping<Self>) {
        let _this = self.as_pin();
    }
}
/* impl Drop for FrenchToast {
    fn drop(&mut self) {}
} */

fn foo<T: PinFields>() {}
fn bar<T: PinFields>(_: Pin<T>) {}

/* {
    const fn foo() {
        /* const_assert!(!::std::intrinsics::needs_drop::<#name>()); */
    }
} */

#[derive(PinFields)]
#[derive(Debug)]
// #[HelloWorldName = "the best Waffles"]
struct Waffles { x: usize }

mod bear {
    use super::{Dropping, Pin, PinDrop, PinFields};
    #[derive(PinFields)]
    #[PinDrop_may_dangle = "the best Bears"]
    pub struct Bears<T, U>(T, pub U);

    impl<T, U> PinDrop for Bears<T, U> {
        fn pin_drop(self: Dropping<Self>) {
            let _this = self.as_pin();
        }
    }

    impl<T, U> Bears<T, U> {
        pub fn new(t: T, u: U) -> Self {
            Bears(t, u)
        }
    }
}

// In general PinBears fields should have the same visibility as do the parent struct's fields.
// struct PinBears<'a, T>(Pin<'a, T>) where T: 'a;

/* trait DerefPinMut<'a> where Self : PinFields + 'a {
    /// FIXME: Move PinMutTarget into PinFields.
    type PinMutTarget;

    fn deref_pin_mut(self: &'a mut Pin<Self>) -> <Self as DerefPinMut<'a>>::PinMutTarget;
}

impl<'a, T> DerefPinMut<'a> for Bears<T> where T: 'a {
    type PinMutTarget = PinBears<'a, T>;

    fn deref_pin_mut(self: &'a mut Pin<Self>) -> <Self as DerefPinMut<'a>>::PinMutTarget {
        unsafe {
            let Bears(ref mut i0) = Pin::get_mut(self);
            PinBears(Pin::new_unchecked(i0))
        }
    }
} */

/* impl<T> Bears<T> {
    fn deref_pin_mut_<'a>(self: &'a mut Pin<Self>) -> PinFieldsBears<'a, T> {
        unsafe {
            let Bears(ref mut i0) = Pin::get_mut(self);
            PinFieldsBears(Pin::new_unchecked(i0))
        }
    }
} */

/* impl<'a, 'b, T> From<&'b mut Pin<'a, Bears<T>>> for PinBears<'b, T> where Bears<T> : PinFields, T: 'a, {
    fn from(this: &'b mut Pin<'a, Bears<T>>) -> Self {
        unsafe {
            let Bears(ref mut i0) = Pin::get_mut(this);
            PinBears(Pin::new_unchecked(i0))
        }
    }
} */

/* impl<T> Drop for Bears<T> {
    fn drop(&mut self) {}
} */

#[derive(PinFields)]
// #[HelloWorldName = "the best Pancakes"]
struct Pancakes;

fn main() {
    // foo::<FrenchToast>();
    foo::<Waffles>();
    foo::<Pancakes>();
    foo::<Bears<FrenchToast, Waffles>>();
    let mut toast = Bears::new(FrenchToast, Waffles {x: 3 });
    let mut x = Pin::new(&mut toast);
    {
        let y = x.deref_pin_mut().1;
        println!("{:?}", y);
    }
    let mut scissors = Bears::new(Pancakes, x);
    let mut z = Pin::new(&mut scissors);
    let mut w = z.deref_pin_mut().1;
    let w = w.deref_pin_mut().1;
    bar(w);

    // let y = PinBears::from(&mut x).0;
}
