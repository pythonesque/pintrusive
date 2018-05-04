// #![feature(core_intrinsics)]
// #![feature(rustc_const_unstable)]
#![feature(arbitrary_self_types,pin)]
// NOTE: This is unsound in the presence of specialization.  It could be fixed by the constant
// reflection trick, but unfortunately that breaks at the moment whenever there are trait bounds
// floating around.
// #![feature(specialization)]
// pub extern crate core;
#[macro_use]
pub extern crate pintrusive_derive;
#[macro_use]
pub extern crate static_assertions;

use std::mem::{Pin};

/// Purpose of Dropping is to be a wrapper around Pin<Self> that can't be safely constructed; this
/// is a hacky way to make it unsafe to call `pin_drop` without actually marking `pin_drop` unsafe
/// (which would mean a spurious use of unsafe in client code, and also that the body of the function
// was trusted, which isn't the intent).
pub struct Dropping<'a, T: ?Sized> where T: 'a { inner: Pin<'a, T> }

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
    fn pin_drop(this: Dropping<Self>);
}

pub unsafe trait PinFields {}

/* pub trait HelloWorld {
    fn hello_world();
} */

#[derive(PinFields)]
#[PinDrop_may_dangle = "the best French Toast"]
struct FrenchToast;

impl PinDrop for FrenchToast {
    fn pin_drop(this: Dropping<Self>) {
        let _this = this.as_pin();
    }
}
/* impl Drop for FrenchToast {
    fn drop(&mut self) {}
} */

fn foo<T: PinFields>() {}

/* {
    const fn foo() {
        /* const_assert!(!::std::intrinsics::needs_drop::<#name>()); */
    }
} */

#[derive(PinFields)]
// #[HelloWorldName = "the best Waffles"]
struct Waffles;

#[derive(PinFields)]
#[PinDrop_may_dangle = "the best Bears"]
struct Bears<T>(T);

impl<T> PinDrop for Bears<T> {
    fn pin_drop(this: Dropping<Self>) {
        let _this = this.as_pin();
    }
}

/* impl<T> Drop for Bears<T> {
    fn drop(&mut self) {}
} */

#[derive(PinFields)]
// #[HelloWorldName = "the best Pancakes"]
struct Pancakes;

fn main() {
    foo::<FrenchToast>();
    foo::<Waffles>();
    foo::<Pancakes>();
    foo::<Bears<FrenchToast>>();
    let x = Bears(3);
    match x {
        Bears(y) => y
    };
}
