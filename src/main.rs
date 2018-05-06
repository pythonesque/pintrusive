#![feature(arbitrary_self_types, pin, optin_builtin_traits)]
// NOTE: This is unsound in the presence of specialization.  It could be fixed by the constant
// reflection trick, but unfortunately that breaks at the moment whenever there are trait bounds
// floating around.  However, I don't think this is a very fundamental issue: we really just need
// Drop -> PinDrop on types deriving PinFields, which could be enforced pretty easily by the compiler if it came to that.
// #![feature(specialization)]
#[macro_use]
pub extern crate pintrusive_derive;
#[macro_use]
pub extern crate static_assertions;

use bear::Bears;
use std::boxed::PinBox;
use std::marker::Unpin;
use std::mem::Pin;
use std::ops::Deref;

/// Purpose of Dropping is to be a wrapper around Pin<Self> that can't be safely constructed; this
/// is a hacky way to make it unsafe to call `pin_drop` without actually marking `pin_drop` unsafe
/// (which would mean a spurious use of unsafe in client code, and also that the body of the
/// function was trusted, which isn't the intent).
pub struct Dropping<'a, T: ?Sized>
where
    T: 'a,
{
    inner: Pin<'a, T>,
}

/// NOTE: Deref implementation is safe because it only gives immutable access to the pin, which is
/// always fine (for now).  We only need the Deref implementation for the same reason that Pin
/// currently has it (being able to use it as a Self trait), which means that it can go away if it
/// can go away on Pin (which is the only reason we wouldn't want this implementation).
impl<'a, T: ?Sized> Deref for Dropping<'a, T>
where
    T: 'a,
{
    type Target = T;

    fn deref(&self) -> &T {
        &self.inner
    }
}

impl<'a, T> Dropping<'a, T>
where
    T: ?Sized + 'a,
{
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
    const REFLECT_DROP: bool = false;
}

/*
#[allow(missing_docs)]
default impl<T> ReflectDrop for T {
    const REFLECT_DROP : bool = false;
} */

#[allow(missing_docs)]
#[doc(hidden)]
unsafe impl<T> ReflectDrop for T
where
    T: Drop + ?Sized,
{
    const REFLECT_DROP: bool = true;
}

#[allow(missing_docs)]
#[doc(hidden)]
pub trait PinDropInternal {
    fn is_valid();
}

/// Inheriting from Drop is mostly used as a lint, since people might otherwise accidentally write
/// a PinDrop implementation without remembering to indicate (using attributes) that this was
/// actually supposed to have a drop implementation at all.
pub trait PinDrop: Drop {
    fn pin_drop(self: Dropping<Self>);
}

pub unsafe trait PinFields {}

#[derive(Debug, PinFields)]
#[PinDrop_may_dangle = "the best French Toast"]
struct FrenchToast;

impl !Unpin for FrenchToast {}

impl PinDrop for FrenchToast {
    fn pin_drop(self: Dropping<Self>) {
        let _this = self.as_pin();
    }
}

fn foo<T: PinFields>() {}
fn bar<T: PinFields>(_: Pin<T>) {}

#[derive(PinFields, Debug)]
struct Waffles {
    x: usize,
}

#[derive(PinFields, Debug)]
enum MyOpt<T> {
    Some(T),
    None,
}

mod bear {
    use super::{Dropping, Pin, PinDrop, PinFields};
    #[derive(PinFields)]
    #[PinDrop_may_dangle = "the best Bears"]
    pub struct Bears<T, U>(T, pub U)
    where
        T: ::std::fmt::Debug;

    impl<T, U> PinDrop for Bears<T, U>
    where
        T: ::std::fmt::Debug,
    {
        fn pin_drop(self: Dropping<Self>) {
            let this = self.as_pin();
            println!(
                "Safely dropping my fields like {:?}, none of which can be moved out of
                     \
                 at this time if they are !Unpin
                     (i.e. I can't invalidate \
                 pinning invariants on my fields before drop is
                     called on \
                 the fields).",
                this.0
            );
        }
    }

    impl<T, U> Bears<T, U>
    where
        T: ::std::fmt::Debug,
    {
        pub fn new(t: T, u: U) -> Self {
            Bears(t, u)
        }
    }
}

#[derive(Debug, PinFields)]
struct Pancakes;

fn main() {
    foo::<FrenchToast>();
    foo::<Waffles>();
    foo::<Pancakes>();
    foo::<Bears<FrenchToast, Waffles>>();
    let toast = Bears::new(FrenchToast, Waffles { x: 3 });
    let mut x = PinBox::new(toast);
    {
        let mut y = x.as_pin();
        let y = y.deref_pin_mut().1;
        println!("{:?}", y);
    }
    let toast = Bears::new(FrenchToast, Waffles { x: 3 });
    let scissors = Bears::new(Pancakes, toast);
    let mut z = PinBox::new(scissors);
    let mut w = z.as_pin();
    let mut w = w.deref_pin_mut().1;
    let w = w.deref_pin_mut().1;
    bar(w);

    let mut z = Box::new(MyOpt::None);
    match *z {
        MyOpt::Some(ref x) => println!("{:?}", x),
        MyOpt::None => println!("It's nothing"),
    };
    *z = MyOpt::Some(FrenchToast);
    let mut foo = PinBox::from(z);
    let mut baz = foo.as_pin();
    match baz.deref_pin_mut() {
        PinFieldsMyOpt::Some(x) => println!("{:?}", x),
        PinFieldsMyOpt::None => println!("It's nothing"),
    }
}
