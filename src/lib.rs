#![warn(missing_docs)]
#![doc = include_str!("../README.md")]

#[cfg(feature = "derive")]
pub use tl_proto_proc::{id, TlRead, TlWrite};

pub use self::boxed::*;
pub use self::hasher::*;
pub use self::seq::*;
pub use self::traits::*;

// None of this crate's error handling needs the `From::from` error conversion
// performed implicitly by the `?` operator or the standard library's `try!`
// macro. This simplified macro gives a 5.5% improvement in compile time
// compared to standard `try!`, and 9% improvement compared to `?`.
macro_rules! ok {
    ($expr:expr) => {
        match $expr {
            Ok(val) => val,
            Err(err) => return Err(err),
        }
    };
}

mod boxed;
mod hasher;
mod option;
mod primitive;
mod seq;
mod traits;
mod tuple;
mod util;

/// Tries to deserialize `T` from the TL representation.
pub fn deserialize<'a, T>(mut packet: &'a [u8]) -> TlResult<T>
where
    T: TlRead<'a>,
{
    T::read_from(&mut packet)
}

/// Tries to deserialize `T` as boxed from the TL representation.
///
/// `T` must be `Bare` type.
///
/// Equivalent to this:
/// ```
/// # use tl_proto::{Bare, BoxedConstructor, BoxedWrapper, TlRead, TlResult};
/// # fn test<'a, T>(packet: &'a [u8]) -> TlResult<T>
/// # where T: TlRead<'a, Repr = Bare> + BoxedConstructor {
/// let BoxedWrapper::<T>(data) = tl_proto::deserialize(packet)?;
/// # Ok(data) }
/// ```
///
/// See [`BoxedWrapper`]
#[inline(always)]
pub fn deserialize_as_boxed<'a, T>(packet: &'a [u8]) -> TlResult<T>
where
    T: TlRead<'a, Repr = Bare> + BoxedConstructor,
{
    match deserialize(packet) {
        Ok(BoxedWrapper(result)) => Ok(result),
        Err(e) => Err(e),
    }
}

/// Serializes `T` into bytes.
pub fn serialize<T>(data: T) -> Vec<u8>
where
    T: TlWrite,
{
    let mut result = Vec::with_capacity(data.max_size_hint());
    data.write_to(&mut result);
    result
}

/// Wraps `T` into [`BoxedWrapper`] and serializes into bytes.
///
/// `T` must be `Bare` type.
#[inline(always)]
pub fn serialize_as_boxed<T>(data: T) -> Vec<u8>
where
    T: TlWrite<Repr = Bare> + BoxedConstructor,
{
    serialize(data.into_boxed())
}

/// Serializes `T` into an existing buffer **overwriting its content**.
pub fn serialize_into<T>(data: T, buffer: &mut Vec<u8>)
where
    T: TlWrite,
{
    buffer.clear();
    buffer.reserve(data.max_size_hint());
    data.write_to(buffer);
}

/// Wraps `T` into [`BoxedWrapper`] and serializes it into an existing
/// buffer **overwriting its content**.
///
/// `T` must be `Bare` type.
#[inline(always)]
pub fn serialize_into_as_boxed<T>(data: T, buffer: &mut Vec<u8>)
where
    T: TlWrite<Repr = Bare> + BoxedConstructor,
{
    serialize_into(data.into_boxed(), buffer);
}

/// Computes the `sha256` hash of the TL representation of `T`.
#[cfg(feature = "hash")]
pub fn hash<T>(data: T) -> [u8; 32]
where
    T: TlWrite<Repr = Boxed>,
{
    use digest::Digest;

    let mut hasher = sha2::Sha256::new();
    HashWrapper(data).update_hasher(&mut hasher);
    hasher.finalize().into()
}

/// Computes the `sha256` hash of the TL representation of `T`
/// wrapped into [`BoxedWrapper`].
///
/// `T` must be `Bare` type.
#[cfg(feature = "hash")]
#[inline(always)]
pub fn hash_as_boxed<T>(data: T) -> [u8; 32]
where
    T: TlWrite<Repr = Bare> + BoxedConstructor,
{
    hash(data.into_boxed())
}

#[doc(hidden)]
pub mod __internal {
    pub use crate::util::unlikely;
}
