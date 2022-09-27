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

pub fn deserialize<'a, T>(packet: &'a [u8]) -> TlResult<T>
where
    T: TlRead<'a>,
{
    T::read_from(packet, &mut 0)
}

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

pub fn serialize<T>(data: T) -> Vec<u8>
where
    T: TlWrite,
{
    let mut result = Vec::with_capacity(data.max_size_hint());
    data.write_to(&mut result);
    result
}

#[inline(always)]
pub fn serialize_as_boxed<T>(data: T) -> Vec<u8>
where
    T: TlWrite<Repr = Bare> + BoxedConstructor,
{
    serialize(data.into_boxed())
}

pub fn serialize_into<T>(data: T, buffer: &mut Vec<u8>)
where
    T: TlWrite,
{
    buffer.clear();
    buffer.reserve(data.max_size_hint());
    data.write_to(buffer);
}

#[inline(always)]
pub fn serialize_into_as_boxed<T>(data: T, buffer: &mut Vec<u8>)
where
    T: TlWrite<Repr = Bare> + BoxedConstructor,
{
    serialize_into(data.into_boxed(), buffer);
}

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

#[cfg(feature = "hash")]
#[inline(always)]
pub fn hash_as_boxed<T>(data: T) -> [u8; 32]
where
    T: TlWrite<Repr = Bare> + BoxedConstructor,
{
    hash(data.into_boxed())
}
