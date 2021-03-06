#[cfg(feature = "derive")]
pub use tl_proto_proc::{id, TlRead, TlWrite};

pub use self::boxed::*;
pub use self::hasher::*;
pub use self::seq::*;
pub use self::traits::*;

mod boxed;
mod hasher;
mod option;
mod primitive;
mod seq;
mod traits;
mod tuple;

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
    let BoxedWrapper(result) = deserialize(packet)?;
    Ok(result)
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
