#[cfg(feature = "derive")]
pub use tl_proto_proc::*;

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

pub fn serialize<T>(data: T) -> Vec<u8>
where
    T: TlWrite,
{
    let mut result = Vec::with_capacity(data.max_size_hint());
    data.write_to(&mut result);
    result
}

pub fn serialize_into<T>(data: T, buffer: &mut Vec<u8>)
where
    T: TlWrite,
{
    buffer.clear();
    buffer.reserve(data.max_size_hint());
    data.write_to(buffer);
}
