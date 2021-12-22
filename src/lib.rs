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

pub fn deserialize<'a, T>(packet: &'a [u8]) -> PacketContentsResult<T>
where
    T: ReadFromPacket<'a>,
{
    let mut offset = 0;
    T::read_from(packet, &mut offset)
}

pub fn serialize<T>(data: T) -> Vec<u8>
where
    T: WriteToPacket,
{
    let mut result = Vec::with_capacity(data.max_size_hint());
    data.write_to(&mut result);
    result
}

pub fn serialize_into<T>(data: T, buffer: &mut Vec<u8>)
where
    T: WriteToPacket,
{
    buffer.clear();
    buffer.reserve(data.max_size_hint());
    data.write_to(buffer);
}
