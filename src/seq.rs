use smallvec::SmallVec;

use crate::traits::*;

/// `ton::bytes` - 1 or 4 bytes of `len`, then `len` bytes of data (aligned to 4)
impl<'a> ReadFromPacket<'a> for &'a [u8] {
    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        read_bytes(packet, offset)
    }
}

/// `ton::bytes` - 1 or 4 bytes of `len`, then `len` bytes of data (aligned to 4)
impl WriteToPacket for &[u8] {
    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        bytes_max_size_hint(self.len())
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        write_bytes(self, packet)
    }
}

/// `ton::bytes` - 1 or 4 bytes of `len`, then `len` bytes of data (aligned to 4)
impl<'a> ReadFromPacket<'a> for Vec<u8> {
    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        Ok(read_bytes(packet, offset)?.to_vec())
    }
}

/// `ton::bytes` - 1 or 4 bytes of `len`, then `len` bytes of data (aligned to 4)
impl WriteToPacket for Vec<u8> {
    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        bytes_max_size_hint(self.len())
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        write_bytes(self.as_slice(), packet)
    }
}

/// `ton::int128 | ton::int256` - N bytes of data
impl<'a, const N: usize> ReadFromPacket<'a> for &'a [u8; N] {
    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        read_fixed_bytes(packet, offset)
    }
}

/// `ton::int128 | ton::int256` - N bytes of data
impl<'a, const N: usize> ReadFromPacket<'a> for [u8; N] {
    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        read_fixed_bytes(packet, offset).map(|&t| t)
    }
}

/// `ton::int128 | ton::int256` - N bytes of data
impl<const N: usize> WriteToPacket for [u8; N] {
    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        N
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        packet.write_raw_slice(self.as_ref())
    }
}

/// `ton::vector` - 4 bytes of `len`, then `len` items
impl<'a, T, const N: usize> ReadFromPacket<'a> for SmallVec<[T; N]>
where
    [T; N]: smallvec::Array,
    <[T; N] as smallvec::Array>::Item: ReadFromPacket<'a>,
{
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        let len = u32::read_from(packet, offset)? as usize;
        let mut items = SmallVec::<[T; N]>::with_capacity(len);
        for _ in 0..len {
            items.push(ReadFromPacket::read_from(packet, offset)?);
        }
        Ok(items)
    }
}

/// `ton::vector` - 4 bytes of `len`, then `len` items
impl<'a, T> ReadFromPacket<'a> for Vec<T>
where
    T: ReadFromPacket<'a>,
{
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        let len = u32::read_from(packet, offset)? as usize;
        let mut items = Vec::with_capacity(len);
        for _ in 0..len {
            items.push(ReadFromPacket::read_from(packet, offset)?);
        }
        Ok(items)
    }
}

/// `ton::vector` - 4 bytes of `len`, then `len` items
impl<T> WriteToPacket for &[T]
where
    T: WriteToPacket,
{
    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        4 + self.iter().map(WriteToPacket::max_size_hint).sum::<usize>()
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        (self.len() as i32).write_to(packet);
        for item in *self {
            item.write_to(packet);
        }
    }
}

/// `ton::vector` - 4 bytes of `len`, then `len` items
impl<T, const N: usize> WriteToPacket for SmallVec<[T; N]>
where
    [T; N]: smallvec::Array,
    <[T; N] as smallvec::Array>::Item: WriteToPacket,
{
    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        self.as_slice().max_size_hint()
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        self.as_slice().write_to(packet)
    }
}

/// Helper type which is used to represent field value as bytes
#[derive(Debug, Clone)]
pub struct IntermediateBytes<T>(pub T);

impl<T> IntermediateBytes<T>
where
    T: AsRef<[u8]>,
{
    pub fn as_slice(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl<'a, T> ReadFromPacket<'a> for IntermediateBytes<T>
where
    T: ReadFromPacket<'a>,
{
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        let intermediate = read_bytes(packet, offset)?;
        T::read_from(intermediate, &mut 0).map(IntermediateBytes)
    }
}

impl<T> WriteToPacket for IntermediateBytes<T>
where
    T: WriteToPacket,
{
    fn max_size_hint(&self) -> usize {
        bytes_max_size_hint(self.0.max_size_hint())
    }

    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        let len = self.0.max_size_hint();
        let mut have_written = write_bytes_len(len, packet);

        self.0.write_to(packet);
        have_written += len;

        let remainder = have_written % 4;
        if remainder != 0 {
            let buf = [0u8; 4];
            packet.write_raw_slice(&buf[remainder..]);
        }
    }
}

/// Helper type which reads remaining packet as is
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RawBytes<'a>(pub &'a [u8]);

impl AsRef<[u8]> for RawBytes<'_> {
    fn as_ref(&self) -> &[u8] {
        self.0
    }
}

impl<'a> ReadFromPacket<'a> for RawBytes<'a> {
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        let len = packet.len() - std::cmp::min(*offset, packet.len());
        let result = unsafe { std::slice::from_raw_parts(packet.as_ptr().add(*offset), len) };
        *offset += len;
        Ok(Self(result))
    }
}

impl WriteToPacket for RawBytes<'_> {
    #[inline]
    fn max_size_hint(&self) -> usize {
        self.0.len()
    }

    #[inline]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        packet.write_raw_slice(self.0);
    }
}

/// Owned version of `RawBytes`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OwnedRawBytes(pub Vec<u8>);

impl AsRef<[u8]> for OwnedRawBytes {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl ReadFromPacket<'_> for OwnedRawBytes {
    fn read_from(packet: &'_ [u8], offset: &mut usize) -> TlResult<Self> {
        Ok(Self(RawBytes::read_from(packet, offset)?.0.to_vec()))
    }
}

impl WriteToPacket for OwnedRawBytes {
    #[inline]
    fn max_size_hint(&self) -> usize {
        self.0.len()
    }

    #[inline]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        packet.write_raw_slice(self.0.as_slice())
    }
}

#[inline(always)]
fn read_fixed_bytes<'a, const N: usize>(
    packet: &'a [u8],
    offset: &mut usize,
) -> TlResult<&'a [u8; N]> {
    if packet.len() < *offset + N {
        Err(TlError::UnexpectedEof)
    } else {
        let ptr = unsafe { &*(packet.as_ptr().add(*offset) as *const [u8; N]) };
        *offset += N;
        Ok(ptr)
    }
}

#[inline(always)]
fn bytes_max_size_hint(mut len: usize) -> usize {
    if len < 254 {
        len += 1;
    } else {
        len += 4;
    }

    let remainder = len % 4;
    if remainder != 0 {
        len += 4 - remainder;
    }

    len
}

#[inline(always)]
fn write_bytes_len<T>(len: usize, packet: &mut T) -> usize
where
    T: TlPacket,
{
    if len < 254 {
        packet.write_raw_slice(&[len as u8]);
        1
    } else {
        packet.write_raw_slice(&[254, len as u8, (len >> 8) as u8, (len >> 16) as u8]);
        4
    }
}

#[inline(always)]
fn write_bytes<T>(bytes: &[u8], packet: &mut T)
where
    T: TlPacket,
{
    let len = bytes.len();
    let mut have_written = write_bytes_len(len, packet);

    packet.write_raw_slice(bytes);
    have_written += len;

    let remainder = have_written % 4;
    if remainder != 0 {
        let buf = [0u8; 4];
        packet.write_raw_slice(&buf[remainder..]);
    }
}

#[inline(always)]
fn read_bytes<'a>(packet: &'a [u8], offset: &mut usize) -> TlResult<&'a [u8]> {
    let packet_len = packet.len();
    let current_offset = *offset;

    if packet_len <= current_offset {
        return Err(TlError::UnexpectedEof);
    }

    let first_bytes = packet[current_offset];
    let (len, have_read) = if first_bytes != 254 {
        (first_bytes as usize, 1)
    } else {
        if packet_len < current_offset + 4 {
            return Err(TlError::UnexpectedEof);
        }

        let mut len = packet[current_offset + 1] as usize;
        len |= (packet[current_offset + 2] as usize) << 8;
        len |= (packet[current_offset + 3] as usize) << 16;
        (len, 4)
    };

    let remainder = {
        let excess = (have_read + len) % 4;
        if excess == 0 {
            0
        } else {
            4 - excess
        }
    };

    if packet_len < current_offset + have_read + len + remainder {
        return Err(TlError::UnexpectedEof);
    }

    let result =
        unsafe { std::slice::from_raw_parts(packet.as_ptr().add(current_offset + have_read), len) };

    *offset += have_read + len + remainder;
    Ok(result)
}
