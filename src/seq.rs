use smallvec::SmallVec;

use crate::traits::*;

/// `ton::bytes` - 1 or 4 bytes of `len`, then `len` bytes of data (aligned to 4)
impl<'a> TlRead<'a> for &'a [u8] {
    type Repr = Bare;

    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        read_bytes(packet, offset)
    }
}

/// `ton::bytes` - 1 or 4 bytes of `len`, then `len` bytes of data (aligned to 4)
impl TlWrite for &[u8] {
    type Repr = Bare;

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
impl<'a> TlRead<'a> for Vec<u8> {
    type Repr = Bare;

    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        Ok(read_bytes(packet, offset)?.to_vec())
    }
}

/// `ton::bytes` - 1 or 4 bytes of `len`, then `len` bytes of data (aligned to 4)
impl TlWrite for Vec<u8> {
    type Repr = Bare;

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

#[cfg(feature = "bytes")]
impl TlRead<'_> for bytes::Bytes {
    type Repr = Bare;

    #[inline(always)]
    fn read_from(packet: &'_ [u8], offset: &mut usize) -> TlResult<Self> {
        Ok(read_bytes(packet, offset)?.to_vec().into())
    }
}

#[cfg(feature = "bytes")]
impl TlWrite for bytes::Bytes {
    type Repr = Bare;

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

/// `ton::int128 | ton::int256` - N bytes of data
impl<'a, const N: usize> TlRead<'a> for &'a [u8; N] {
    type Repr = Bare;

    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        read_fixed_bytes(packet, offset)
    }
}

/// `ton::int128 | ton::int256` - N bytes of data
impl<'a, const N: usize> TlRead<'a> for [u8; N] {
    type Repr = Bare;

    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        read_fixed_bytes(packet, offset).map(|&t| t)
    }
}

/// `ton::int128 | ton::int256` - N bytes of data
impl<const N: usize> TlWrite for [u8; N] {
    type Repr = Bare;

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
impl<'a, T, const N: usize> TlRead<'a> for SmallVec<[T; N]>
where
    [T; N]: smallvec::Array,
    <[T; N] as smallvec::Array>::Item: TlRead<'a>,
{
    type Repr = Bare;

    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        let len = read_vector_len(packet, offset)?;

        let mut items = SmallVec::<[T; N]>::with_capacity(len);
        for _ in 0..len {
            items.push(TlRead::read_from(packet, offset)?);
        }
        Ok(items)
    }
}

/// `ton::vector` - 4 bytes of `len`, then `len` items
impl<'a, T> TlRead<'a> for Vec<T>
where
    T: TlRead<'a>,
{
    type Repr = Bare;

    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        let len = read_vector_len(packet, offset)?;

        let mut items = Vec::with_capacity(len);
        for _ in 0..len {
            items.push(TlRead::read_from(packet, offset)?);
        }
        Ok(items)
    }
}

/// `ton::vector` - 4 bytes of `len`, then `len` items
impl<T> TlWrite for Vec<T>
where
    T: TlWrite,
{
    type Repr = Bare;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        <&[T]>::max_size_hint(&self.as_slice())
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        <&[T]>::write_to(&self.as_slice(), packet)
    }
}

/// `ton::vector` - 4 bytes of `len`, then `len` items
impl<T> TlWrite for &[T]
where
    T: TlWrite,
{
    type Repr = Bare;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        4 + self.iter().map(TlWrite::max_size_hint).sum::<usize>()
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
impl<T, const N: usize> TlWrite for SmallVec<[T; N]>
where
    [T; N]: smallvec::Array,
    <[T; N] as smallvec::Array>::Item: TlWrite,
{
    type Repr = Bare;

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

impl<'a, T> TlRead<'a> for IntermediateBytes<T>
where
    T: TlRead<'a>,
{
    type Repr = Bare;

    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        let intermediate = read_bytes(packet, offset)?;
        T::read_from(intermediate, &mut 0).map(IntermediateBytes)
    }
}

impl<T> TlWrite for IntermediateBytes<T>
where
    T: TlWrite,
{
    type Repr = Bare;

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
pub struct RawBytes<'a, R>(&'a [u8], std::marker::PhantomData<R>);

impl<'a, R> RawBytes<'a, R> {
    pub fn new(raw: &'a [u8]) -> Self {
        RawBytes(raw, std::marker::PhantomData)
    }

    #[inline(always)]
    pub fn into_inner(self) -> &'a [u8] {
        self.0
    }
}

impl<R> AsRef<[u8]> for RawBytes<'_, R> {
    fn as_ref(&self) -> &[u8] {
        self.0
    }
}

impl<R> std::fmt::Debug for RawBytes<'_, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RawBytes").field(&self.0).finish()
    }
}

impl<R> Eq for RawBytes<'_, R> {}
impl<R> PartialEq for RawBytes<'_, R> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(other.0)
    }
}

impl<R> Copy for RawBytes<'_, R> {}
impl<R> Clone for RawBytes<'_, R> {
    fn clone(&self) -> Self {
        Self(self.0, std::marker::PhantomData)
    }
}

impl<'a, R: Repr> TlRead<'a> for RawBytes<'a, R> {
    type Repr = R;

    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        let len = packet.len() - std::cmp::min(*offset, packet.len());
        let result = unsafe { std::slice::from_raw_parts(packet.as_ptr().add(*offset), len) };
        *offset += len;
        Ok(Self::new(result))
    }
}

impl<R: Repr> TlWrite for RawBytes<'_, R> {
    type Repr = R;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        self.0.len()
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        packet.write_raw_slice(self.0);
    }
}

pub struct OwnedRawBytes<R>(Vec<u8>, std::marker::PhantomData<R>);

impl<R> OwnedRawBytes<R> {
    pub fn new(raw: Vec<u8>) -> Self {
        OwnedRawBytes(raw, std::marker::PhantomData)
    }

    #[inline(always)]
    pub fn into_inner(self) -> Vec<u8> {
        self.0
    }
}

impl<R> std::fmt::Debug for OwnedRawBytes<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("OwnedRawBytes").field(&self.0).finish()
    }
}

impl<R> Eq for OwnedRawBytes<R> {}
impl<R> PartialEq for OwnedRawBytes<R> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<R> Clone for OwnedRawBytes<R> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), std::marker::PhantomData)
    }
}

impl<R> AsRef<[u8]> for OwnedRawBytes<R> {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl<R: Repr> TlRead<'_> for OwnedRawBytes<R> {
    type Repr = R;

    fn read_from(packet: &'_ [u8], offset: &mut usize) -> TlResult<Self> {
        Ok(Self::new(
            RawBytes::<R>::read_from(packet, offset)?
                .into_inner()
                .to_vec(),
        ))
    }
}

impl<R: Repr> TlWrite for OwnedRawBytes<R> {
    type Repr = R;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        self.0.len()
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        packet.write_raw_slice(self.0.as_slice())
    }
}

#[inline(always)]
fn read_vector_len(packet: &[u8], offset: &mut usize) -> TlResult<usize> {
    let len = u32::read_from(packet, offset)? as usize;

    // Length cannot be greater than the rest of the packet.
    // However min item size is 4 bytes so we could reduce it four times
    if unlikely((len + *offset) > packet.len() >> 2) {
        Err(TlError::UnexpectedEof)
    } else {
        Ok(len)
    }
}

#[inline(always)]
fn read_fixed_bytes<'a, const N: usize>(
    packet: &'a [u8],
    offset: &mut usize,
) -> TlResult<&'a [u8; N]> {
    if unlikely(packet.len() < *offset + N) {
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

    // Align to 4
    len + (4 - len % 4) % 4
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

    if unlikely(packet_len <= current_offset) {
        return Err(TlError::UnexpectedEof);
    }

    // SAFETY: `current_offset` is guaranteed to be less than `packet_len`
    // but the compiler is not able to eliminate bounds check
    let first_bytes = unsafe { *packet.get_unchecked(current_offset) };
    let (len, have_read) = if first_bytes != 254 {
        (first_bytes as usize, 1)
    } else {
        if packet_len <= current_offset + 3 {
            return Err(TlError::UnexpectedEof);
        }

        let mut len;

        // SAFETY: `current_offset + 3` is guaranteed to be less than `packet_len`
        unsafe {
            len = *packet.get_unchecked(current_offset + 1) as usize;
            len |= (*packet.get_unchecked(current_offset + 2) as usize) << 8;
            len |= (*packet.get_unchecked(current_offset + 3) as usize) << 16;
        }

        (len, 4)
    };

    let remainder = (4 - (have_read + len) % 4) % 4;

    if unlikely(packet_len < current_offset + have_read + len + remainder) {
        return Err(TlError::UnexpectedEof);
    }

    let result =
        unsafe { std::slice::from_raw_parts(packet.as_ptr().add(current_offset + have_read), len) };

    *offset += have_read + len + remainder;
    Ok(result)
}

/// Brings [unlikely](core::intrinsics::unlikely) to stable rust.
#[inline(always)]
const fn unlikely(b: bool) -> bool {
    #[allow(clippy::needless_bool)]
    if (1i32).checked_div(if b { 0 } else { 1 }).is_none() {
        true
    } else {
        false
    }
}
