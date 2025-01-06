use smallvec::SmallVec;

use crate::traits::*;
use crate::util::*;

/// `ton::bytes` meta.
///
/// NOTE: Doesn't consume slice (leaves offset as unchanged).
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct BytesMeta {
    /// Length in bytes of the prefix (1 or 4 bytes).
    pub prefix_len: usize,
    /// Length in bytes.
    pub len: usize,
    /// Precomputed bytes padding (`0..=3`).
    pub padding: usize,
}

impl<'a> TlRead<'a> for BytesMeta {
    type Repr = Bare;

    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        match compute_bytes_meta(packet, *offset) {
            Ok((prefix_len, len, padding)) => Ok(Self {
                prefix_len,
                len,
                padding,
            }),
            Err(e) => Err(e),
        }
    }
}

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
impl<'a> TlRead<'a> for Box<[u8]> {
    type Repr = Bare;

    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        Ok(Box::from(ok!(read_bytes(packet, offset))))
    }
}

/// `ton::bytes` - 1 or 4 bytes of `len`, then `len` bytes of data (aligned to 4)
impl TlWrite for Box<[u8]> {
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
        match read_bytes(packet, offset) {
            Ok(bytes) => Ok(bytes.to_vec()),
            Err(e) => Err(e),
        }
    }
}

/// `ton::bytes` - 1 or 4 bytes of `len`, then `len` bytes of data (aligned to 4).
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

/// `ton::bytes` - 1 or 4 bytes of `len`, then `len` bytes of data (aligned to 4).
#[cfg(feature = "bytes")]
impl TlRead<'_> for bytes::Bytes {
    type Repr = Bare;

    #[inline(always)]
    fn read_from(packet: &'_ [u8], offset: &mut usize) -> TlResult<Self> {
        match read_bytes(packet, offset) {
            Ok(bytes) => Ok(bytes::Bytes::from(Box::from(bytes))),
            Err(e) => Err(e),
        }
    }
}

/// `ton::bytes` - 1 or 4 bytes of `len`, then `len` bytes of data (aligned to 4).
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

/// `ton::int128 | ton::int256` - N bytes of data.
impl<'a, const N: usize> TlRead<'a> for &'a [u8; N] {
    type Repr = Bare;

    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        read_fixed_bytes(packet, offset)
    }
}

/// `ton::int128 | ton::int256` - N bytes of data.
impl<'a, const N: usize> TlRead<'a> for [u8; N] {
    type Repr = Bare;

    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        match read_fixed_bytes(packet, offset) {
            Ok(data) => Ok(*data),
            Err(e) => Err(e),
        }
    }
}

/// `ton::int128 | ton::int256` - N bytes of data.
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

/// `ton::vector` - 4 bytes of `len`, then `len` items.
impl<'a, T, const N: usize> TlRead<'a> for SmallVec<[T; N]>
where
    [T; N]: smallvec::Array,
    <[T; N] as smallvec::Array>::Item: TlRead<'a>,
{
    type Repr = Bare;

    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        let len = ok!(read_vector_len(packet, offset));

        let mut items = SmallVec::<[T; N]>::with_capacity(len);
        for _ in 0..len {
            items.push(ok!(TlRead::read_from(packet, offset)));
        }
        Ok(items)
    }
}

/// `ton::vector` - 4 bytes of `len`, then `len` items.
impl<'a, T> TlRead<'a> for Vec<T>
where
    T: TlRead<'a>,
{
    type Repr = Bare;

    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        let len = ok!(read_vector_len(packet, offset));

        let mut items = Vec::with_capacity(len);
        for _ in 0..len {
            items.push(ok!(TlRead::read_from(packet, offset)));
        }
        Ok(items)
    }
}

/// `ton::vector` - 4 bytes of `len`, then `len` items.
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

/// `ton::vector` - 4 bytes of `len`, then `len` items.
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
        (self.len() as u32).write_to(packet);
        for item in *self {
            item.write_to(packet);
        }
    }
}

/// `ton::vector` - 4 bytes of `len`, then `len` items.
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

/// Helper type which is used to serialize iterator as vector.
///
/// NOTE: iterator is cloned for `max_size_hint` and `write_to`.
#[derive(Copy, Clone)]
pub struct IterRef<'a, I: Sized>(pub &'a I);

impl<I, T> TlWrite for IterRef<'_, I>
where
    I: Iterator<Item = T> + ExactSizeIterator + Clone,
    T: TlWrite,
{
    type Repr = Bare;

    fn max_size_hint(&self) -> usize {
        let mut total = 4;
        for item in self.0.clone() {
            total += item.max_size_hint();
        }
        total
    }

    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        (self.0.len() as u32).write_to(packet);
        for item in self.0.clone() {
            item.write_to(packet);
        }
    }
}

/// Bytes slice with a max length bound.
#[derive(Debug)]
#[repr(transparent)]
pub struct BoundedBytes<const N: usize>([u8]);

impl<const N: usize> BoundedBytes<N> {
    /// Wraps a byte slice into a new type with length check.
    #[inline]
    pub const fn try_wrap(bytes: &[u8]) -> Option<&Self> {
        if bytes.len() <= N {
            // SAFETY: `BoundedBytes` has the same repr as `[u8]`
            Some(unsafe { &*(bytes as *const [u8] as *const BoundedBytes<N>) })
        } else {
            None
        }
    }

    /// Wraps a byte slice into a new type without any checks.
    ///
    /// # Safety
    ///
    /// The following must be true:
    /// - `bytes` must have length not greater than `N`
    #[inline]
    pub unsafe fn wrap_unchecked(bytes: &[u8]) -> &Self {
        // SAFETY: `BoundedBytes` has the same repr as `[u8]`
        unsafe { &*(bytes as *const [u8] as *const BoundedBytes<N>) }
    }
}

impl<const N: usize> AsRef<[u8]> for BoundedBytes<N> {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl<const N: usize> AsMut<[u8]> for BoundedBytes<N> {
    #[inline]
    fn as_mut(&mut self) -> &mut [u8] {
        &mut self.0
    }
}

impl<const N: usize> std::ops::Deref for BoundedBytes<N> {
    type Target = [u8];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<const N: usize> std::ops::DerefMut for BoundedBytes<N> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a, const N: usize> TlRead<'a> for &'a BoundedBytes<N> {
    type Repr = Bare;

    #[inline]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        fn read_bytes_with_max_len<'a>(
            packet: &'a [u8],
            max_len: usize,
            offset: &mut usize,
        ) -> TlResult<&'a [u8]> {
            let current_offset = *offset;
            let (prefix_len, len, padding) = ok!(compute_bytes_meta(packet, current_offset));
            if len > max_len {
                return Err(TlError::InvalidData);
            }

            let result = unsafe {
                std::slice::from_raw_parts(packet.as_ptr().add(current_offset + prefix_len), len)
            };

            *offset += prefix_len + len + padding;
            Ok(result)
        }

        let result = ok!(read_bytes_with_max_len(packet, N, offset));

        // SAFETY: `len <= N`
        Ok(unsafe { BoundedBytes::wrap_unchecked(result) })
    }
}

impl<const N: usize> TlWrite for &BoundedBytes<N> {
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

/// Helper type which is used to represent field value as bytes.
#[derive(Debug, Clone)]
pub struct IntermediateBytes<T>(pub T);

impl<T> IntermediateBytes<T>
where
    T: AsRef<[u8]>,
{
    /// Returns the underlying slice.
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
        match read_bytes(packet, offset) {
            Ok(intermediate) => match T::read_from(intermediate, &mut 0) {
                Ok(data) => Ok(IntermediateBytes(data)),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        }
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

/// Helper type which reads remaining packet as is.
pub struct RawBytes<'a, R>(&'a [u8], std::marker::PhantomData<R>);

impl<'a, R> RawBytes<'a, R> {
    /// Creates new bytes wrapper.
    pub fn new(raw: &'a [u8]) -> Self {
        RawBytes(raw, std::marker::PhantomData)
    }

    /// Converts into the underlying bytes.
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
    #[inline]
    fn clone(&self) -> Self {
        *self
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

/// Helper type which reads remaining packet as is.
///
/// Use [`RawBytes`] if you don't need to move bytes.
pub struct OwnedRawBytes<R>(Vec<u8>, std::marker::PhantomData<R>);

impl<R> OwnedRawBytes<R> {
    /// Creates new bytes wrapper.
    #[inline(always)]
    pub fn new(raw: Vec<u8>) -> Self {
        OwnedRawBytes(raw, std::marker::PhantomData)
    }

    /// Converts into the underlying bytes.
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
        match RawBytes::<R>::read_from(packet, offset) {
            Ok(RawBytes(inner, ..)) => Ok(Self::new(inner.to_vec())),
            Err(e) => Err(e),
        }
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
    let len = ok!(u32::read_from(packet, offset)) as usize;

    // Length cannot be greater than the rest of the packet.
    // However min item size is 4 bytes so we could reduce it four times
    if unlikely((len * 4 + *offset) > packet.len()) {
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

/// Computes the number of bytes required to encode the `[u8]` of the specified length.
#[inline(always)]
pub const fn bytes_max_size_hint(mut len: usize) -> usize {
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
    let current_offset = *offset;
    let (prefix_len, len, padding) = ok!(compute_bytes_meta(packet, current_offset));

    let result = unsafe {
        std::slice::from_raw_parts(packet.as_ptr().add(current_offset + prefix_len), len)
    };

    *offset += prefix_len + len + padding;
    Ok(result)
}

/// Fetches bytes meta without consuming slice
///
/// Returns **prefix length**, **bytes length** and **padding length**
#[inline(always)]
fn compute_bytes_meta(packet: &[u8], offset: usize) -> TlResult<(usize, usize, usize)> {
    let packet_len = packet.len();
    if unlikely(packet_len < offset + 4) {
        return Err(TlError::UnexpectedEof);
    }

    let first_bytes = unsafe { packet.as_ptr().add(offset).cast::<u32>().read_unaligned() };
    let (len, have_read) = if first_bytes & 0xff != SIZE_MAGIC as u32 {
        ((first_bytes & 0xff) as usize, 1)
    } else {
        ((first_bytes >> 8) as usize, 4)
    };

    let padding = (4 - (have_read + len) % 4) % 4;
    if unlikely(packet_len < offset + have_read + len + padding) {
        return Err(TlError::UnexpectedEof);
    }

    Ok((have_read, len, padding))
}

const SIZE_MAGIC: u8 = 254;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_small_slice() {
        assert_eq!(read_bytes(&[0, 0, 0, 0], &mut 0).unwrap(), &[]);
        assert_eq!(read_bytes(&[1, 123, 0, 0], &mut 0).unwrap(), &[123]);
        assert_eq!(read_bytes(&[2, 123, 3, 0], &mut 0).unwrap(), &[123, 3]);
        assert_eq!(read_bytes(&[3, 123, 3, 2], &mut 0).unwrap(), &[123, 3, 2]);
    }
}
