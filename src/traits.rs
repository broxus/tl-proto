use std::sync::Arc;

/// Serialized object representation
///
/// - [`Boxed`] - object with explicit type id. Can be used for enums or dynamic dispatch.
/// - [`Bare`] - object without explicit type id. Can only be used for structs, or write-only enums.
pub trait Repr: private::Sealed {}

/// Object representation with explicit type id.
/// Can be used for enums or dynamic dispatch.
pub enum Boxed {}
impl private::Sealed for Boxed {}
impl Repr for Boxed {}

/// Object representation without explicit type id.
/// Can only be used for structs, or write-only enums.
pub enum Bare {}
impl private::Sealed for Bare {}
impl Repr for Bare {}

/// Specifies how this type can read from the packet
pub trait TlRead<'a>: Sized {
    type Repr: Repr;

    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self>;
}

impl<'a, T> TlRead<'a> for Arc<T>
where
    T: TlRead<'a>,
{
    type Repr = T::Repr;

    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        match T::read_from(packet, offset) {
            Ok(data) => Ok(Arc::new(data)),
            Err(e) => Err(e),
        }
    }
}

impl<'a, T> TlRead<'a> for Box<T>
where
    T: TlRead<'a>,
{
    type Repr = T::Repr;

    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        match T::read_from(packet, offset) {
            Ok(data) => Ok(Box::new(data)),
            Err(e) => Err(e),
        }
    }
}

/// Specifies how this type can be written to the packet
pub trait TlWrite {
    type Repr: Repr;

    /// Max required number of bytes
    fn max_size_hint(&self) -> usize;

    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket;
}

impl<T> TlWrite for &T
where
    T: TlWrite,
{
    type Repr = T::Repr;

    fn max_size_hint(&self) -> usize {
        TlWrite::max_size_hint(*self)
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        TlWrite::write_to(*self, packet)
    }
}

impl<T> TlWrite for Box<T>
where
    T: TlWrite,
{
    type Repr = T::Repr;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        TlWrite::max_size_hint(&**self)
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        TlWrite::write_to(&**self, packet)
    }
}

impl<T> TlWrite for Arc<T>
where
    T: TlWrite,
{
    type Repr = T::Repr;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        TlWrite::max_size_hint(&**self)
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        TlWrite::write_to(&**self, packet)
    }
}

/// TL packet interface
pub trait TlPacket {
    const TARGET: TlTarget;

    fn write_u32(&mut self, data: u32);
    fn write_i32(&mut self, data: i32);
    fn write_u64(&mut self, data: u64);
    fn write_i64(&mut self, data: i64);
    fn write_raw_slice(&mut self, data: &[u8]);
}

impl TlPacket for Vec<u8> {
    const TARGET: TlTarget = TlTarget::Packet;

    #[inline(always)]
    fn write_u32(&mut self, data: u32) {
        self.extend_from_slice(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_i32(&mut self, data: i32) {
        self.extend_from_slice(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_u64(&mut self, data: u64) {
        self.extend_from_slice(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_i64(&mut self, data: i64) {
        self.extend_from_slice(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_raw_slice(&mut self, data: &[u8]) {
        self.extend_from_slice(data);
    }
}

#[cfg(feature = "bytes")]
impl TlPacket for bytes::BytesMut {
    const TARGET: TlTarget = TlTarget::Packet;

    #[inline(always)]
    fn write_u32(&mut self, data: u32) {
        self.extend_from_slice(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_i32(&mut self, data: i32) {
        self.extend_from_slice(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_u64(&mut self, data: u64) {
        self.extend_from_slice(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_i64(&mut self, data: i64) {
        self.extend_from_slice(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_raw_slice(&mut self, data: &[u8]) {
        self.extend_from_slice(data);
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TlTarget {
    Packet,
    Hasher,
}

pub type TlResult<T> = Result<T, TlError>;

#[derive(thiserror::Error, Debug)]
pub enum TlError {
    #[error("Unexpected packet EOF")]
    UnexpectedEof,
    #[error("Unknown constructor")]
    UnknownConstructor,
    #[error("Invalid data")]
    InvalidData,
}

mod private {
    pub trait Sealed {}
}
