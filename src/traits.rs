use std::sync::Arc;

/// Serialized object representation.
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

/// Specifies how this type can read from the packet.
pub trait TlRead<'a>: Sized {
    /// Serialized object representation.
    type Repr: Repr;

    /// Tries to read itself from bytes at the specified offset, incrementing that offset.
    fn read_from(packet: &mut &'a [u8]) -> TlResult<Self>;
}

impl<'a, T> TlRead<'a> for Arc<T>
where
    T: TlRead<'a>,
{
    type Repr = T::Repr;

    #[inline(always)]
    fn read_from(packet: &mut &'a [u8]) -> TlResult<Self> {
        match T::read_from(packet) {
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
    fn read_from(packet: &mut &'a [u8]) -> TlResult<Self> {
        match T::read_from(packet) {
            Ok(data) => Ok(Box::new(data)),
            Err(e) => Err(e),
        }
    }
}

/// Specifies how this type can be written to the packet.
pub trait TlWrite {
    /// Serialized object representation.
    type Repr: Repr;

    /// Max required number of bytes.
    fn max_size_hint(&self) -> usize;

    /// Writes itself to the specified [`TlPacket`].
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

/// TL packet interface.
pub trait TlPacket {
    /// TL packet type.
    const TARGET: TlTarget;

    /// Writes `u32` to the packet.
    fn write_u32(&mut self, data: u32);
    /// Writes `i32` to the packet.
    fn write_i32(&mut self, data: i32);
    /// Writes `u64` to the packet.
    fn write_u64(&mut self, data: u64);
    /// Writes `i64` to the packet.
    fn write_i64(&mut self, data: i64);
    /// Writes raw bytes to the packet.
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

/// A wrapper type for writing to [`std::io::Write`] types.
///
/// Ignores all errors afther the first one.
/// The status can be retrieved using [`IoWriter::into_parts`].
pub struct IoWriter<W> {
    writer: W,
    status: std::io::Result<()>,
}

impl<W> IoWriter<W> {
    /// Creates a new writer.
    pub const fn new(writer: W) -> Self {
        Self {
            writer,
            status: Ok(()),
        }
    }

    /// Gets a mutable reference to the underlying writer.
    pub fn get_mut(&mut self) -> &mut W {
        &mut self.writer
    }

    /// Gets a reference to the underlying writer.
    pub fn get_ref(&self) -> &W {
        &self.writer
    }

    /// Disassembles the [`IoWriter<W>`], returning the underlying writer, and the status.
    pub fn into_parts(self) -> (W, std::io::Result<()>) {
        (self.writer, self.status)
    }
}

impl<W: std::io::Write> TlPacket for IoWriter<W> {
    const TARGET: TlTarget = TlTarget::Packet;

    #[inline(always)]
    fn write_u32(&mut self, data: u32) {
        if self.status.is_ok() {
            self.status = self.writer.write_all(&data.to_le_bytes());
        }
    }

    fn write_i32(&mut self, data: i32) {
        if self.status.is_ok() {
            self.status = self.writer.write_all(&data.to_le_bytes());
        }
    }

    fn write_u64(&mut self, data: u64) {
        if self.status.is_ok() {
            self.status = self.writer.write_all(&data.to_le_bytes());
        }
    }

    fn write_i64(&mut self, data: i64) {
        if self.status.is_ok() {
            self.status = self.writer.write_all(&data.to_le_bytes());
        }
    }

    fn write_raw_slice(&mut self, data: &[u8]) {
        if self.status.is_ok() {
            self.status = self.writer.write_all(data);
        }
    }
}

/// TL packet type.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TlTarget {
    /// Ordinary packet (bytes).
    Packet,
    /// Hasher packet (to compute TL hash without allocations).
    Hasher,
}

/// TL result wrapper.
pub type TlResult<T> = Result<T, TlError>;

/// Error type for parsing related errors.
#[derive(thiserror::Error, Debug)]
pub enum TlError {
    /// An unexpected end of packet has been reached.
    #[error("Unexpected packet EOF")]
    UnexpectedEof,
    /// Expected boxed type with different constructor.
    #[error("Unknown constructor")]
    UnknownConstructor,
    /// Parsed data is not valid.
    #[error("Invalid data")]
    InvalidData,
}

mod private {
    pub trait Sealed {}
}
