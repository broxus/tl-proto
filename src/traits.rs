use std::sync::Arc;

/// Specifies how this type can read from the packet
pub trait TlRead<'a>: Sized {
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self>;
}

impl<'a, T> TlRead<'a> for Arc<T>
where
    T: TlRead<'a>,
{
    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        Ok(Arc::new(T::read_from(packet, offset)?))
    }
}

impl<'a, T> TlRead<'a> for Box<T>
where
    T: TlRead<'a>,
{
    #[inline(always)]
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        Ok(Box::new(T::read_from(packet, offset)?))
    }
}

/// Specifies how this type can be written to the packet
pub trait TlWrite {
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

/// Trait for types which can be signed. Used to overwrite serialization for signer
pub trait TlHash {
    fn update_hasher<H>(&self, hasher: &mut H)
    where
        H: TlPacket;
}

impl<T> TlHash for &T
where
    T: TlHash,
{
    #[inline(always)]
    fn update_hasher<H>(&self, hasher: &mut H)
    where
        H: TlPacket,
    {
        T::update_hasher(*self, hasher)
    }
}

/// TL packet interface
pub trait TlPacket {
    fn write_u32(&mut self, data: u32);
    fn write_i32(&mut self, data: i32);
    fn write_u64(&mut self, data: u64);
    fn write_i64(&mut self, data: i64);
    fn write_f64(&mut self, data: f64);
    fn write_raw_slice(&mut self, data: &[u8]);
}

impl TlPacket for Vec<u8> {
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
    fn write_f64(&mut self, data: f64) {
        self.extend_from_slice(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_raw_slice(&mut self, data: &[u8]) {
        self.extend_from_slice(data);
    }
}

pub type TlResult<T> = Result<T, TlError>;

#[derive(thiserror::Error, Debug)]
pub enum TlError {
    #[error("Unexpected packet EOF")]
    UnexpectedEof,
    #[error("Unknown constructor")]
    UnknownConstructor,
}
