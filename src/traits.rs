/// Specifies how this type can read from the packet
pub trait ReadFromPacket<'a>: Sized {
    fn read_from(packet: &'a [u8], offset: &mut usize) -> PacketContentsResult<Self>;
}

/// Specifies how this type can be written to the packet
pub trait WriteToPacket {
    /// Max required number of bytes
    fn max_size_hint(&self) -> usize;

    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket;
}

impl<T> WriteToPacket for &T
where
    T: WriteToPacket,
{
    fn max_size_hint(&self) -> usize {
        WriteToPacket::max_size_hint(*self)
    }

    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        WriteToPacket::write_to(*self, packet)
    }
}

/// Trait for types which can be signed. Used to overwrite serialization for signer
pub trait UpdateSignatureHasher {
    fn update_hasher<H>(&self, hasher: &mut H)
    where
        H: TlPacket;
}

impl<T> UpdateSignatureHasher for &T
where
    T: UpdateSignatureHasher,
{
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

pub type PacketContentsResult<T> = Result<T, PacketContentsError>;

#[derive(thiserror::Error, Debug)]
pub enum PacketContentsError {
    #[error("Unexpected packet EOF")]
    UnexpectedEof,
    #[error("Unknown constructor")]
    UnknownConstructor,
}
