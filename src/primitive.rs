use crate::traits::*;

impl ReadFromPacket<'_> for bool {
    fn read_from(packet: &[u8], offset: &mut usize) -> PacketContentsResult<Self> {
        match u32::read_from(packet, offset)? {
            BOOL_TRUE => Ok(true),
            BOOL_FALSE => Ok(false),
            _ => Err(PacketContentsError::UnknownConstructor),
        }
    }
}

impl WriteToPacket for bool {
    fn max_size_hint(&self) -> usize {
        std::mem::size_of::<u32>()
    }

    fn write_to<T>(&self, packet: &mut T)
    where
        T: TlPacket,
    {
        packet.write_u32(if *self { BOOL_TRUE } else { BOOL_FALSE })
    }
}

macro_rules! impl_read_from_packet(
    ($ty:ty) => {
        impl ReadFromPacket<'_> for $ty {
            #[inline(always)]
            fn read_from(packet: &[u8], offset: &mut usize) -> PacketContentsResult<Self> {
                if packet.len() < *offset + std::mem::size_of::<$ty>() {
                    Err(PacketContentsError::UnexpectedEof)
                } else {
                    let value = <$ty>::from_le_bytes(unsafe {
                        *(packet.as_ptr().add(*offset) as *const [u8; std::mem::size_of::<$ty>()])
                    });
                    *offset += std::mem::size_of::<$ty>();
                    Ok(value)
                }
            }
        }
    }
);

impl_read_from_packet!(u32);

impl WriteToPacket for u32 {
    fn max_size_hint(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    fn write_to<T>(&self, packet: &mut T)
    where
        T: TlPacket,
    {
        packet.write_u32(*self)
    }
}

impl_read_from_packet!(i32);

impl WriteToPacket for i32 {
    fn max_size_hint(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    fn write_to<T>(&self, packet: &mut T)
    where
        T: TlPacket,
    {
        packet.write_i32(*self)
    }
}

impl_read_from_packet!(u64);

impl WriteToPacket for u64 {
    fn max_size_hint(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    fn write_to<T>(&self, packet: &mut T)
    where
        T: TlPacket,
    {
        packet.write_u64(*self)
    }
}

impl_read_from_packet!(i64);

impl WriteToPacket for i64 {
    fn max_size_hint(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    fn write_to<T>(&self, packet: &mut T)
    where
        T: TlPacket,
    {
        packet.write_i64(*self)
    }
}

impl_read_from_packet!(f64);

impl WriteToPacket for f64 {
    fn max_size_hint(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    fn write_to<T>(&self, packet: &mut T)
    where
        T: TlPacket,
    {
        packet.write_f64(*self)
    }
}

const BOOL_FALSE: u32 = 0xbc799737;
const BOOL_TRUE: u32 = 0x997275b5;
