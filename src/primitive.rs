use crate::traits::*;

impl TlRead<'_> for bool {
    const TL_READ_BOXED: bool = true;

    fn read_from(packet: &[u8], offset: &mut usize) -> TlResult<Self> {
        match u32::read_from(packet, offset)? {
            BOOL_TRUE => Ok(true),
            BOOL_FALSE => Ok(false),
            _ => Err(TlError::UnknownConstructor),
        }
    }
}

impl TlWrite for bool {
    const TL_WRITE_BOXED: bool = true;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        std::mem::size_of::<u32>()
    }

    #[inline(always)]
    fn write_to<T>(&self, packet: &mut T)
    where
        T: TlPacket,
    {
        packet.write_u32(if *self { BOOL_TRUE } else { BOOL_FALSE })
    }
}

macro_rules! impl_read_from_packet(
    ($ty:ty) => {
        impl TlRead<'_> for $ty {
            const TL_READ_BOXED: bool = false;

            #[inline(always)]
            fn read_from(packet: &[u8], offset: &mut usize) -> TlResult<Self> {
                if packet.len() < *offset + std::mem::size_of::<$ty>() {
                    Err(TlError::UnexpectedEof)
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

impl TlWrite for u32 {
    const TL_WRITE_BOXED: bool = false;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    #[inline(always)]
    fn write_to<T>(&self, packet: &mut T)
    where
        T: TlPacket,
    {
        packet.write_u32(*self)
    }
}

impl_read_from_packet!(i32);

impl TlWrite for i32 {
    const TL_WRITE_BOXED: bool = false;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    #[inline(always)]
    fn write_to<T>(&self, packet: &mut T)
    where
        T: TlPacket,
    {
        packet.write_i32(*self)
    }
}

impl_read_from_packet!(u64);

impl TlWrite for u64 {
    const TL_WRITE_BOXED: bool = false;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    #[inline(always)]
    fn write_to<T>(&self, packet: &mut T)
    where
        T: TlPacket,
    {
        packet.write_u64(*self)
    }
}

impl_read_from_packet!(i64);

impl TlWrite for i64 {
    const TL_WRITE_BOXED: bool = false;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    #[inline(always)]
    fn write_to<T>(&self, packet: &mut T)
    where
        T: TlPacket,
    {
        packet.write_i64(*self)
    }
}

impl_read_from_packet!(f64);

impl TlWrite for f64 {
    const TL_WRITE_BOXED: bool = false;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    #[inline(always)]
    fn write_to<T>(&self, packet: &mut T)
    where
        T: TlPacket,
    {
        packet.write_u64(convert_f64(self))
    }
}

#[inline(always)]
fn convert_f64(f: &f64) -> u64 {
    const SIGN_MASK: u64 = 0x8000000000000000u64;
    const EXP_MASK: u64 = 0x7ff0000000000000u64;
    const MAN_MASK: u64 = 0x000fffffffffffffu64;

    const CANONICAL_NAN_BITS: u64 = 0x7ff8000000000000u64;
    const CANONICAL_ZERO_BITS: u64 = 0x0u64;

    if f.is_nan() {
        return CANONICAL_NAN_BITS;
    }

    let bits = f.to_bits();

    let sign = if bits >> 63 == 0 { 1i8 } else { -1 };
    let mut exp = ((bits >> 52) & 0x7ff) as i16;
    let man = if exp == 0 {
        (bits & 0xfffffffffffff) << 1
    } else {
        (bits & 0xfffffffffffff) | 0x10000000000000
    };

    if man == 0 {
        return CANONICAL_ZERO_BITS;
    }

    // Exponent bias + mantissa shift
    exp -= 1023 + 52;

    let exp_u64 = exp as u64;
    let sign_u64 = if sign > 0 { 1u64 } else { 0u64 };
    (man & MAN_MASK) | ((exp_u64 << 52) & EXP_MASK) | ((sign_u64 << 63) & SIGN_MASK)
}

const BOOL_FALSE: u32 = 0xbc799737;
const BOOL_TRUE: u32 = 0x997275b5;
