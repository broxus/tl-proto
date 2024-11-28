use crate::traits::*;

impl TlRead<'_> for () {
    type Repr = Bare;

    fn read_from(_packet: &mut &'_ [u8]) -> TlResult<Self> {
        Ok(())
    }
}

impl TlWrite for () {
    type Repr = Bare;

    #[inline]
    fn max_size_hint(&self) -> usize {
        0
    }

    #[inline]
    fn write_to<P>(&self, _packet: &mut P)
    where
        P: TlPacket,
    {
    }
}

impl TlRead<'_> for bool {
    type Repr = Boxed;

    fn read_from(packet: &mut &'_ [u8]) -> TlResult<Self> {
        match u32::read_from(packet) {
            Ok(BOOL_TRUE) => Ok(true),
            Ok(BOOL_FALSE) => Ok(false),
            Ok(_) => Err(TlError::UnknownConstructor),
            Err(e) => Err(e),
        }
    }
}

impl TlWrite for bool {
    type Repr = Boxed;

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
            type Repr = Bare;

            #[inline(always)]
            fn read_from(packet: &mut &'_ [u8]) -> TlResult<Self> {
                match packet.split_first_chunk() {
                    Some((first, tail)) => {
                        let value = <$ty>::from_le_bytes(*first);
                        *packet = tail;
                        Ok(value)
                    }
                    None => Err(TlError::UnexpectedEof),
                }
            }
        }
    }
);

impl_read_from_packet!(u32);

impl TlWrite for u32 {
    type Repr = Bare;

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
    type Repr = Bare;

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
    type Repr = Bare;

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
    type Repr = Bare;

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
    type Repr = Bare;

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
    let sign_u64 = u64::from(sign > 0);
    (man & MAN_MASK) | ((exp_u64 << 52) & EXP_MASK) | ((sign_u64 << 63) & SIGN_MASK)
}

const BOOL_FALSE: u32 = 0xbc799737;
const BOOL_TRUE: u32 = 0x997275b5;

macro_rules! impl_non_zero {
    ($($ty:ty => ($write_method:ident, $read_ty:ty)),*$(,)?) => {
        $(
        impl TlWrite for $ty {
            type Repr = Bare;

            #[inline(always)]
            fn max_size_hint(&self) -> usize {
                std::mem::size_of::<Self>()
            }

            #[inline(always)]
            fn write_to<T>(&self, packet: &mut T)
            where
                T: TlPacket,
            {
                packet.$write_method(self.get())
            }
        }

        impl TlRead<'_> for $ty {
            type Repr = Bare;

            #[inline(always)]
            fn read_from(packet: &mut &'_ [u8]) -> TlResult<Self> {
                match <$ty>::new(<$read_ty>::read_from(packet)?) {
                    Some(value) => Ok(value),
                    None => Err(TlError::InvalidData),
                }
            }
        }

        )*
    };
}

impl_non_zero! {
    std::num::NonZeroU32 => (write_u32, u32),
    std::num::NonZeroI32 => (write_i32, i32),
    std::num::NonZeroU64 => (write_u64, u64),
    std::num::NonZeroI64 => (write_i64, i64),
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn write_non_zero() {
        macro_rules! decl_writes {
            ($($ty:ty => [$($lit:expr),*$(,)?]),*$(,)?) => {
                $($(assert_eq!(crate::serialize(<$ty>::new($lit).unwrap()), $lit.to_le_bytes());)*)*
            };
        }

        decl_writes! {
            std::num::NonZeroU32 => [1u32, 123u32, u32::MAX],
            std::num::NonZeroI32 => [-123i32, 123i32, i32::MIN, i32::MAX],
            std::num::NonZeroU64 => [1u64, 123u64, u64::MAX],
            std::num::NonZeroI64 => [-123i64, 123i64, i64::MIN, i64::MAX]
        }
    }

    #[test]
    fn read_non_zero() {
        // u32
        assert!(matches!(
            std::num::NonZeroU32::read_from(&mut [0, 0].as_ref()).unwrap_err(),
            TlError::UnexpectedEof
        ));
        assert!(matches!(
            std::num::NonZeroU32::read_from(&mut [0, 0, 0, 0].as_ref()).unwrap_err(),
            TlError::InvalidData
        ));
        let mut packet: &[u8] = &[123, 0, 0, 0];
        assert_eq!(
            std::num::NonZeroU32::read_from(&mut packet).unwrap(),
            std::num::NonZeroU32::new(123).unwrap(),
        );
        assert!(packet.is_empty());

        // i32
        assert!(matches!(
            std::num::NonZeroI32::read_from(&mut [0, 0].as_ref()).unwrap_err(),
            TlError::UnexpectedEof
        ));
        assert!(matches!(
            std::num::NonZeroI32::read_from(&mut [0, 0, 0, 0].as_ref()).unwrap_err(),
            TlError::InvalidData
        ));
        let mut packet: &[u8] = &[0xfe, 0xff, 0xff, 0xff];
        assert_eq!(
            std::num::NonZeroI32::read_from(&mut packet).unwrap(),
            std::num::NonZeroI32::new(-2).unwrap(),
        );
        assert!(packet.is_empty());

        // u64
        assert!(matches!(
            std::num::NonZeroU64::read_from(&mut [0, 0, 0, 0].as_ref()).unwrap_err(),
            TlError::UnexpectedEof
        ));
        assert!(matches!(
            std::num::NonZeroU64::read_from(&mut [0, 0, 0, 0, 0, 0, 0, 0].as_ref()).unwrap_err(),
            TlError::InvalidData
        ));
        let mut packet: &[u8] = &[123, 0, 0, 0, 0, 0, 0, 0];
        assert_eq!(
            std::num::NonZeroU64::read_from(&mut packet).unwrap(),
            std::num::NonZeroU64::new(123).unwrap(),
        );
        assert!(packet.is_empty());

        // i64
        assert!(matches!(
            std::num::NonZeroI64::read_from(&mut [0, 0, 0, 0].as_ref()).unwrap_err(),
            TlError::UnexpectedEof
        ));

        assert!(matches!(
            std::num::NonZeroI64::read_from(&mut [0, 0, 0, 0, 0, 0, 0, 0].as_ref()).unwrap_err(),
            TlError::InvalidData
        ));
        let mut packet: &[u8] = &[0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff];
        assert_eq!(
            std::num::NonZeroI64::read_from(&mut packet,).unwrap(),
            std::num::NonZeroI64::new(-2).unwrap(),
        );
        assert!(packet.is_empty());
    }
}
