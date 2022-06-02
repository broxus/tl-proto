use crate::traits::*;

/// Implements `TlRead` and `TlWrite` for tuples
macro_rules! impl_traits_for_tuple {
    ($($num:tt $ty:ident),*) => {
        impl<'a, $($ty),*> TlRead<'a> for ($($ty),*,)
        where
            $($ty: TlRead<'a>),*
        {
            type Repr = Bare;

            #[inline(always)]
            fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
                Ok(($($ty::read_from(packet, offset)?),*,))
            }
        }

        impl<$($ty),*> TlWrite for ($($ty),*,)
        where
            $($ty: TlWrite),*
        {
            type Repr = Bare;

            #[inline(always)]
            fn max_size_hint(&self) -> usize {
                let mut result = 0;
                $(result += self.$num.max_size_hint());*;
                result
            }

            #[inline(always)]
            fn write_to<P>(&self, packet: &mut P)
            where
                P: TlPacket,
            {
                $(self.$num.write_to(packet));*;
            }
        }
    };
}

impl_traits_for_tuple!(0 T0);
impl_traits_for_tuple!(0 T0, 1 T1);
impl_traits_for_tuple!(0 T0, 1 T1, 2 T2);
impl_traits_for_tuple!(0 T0, 1 T1, 2 T2, 3 T3);
impl_traits_for_tuple!(0 T0, 1 T1, 2 T2, 3 T3, 4 T4);
impl_traits_for_tuple!(0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5);
