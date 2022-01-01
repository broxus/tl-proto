use crate::traits::*;

/// Marks type as it is already boxed
pub trait Boxed {}

impl<T> Boxed for &T where T: Boxed {}

/// Marks bare type with the appropriate constructor id
pub trait BoxedConstructor: Sized {
    const ID: u32;

    /// Wraps bare type reference into `BoxedWrapper`
    fn wrap(&self) -> BoxedWrapper<&Self> {
        BoxedWrapper(self)
    }

    fn into_wrapped(self) -> BoxedWrapper<Self> {
        BoxedWrapper(self)
    }
}

impl<T> BoxedConstructor for &T
where
    T: BoxedConstructor,
{
    const ID: u32 = T::ID;
}

/// Simple helper which contains inner value and constructor id.
///
/// Used mostly for serialization, so can contain references
#[derive(Debug, Clone)]
pub struct BoxedWrapper<T>(pub T);

impl<T> Boxed for BoxedWrapper<T> {}

impl<T> TlWrite for BoxedWrapper<T>
where
    T: BoxedConstructor + TlWrite,
{
    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        4 + self.0.max_size_hint()
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        T::ID.write_to(packet);
        self.0.write_to(packet);
    }
}

impl<'a, T> TlRead<'a> for BoxedWrapper<T>
where
    T: BoxedConstructor + TlRead<'a>,
{
    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        if u32::read_from(packet, offset)? == T::ID {
            T::read_from(packet, offset).map(BoxedWrapper)
        } else {
            Err(TlError::UnknownConstructor)
        }
    }
}
