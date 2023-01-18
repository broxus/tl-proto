use crate::traits::*;

/// Marks bare type with the appropriate constructor id.
pub trait BoxedConstructor: Sized {
    /// Constructor id.
    const TL_ID: u32;

    /// Wraps bare type reference into `BoxedWrapper`.
    fn as_boxed(&self) -> BoxedWrapper<&Self> {
        BoxedWrapper(self)
    }

    /// Converts bare type into `BoxedWrapper`.
    fn into_boxed(self) -> BoxedWrapper<Self> {
        BoxedWrapper(self)
    }
}

impl<T> BoxedConstructor for &T
where
    T: BoxedConstructor,
{
    const TL_ID: u32 = T::TL_ID;
}

/// Simple helper which contains inner value and constructor id.
///
/// Used mostly for serialization, so can contain references.
#[derive(Debug, Clone)]
pub struct BoxedWrapper<T>(pub T);

impl<T> TlWrite for BoxedWrapper<T>
where
    T: BoxedConstructor + TlWrite<Repr = Bare>,
{
    type Repr = Boxed;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        4 + self.0.max_size_hint()
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        T::TL_ID.write_to(packet);
        self.0.write_to(packet);
    }
}

impl<'a, T> TlRead<'a> for BoxedWrapper<T>
where
    T: BoxedConstructor + TlRead<'a, Repr = Bare>,
{
    type Repr = Boxed;

    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        match u32::read_from(packet, offset) {
            Ok(id) if id == T::TL_ID => match T::read_from(packet, offset) {
                Ok(data) => Ok(BoxedWrapper(data)),
                Err(e) => Err(e),
            },
            Ok(_) => Err(TlError::UnknownConstructor),
            Err(e) => Err(e),
        }
    }
}
