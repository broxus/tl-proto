use crate::traits::*;

/// Marks bare type with the appropriate constructor id
pub trait BoxedConstructor: Sized {
    const TL_ID: u32;

    /// Wraps bare type reference into `BoxedWriter`
    fn boxed_writer(&self) -> BoxedWriter<&Self> {
        BoxedWriter(self)
    }

    /// Converts bare type into `BoxedWriter`
    fn into_boxed_writer(self) -> BoxedWriter<Self> {
        BoxedWriter(self)
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
/// Used mostly for serialization, so can contain references
#[derive(Debug, Clone)]
pub struct BoxedWriter<T>(pub T);

impl<T> TlWrite for BoxedWriter<T>
where
    T: BoxedConstructor + TlWrite,
{
    const TL_WRITE_BOXED: bool = true;

    #[inline(always)]
    fn max_size_hint(&self) -> usize {
        4 + self.0.max_size_hint()
    }

    #[inline(always)]
    fn write_to<P>(&self, packet: &mut P)
    where
        P: TlPacket,
    {
        let _ = Assert::<T>::NOT_BOXED_WRITE;

        T::TL_ID.write_to(packet);
        self.0.write_to(packet);
    }
}

/// Simple helper which contains inner value and constructor id.
///
/// Used mostly for serialization, so can contain references
#[derive(Debug, Clone)]
pub struct BoxedReader<T>(pub T);

impl<'a, T> TlRead<'a> for BoxedReader<T>
where
    T: BoxedConstructor + TlRead<'a>,
{
    const TL_READ_BOXED: bool = true;

    fn read_from(packet: &'a [u8], offset: &mut usize) -> TlResult<Self> {
        let _ = Assert::<T>::NOT_BOXED_READ;

        if u32::read_from(packet, offset)? == T::TL_ID {
            T::read_from(packet, offset).map(BoxedReader)
        } else {
            Err(TlError::UnknownConstructor)
        }
    }
}

struct Assert<T>(std::marker::PhantomData<T>);

impl<'a, T> Assert<T>
where
    T: TlWrite,
{
    const NOT_BOXED_WRITE: () = if T::TL_WRITE_BOXED {
        panic!("Boxed writer can only be used for bare types")
    } else {
        ()
    };
}

impl<'a, T> Assert<T>
where
    T: TlRead<'a>,
{
    const NOT_BOXED_READ: () = if T::TL_READ_BOXED {
        panic!("Boxed reader can only be used for bare types")
    } else {
        ()
    };
}
