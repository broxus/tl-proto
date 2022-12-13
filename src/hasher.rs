use std::hash::{Hash, Hasher};

use crate::traits::*;

#[derive(Eq)]
pub struct HashWrapper<T>(pub T);

impl<T> HashWrapper<T>
where
    T: TlWrite,
{
    #[inline(always)]
    pub fn update_hasher<H: digest::Update>(&self, engine: &mut H) {
        self.0.write_to(&mut DigestWriter(engine));
    }
}

impl<T: PartialEq> PartialEq for HashWrapper<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

struct DigestWriter<'a, T>(&'a mut T);

impl<T> TlPacket for DigestWriter<'_, T>
where
    T: digest::Update,
{
    const TARGET: TlTarget = TlTarget::Hasher;

    #[inline(always)]
    fn write_u32(&mut self, data: u32) {
        self.0.update(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_i32(&mut self, data: i32) {
        self.0.update(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_u64(&mut self, data: u64) {
        self.0.update(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_i64(&mut self, data: i64) {
        self.0.update(&data.to_le_bytes());
    }

    #[inline(always)]
    fn write_raw_slice(&mut self, data: &[u8]) {
        self.0.update(data);
    }
}

impl<T> Hash for HashWrapper<T>
where
    T: TlWrite,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.write_to(&mut HashWriter(state))
    }
}

struct HashWriter<'a>(pub &'a mut dyn Hasher);

impl<'a> TlPacket for HashWriter<'a> {
    const TARGET: TlTarget = TlTarget::Hasher;

    #[inline(always)]
    fn write_u32(&mut self, data: u32) {
        Hasher::write_u32(&mut self.0, data);
    }

    #[inline(always)]
    fn write_i32(&mut self, data: i32) {
        Hasher::write_i32(&mut self.0, data);
    }

    #[inline(always)]
    fn write_u64(&mut self, data: u64) {
        Hasher::write_u64(&mut self.0, data);
    }

    #[inline(always)]
    fn write_i64(&mut self, data: i64) {
        Hasher::write_i64(&mut self.0, data);
    }

    #[inline(always)]
    fn write_raw_slice(&mut self, data: &[u8]) {
        Hasher::write(&mut self.0, data);
    }
}
