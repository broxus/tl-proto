use std::hash::{Hash, Hasher};

use crate::traits::*;

pub struct HashWrapper<T>(pub T);

impl<T> Hash for HashWrapper<T>
where
    T: TlHash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.update_hasher(&mut HashWriter(state))
    }
}

struct HashWriter<T>(pub T);

impl<T> TlPacket for HashWriter<T>
where
    T: Hasher,
{
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
    fn write_f64(&mut self, data: f64) {
        Hasher::write_u64(&mut self.0, convert_f64(&data));
    }

    #[inline(always)]
    fn write_raw_slice(&mut self, data: &[u8]) {
        Hasher::write(&mut self.0, data);
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
