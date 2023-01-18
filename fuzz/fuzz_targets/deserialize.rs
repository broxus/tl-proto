#![no_main]
use libfuzzer_sys::fuzz_target;

use bytes::Bytes;
use smallvec::SmallVec;

macro_rules! fuzz_types {
    (
        $data:ident;
        $( $type:ty, )*
    ) => {
        $(
            _ = tl_proto::deserialize::<$type>($data);
        )*
    };

}

fuzz_target!(|data: &[u8]| {
    fuzz_types!(
        data;

        bool,
        u32,
        u64,
        i32,
        i64,
        f64,
        &[u8],
        (u32,u64,i32,i64,f64,),
        Vec<u8>,
        Box<[u8]>,
        Bytes,
        &[u8; 32],
        [u8; 32],
        Vec<u32>,
        SmallVec<[u64; 4]>,
        Vec<Vec<u32>>,
        Vec<Vec<u32>>,
    );
});
