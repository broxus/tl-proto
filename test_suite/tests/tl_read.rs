#[allow(dead_code)]
mod tests {
    use tl_proto::{BoundedBytes, BytesMeta, TlError, TlRead, TlResult};

    #[derive(TlRead)]
    struct SimpleStruct {
        value: u32,
    }

    #[derive(TlRead)]
    struct StructWithRef<'tl> {
        meta: BytesMeta,
        value: &'tl [u8],
    }

    #[derive(TlRead)]
    #[tl(boxed)]
    enum BoxedEnum<'tl> {
        #[tl(id = 0x1)]
        First {
            value: StructWithRef<'tl>,
            another: u64,
        },

        #[tl(id = 0x2)]
        Second(u64, u32),

        #[tl(id = 0x3)]
        Third,
    }

    #[derive(TlRead)]
    struct StructWithFlags {
        #[tl(flags)]
        flags: (),
        #[tl(flags_bit = 0)]
        value_1: Option<u32>,
        #[tl(flags_bit = 1)]
        value_2: Option<()>,
        #[tl(flags_bit = 31)]
        value_3: Option<bool>,
    }

    #[derive(TlRead)]
    struct StructWithMultipleFlags {
        #[tl(flags, default_flags = 0x40000000)]
        flags: (),
        #[tl(flags_bit = "flags.0")]
        value_0: Option<u32>,
        #[tl(flags)]
        another_flags: (),
        #[tl(flags_field = "another_flags", flags_bit = 30)]
        value_30: Option<bool>,
        #[tl(flags_bit = "another_flags.31")]
        value_31: Option<bool>,
    }

    #[derive(TlRead)]
    struct StructWithCustom {
        #[tl(read_with = "read_f32")]
        value: f32,
        #[tl(with = "tl_u128")]
        another: u128,
    }

    #[derive(TlRead)]
    struct StructWithSignature<'tl> {
        #[tl(signature)]
        signature: &'tl [u8],
    }

    fn read_f32(packet: &mut &[u8]) -> TlResult<f32> {
        let Some((bytes, tail)) = packet.split_first_chunk() else {
            return Err(TlError::UnexpectedEof);
        };
        *packet = tail;
        Ok(u32::from_le_bytes(*bytes) as f32)
    }

    mod tl_u128 {
        use super::*;

        pub fn read(packet: &mut &[u8]) -> TlResult<u128> {
            let Some((bytes, tail)) = packet.split_first_chunk() else {
                return Err(TlError::UnexpectedEof);
            };
            *packet = tail;
            Ok(u128::from_be_bytes(*bytes))
        }
    }

    #[test]
    fn test_build() {}

    #[test]
    fn correct_deserialization() {
        assert_eq!(BoxedEnum::TL_ID_FIRST, 0x1);
        let target = [
            1, 0, 0, 0, // id
            10, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, // value
            123, 0, 0, 0, 0, 0, 0, 0, // another
        ];
        let data: BoxedEnum = tl_proto::deserialize(&target).unwrap();
        if let BoxedEnum::First { value, another } = data {
            assert_eq!(value.meta.prefix_len, 1);
            assert_eq!(value.meta.len, value.value.len());
            assert_eq!(value.meta.padding, 1);
            assert_eq!(value.value.len(), 10);
            assert_eq!(another, 123);
        } else {
            panic!("Unknown variant");
        }

        let target = [
            0, 0, 0, 0x80, // flags (little-endian)
            181, 117, 114, 153, // value_3: Some
        ];
        let data: StructWithFlags = tl_proto::deserialize(&target).unwrap();
        assert_eq!(data.value_1, None);
        assert_eq!(data.value_2, None);
        assert_eq!(data.value_3, Some(true));

        let target = [
            0b11, 0, 0, 0x80, // flags (little-endian)
            123, 0, 0, 0, // value_2: Some
            181, 117, 114, 153, // value_3: Some
        ];
        let data: StructWithFlags = tl_proto::deserialize(&target).unwrap();
        assert_eq!(data.value_1, Some(123));
        assert_eq!(data.value_2, Some(()));
        assert_eq!(data.value_3, Some(true));

        let target = [
            0u8, 0, 0, 0b01000000, // flags
            0, 0, 0, 0b01000000, // another_flags
            181, 117, 114, 153, // value_2: Some
        ];
        let data: StructWithMultipleFlags = tl_proto::deserialize(&target).unwrap();
        assert_eq!(data.value_0, None);
        assert_eq!(data.value_30, Some(true));
        assert_eq!(data.value_31, None);

        let target = [
            1, 0, 0, 0, // value
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 123, // another
        ];
        let data: StructWithCustom = tl_proto::deserialize(&target).unwrap();
        assert_eq!(data.value, 1.0);
        assert_eq!(data.another, 123);
    }

    #[test]
    fn struct_with_last_vector() {
        #[derive(TlRead)]
        struct LastVector {
            some_value: u64,
            array: Vec<u64>,
        }

        let target = [
            1, 2, 3, 4, 5, 6, 7, 8, // some_value
            4, 0, 0, 0, // array len
            1, 2, 3, 4, 5, 6, 7, 8, // array[0]
            1, 2, 3, 4, 5, 6, 7, 8, // array[1]
            1, 2, 3, 4, 5, 6, 7, 8, // array[2]
            1, 2, 3, 4, 5, 6, 7, 8, // array[3]
        ];
        let data: LastVector = tl_proto::deserialize(&target).unwrap();
        assert_eq!(data.array.len(), 4);

        assert!(matches!(
            tl_proto::deserialize::<LastVector>(&target[..40]),
            Err(TlError::UnexpectedEof)
        ));
    }

    #[test]
    fn bounded_bytes() {
        #[derive(TlRead)]
        struct Data<'tl> {
            bytes: &'tl BoundedBytes<4>,
        }

        let packet = [4, 1, 2, 3, 4, 0, 0, 0];
        let Data { bytes } = tl_proto::deserialize(&packet).unwrap();
        assert_eq!(bytes.as_ref(), &[1, 2, 3, 4]);

        let big_packet = [5, 1, 2, 3, 4, 5, 0, 0];
        assert!(matches!(
            tl_proto::deserialize::<Data>(&big_packet),
            Err(TlError::InvalidData)
        ));
    }
}
