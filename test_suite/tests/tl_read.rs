#[allow(dead_code)]
mod tests {
    use tl_proto::{BytesMeta, TlError, TlRead, TlResult};

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
        #[tl(flags_bit = 31)]
        value_2: Option<bool>,
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

    fn read_f32(mut packet: &[u8], offset: &mut usize) -> TlResult<f32> {
        use std::io::Read;

        let mut bytes = [0; 4];
        packet
            .read_exact(&mut bytes)
            .map_err(|_| TlError::UnexpectedEof)?;
        *offset += 4;
        Ok(u32::from_le_bytes(bytes) as f32)
    }

    mod tl_u128 {
        use super::*;

        pub fn read(packet: &[u8], offset: &mut usize) -> TlResult<u128> {
            use std::io::Read;

            let mut bytes = [0; 16];
            (&packet[*offset..])
                .read_exact(&mut bytes)
                .map_err(|_| TlError::UnexpectedEof)?;
            *offset += 16;
            Ok(u128::from_be_bytes(bytes))
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
            0, 0, 0, 0x80, // flags
            181, 117, 114, 153, // value_2: Some
        ];
        let data: StructWithFlags = tl_proto::deserialize(&target).unwrap();
        assert_eq!(data.value_1, None);
        assert_eq!(data.value_2, Some(true));

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
}
