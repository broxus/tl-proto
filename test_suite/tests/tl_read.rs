#[allow(dead_code)]
mod tests {
    use tl_proto::{TlError, TlRead, TlResult};

    #[derive(TlRead)]
    struct SimpleStruct {
        value: u32,
    }

    #[derive(TlRead)]
    struct StructWithRef<'tl> {
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
            0, 0, 0, 0x80, // flags
            181, 117, 114, 153, // value_2: Some
        ];
        let data: StructWithFlags = tl_proto::deserialize(&target).unwrap();
        assert_eq!(data.value_1, None);
        assert_eq!(data.value_2, Some(true));

        let target = [
            1, 0, 0, 0, // value
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 123, // another
        ];
        let data: StructWithCustom = tl_proto::deserialize(&target).unwrap();
        assert_eq!(data.value, 1.0);
        assert_eq!(data.another, 123);
    }
}
