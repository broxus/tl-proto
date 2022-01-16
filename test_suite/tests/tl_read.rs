#[allow(dead_code)]
mod tests {
    use tl_proto::TlRead;

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

    #[test]
    fn test_build() {}

    #[test]
    fn correct_deserialization() {
        let target = [
            0, 0, 0, 0x80, // flags
            181, 117, 114, 153, // value_2: Some
        ];
        let data: StructWithFlags = tl_proto::deserialize(&target).unwrap();
        assert_eq!(data.value_1, None);
        assert_eq!(data.value_2, Some(true))
    }
}
