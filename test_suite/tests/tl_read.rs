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

    #[test]
    fn test_build() {}
}
