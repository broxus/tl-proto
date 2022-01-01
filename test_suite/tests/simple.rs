#[allow(dead_code)]
mod tests {
    use tl_proto::*;

    #[derive(TlWrite)]
    #[tl(boxed)]
    enum MyBool {
        #[tl(id = 0x1)]
        Yes,
        #[tl(id = 0x2)]
        No,
    }

    #[derive(TlWrite)]
    #[tl(boxed)]
    enum AllVariantsBoxed {
        #[tl(id = 0x1)]
        Unit,
        #[tl(id = 0x2)]
        Tuple(u32, u64),
        #[tl(id = 0x3)]
        Struct { first: u32, second: u64 },
    }

    #[derive(TlWrite)]
    enum AllVariantsBare {
        Unit,
        Tuple(u32, u64),
        Struct { first: u32, second: u64 },
    }

    #[derive(TlWrite)]
    #[tl(boxed, id = 0xffffffff)]
    struct SimpleStruct {
        #[tl(size_hint = 4)]
        item: u32,
        struct_field: InnerStruct,
        #[tl(skip_write)]
        _unknown: u8,
    }

    #[derive(TlWrite)]
    struct InnerStruct {
        what: bool,
    }

    #[test]
    fn test_build() {}
}
