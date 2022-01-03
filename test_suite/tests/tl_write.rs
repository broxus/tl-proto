#[allow(dead_code)]
mod tests {
    use tl_proto::TlWrite;

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
        on_heap: Box<u32>,
        #[tl(skip_write)]
        _unknown: u8,
    }

    #[derive(TlWrite)]
    struct InnerStruct {
        what: bool,
    }

    #[derive(TlWrite)]
    struct SliceWrapper<'a> {
        slice: &'a [u8],
    }

    #[derive(TlWrite)]
    #[tl(boxed, id = 0x1)]
    struct WithSignature {
        data: u32,
        #[tl(signature)]
        sign: [u8; 4],
    }

    #[test]
    fn test_build() {}

    #[test]
    fn correct_serialization() {
        let target = [
            0xffu8, 0xff, 0xff, 0xff, // id
            0xad, 0x00, 0x00, 0x00, // item
            0xb5, 0x75, 0x72, 0x99, // InnerStruct.what
            123, 0, 0, 0, // on_heap
        ];
        let data = tl_proto::serialize(&SimpleStruct {
            item: 0xad,
            struct_field: InnerStruct { what: true },
            on_heap: Box::new(123),
            _unknown: 0,
        });
        assert_eq!(&data, &target);

        let target = [
            4u8, // size
            0, 1, 2, 3, // data
            0, 0, 0, // alignment
        ];
        let data_bytes = [0, 1, 2, 3];
        let data = tl_proto::serialize(&SliceWrapper { slice: &data_bytes });
        assert_eq!(&data, &target);

        let target = [
            1, 0, 0, 0, // id
            123, 0, 0, 0, // data
            1, 2, 3, 4, // signature
        ];
        let data = tl_proto::serialize(&WithSignature {
            data: 123,
            sign: [1, 2, 3, 4],
        });
        assert_eq!(&data, &target);
    }
}
