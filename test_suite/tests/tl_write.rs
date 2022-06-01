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
    #[tl(boxed)]
    enum EnumWithSizeHint {
        #[tl(id = 1, size_hint = 0)]
        Struct,
        #[tl(id = 2, size_hint = 8)]
        Struct3 { first: u64 },
        #[tl(id = 3)]
        Struct2 { first: u32, second: u64 },
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

    #[derive(TlWrite)]
    struct StructWithFlags {
        #[tl(flags, default_flags = 0x40000000)]
        flags: (),
        #[tl(flags_bit = 0)]
        value_1: Option<u32>,
        #[tl(flags_bit = 31)]
        value_2: Option<bool>,
    }

    #[test]
    fn test_build() {}

    #[test]
    fn correct_serialization() {
        // 1
        let object = SimpleStruct {
            item: 0xad,
            struct_field: InnerStruct { what: true },
            on_heap: Box::new(123),
            _unknown: 0,
        };
        assert_eq!(object.max_size_hint(), 4 + 4 + 4 + 4);
        let target = [
            0xffu8, 0xff, 0xff, 0xff, // id
            0xad, 0x00, 0x00, 0x00, // item
            0xb5, 0x75, 0x72, 0x99, // InnerStruct.what
            123, 0, 0, 0, // on_heap
        ];
        let data = tl_proto::serialize(&object);
        assert_eq!(&data, &target);

        // 2
        let data_bytes = [0, 1, 2, 3];
        let object = SliceWrapper { slice: &data_bytes };
        assert_eq!(object.max_size_hint(), 1 + 4 + 3);
        let target = [
            4u8, // size
            0, 1, 2, 3, // data
            0, 0, 0, // alignment
        ];
        let data = tl_proto::serialize(object);
        assert_eq!(&data, &target);

        // 3
        let object = WithSignature {
            data: 123,
            sign: [1, 2, 3, 4],
        };
        assert_eq!(object.max_size_hint(), 4 + 4 + 4);
        let target = [
            1u8, 0, 0, 0, // id
            123, 0, 0, 0, // data
            1, 2, 3, 4, // signature
        ];
        let data = tl_proto::serialize(object);
        assert_eq!(&data, &target);

        // 4
        let object = StructWithFlags {
            flags: (),
            value_1: None,
            value_2: Some(true),
        };
        assert_eq!(object.max_size_hint(), 4 + 4);
        let target = [
            0u8, 0, 0, 0b11000000, // flags
            181, 117, 114, 153, // value_2: Some
        ];
        let data = tl_proto::serialize(object);
        assert_eq!(&data, &target);
    }
}
