#[allow(dead_code)]
mod tests {
    use tl_proto::{TlPacket, TlWrite};

    #[derive(TlWrite)]
    #[tl(boxed)]
    #[tl(scheme_inline = r##"
        boolTrue = Bool;
        boolFalse = Bool;
    "##)]
    enum MyBool {
        #[tl(id = "boolTrue")]
        Yes,
        #[tl(id = "boolFalse")]
        No,
    }

    #[derive(TlWrite)]
    #[tl(boxed, scheme = "test.tl")]
    enum AdnlMessage {
        #[tl(id = "adnl.message.query")]
        Query { query_id: [u8; 32], query: Vec<u8> },
        #[tl(id = "adnl.message.answer")]
        Answer { query_id: [u8; 32], answer: Vec<u8> },
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

    #[derive(TlWrite)]
    struct StructWithCustom {
        #[tl(write_with = "write_f32", size_hint = 4)]
        value: f32,
        #[tl(with = "tl_u128")]
        another: u128,
    }

    fn write_f32<P: TlPacket>(test: &f32, packet: &mut P) {
        packet.write_u32(*test as u32);
    }

    mod tl_u128 {
        use super::*;

        pub const fn size_hint(_: &u128) -> usize {
            16
        }

        pub fn write<P: TlPacket>(v: &u128, packet: &mut P) {
            packet.write_raw_slice(&v.to_be_bytes())
        }
    }

    #[test]
    fn test_build() {}

    #[test]
    fn correct_serialization() {
        assert_eq!(tl_proto::serialize(MyBool::Yes), tl_proto::serialize(true));
        assert_eq!(tl_proto::serialize(MyBool::No), tl_proto::serialize(false));

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

        // 5
        let object = StructWithCustom {
            value: 1.123,
            another: 123,
        };
        assert_eq!(object.max_size_hint(), 4 + 16);
        let data = tl_proto::serialize(object);
        assert_eq!(
            &data,
            &[
                1, 0, 0, 0, // value
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 123, // another
            ]
        );
    }
}
