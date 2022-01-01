#[allow(dead_code)]
mod tests {
    use tl_proto::*;

    #[derive(TlWrite)]
    #[tl(boxed, id = 0xffffffff)]
    struct Test {
        #[tl(size_hint = 1)]
        item: u32,
        struct_field: InnerTest,
        #[tl(skip_write)]
        _unknown: u8,
    }

    #[derive(TlWrite)]
    struct InnerTest {
        what: bool,
    }

    #[test]
    fn test_build() {}
}
