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

    #[test]
    fn test_build() {}
}
