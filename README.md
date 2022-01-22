## tl-proto &emsp; [![Latest Version]][crates.io] [![tl-proto: rustc 1.56+]][Rust 1.56] [![Workflow badge]][Workflow] [![License MIT badge]][License MIT]

[Latest Version]: https://img.shields.io/crates/v/tl-proto.svg
[crates.io]: https://crates.io/crates/tl-proto
[tl-proto: rustc 1.56+]: https://img.shields.io/badge/rustc-1.56+-lightgray.svg
[Rust 1.56]: https://blog.rust-lang.org/2021/10/21/Rust-1.56.0.html
[Workflow badge]: https://img.shields.io/github/workflow/status/broxus/tl-proto/master
[Workflow]: https://github.com/broxus/tl-proto/actions?query=workflow%3Amaster
[License MIT badge]: https://img.shields.io/badge/license-MIT-blue.svg
[License MIT]: https://opensource.org/licenses/MIT

A collection of traits for working with [TL](https://core.telegram.org/mtproto/TL) serialization/deserialization.

### Example

```rust
use tl_proto::{TlRead, TlWrite};

#[derive(TlRead, TlWrite)]
#[tl(size_hint = 32)]
struct HashRef<'tl>(&'tl [u8; 32]);

#[derive(TlRead, TlWrite)]
#[tl(boxed)]
enum PublicKey<'tl> {
    #[tl(id = 0x2dbcadd4)]
    Aes { key: HashRef<'tl> },

    #[tl(id = 0x4813b4c6)]
    Ed25519 { key: HashRef<'tl> },

    #[tl(id = 0x34ba45cb)]
    Overlay { name: &'tl [u8] },
}

#[derive(TlRead, TlWrite)]
#[tl(boxed)]
enum Address<'tl> {
    #[tl(id = 0x670da6e7)]
    Udp { ip: i32, port: i32 },

    #[tl(id = 0xe31d63fa)]
    Udp6 { ip: &'tl [u8; 16], port: i32 },

    #[tl(id = 0x092b02eb)]
    Tunnel {
        to: HashRef<'tl>,
        pubkey: PublicKey<'tl>,
    },
}

fn main() {    
    let bytes = tl_proto::serialize(&Address::Udp {
        ip: 123,
        port: 3000,
    });

    let decoded = tl_proto::deserialize::<Address>(&bytes).unwrap();
    assert!(matches!(
        decoded,
        Address::Udp {
            ip: 123,
            port: 3000,
        }
    ));
}
```

### Specification

- `i32`, `u32` - 4 bytes in little-endian order.
- `i64`, `u64` - 8 bytes in little-endian order.
- `true` - `0x997275b5` as 4 bytes in little-endian order.
- `false` - `0xbc799737` as 4 bytes in little-endian order.
- Fixed bytes array of length `N` (where `N % 4 == 0`) - `N` bytes as it is.
- Bytes array of arbitrary length (`<254`) - 1 byte with length, bytes from array, 
  padding up to a length multiple of 4.
- Bytes array of arbitrary length (`≥254`) - 1 byte with value `254`, 3 bytes of length in little-endian order, 
  bytes from array, padding up to a length multiple of 4.
- Vector of values with the same type - 4 bytes of length in little-endian order, values one by one.
- Tuples - values one by one.
- Enums (boxed types) - 4 bytes of type id in little-endian order, variant value
  (btw. you can read more about how type id is calculated in telegram docs).
