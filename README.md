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

```text
/* my_proto.tl */

int ? = Int;
string ? = String;
bytes data:string = Bytes;

int256 8*[ int ] = Int256;

pub.ed25519 key:int256 = PublicKey;
pub.aes key:int256 = PublicKey;
pub.overlay name:bytes = PublicKey;
```

> NOTE: TL scheme is parsed by [`tl-scheme`](./scheme) crate at compile time.

```rust
use tl_proto::{TlRead, TlWrite};

#[derive(TlRead, TlWrite)]
#[tl(size_hint = 32)]
struct HashRef<'tl>(&'tl [u8; 32]);

#[derive(TlRead, TlWrite)]
#[tl(boxed, scheme = "my_proto.tl")]
enum PublicKey<'tl> {
    #[tl(id = "pub.aes")]
    Aes { key: HashRef<'tl> },

    #[tl(id = "pub.ed25519", size_hint = 32)]
    Ed25519 { key: HashRef<'tl> },

    #[tl(id = "pub.overlay")]
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
    assert_eq!(PublicKey::TL_ID_AES, 0x2dbcadd4);
    
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

| Type | Pseudocode |
| -------- | -------- |
| `()` | `[]` |
| `i32`,`u32`,`i64`,`u64` | `little_endian(x)` |
| `true` | `[0xb5, 0x75, 0x72, 0x99]` |
| `false` | `[0x37, 0x97, 0x79, 0xbc]` 
| `[u8; N], N % 4 ≡ 0`) | `[…x]` |
| `Vec<u8>, len < 254`) | <code>[len as u8, …x, …padding_to_4(len)]</code> |
| `Vec<u8>, len ≥ 254`) | <code>[254, …little_endian(x)[0..=2], …x, …padding_to_4(len)]</code> |
| `Vec<T>` | `[…little_endian(len as u32), …map(…x, repr)]` |
| `(T0, … , Tn)` | `[…repr(T0), … , …repr(Tn)]`  |
| `Option<T>` | `{ Some(x) ⇒ repr(x), None ⇒ [] }` |
| `enum { T0, …, Tn }` | `{ T0(x) ⇒ […id(T0), …repr(x)], …, Tn(x) ⇒ […id(Tn), …repr(x)] }` |
