## tl-proto

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
