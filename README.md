## tl-proto &emsp; [![Latest Version]][crates.io] [![tl-proto: rustc 1.56+]][Rust 1.56] [![Workflow badge]][Workflow] [![License MIT badge]][License MIT]

[Latest Version]: https://img.shields.io/crates/v/tl-proto.svg

[crates.io]: https://crates.io/crates/tl-proto

[tl-proto: rustc 1.56+]: https://img.shields.io/badge/rustc-1.56+-lightgray.svg

[Rust 1.56]: https://blog.rust-lang.org/2021/10/21/Rust-1.56.0.html

[Workflow badge]: https://img.shields.io/github/actions/workflow/status/broxus/tl-proto/master.yml?branch=master

[Workflow]: https://github.com/broxus/tl-proto/actions?query=workflow%3Amaster

[License MIT badge]: https://img.shields.io/badge/license-MIT-blue.svg

[License MIT]: https://opensource.org/licenses/MIT

A collection of traits for working with [TL](https://core.telegram.org/mtproto/TL) serialization/deserialization.

### Example

```text
/* my_proto.tl */

int ? = Int;
long ? = Long;
string ? = String;
bytes data:string = Bytes;

int256 8*[ int ] = Int256;

pub.ed25519 key:int256 = PublicKey;
pub.aes key:int256 = PublicKey;
pub.overlay name:bytes = PublicKey;

adnl.address.udp ip:int port:int = adnl.Address;

tonNode.blockId workchain:int shard:long seqno:int = tonNode.BlockId;

--- functions ---

liteServer.lookupBlock mode:# id:tonNode.blockId lt:mode.1?long utime:mode.2?int = liteServer.BlockHeader;
```

> NOTE: TL scheme is parsed by [`tl-scheme`](./scheme) crate at compile time.
> It doesn't cover full TL grammer, but it's enough for most of the cases.

```rust,ignore
use tl_proto::{TlRead, TlWrite};

/// You can declare "bare" structs, which
/// doesn't have an associated TL id.
///
/// NOTE: enums can only be used as bare
/// with TlWrite, because there is no way to
/// know the exact variant without an id to
/// implement TlRead.
#[derive(TlRead, TlWrite)]
#[tl(size_hint = 32)]
struct HashRef<'tl>(&'tl [u8; 32]);

/// Or you can declare "boxed" structs, which
/// have one or more associated TL ids.
///
/// NOTE: in case of boxed enum with provided scheme,
/// all variants must have the same constructor kind
/// (all functions or all types). And if all variants
/// are types, they must refer to the same boxed type.
#[derive(TlRead, TlWrite)]
#[tl(boxed, scheme = "my_proto.tl")]
enum PublicKey<'tl> {
    /// `id` attribute is required for boxed enums.
    /// It can be either a raw id or a name of a variant
    /// from the scheme (in later case, `scheme` or `scheme_inline`
    /// container attribute is required).
    #[tl(id = "pub.aes")]
    Aes { key: HashRef<'tl> },

    /// `size_hint` is used to optimize `TlWrite::max_size_hint`
    /// implementation. If this attribute is specified, it
    /// will be used as is instead of computing the size of fields.
    #[tl(id = "pub.ed25519", size_hint = 32)]
    Ed25519 { key: HashRef<'tl> },

    /// Note that lifetime is called `'tl`, it is a special
    /// name which refers to the lifetime of the buffer
    /// from which this struct was deserialized.
    #[tl(id = "pub.overlay")]
    Overlay { name: &'tl [u8] },
}

#[derive(TlRead, TlWrite)]
#[tl(boxed)]
enum Address<'tl> {
    /// You can also specify raw id of a variant
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

#[derive(TlRead, TlWrite)]
struct BlockId {
    workchain: i32,
    /// If you need custom deserialization logic for the field,
    /// you can specify either `with` attribute or separate
    /// `write_with`/`read_with` attributes.
    ///
    /// `with` must point to a module which must contain the
    /// following public functions:
    /// For `TlWrite`:
    ///   - `fn size_hint(v: &T) -> usize;`
    ///   - `fn write<P: TlPacket>(v: &T, p: &mut P);`
    /// For `TlRead`:
    ///   - `fn read(packet: &[u8], offset: &mut usize) -> TlResult<T>;`
    ///
    /// `write_with` must point to a function with the following signature:
    /// `fn write<P: TlPacket>(v: &T, p: &mut P);`
    /// NOTE: `write_with` requires `size_hint` attribute.
    ///
    /// `read_with` must point to a function with the following signature:
    /// `fn read(packet: &[u8], offset: &mut usize) -> TlResult<T>;`
    #[tl(with = "tl_shard")]
    shard: u64,
    seqno: u32,
}

/// `with` is similar to the same attribute in serde
mod tl_shard {
    use tl_proto::{TlPacket, TlRead, TlWrite};

    pub const fn size_hint(_: &u64) -> usize { 8 }

    pub fn write<P: TlPacket>(shard: &u64, packet: &mut P) {
        shard.write_to(packet);
    }

    pub fn read(packet: &mut &[u8]) -> tl_proto::TlResult<u64> {
        let shard = u64::read_from(packet)?;
        if shard % 10000 == 0 {
            Ok(shard)
        } else {
            Err(tl_proto::TlError::InvalidData)
        }
    }
}

/// You can also declare "bare" structs and specify
/// the type id of their boxed variant, so something
/// like `tl_proto::BoxedWrapper` can be used later.
///
/// See also:
/// - `tl_proto::deserialize_as_boxed` - read bare type as boxed
/// - `tl_proto::serialize_as_boxed` - write bare type as boxed
/// - `tl_proto::hash_as_boxed` - compute hash of the boxed repr
impl tl_proto::BoxedConstructor for BlockId {
    /// There is a way to compute id of a variant at
    /// compile time using the provided scheme
    const TL_ID: u32 = tl_proto::id!("liteServer.lookupBlock", scheme = "my_proto.tl");
}

/// There s a way to have a struct with optional fields
#[derive(TlRead, TlWrite)]
#[tl(boxed, id = "liteServer.lookupBlock", scheme = "my_proto.tl")]
struct LookupBlock {
    /// At first, there must be a field, marked with `flags` attribute.
    ///
    /// NOTE: It must precede the fields that depend on it.
    #[tl(flags)]
    mode: (),
    id: BlockId,
    /// Fields with `flags_bit` attribute must be `Option`s
    #[tl(flags_bit = 1)]
    lt: Option<u64>,
    /// You can also explicitly specify the flags field
    /// (e.g. when multiple fields with `flags` attribute are used)
    #[tl(flags_field = "mode", flags_bit = 2)]
    utime: Option<u32>,

    // Or you can use the shorter syntax:
    //
    // #[tl(flags_bit = "mode.2")]
    // utime: Option<u32>,
}

#[derive(TlWrite)]
struct StructWithSignature {
    value: u64,
    /// `signature` is used by `TlWrite` to simplify signature
    /// verification. In most cases you sign a data with an empty signature,
    /// so this attribute just writes `&[]` to the packet of type `P` if
    /// `<P as TlPacket>::TARGET == TlTarget::Hasher`
    #[tl(signature)]
    my_signature: [u8; 64],
}

/// You can constraint the type by its representation
/// (`tl_proto::Bare` / `tl_proto::Boxed`)
fn ultra_hash<T: TlWrite<Repr = tl_proto::Boxed>>(object: T) -> u32 {
    tl_proto::serialize(object).len() as u32
}

fn main() {
    // When the struct or enum has `TlRead` derive macro
    // and it is marked `boxed`, it also exposes
    // either `TL_ID` constant (in case of struct)
    // or `TL_ID_*` constants (in case of enum) where
    // `*` is a variant name in screaming snake case
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
