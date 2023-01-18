#![no_main]
use libfuzzer_sys::fuzz_target;

use smallvec::SmallVec;
use tl_proto::*;

fuzz_target!(|data: &[u8]| {
    tl_proto::deserialize::<PacketContents<'_>>(data).ok();
});

pub type HashRef<'a> = &'a [u8; 32];

#[allow(unused)]
#[derive(Debug, Clone, TlRead)]
#[tl(boxed, id = 0xd142cd89)]
pub struct PacketContents<'tl> {
    pub rand1: &'tl [u8],

    #[tl(flags)]
    pub flags: (),

    #[tl(flags_bit = 0)]
    pub from: Option<PublicKey<'tl>>,
    #[tl(flags_bit = 1)]
    pub from_short: Option<HashRef<'tl>>,

    #[tl(flags_bit = 2)]
    pub message: Option<Message<'tl>>,
    #[tl(flags_bit = 3)]
    pub messages: Option<SmallVec<[Message<'tl>; 4]>>,

    #[tl(flags_bit = 4)]
    pub address: Option<AddressList<'tl>>,
    #[tl(flags_bit = 5)]
    pub priority_address: Option<AddressList<'tl>>,

    #[tl(flags_bit = 6)]
    pub seqno: Option<u64>,
    #[tl(flags_bit = 7)]
    pub confirm_seqno: Option<u64>,

    #[tl(flags_bit = 8)]
    pub recv_addr_list_version: Option<u32>,
    #[tl(flags_bit = 9)]
    pub recv_priority_addr_list_version: Option<u32>,

    #[tl(flags_bit = 10)]
    pub reinit_date: Option<u32>,
    #[tl(flags_bit = 10)]
    pub dst_reinit_date: Option<u32>,

    #[tl(flags_bit = 11)]
    pub signature: Option<&'tl [u8]>,

    pub rand2: &'tl [u8],
}

#[allow(unused)]
#[derive(Debug, Copy, Clone)]
pub struct AddressList<'tl> {
    pub address: Option<Address<'tl>>,
    pub version: u32,
    pub reinit_date: u32,
    pub priority: u32,
    pub expire_at: u32,
}

impl<'tl> TlRead<'tl> for AddressList<'tl> {
    fn read_from(packet: &'tl [u8], offset: &mut usize) -> TlResult<Self> {
        let address_count = u32::read_from(packet, offset)?;
        let mut address = None;
        for _ in 0..address_count {
            let item = Address::read_from(packet, offset)?;
            if address.is_none() {
                address = Some(item);
            }
        }

        let version = u32::read_from(packet, offset)?;
        let reinit_date = u32::read_from(packet, offset)?;
        let priority = u32::read_from(packet, offset)?;
        let expire_at = u32::read_from(packet, offset)?;

        Ok(Self {
            address,
            version,
            reinit_date,
            priority,
            expire_at,
        })
    }
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, TlRead)]
#[tl(boxed)]
pub enum Address<'tl> {
    #[tl(id = 0x670da6e7, size_hint = 8)]
    Udp { ip: u32, port: u32 },
    #[tl(id = 0xe31d63fa, size_hint = 20)]
    Udp6 { ip: &'tl [u8; 16], port: u32 },
    #[tl(id = 0x092b02eb)]
    Tunnel {
        to: HashRef<'tl>,
        pubkey: PublicKey<'tl>,
    },
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, TlRead)]
#[tl(boxed)]
pub enum PublicKey<'tl> {
    #[tl(id = 0x4813b4c6, size_hint = 32)]
    Ed25519 { key: HashRef<'tl> },
    #[tl(id = 0x34ba45cb)]
    Overlay { name: &'tl [u8] },
    #[tl(id = 0x2dbcadd4, size_hint = 32)]
    Aes { key: HashRef<'tl> },
    #[tl(id = 0xb61f450a)]
    Unencoded { data: &'tl [u8] },
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, TlRead)]
#[tl(boxed)]
pub enum Message<'tl> {
    #[tl(id = 0x0fac8416)]
    Answer {
        query_id: HashRef<'tl>,
        answer: &'tl [u8],
    },
    #[tl(id = 0x60dd1d69, size_hint = 68)]
    ConfirmChannel {
        key: HashRef<'tl>,
        peer_key: HashRef<'tl>,
        date: u32,
    },
    #[tl(id = 0xe673c3bb, size_hint = 36)]
    CreateChannel { key: HashRef<'tl>, date: u32 },
    #[tl(id = 0x204818f5)]
    Custom { data: &'tl [u8] },
    #[tl(id = 0x17f8dfda)]
    Nop,
    #[tl(id = 0xfd452d39)]
    Part {
        hash: HashRef<'tl>,
        total_size: u32,
        offset: u32,
        data: &'tl [u8],
    },
    #[tl(id = 0xb48bf97a)]
    Query {
        query_id: HashRef<'tl>,
        query: &'tl [u8],
    },
    #[tl(id = 0x10c20520, size_hint = 4)]
    Reinit { date: u32 },
}
