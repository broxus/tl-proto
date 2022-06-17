use std::collections::hash_map;

use pest::iterators::{Pair, Pairs};
use pest::Parser;
use rustc_hash::FxHashMap;

use self::grammar::{Grammar, Rule};
pub use self::tokens::{BuiltinConstructor, Constructor, ConstructorKind, Field, OutputType, Type};

pub mod grammar;
mod tokens;

#[derive(Debug, Clone, Default)]
pub struct Scheme<'a> {
    pub builtins: FxHashMap<&'a str, BuiltinConstructor<'a>>,
    pub types: FxHashMap<&'a str, Constructor<'a>>,
    pub boxed_types: FxHashMap<&'a str, Vec<&'a str>>,
    pub functions: FxHashMap<&'a str, Constructor<'a>>,
}

impl<'a> Scheme<'a> {
    /// Computes all TL ids in the scheme
    pub fn compute_all_ids(&self) -> FxHashMap<u32, (ConstructorKind, &Constructor<'a>)> {
        let mut ids = FxHashMap::with_capacity_and_hasher(
            self.types.len() + self.functions.len(),
            Default::default(),
        );

        for ty in self.types.values() {
            ids.insert(ty.compute_tl_id(), (ConstructorKind::Type, ty));
        }

        for func in self.functions.values() {
            ids.insert(func.compute_tl_id(), (ConstructorKind::Function, func));
        }

        ids
    }

    /// Resolves all types in the scheme
    pub fn validate(&self) -> Result<(), Error> {
        for ty in self.types.values() {
            self.check_constructor(ty)?;
        }
        for func in self.functions.values() {
            self.check_constructor(func)?;
        }
        Ok(())
    }

    /// Finds a type variant or function with the given name
    pub fn find_constructor(&self, name: &str) -> Option<(ConstructorKind, &Constructor<'a>)> {
        if let Some(constructor) = self.get_type_variant(name) {
            Some((ConstructorKind::Type, constructor))
        } else {
            self.get_function(name)
                .map(|constructor| (ConstructorKind::Function, constructor))
        }
    }

    /// Finds type variant with the given name
    pub fn get_type_variant(&self, name: &str) -> Option<&Constructor<'a>> {
        self.types.get(name)
    }

    /// Finds function with the given name
    pub fn get_function(&self, name: &str) -> Option<&Constructor<'a>> {
        self.functions.get(name)
    }

    fn check_constructor(&self, decl: &Constructor<'a>) -> Result<(), Error> {
        for (i, field) in decl.fields.iter().enumerate() {
            let prev_fields = &decl.fields[..i];
            self.check_type(&field.ty, &decl.type_parameters, prev_fields)?;
        }

        if !self.boxed_types.contains_key(decl.output.ty)
            && !decl
                .type_parameters
                .iter()
                .any(|field| field.name == Some(decl.output.ty))
        {
            return Err(Error::UnknownType(decl.output.to_string()));
        }

        if let Some(ty_param) = &decl.output.ty_param {
            self.check_type(ty_param, &decl.type_parameters, &[])?;
        }

        Ok(())
    }

    fn check_type(
        &self,
        ty: &Type<'a>,
        type_parameters: &[Field<'a>],
        prev_fields: &[Field<'a>],
    ) -> Result<(), Error> {
        match ty {
            Type::Int => {}
            Type::Named { ty } => {
                if !self.is_declared(ty)
                    && !type_parameters.iter().any(|field| field.name == Some(ty))
                {
                    return Err(Error::UnknownType(ty.to_string()));
                }
            }
            Type::Generic { ty, ty_param } => {
                if !self.is_declared(ty)
                    && !type_parameters.iter().any(|field| field.name == Some(ty))
                {
                    return Err(Error::UnknownType(ty.to_string()));
                }
                self.check_type(ty_param, type_parameters, &[])?;
            }
            Type::Flagged {
                flags_field, ty, ..
            } => {
                prev_fields
                    .iter()
                    .find(|field| field.name == Some(flags_field))
                    .ok_or_else(|| Error::UnknownField(flags_field.to_string()))?;
                self.check_type(ty, type_parameters, prev_fields)?;
            }
            Type::Repeated { ty, .. } => {
                for field in ty {
                    self.check_type(&field.ty, type_parameters, prev_fields)?;
                }
            }
        }

        Ok(())
    }

    fn is_declared(&self, ty: &str) -> bool {
        self.types.contains_key(ty)
            || self.boxed_types.contains_key(ty)
            || self.builtins.contains_key(ty)
    }
}

pub fn parse_scheme(scheme: &str) -> Result<Scheme, Error> {
    let pairs = Grammar::parse(Rule::tl_scheme, scheme)?;

    let mut scheme = Scheme::default();
    for pair in pairs {
        parse_declarations(&mut scheme, pair)?;
    }

    Ok(scheme)
}

pub fn parse_constructor(declaration: &str) -> Result<Constructor, Error> {
    let pairs = Grammar::parse(Rule::tl_constructor, declaration)?;
    let pair = pairs
        .into_iter()
        .next()
        .ok_or(Error::ExpectedRule(Rule::combinator_decl))?;

    parse_declaration(pair)
}

fn parse_declarations<'a>(scheme: &mut Scheme<'a>, pair: Pair<'a, Rule>) -> Result<(), Error> {
    let (declarations, is_type) = match pair.as_rule() {
        Rule::type_declarations => (&mut scheme.types, true),
        Rule::fn_declarations => (&mut scheme.functions, false),
        _ => return Ok(()),
    };

    for pair in pair.into_inner() {
        if pair.as_rule() == Rule::builtin_combinator_decl {
            let decl = parse_builtin_declaration(pair)?;
            scheme
                .boxed_types
                .entry(decl.output)
                .or_default()
                .push(decl.variant);

            if scheme.builtins.insert(decl.variant, decl).is_some() {
                return Err(Error::DuplicateVariant(decl.variant.to_owned()));
            }
        } else {
            let decl = parse_declaration(pair)?;
            if is_type {
                scheme
                    .boxed_types
                    .entry(decl.output.ty)
                    .or_default()
                    .push(decl.variant);
            }

            match declarations.entry(decl.variant) {
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(decl);
                }
                hash_map::Entry::Occupied(_) => {
                    return Err(Error::DuplicateVariant(decl.variant.to_owned()));
                }
            }
        }
    }

    Ok(())
}

fn parse_builtin_declaration(pair: Pair<'_, Rule>) -> Result<BuiltinConstructor<'_>, Error> {
    let mut pairs = pair.into_inner();

    let (variant, tl_id) = pairs
        .next()
        .ok_or(Error::ExpectedRule(Rule::variant))
        .and_then(parse_variant)?;

    let output = pairs
        .next()
        .ok_or(Error::ExpectedRule(Rule::boxed_type_ident))?
        .as_str();

    Ok(BuiltinConstructor {
        variant,
        tl_id,
        output,
    })
}

fn parse_declaration(pair: Pair<'_, Rule>) -> Result<Constructor<'_>, Error> {
    let mut pairs = pair.into_inner();

    let (variant, tl_id) = pairs
        .next()
        .ok_or(Error::ExpectedRule(Rule::variant))
        .and_then(parse_variant)?;

    let mut type_parameters = Vec::new();
    read_same_rules(&mut pairs, Rule::type_arg, |pair| {
        type_parameters.push(parse_type_arg(pair)?);
        Ok(())
    })?;

    let mut fields = Vec::new();
    read_same_rules(&mut pairs, Rule::field, |pair| {
        fields.push(parse_field(pair)?);
        Ok(())
    })?;

    let output_type = pairs.next().ok_or(Error::ExpectedRule(Rule::output_type))?;
    let output = parse_output_type(output_type)?;

    Ok(Constructor {
        variant,
        tl_id,
        type_parameters,
        fields,
        output,
    })
}

fn parse_variant(pair: Pair<'_, Rule>) -> Result<(&'_ str, Option<u32>), Error> {
    let mut pairs = pair.into_inner();

    let name = pairs.next().ok_or(Error::ExpectedRule(Rule::lc_ident_ns))?;
    let tl_id = pairs
        .next()
        .map(|pair| u32::from_str_radix(pair.as_str(), 16))
        .transpose()
        .map_err(Error::InvalidTlId)?;

    Ok((name.as_str(), tl_id))
}

fn parse_type_arg(pair: Pair<'_, Rule>) -> Result<Field<'_>, Error> {
    let mut pairs = pair.into_inner();

    let name = pairs.next().ok_or(Error::ExpectedRule(Rule::field_ident))?;

    Ok(Field {
        name: Some(name.as_str()),
        ty: pairs
            .next()
            .ok_or(Error::ExpectedRule(Rule::type_expr))
            .and_then(parse_type_expr)?,
    })
}

fn parse_field(pair: Pair<'_, Rule>) -> Result<Field<'_>, Error> {
    let pair = pair
        .into_inner()
        .next()
        .ok_or(Error::ExpectedRule(Rule::field_ident_opt))?;

    match pair.as_rule() {
        Rule::field_simple => {
            let mut pairs = pair.into_inner();
            let name = pairs
                .next()
                .ok_or(Error::ExpectedRule(Rule::field_ident_opt))?
                .as_str();

            let mut pair = pairs.next().ok_or(Error::ExpectedRule(Rule::type_expr))?;
            let ty = if pair.as_rule() == Rule::conditional_def {
                let mut conditional = pair.into_inner();
                let flags_field = conditional
                    .next()
                    .ok_or(Error::ExpectedRule(Rule::field_ident))?
                    .as_str();

                let bit = conditional
                    .next()
                    .map(|pair| pair.as_str().parse::<u8>())
                    .transpose()
                    .map_err(Error::InvalidFlagsBit)?
                    .ok_or(Error::ExpectedRule(Rule::nat_const))?;

                pair = pairs.next().ok_or(Error::ExpectedRule(Rule::type_expr))?;

                Type::Flagged {
                    flags_field,
                    bit,
                    ty: Box::new(parse_type_expr(pair)?),
                }
            } else {
                parse_type_expr(pair)?
            };

            Ok(Field {
                name: Some(name),
                ty,
            })
        }
        Rule::field_repeated => {
            let mut pairs = pair.into_inner();
            let mut pair = pairs.peek().ok_or(Error::ExpectedRule(Rule::field))?;

            let mut name = None;
            if pair.as_rule() == Rule::field_ident_opt {
                name = Some(pair.as_str());
                pairs.next();

                pair = pairs.peek().ok_or(Error::ExpectedRule(Rule::field))?;
            }

            let mut multiplicity = None;
            if pair.as_rule() == Rule::nat_const {
                multiplicity = pair
                    .as_str()
                    .parse::<u32>()
                    .map(Some)
                    .map_err(Error::InvalidRepetition)?;
                pairs.next();
            }

            let ty = pairs.map(parse_field).collect::<Result<Vec<_>, _>>()?;

            Ok(Field {
                name,
                ty: Type::Repeated { multiplicity, ty },
            })
        }
        Rule::type_expr => Ok(Field {
            name: None,
            ty: parse_type_expr(pair)?,
        }),
        rule => Err(Error::UnexpectedRule(rule)),
    }
}

fn parse_output_type(pair: Pair<'_, Rule>) -> Result<OutputType<'_>, Error> {
    let mut pairs = pair.into_inner();

    let ty = pairs
        .next()
        .ok_or(Error::ExpectedRule(Rule::boxed_type_ident))?
        .as_str();

    let ty_param = pairs.next().map(parse_type_expr).transpose()?.map(Box::new);

    Ok(OutputType { ty, ty_param })
}

fn parse_type_expr(pair: Pair<'_, Rule>) -> Result<Type<'_>, Error> {
    if pair.as_rule() != Rule::type_expr {
        return Err(Error::ExpectedRule(Rule::type_expr));
    }

    let mut pairs = pair.into_inner();

    let param = pairs.next().ok_or(Error::ExpectedRule(Rule::type_expr))?;
    let ty = match param.as_rule() {
        Rule::type_ident => param.as_str(),
        Rule::nat_type => return Ok(Type::Int),
        rule => return Err(Error::UnexpectedRule(rule)),
    };

    Ok(if let Some(param) = pairs.next() {
        Type::Generic {
            ty,
            ty_param: Box::new(parse_type_expr(param)?),
        }
    } else {
        Type::Named { ty }
    })
}

fn read_same_rules<'a, F>(pairs: &mut Pairs<'a, Rule>, rule: Rule, mut f: F) -> Result<(), Error>
where
    F: FnMut(Pair<'a, Rule>) -> Result<(), Error>,
{
    while pairs
        .peek()
        .map(|pair| pair.as_rule() == rule)
        .unwrap_or_default()
    {
        if let Some(pair) = pairs.next() {
            f(pair)?;
        }
    }
    Ok(())
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("parser error: {0}")]
    ParserError(#[from] pest::error::Error<Rule>),
    #[error("duplicate variant: {0}")]
    DuplicateVariant(String),
    #[error("unexpected rule: {0:?}")]
    UnexpectedRule(Rule),
    #[error("expected rule: {0:?}")]
    ExpectedRule(Rule),
    #[error("invalid TL id")]
    InvalidTlId(#[source] std::num::ParseIntError),
    #[error("invalid flags bit")]
    InvalidFlagsBit(#[source] std::num::ParseIntError),
    #[error("invalid repetition")]
    InvalidRepetition(#[source] std::num::ParseIntError),
    #[error("unknown field: {0}")]
    UnknownField(String),
    #[error("unknown type: {0}")]
    UnknownType(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn correct_constructor_parser() {
        assert_eq!(
            parse_constructor("boolTrue = Bool")
                .unwrap()
                .compute_tl_id(),
            0x997275b5
        );

        assert_eq!(
            parse_constructor("pub.ed25519 key:int256 = PublicKey;")
                .unwrap()
                .compute_tl_id(),
            0x4813b4c6
        );

        const PACKET_CONTENTS: &str = r###"
        adnl.packetContents
            rand1:bytes
            flags:#
            from:flags.0?PublicKey
            from_short:flags.1?adnl.id.short
            message:flags.2?adnl.Message
            messages:flags.3?(vector adnl.Message)
            address:flags.4?adnl.addressList
            priority_address:flags.5?adnl.addressList
            seqno:flags.6?long
            confirm_seqno:flags.7?long
            recv_addr_list_version:flags.8?int
            recv_priority_addr_list_version:flags.9?int
            reinit_date:flags.10?int
            dst_reinit_date:flags.10?int
            signature:flags.11?bytes
            rand2:bytes
        = adnl.PacketContents
        "###;

        assert_eq!(
            parse_constructor(PACKET_CONTENTS).unwrap().compute_tl_id(),
            0xd142cd89
        );
    }

    #[test]
    fn correct_scheme_parser() {
        const DATA: &str = r###"
int ? = Int;
long ? = Long;
double ? = Double;
string ? = String;
object ? = Object;
function ? = Function;
bytes data:string = Bytes;
true = True;
boolTrue = Bool;
boolFalse = Bool;


vector {t:Type} # [ t ] = Vector t;

int128 4*[ int ] = Int128;
int256 8*[ int ] = Int256;

testObject value:int o:object f:function = TestObject;
testString value:string = TestObject;
testInt value:int = TestObject;
testVectorBytes value:(vector bytes) = TestObject;

tcp.authentificate nonce:bytes = tcp.Message;
tcp.authentificationNonce nonce:bytes = tcp.Message;
tcp.authentificationComplete key:PublicKey signature:bytes = tcp.Message;

fec.raptorQ data_size:int symbol_size:int symbols_count:int = fec.Type;
fec.roundRobin data_size:int symbol_size:int symbols_count:int = fec.Type;
fec.online data_size:int symbol_size:int symbols_count:int = fec.Type;

---functions---

getTestObject = TestObject;

---types---

pk.unenc data:bytes = PrivateKey;
pk.ed25519 key:int256 = PrivateKey;
pk.aes key:int256 = PrivateKey;
pk.overlay name:bytes = PrivateKey;

pub.unenc data:bytes = PublicKey;
pub.ed25519 key:int256 = PublicKey;
pub.aes key:int256 = PublicKey;
pub.overlay name:bytes = PublicKey;


---functions---

---types---

adnl.id.short id:int256 = adnl.id.Short;

adnl.proxyToFastHash ip:int port:int date:int data_hash:int256 shared_secret:int256 = adnl.ProxyTo;
adnl.proxyToFast ip:int port:int date:int signature:int256 = adnl.ProxyToSign;

adnl.proxy.none id:int256 = adnl.Proxy;
adnl.proxy.fast id:int256 shared_secret:bytes = adnl.Proxy;


adnl.address.udp ip:int port:int = adnl.Address;
adnl.address.udp6 ip:int128 port:int = adnl.Address;
//adnl.address.tcp ip:int port:int = adnl.Address;
//adnl.address.tcp6 ip:int128 port:int = adnl.Address;

adnl.address.tunnel to:int256 pubkey:PublicKey = adnl.Address;

adnl.addressList addrs:(vector adnl.Address) version:int reinit_date:int priority:int expire_at:int = adnl.AddressList;

adnl.node id:PublicKey addr_list:adnl.addressList = adnl.Node;
adnl.nodes nodes:(vector adnl.node) = adnl.Nodes;

---functions---

---types---

adnl.packetContents
  rand1:bytes
  flags:#
  from:flags.0?PublicKey
  from_short:flags.1?adnl.id.short
  message:flags.2?adnl.Message
  messages:flags.3?(vector adnl.Message)
  address:flags.4?adnl.addressList
  priority_address:flags.5?adnl.addressList
  seqno:flags.6?long
  confirm_seqno:flags.7?long
  recv_addr_list_version:flags.8?int
  recv_priority_addr_list_version:flags.9?int
  reinit_date:flags.10?int
  dst_reinit_date:flags.10?int
  signature:flags.11?bytes
  rand2:bytes
        = adnl.PacketContents;

adnl.tunnelPacketContents
  rand1:bytes
  flags:#
  from_ip:flags.0?int
  from_port:flags.0?int
  message:flags.1?bytes
  statistics:flags.2?bytes
  payment:flags.3?bytes
  rand2:bytes
        = adnl.TunnelPacketContents;


// flag 16 - packet is outbound
// flag 17 - control packet
adnl.proxyPacketHeader
  proxy_id:int256
  flags:#
  ip:flags.0?int
  port:flags.0?int
  adnl_start_time:flags.1?int
  seqno:flags.2?long
  date:flags.3?int
  signature:int256 = adnl.ProxyPacketHeader;

adnl.proxyControlPacketPing id:int256 = adnl.ProxyControlPacket;
adnl.proxyControlPacketPong id:int256 = adnl.ProxyControlPacket;
adnl.proxyControlPacketRegister ip:int port:int = adnl.ProxyControlPacket;


adnl.message.createChannel key:int256 date:int = adnl.Message;
adnl.message.confirmChannel key:int256 peer_key:int256 date:int = adnl.Message;

adnl.message.custom data:bytes = adnl.Message;

adnl.message.nop = adnl.Message;
adnl.message.reinit date:int = adnl.Message;

adnl.message.query query_id:int256 query:bytes = adnl.Message;
adnl.message.answer query_id:int256 answer:bytes = adnl.Message;

adnl.message.part hash:int256 total_size:int offset:int data:bytes = adnl.Message;

---functions---
---types---

adnl.db.node.key local_id:int256 peer_id:int256 = adnl.db.Key;
adnl.db.node.value date:int id:PublicKey addr_list:adnl.addressList priority_addr_list:adnl.addressList = adnl.db.node.Value;

---functions---


---types---

rldp2.messagePart transfer_id:int256 fec_type:fec.Type part:int total_size:long seqno:int data:bytes = rldp2.MessagePart;
rldp2.confirm transfer_id:int256 part:int max_seqno:int received_mask:int received_count:int = rldp2.MessagePart;
rldp2.complete transfer_id:int256 part:int = rldp2.MessagePart;

rldp.messagePart transfer_id:int256 fec_type:fec.Type part:int total_size:long seqno:int data:bytes = rldp.MessagePart;
rldp.confirm transfer_id:int256 part:int seqno:int = rldp.MessagePart;
rldp.complete transfer_id:int256 part:int = rldp.MessagePart;

rldp.message id:int256 data:bytes = rldp.Message;
rldp.query query_id:int256 max_answer_size:long timeout:int data:bytes = rldp.Message;
rldp.answer query_id:int256 data:bytes = rldp.Message;


---functions---
---types---
dht.node id:PublicKey addr_list:adnl.addressList version:int signature:bytes = dht.Node;
dht.nodes nodes:(vector dht.node) = dht.Nodes;

dht.key id:int256 name:bytes idx:int = dht.Key;

dht.updateRule.signature = dht.UpdateRule;
dht.updateRule.anybody = dht.UpdateRule;
dht.updateRule.overlayNodes = dht.UpdateRule;

dht.keyDescription key:dht.key id:PublicKey update_rule:dht.UpdateRule signature:bytes = dht.KeyDescription;

dht.value key:dht.keyDescription value:bytes ttl:int signature:bytes = dht.Value;

dht.pong random_id:long = dht.Pong;

dht.valueNotFound nodes:dht.nodes = dht.ValueResult;
dht.valueFound value:dht.Value = dht.ValueResult;

dht.stored = dht.Stored;
dht.message node:dht.node = dht.Message;

dht.db.bucket nodes:dht.nodes = dht.db.Bucket;
dht.db.key.bucket id:int = dht.db.Key;

---functions---

dht.ping random_id:long = dht.Pong;
dht.store value:dht.value = dht.Stored;
dht.findNode key:int256 k:int = dht.Nodes;
dht.findValue key:int256 k:int = dht.ValueResult;
dht.getSignedAddressList = dht.Node;

dht.query node:dht.node = True;

---types---

overlay.node.toSign id:adnl.id.short overlay:int256 version:int = overlay.node.ToSign;
overlay.node id:PublicKey overlay:int256 version:int signature:bytes = overlay.Node;
overlay.nodes nodes:(vector overlay.node) = overlay.Nodes;

overlay.message overlay:int256 = overlay.Message;
//overlay.randomPeers peers:(vector adnl.node) = overlay.RandomPeers;
overlay.broadcastList hashes:(vector int256) = overlay.BroadcastList;

overlay.fec.received hash:int256 = overlay.Broadcast;
overlay.fec.completed hash:int256 = overlay.Broadcast;

overlay.broadcast.id src:int256 data_hash:int256 flags:int = overlay.broadcast.Id;
overlay.broadcastFec.id src:int256 type:int256 data_hash:int256 size:int flags:int = overlay.broadcastFec.Id;
overlay.broadcastFec.partId broadcast_hash:int256 data_hash:int256 seqno:int = overlay.broadcastFec.PartId;

overlay.broadcast.toSign hash:int256 date:int = overlay.broadcast.ToSign;

overlay.certificate issued_by:PublicKey expire_at:int max_size:int signature:bytes = overlay.Certificate;
overlay.emptyCertificate = overlay.Certificate;

overlay.certificateId overlay_id:int256 node:int256 expire_at:int max_size:int = overlay.CertificateId;

overlay.unicast data:bytes = overlay.Broadcast;
overlay.broadcast src:PublicKey certificate:overlay.Certificate flags:int data:bytes date:int signature:bytes = overlay.Broadcast;
overlay.broadcastFec src:PublicKey certificate:overlay.Certificate data_hash:int256 data_size:int flags:int
          data:bytes seqno:int fec:fec.Type date:int signature:bytes = overlay.Broadcast;
overlay.broadcastFecShort src:PublicKey certificate:overlay.Certificate broadcast_hash:int256 part_data_hash:int256 seqno:int signature:bytes = overlay.Broadcast;
overlay.broadcastNotFound = overlay.Broadcast;

---functions---

overlay.getRandomPeers peers:overlay.nodes = overlay.Nodes;

overlay.query overlay:int256 = True;
overlay.getBroadcast hash:int256 = overlay.Broadcast;
overlay.getBroadcastList list:overlay.broadcastList = overlay.BroadcastList;

---types---

overlay.db.nodes nodes:overlay.nodes = overlay.db.Nodes;
overlay.db.key.nodes local_id:int256 overlay:int256 = overlay.db.Key;

---functions---

---types---

catchain.block.id incarnation:int256 src:int256 height:int data_hash:int256 = catchain.block.Id;
catchain.block.dep src:int height:int data_hash:int256 signature:bytes = catchain.block.Dep;
catchain.block.data prev:catchain.block.dep deps:(vector catchain.block.dep) = catchain.block.Data;
catchain.block incarnation:int256 src:int height:int data:catchain.block.data signature:bytes = catchain.Block;
catchain.blocks blocks:(vector catchain.block) = catchain.Blocks;
catchain.blockUpdate block:catchain.block = catchain.Update;

catchain.block.data.badBlock block:catchain.block = catchain.block.inner.Data;
catchain.block.data.fork left:catchain.block.Dep right:catchain.block.Dep = catchain.block.inner.Data;
catchain.block.data.nop = catchain.block.inner.Data;
catchain.block.data.vector msgs:(vector bytes) = catchain.block.inner.Data;
//catchain.block.data.custom = catchain.block.inner.Data;

catchain.firstblock unique_hash:int256 nodes:(vector int256) = catchain.FirstBlock;

catchain.difference sent_upto:(vector int) = catchain.Difference;
catchain.differenceFork left:catchain.block.dep right:catchain.block.dep = catchain.Difference;

catchain.blockNotFound = catchain.BlockResult;
catchain.blockResult block:catchain.block = catchain.BlockResult;

catchain.sent cnt:int = catchain.Sent;

---functions---

catchain.getBlock block:int256 = catchain.BlockResult;
catchain.getBlocks blocks:(vector int256) = catchain.Sent;
catchain.getDifference rt:(vector int) = catchain.Difference;
catchain.getBlockHistory block:int256 height:long stop_if:(vector int256) = catchain.Sent;
//catchain.getForkDifference src:int fork:catchain.fork = catchain.ForkDifference;

---types---

validatorSession.round.id session:int256 height:long prev_block:int256 seqno:int = validatorSession.round.Id;

validatorSession.candidate.id round:int256 block_hash:int256 = validatorSession.tempBlock.Id;

validatorSession.message.startSession = validatorSession.Message;
validatorSession.message.finishSession = validatorSession.Message;

validatorSession.message.submittedBlock round:int root_hash:int256 file_hash:int256
               collated_data_file_hash:int256 = validatorSession.round.Message;
validatorSession.message.approvedBlock round:int candidate:int256 signature:bytes = validatorSession.round.Message;
validatorSession.message.rejectedBlock round:int candidate:int256 reason:bytes = validatorSession.round.Message;
validatorSession.message.commit round:int candidate:int256 signature:bytes = validatorSession.round.Message;

validatorSession.message.vote round:int attempt:int candidate:int256 = validatorSession.round.Message;
validatorSession.message.voteFor round:int attempt:int candidate:int256 = validatorSession.round.Message;
validatorSession.message.precommit round:int attempt:int candidate:int256 = validatorSession.round.Message;
validatorSession.message.empty round:int attempt:int = validatorSession.round.Message;

validatorSession.pong hash:long = validatorSession.Pong;

validatorSession.candidateId src:int256 root_hash:int256 file_hash:int256 collated_data_file_hash:int256 = validatorSession.CandidateId;

validatorSession.blockUpdate ts:long actions:(vector validatorSession.round.Message) state:int = validatorSession.BlockUpdate;
validatorSession.candidate src:int256 round:int root_hash:int256 data:bytes collated_data:bytes = validatorSession.Candidate;

validatorSession.config catchain_idle_timeout:double catchain_max_deps:int round_candidates:int next_candidate_delay:double round_attempt_duration:int
        max_round_attempts:int max_block_size:int max_collated_data_size:int = validatorSession.Config;
validatorSession.configNew catchain_idle_timeout:double catchain_max_deps:int round_candidates:int next_candidate_delay:double round_attempt_duration:int
        max_round_attempts:int max_block_size:int max_collated_data_size:int new_catchain_ids:Bool = validatorSession.Config;

---functions---

validatorSession.ping hash:long = validatorSession.Pong;
validatorSession.downloadCandidate round:int id:validatorSession.candidateId = validatorSession.Candidate;

---types---

hashable.bool value:Bool = Hashable;
hashable.int32 value:int = Hashable;
hashable.int64 value:long = Hashable;
hashable.int256 value:int256 = Hashable;
hashable.bytes value:bytes = Hashable;
hashable.pair left:int right:int = Hashable;
hashable.vector value:(vector int) = Hashable;
hashable.validatorSessionOldRound seqno:int block:int signatures:int approve_signatures:int = Hashable;
hashable.validatorSessionRoundAttempt seqno:int votes:int precommitted:int vote_for_inited:int vote_for:int = Hashable;
hashable.validatorSessionRound locked_round:int locked_block:int seqno:int precommitted:Bool
          first_attempt:int approved_blocks:int signatures:int attempts:int = Hashable;
hashable.blockSignature signature:int = Hashable;
hashable.sentBlock src:int root_hash:int file_hash:int collated_data_file_hash:int = Hashable;
hashable.sentBlockEmpty = Hashable;
hashable.vote block:int node:int = Hashable;
hashable.blockCandidate block:int approved:int = Hashable;
hashable.blockVoteCandidate block:int approved:int = Hashable;
hashable.blockCandidateAttempt block:int votes:int = Hashable;

hashable.cntVector data:int = Hashable;
hashable.cntSortedVector data:int = Hashable;

hashable.validatorSession ts:int old_rounds:int cur_round:int = Hashable;

---functions---
---types---


tonNode.sessionId workchain:int shard:long cc_seqno:int opts_hash:int256 = tonNode.SessionId;


tonNode.blockSignature who:int256 signature:bytes = tonNode.BlockSignature;

tonNode.blockId workchain:int shard:long seqno:int = tonNode.BlockId;
tonNode.blockIdExt workchain:int shard:long seqno:int root_hash:int256 file_hash:int256 = tonNode.BlockIdExt;
tonNode.zeroStateIdExt workchain:int root_hash:int256 file_hash:int256 = tonNode.ZeroStateIdExt;

tonNode.blockDescriptionEmpty = tonNode.BlockDescription;
tonNode.blockDescription id:tonNode.blockIdExt = tonNode.BlockDescription;
tonNode.blocksDescription ids:(vector tonNode.blockIdExt) incomplete:Bool = tonNode.BlocksDescription;
tonNode.preparedProofEmpty = tonNode.PreparedProof;
tonNode.preparedProof = tonNode.PreparedProof;
tonNode.preparedProofLink = tonNode.PreparedProof;
tonNode.preparedState = tonNode.PreparedState;
tonNode.notFoundState = tonNode.PreparedState;
tonNode.prepared = tonNode.Prepared;
tonNode.notFound = tonNode.Prepared;
tonNode.data data:bytes = tonNode.Data;
//tonNode.preparedKeyBlockProofEmpty = tonNode.PreparedKeyBlockProof;
//tonNode.preparedKeyBlockProof block_id:tonNode.blockIdExt = tonNode.PreparedKeyBlockProof;

tonNode.ihrMessage data:bytes = tonNode.IhrMessage;
tonNode.externalMessage data:bytes = tonNode.ExternalMessage;

tonNode.newShardBlock block:tonNode.blockIdExt cc_seqno:int data:bytes = tonNode.NewShardBlock;

tonNode.blockBroadcast id:tonNode.blockIdExt catchain_seqno:int validator_set_hash:int
              signatures:(vector tonNode.blockSignature)
              proof:bytes data:bytes = tonNode.Broadcast;
tonNode.ihrMessageBroadcast message:tonNode.ihrMessage = tonNode.Broadcast;
tonNode.externalMessageBroadcast message:tonNode.externalMessage = tonNode.Broadcast;
tonNode.newShardBlockBroadcast block:tonNode.newShardBlock = tonNode.Broadcast;

tonNode.shardPublicOverlayId workchain:int shard:long zero_state_file_hash:int256 = tonNode.ShardPublicOverlayId;

tonNode.keyBlocks blocks:(vector tonNode.blockIdExt) incomplete:Bool error:Bool = tonNode.KeyBlocks;

ton.blockId root_cell_hash:int256 file_hash:int256 = ton.BlockId;
ton.blockIdApprove root_cell_hash:int256 file_hash:int256 = ton.BlockId;

tonNode.dataList data:(vector bytes) = tonNode.DataList;

tonNode.dataFull id:tonNode.blockIdExt proof:bytes block:bytes is_link:Bool = tonNode.DataFull;
tonNode.dataFullEmpty = tonNode.DataFull;

tonNode.capabilities version:int capabilities:long = tonNode.Capabilities;

tonNode.success = tonNode.Success;

tonNode.archiveNotFound = tonNode.ArchiveInfo;
tonNode.archiveInfo id:long = tonNode.ArchiveInfo;

---functions---

tonNode.getNextBlockDescription prev_block:tonNode.blockIdExt = tonNode.BlockDescription;
tonNode.getNextBlocksDescription prev_block:tonNode.blockIdExt limit:int = tonNode.BlocksDescription;
tonNode.getPrevBlocksDescription next_block:tonNode.blockIdExt limit:int cutoff_seqno:int = tonNode.BlocksDescription;
tonNode.prepareBlockProof block:tonNode.blockIdExt allow_partial:Bool = tonNode.PreparedProof;
tonNode.prepareKeyBlockProof block:tonNode.blockIdExt allow_partial:Bool = tonNode.PreparedProof;
tonNode.prepareBlockProofs blocks:(vector tonNode.blockIdExt) allow_partial:Bool = tonNode.PreparedProof;
tonNode.prepareKeyBlockProofs blocks:(vector tonNode.blockIdExt) allow_partial:Bool = tonNode.PreparedProof;
tonNode.prepareBlock block:tonNode.blockIdExt = tonNode.Prepared;
tonNode.prepareBlocks blocks:(vector tonNode.blockIdExt) = tonNode.Prepared;
tonNode.preparePersistentState block:tonNode.blockIdExt masterchain_block:tonNode.blockIdExt = tonNode.PreparedState;
tonNode.prepareZeroState block:tonNode.blockIdExt = tonNode.PreparedState;
tonNode.getNextKeyBlockIds block:tonNode.blockIdExt max_size:int = tonNode.KeyBlocks;
tonNode.downloadNextBlockFull prev_block:tonNode.blockIdExt = tonNode.DataFull;
tonNode.downloadBlockFull block:tonNode.blockIdExt = tonNode.DataFull;
tonNode.downloadBlock block:tonNode.blockIdExt = tonNode.Data;
tonNode.downloadBlocks blocks:(vector tonNode.blockIdExt) = tonNode.DataList;
tonNode.downloadPersistentState block:tonNode.blockIdExt masterchain_block:tonNode.blockIdExt = tonNode.Data;
tonNode.downloadPersistentStateSlice block:tonNode.blockIdExt masterchain_block:tonNode.blockIdExt offset:long max_size:long = tonNode.Data;
tonNode.downloadZeroState block:tonNode.blockIdExt = tonNode.Data;
tonNode.downloadBlockProof block:tonNode.blockIdExt = tonNode.Data;
tonNode.downloadKeyBlockProof block:tonNode.blockIdExt = tonNode.Data;
tonNode.downloadBlockProofs blocks:(vector tonNode.blockIdExt) = tonNode.DataList;
tonNode.downloadKeyBlockProofs blocks:(vector tonNode.blockIdExt) = tonNode.DataList;
tonNode.downloadBlockProofLink block:tonNode.blockIdExt = tonNode.Data;
tonNode.downloadKeyBlockProofLink block:tonNode.blockIdExt = tonNode.Data;
tonNode.downloadBlockProofLinks blocks:(vector tonNode.blockIdExt) = tonNode.DataList;
tonNode.downloadKeyBlockProofLinks blocks:(vector tonNode.blockIdExt) = tonNode.DataList;
tonNode.getArchiveInfo masterchain_seqno:int = tonNode.ArchiveInfo;
tonNode.getArchiveSlice archive_id:long offset:long max_size:int = tonNode.Data;

tonNode.getCapabilities = tonNode.Capabilities;

tonNode.slave.sendExtMessage message:tonNode.externalMessage = tonNode.Success;

tonNode.query = Object;

---types---

// bit 0 - started
// bit 1 - ready to switch
// bit 2 - switched from
// bit 3 - archived
// bit 4 - disabled

db.root.dbDescription version:int first_masterchain_block_id:tonNode.blockIdExt flags:int = db.root.DbDescription;

db.root.key.cellDb version:int = db.root.Key;
db.root.key.blockDb version:int = db.root.Key;

db.root.config celldb_version:int blockdb_version:int = db.root.Config;
db.root.key.config = db.root.Key;

db.celldb.value block_id:tonNode.blockIdExt prev:int256 next:int256 root_hash:int256 = db.celldb.Value;
db.celldb.key.value hash:int256 = db.celldb.key.Value;

db.block.info#4ac6e727 id:tonNode.blockIdExt flags:# prev_left:flags.1?tonNode.blockIdExt
                                            prev_right:flags.2?tonNode.blockIdExt
                                            next_left:flags.3?tonNode.blockIdExt
                                            next_right:flags.4?tonNode.blockIdExt
                                            lt:flags.13?long
                                            ts:flags.14?int
                                            state:flags.17?int256
                                            masterchain_ref_seqno:flags.23?int = db.block.Info;
db.block.packedInfo id:tonNode.blockIdExt unixtime:int offset:long = db.block.Info;
db.block.archivedInfo id:tonNode.blockIdExt flags:# next:flags.0?tonNode.blockIdExt = db.block.Info;

db.blockdb.value next:tonNode.blockIdExt data:bytes = db.blockdb.Value;
db.blockdb.lru id:tonNode.blockIdExt prev:int256 next:int256 = db.blockdb.Lru;
db.blockdb.key.lru id:tonNode.blockIdExt = db.blockdb.Key;
db.blockdb.key.value id:tonNode.blockIdExt = db.blockdb.Key;

db.candidate source:PublicKey id:tonNode.blockIdExt data:bytes collated_data:bytes = db.Candidate;
db.candidate.id source:PublicKey id:tonNode.blockIdExt collated_data_file_hash:int256 = db.candidate.Id;

db.filedb.key.empty = db.filedb.Key;
db.filedb.key.blockFile block_id:tonNode.blockIdExt = db.filedb.Key;
db.filedb.key.zeroStateFile block_id:tonNode.blockIdExt = db.filedb.Key;
db.filedb.key.persistentStateFile block_id:tonNode.blockIdExt masterchain_block_id:tonNode.blockIdExt = db.filedb.Key;
db.filedb.key.proof block_id:tonNode.blockIdExt = db.filedb.Key;
db.filedb.key.proofLink block_id:tonNode.blockIdExt = db.filedb.Key;
db.filedb.key.signatures block_id:tonNode.blockIdExt = db.filedb.Key;
db.filedb.key.candidate id:db.candidate.id = db.filedb.Key;
db.filedb.key.blockInfo block_id:tonNode.blockIdExt = db.filedb.Key;

db.filedb.value key:db.filedb.Key prev:int256 next:int256 file_hash:int256 = db.filedb.Value;

db.state.destroyedSessions sessions:(vector int256) = db.state.DestroyedSessions;
db.state.initBlockId block:tonNode.blockIdExt = db.state.InitBlockId;
db.state.gcBlockId block:tonNode.blockIdExt = db.state.GcBlockId;
db.state.shardClient block:tonNode.blockIdExt = db.state.ShardClient;
db.state.asyncSerializer block:tonNode.blockIdExt last:tonNode.blockIdExt last_ts:int = db.state.AsyncSerializer;
db.state.hardforks blocks:(vector tonNode.blockIdExt) = db.state.Hardforks;
db.state.dbVersion version:int = db.state.DbVersion;

db.state.key.destroyedSessions = db.state.Key;
db.state.key.initBlockId = db.state.Key;
db.state.key.gcBlockId = db.state.Key;
db.state.key.shardClient = db.state.Key;
db.state.key.asyncSerializer = db.state.Key;
db.state.key.hardforks = db.state.Key;
db.state.key.dbVersion = db.state.Key;

db.lt.el.key workchain:int shard:long idx:int = db.lt.Key;
db.lt.desc.key workchain:int shard:long = db.lt.Key;
db.lt.shard.key idx:int = db.lt.Key;
db.lt.status.key = db.lt.Key;
db.lt.el.value id:tonNode.blockIdExt lt:long ts:int = db.lt.el.Value;
db.lt.desc.value first_idx:int last_idx:int last_seqno:int last_lt:long last_ts:int = db.lt.desc.Value;
db.lt.shard.value workchain:int shard:long = db.lt.shard.Value;
db.lt.status.value total_shards:int = db.lt.status.Value;

db.files.index.key = db.files.Key;
db.files.package.key package_id:int key:Bool temp:Bool = db.files.Key;

db.files.index.value packages:(vector int) key_packages:(vector int) temp_packages:(vector int) = db.files.index.Value;
db.files.package.firstBlock workchain:int shard:long seqno:int unixtime:int lt:long = db.files.package.FirstBlock;
db.files.package.value package_id:int key:Bool temp:Bool firstblocks:(vector db.files.package.firstBlock) deleted:Bool
                   = db.files.package.Value;

---functions---

---types---

validator.groupMember public_key_hash:int256 adnl:int256 weight:long = engine.validator.GroupMember;
validator.group workchain:int shard:long catchain_seqno:int config_hash:int256 members:(vector validator.groupMember) = validator.Group;
validator.groupEx workchain:int shard:long vertical_seqno:int catchain_seqno:int config_hash:int256 members:(vector validator.groupMember) = validator.Group;
validator.groupNew workchain:int shard:long vertical_seqno:int last_key_block_seqno:int catchain_seqno:int config_hash:int256 members:(vector validator.groupMember) = validator.Group;

---functions---


---types---


id.config.local id:PrivateKey = id.config.Local;
dht.config.local id:adnl.id.short = dht.config.Local;
dht.config.random.local cnt:int = dht.config.Local;
liteserver.config.local id:PrivateKey port:int = liteserver.config.Local;
liteserver.config.random.local port:int = liteserver.config.Local;
validator.config.local id:adnl.id.short = validator.config.Local;
validator.config.random.local addr_list:adnl.addressList = validator.config.Local;
control.config.local priv:PrivateKey pub:int256 port:int = control.config.Local;
config.local local_ids:(vector id.config.local) dht:(vector dht.config.Local) validators:(vector validator.config.Local) liteservers:(vector liteserver.config.Local) control:(vector control.config.local) = config.Local;

dht.config.global static_nodes:dht.nodes k:int a:int = dht.config.Global;
adnl.config.global static_nodes:adnl.nodes = adnl.config.Global;
catchain.config.global tag:int256 nodes:(vector PublicKey) = catchain.config.Global;
dummyworkchain0.config.global zero_state_hash:int256 = dummyworkchain0.config.Global;
validator.config.global zero_state:tonNode.blockIdExt init_block:tonNode.blockIdExt hardforks:(vector tonNode.blockIdExt) = validator.config.Global;
config.global adnl:adnl.config.global dht:dht.config.global validator:validator.config.global = config.Global;

liteserver.desc id:PublicKey ip:int port:int = liteserver.Desc;
liteclient.config.global liteservers:(vector liteserver.desc) validator:validator.config.global = liteclient.config.Global;

engine.adnl id:int256 category:int = engine.Adnl;
engine.addr ip:int port:int categories:(vector int) priority_categories:(vector int) = engine.Addr;
engine.addrProxy in_ip:int in_port:int out_ip:int out_port:int
          proxy_type:adnl.Proxy categories:(vector int) priority_categories:(vector int) = engine.Addr;
engine.dht id:int256 = engine.Dht;
engine.validatorTempKey key:int256 expire_at:int = engine.ValidatorTempKey;
engine.validatorAdnlAddress id:int256 expire_at:int = engine.ValidatorAdnlAddress;
engine.validator id:int256 temp_keys:(vector engine.validatorTempKey) adnl_addrs:(vector engine.validatorAdnlAddress) election_date:int expire_at:int = engine.Validator;
engine.liteServer id:int256 port:int = engine.LiteServer;
engine.controlProcess id:int256 permissions:int = engine.ControlProcess;
engine.controlInterface id:int256 port:int allowed:(vector engine.controlProcess) = engine.ControlInterface;
engine.gc ids:(vector int256) = engine.Gc;

engine.dht.config dht:(vector engine.dht) gc:engine.gc = engine.dht.Config;
engine.validator.fullNodeMaster port:int adnl:int256 = engine.validator.FullNodeMaster;
engine.validator.fullNodeSlave ip:int port:int adnl:PublicKey = engine.validator.FullNodeSlave;
engine.validator.config out_port:int addrs:(vector engine.Addr) adnl:(vector engine.adnl)
        dht:(vector engine.dht)
        validators:(vector engine.validator) fullnode:int256 fullnodeslaves:(vector engine.validator.fullNodeSlave)
        fullnodemasters:(vector engine.validator.fullNodeMaster)
        liteservers:(vector engine.liteServer) control:(vector engine.controlInterface)
        gc:engine.gc = engine.validator.Config;

---functions---
---types---

engine.adnlProxy.port in_port:int out_port:int dst_ip:int dst_port:int proxy_type:adnl.Proxy = engine.adnlProxy.Port;

engine.adnlProxy.config ports:(vector engine.adnlProxy.port) = engine.adnlProxy.Config;

---functions---

---types---

engine.validator.keyHash key_hash:int256 = engine.validator.KeyHash;
engine.validator.signature signature:bytes = engine.validator.Signature;

engine.validator.oneStat key:string value:string = engine.validator.OneStat;
engine.validator.stats stats:(vector engine.validator.oneStat) = engine.validator.Stats;

engine.validator.controlQueryError code:int message:string = engine.validator.ControlQueryError;

engine.validator.time time:int = engine.validator.Time;
engine.validator.success = engine.validator.Success;

engine.validator.jsonConfig data:string = engine.validator.JsonConfig;

engine.validator.electionBid election_date:int perm_key:int256 adnl_addr:int256 to_send_payload:bytes = engine.validator.ElectionBid;
engine.validator.proposalVote perm_key:int256 to_send:bytes = engine.validator.ProposalVote;

engine.validator.dhtServerStatus id:int256 status:int = engine.validator.DhtServerStatus;
engine.validator.dhtServersStatus servers:(vector engine.validator.dhtServerStatus) = engine.validator.DhtServersStatus;

engine.validator.validatorKeysSet election_date:int perm_key:int256 temp_keys:(vector int256) adnl_addrs:(vector int256) = engine.validator.ValidatorKeysSet;
engine.validator.validatorKeys validators:(vector engine.validator.ValidatorKeysSet) = engine.validator.ValidatorKeys;

---functions---

engine.validator.getTime = engine.validator.Time;
engine.validator.importPrivateKey key:PrivateKey = engine.validator.KeyHash;
engine.validator.exportPrivateKey key_hash:int256 = PrivateKey;
engine.validator.exportPublicKey key_hash:int256 = PublicKey;
engine.validator.generateKeyPair = engine.validator.KeyHash;
engine.validator.addAdnlId key_hash:int256 category:int = engine.validator.Success;
engine.validator.addDhtId key_hash:int256 = engine.validator.Success;
engine.validator.addValidatorPermanentKey key_hash:int256 election_date:int ttl:int = engine.validator.Success;
engine.validator.addValidatorTempKey permanent_key_hash:int256 key_hash:int256 ttl:int = engine.validator.Success;
engine.validator.addValidatorAdnlAddress permanent_key_hash:int256 key_hash:int256 ttl:int = engine.validator.Success;
engine.validator.changeFullNodeAdnlAddress adnl_id:int256 = engine.validator.Success;
engine.validator.addLiteserver key_hash:int256 port:int = engine.validator.Success;
engine.validator.addControlInterface key_hash:int256 port:int = engine.validator.Success;
engine.validator.addControlProcess key_hash:int256 port:int peer_key:int256 permissions:int = engine.validator.Success;

engine.validator.getValidatorKeys = engine.validator.ValidatorKeys;
engine.validator.delDhtId key_hash:int256 = engine.validator.Success;
engine.validator.delAdnlId key_hash:int256 = engine.validator.Success;
engine.validator.delValidatorPermanentKey key_hash:int256 = engine.validator.Success;
engine.validator.delValidatorTempKey permanent_key_hash:int256 key_hash:int256 = engine.validator.Success;
engine.validator.delValidatorAdnlAddress permanent_key_hash:int256 key_hash:int256 = engine.validator.Success;

engine.validator.addListeningPort ip:int port:int categories:(vector int) priority_categories:(vector int) = engine.validator.Success;
engine.validator.addProxy in_ip:int in_port:int out_ip:int out_port:int proxy:adnl.Proxy categories:(vector int) priority_categories:(vector int) = engine.validator.Success;
engine.validator.delListeningPort ip:int port:int categories:(vector int) priority_categories:(vector int) = engine.validator.Success;
engine.validator.delProxy out_ip:int out_port:int categories:(vector int) priority_categories:(vector int) = engine.validator.Success;

engine.validator.sign key_hash:int256 data:bytes = engine.validator.Signature;

engine.validator.getStats = engine.validator.Stats;
engine.validator.getConfig = engine.validator.JsonConfig;

engine.validator.setVerbosity verbosity:int = engine.validator.Success;

engine.validator.createElectionBid election_date:int election_addr:string wallet:string = engine.validator.ElectionBid;
engine.validator.createProposalVote vote:bytes = engine.validator.ProposalVote;
engine.validator.createComplaintVote election_id:int vote:bytes = engine.validator.ProposalVote;

engine.validator.checkDhtServers id:int256 = engine.validator.DhtServersStatus;

engine.validator.controlQuery data:bytes = Object;

engine.validator.downloadBlock block:tonNode.blockIdExt = engine.validator.Success;

---types---

storage.pong = storage.Pong;
storage.ok = Ok;

storage.state will_upload:Bool want_download:Bool = storage.State;
storage.piece proof:bytes data:bytes = storage.Piece;

storage.updateInit have_pieces:bytes state:storage.State = storage.Update;
storage.updateHavePieces piece_id:(vector int) = storage.Update;
storage.updateState state:storage.State = storage.Update;

---functions---

storage.ping session_id:long = storage.Pong;
storage.addUpdate session_id:long seqno:int update:storage.Update = Ok;

storage.getPiece piece_id:int = storage.Piece;

storage.queryPrefix id:int256 = Object;

---types---

http.header name:string value:string = http.Header;
http.payloadPart data:bytes trailer:(vector http.header) last:Bool = http.PayloadPart;
http.response http_version:string status_code:int reason:string headers:(vector http.header) = http.Response;

---functions---

http.request id:int256 method:string url:string http_version:string headers:(vector http.header) = http.Response;
http.getNextPayloadPart id:int256 seqno:int max_chunk_size:int = http.PayloadPart;

---types---


http.server.dnsEntry domain:string addr:adnl.id.short = http.server.DnsEntry;
http.server.host domains:(vector string) ip:int port:int adnl_id:adnl.id.short = http.server.Host;

http.server.config dhs:(vector http.server.dnsEntry) local_hosts:(vector http.server.host) = http.server.Config;

---functions---
        "###;

        let test = parse_scheme(DATA).unwrap();
        test.validate().unwrap();
        test.compute_all_ids();
    }
}
