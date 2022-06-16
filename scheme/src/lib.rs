use pest::iterators::{Pair, Pairs};
use pest::Parser;

use self::grammar::{Grammar, Rule};
pub use self::tokens::{BuiltinConstructor, Constructor, Field, Type};

pub mod grammar;
mod tokens;

#[derive(Debug, Clone, Default)]
pub struct Scheme<'a> {
    pub builtins: Vec<BuiltinConstructor<'a>>,
    pub type_declarations: Vec<Constructor<'a>>,
    pub fn_declarations: Vec<Constructor<'a>>,
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
    let declarations = match pair.as_rule() {
        Rule::type_declarations => &mut scheme.type_declarations,
        Rule::fn_declarations => &mut scheme.fn_declarations,
        _ => return Ok(()),
    };

    for pair in pair.into_inner() {
        if pair.as_rule() == Rule::builtin_combinator_decl {
            scheme.builtins.push(parse_builtin_declaration(pair)?);
        } else {
            let decl = parse_declaration(pair)?;
            declarations.push(decl);
        }
    }

    Ok(())
}

fn parse_builtin_declaration<'a>(pair: Pair<'a, Rule>) -> Result<BuiltinConstructor<'a>, Error> {
    let mut pairs = pair.into_inner();

    let (variant, tl_id) = pairs
        .next()
        .ok_or(Error::ExpectedRule(Rule::variant))
        .and_then(parse_variant)?;

    let output_ty = pairs
        .next()
        .ok_or(Error::ExpectedRule(Rule::boxed_type_ident))?
        .as_str();

    Ok(BuiltinConstructor {
        variant,
        tl_id,
        output_ty,
    })
}

fn parse_declaration<'a>(pair: Pair<'a, Rule>) -> Result<Constructor<'a>, Error> {
    let mut pairs = pair.into_inner();

    let (variant, tl_id) = pairs
        .next()
        .ok_or(Error::ExpectedRule(Rule::variant))
        .and_then(parse_variant)?;

    let mut type_parameters = Vec::new();
    read_same_rules(&mut pairs, Rule::type_arg, |pair| {
        Ok(type_parameters.push(parse_type_arg(pair)?))
    })?;

    let mut fields = Vec::new();
    read_same_rules(&mut pairs, Rule::field, |pair| {
        Ok(fields.push(parse_field(pair)?))
    })?;

    let result_type = pairs.next().ok_or(Error::ExpectedRule(Rule::result_type))?;
    let output = parse_result_type(result_type)?;

    Ok(Constructor {
        variant,
        tl_id,
        type_parameters,
        fields,
        output,
    })
}

fn parse_variant<'a>(pair: Pair<'a, Rule>) -> Result<(&'a str, Option<u32>), Error> {
    let mut pairs = pair.into_inner();

    let name = pairs.next().ok_or(Error::ExpectedRule(Rule::lc_ident_ns))?;
    let tl_id = pairs
        .next()
        .map(|pair| u32::from_str_radix(pair.as_str(), 16))
        .transpose()
        .map_err(Error::InvalidTlId)?;

    Ok((name.as_str(), tl_id))
}

fn parse_type_arg<'a>(pair: Pair<'a, Rule>) -> Result<Field<'a>, Error> {
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

fn parse_field<'a>(pair: Pair<'a, Rule>) -> Result<Field<'a>, Error> {
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

            let ty = pairs
                .map(|pair| parse_field(pair))
                .collect::<Result<Vec<_>, _>>()?;

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

fn parse_result_type<'a>(pair: Pair<'a, Rule>) -> Result<Type<'a>, Error> {
    let mut pairs = pair.into_inner();

    let ty = pairs
        .next()
        .ok_or(Error::ExpectedRule(Rule::boxed_type_ident))?
        .as_str();

    let type_param = pairs.next().map(parse_type_expr).transpose()?.map(Box::new);

    Ok(match type_param {
        Some(ty_param) => Type::Generic { ty, ty_param },
        None => Type::Named { ty },
    })
}

fn parse_type_expr<'a>(pair: Pair<'a, Rule>) -> Result<Type<'a>, Error> {
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
        boolTrue = Bool;
        boolFalse = Bool;

        vector {t:Type} # [ t ] = Vector t;

        int128 4*[ int ] = Int128;

        testObject value:int o:object f:function = TestObject;
        testString value:string = TestObject;
        testInt value:int = TestObject;
        testVectorBytes value:(vector bytes) = TestObject;

        tcp.authentificate nonce:bytes = tcp.Message;
        tcp.authentificationNonce nonce:bytes = tcp.Message;

        ---functions---

        catchain.getBlock block:int256 = catchain.BlockResult;

        ---types---

        tcp.authentificationComplete key:PublicKey signature:bytes = tcp.Message;

        ---functions---

        catchain.getBlock block:int256 = catchain.BlockResult;
        catchain.getBlocks blocks:(vector int256) = catchain.Sent;
        catchain.getDifference rt:(vector int) = catchain.Difference;
        catchain.getBlockHistory block:int256 height:long stop_if:(vector int256) = catchain.Sent;
        //catchain.getForkDifference src:int fork:catchain.fork = catchain.ForkDifference;
        "###;

        let test = parse_scheme(DATA).unwrap();
        println!("{:#?}", test);
    }
}
