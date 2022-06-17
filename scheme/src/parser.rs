use std::collections::hash_map;

use pest::iterators::{Pair, Pairs};
use pest::Parser;

use crate::grammar::{Grammar, Rule};
use crate::tokens::{BuiltinConstructor, Constructor, Field, OutputType, Scheme, Type};

impl<'a> Scheme<'a> {
    pub fn parse(s: &'a str) -> Result<Self, ParserError> {
        let mut scheme = Scheme::default();

        let pairs = Grammar::parse(Rule::tl_scheme, s)
            .map_err(|e| ParserError::InvalidInput(e.to_string()))?;
        for pair in pairs {
            parse_declarations(&mut scheme, pair)?;
        }

        Ok(scheme)
    }
}

impl<'a> Constructor<'a> {
    pub fn parse(s: &'a str) -> Result<Self, ParserError> {
        let pairs = Grammar::parse(Rule::tl_constructor, s)
            .map_err(|e| ParserError::InvalidInput(e.to_string()))?;
        let pair = pairs.into_iter().next().ok_or(ParserError::ExpectedEof)?;

        parse_declaration(pair)
    }
}

fn parse_declarations<'a>(
    scheme: &mut Scheme<'a>,
    pair: Pair<'a, Rule>,
) -> Result<(), ParserError> {
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
                return Err(ParserError::DuplicateVariant(decl.variant.to_owned()));
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
                    return Err(ParserError::DuplicateVariant(decl.variant.to_owned()));
                }
            }
        }
    }

    Ok(())
}

fn parse_builtin_declaration(pair: Pair<'_, Rule>) -> Result<BuiltinConstructor<'_>, ParserError> {
    let mut pairs = pair.into_inner();

    let (variant, tl_id) = pairs
        .next()
        .ok_or(ParserError::ExpectedEof)
        .and_then(parse_variant)?;

    let output = pairs.next().ok_or(ParserError::ExpectedEof)?.as_str();

    Ok(BuiltinConstructor {
        variant,
        tl_id,
        output,
    })
}

fn parse_declaration(pair: Pair<'_, Rule>) -> Result<Constructor<'_>, ParserError> {
    let mut pairs = pair.into_inner();

    let (variant, tl_id) = pairs
        .next()
        .ok_or(ParserError::ExpectedEof)
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

    let output_type = pairs.next().ok_or(ParserError::ExpectedEof)?;
    let output = parse_output_type(output_type)?;

    Ok(Constructor {
        variant,
        tl_id,
        type_parameters,
        fields,
        output,
    })
}

fn parse_variant(pair: Pair<'_, Rule>) -> Result<(&'_ str, Option<u32>), ParserError> {
    let mut pairs = pair.into_inner();

    let name = pairs.next().ok_or(ParserError::ExpectedEof)?;
    let tl_id = pairs
        .next()
        .map(|pair| u32::from_str_radix(pair.as_str(), 16))
        .transpose()
        .map_err(ParserError::InvalidTlId)?;

    Ok((name.as_str(), tl_id))
}

fn parse_type_arg(pair: Pair<'_, Rule>) -> Result<Field<'_>, ParserError> {
    let mut pairs = pair.into_inner();

    let name = pairs.next().ok_or(ParserError::ExpectedEof)?;

    Ok(Field {
        name: Some(name.as_str()),
        ty: pairs
            .next()
            .ok_or(ParserError::ExpectedEof)
            .and_then(parse_type_expr)?,
    })
}

fn parse_field(pair: Pair<'_, Rule>) -> Result<Field<'_>, ParserError> {
    let pair = pair.into_inner().next().ok_or(ParserError::ExpectedEof)?;

    match pair.as_rule() {
        Rule::field_simple => {
            let mut pairs = pair.into_inner();
            let name = pairs.next().ok_or(ParserError::ExpectedEof)?.as_str();

            let mut pair = pairs.next().ok_or(ParserError::ExpectedEof)?;
            let ty = if pair.as_rule() == Rule::conditional_def {
                let mut conditional = pair.into_inner();
                let flags_field = conditional.next().ok_or(ParserError::ExpectedEof)?.as_str();

                let bit = conditional
                    .next()
                    .map(|pair| pair.as_str().parse::<u8>())
                    .transpose()
                    .map_err(ParserError::InvalidFlagsBit)?
                    .ok_or(ParserError::ExpectedEof)?;

                pair = pairs.next().ok_or(ParserError::ExpectedEof)?;

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
            let mut pair = pairs.peek().ok_or(ParserError::ExpectedEof)?;

            let mut name = None;
            if pair.as_rule() == Rule::field_ident_opt {
                name = Some(pair.as_str());
                pairs.next();

                pair = pairs.peek().ok_or(ParserError::ExpectedEof)?;
            }

            let mut multiplicity = None;
            if pair.as_rule() == Rule::nat_const {
                multiplicity = pair
                    .as_str()
                    .parse::<u32>()
                    .map(Some)
                    .map_err(ParserError::InvalidRepetition)?;
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
        rule => Err(ParserError::UnexpectedRule(format!("{rule:?}"))),
    }
}

fn parse_output_type(pair: Pair<'_, Rule>) -> Result<OutputType<'_>, ParserError> {
    let mut pairs = pair.into_inner();

    let ty = pairs.next().ok_or(ParserError::ExpectedEof)?.as_str();

    let ty_param = pairs.next().map(parse_type_expr).transpose()?.map(Box::new);

    Ok(OutputType { ty, ty_param })
}

fn parse_type_expr(pair: Pair<'_, Rule>) -> Result<Type<'_>, ParserError> {
    if pair.as_rule() != Rule::type_expr {
        return Err(ParserError::ExpectedEof);
    }

    let mut pairs = pair.into_inner();

    let param = pairs.next().ok_or(ParserError::ExpectedEof)?;
    let ty = match param.as_rule() {
        Rule::type_ident => param.as_str(),
        Rule::nat_type => return Ok(Type::Int),
        rule => return Err(ParserError::UnexpectedRule(format!("{rule:?}"))),
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

fn read_same_rules<'a, F>(
    pairs: &mut Pairs<'a, Rule>,
    rule: Rule,
    mut f: F,
) -> Result<(), ParserError>
where
    F: FnMut(Pair<'a, Rule>) -> Result<(), ParserError>,
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
pub enum ParserError {
    #[error("invalid input:\n{0}")]
    InvalidInput(String),
    #[error("duplicate variant: {0}")]
    DuplicateVariant(String),
    #[error("unexpected rule: {0:?}")]
    UnexpectedRule(String),
    #[error("unexpected end of input")]
    ExpectedEof,
    #[error("invalid TL id")]
    InvalidTlId(#[source] std::num::ParseIntError),
    #[error("invalid flags bit")]
    InvalidFlagsBit(#[source] std::num::ParseIntError),
    #[error("invalid repetition")]
    InvalidRepetition(#[source] std::num::ParseIntError),
}
