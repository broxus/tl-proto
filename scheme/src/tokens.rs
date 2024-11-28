use crc::{Crc, CRC_32_ISO_HDLC};
use rustc_hash::FxHashMap;

/// Parsed scheme with all types and functions.
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
    pub fn validate(&self) -> Result<(), ValidationError> {
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

    fn check_constructor(&self, decl: &Constructor<'a>) -> Result<(), ValidationError> {
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
            return Err(ValidationError::UnknownType(decl.output.to_string()));
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
    ) -> Result<(), ValidationError> {
        match ty {
            Type::Int => {}
            Type::Named { ty } => {
                if !self.is_declared(ty)
                    && !type_parameters.iter().any(|field| field.name == Some(ty))
                {
                    return Err(ValidationError::UnknownType(ty.to_string()));
                }
            }
            Type::Generic { ty, ty_param } => {
                if !self.is_declared(ty)
                    && !type_parameters.iter().any(|field| field.name == Some(ty))
                {
                    return Err(ValidationError::UnknownType(ty.to_string()));
                }
                self.check_type(ty_param, type_parameters, &[])?;
            }
            Type::Flagged {
                flags_field, ty, ..
            } => {
                prev_fields
                    .iter()
                    .find(|field| field.name == Some(flags_field))
                    .ok_or_else(|| ValidationError::UnknownField(flags_field.to_string()))?;
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

/// Predefined constructor
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct BuiltinConstructor<'a> {
    pub variant: &'a str,
    pub tl_id: Option<u32>,
    pub output: &'a str,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum ConstructorKind {
    Type,
    Function,
}

impl std::fmt::Display for ConstructorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstructorKind::Type => write!(f, "type"),
            ConstructorKind::Function => write!(f, "function"),
        }
    }
}

/// Type or function declaration
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Constructor<'a> {
    pub variant: &'a str,
    pub tl_id: Option<u32>,
    pub type_parameters: Vec<Field<'a>>,
    pub fields: Vec<Field<'a>>,
    pub output: OutputType<'a>,
}

impl Constructor<'_> {
    pub fn as_normalized(&self) -> String {
        NormalizedConstructor(self).to_string()
    }

    pub fn compute_tl_id(&self) -> u32 {
        use std::fmt::Write;

        if let Some(id) = self.tl_id {
            return id;
        }

        struct Checksum<'a>(crc::Digest<'a, u32>);

        impl Write for Checksum<'_> {
            #[inline(always)]
            fn write_str(&mut self, s: &str) -> std::fmt::Result {
                self.0.update(s.as_bytes());
                Ok(())
            }
        }

        let mut checksum = Checksum(CRC.digest());
        write!(&mut checksum, "{}", NormalizedConstructor(self)).unwrap();

        checksum.0.finalize()
    }
}

struct NormalizedConstructor<'c, 'a>(&'c Constructor<'a>);

impl std::fmt::Display for NormalizedConstructor<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.variant.fmt(f)?;

        for ty in &self.0.type_parameters {
            f.write_fmt(format_args!(" {{{ty}}}"))?;
        }

        for field in &self.0.fields {
            f.write_fmt(format_args!(" {field}"))?
        }

        f.write_fmt(format_args!(" = {}", self.0.output))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Field<'a> {
    pub name: Option<&'a str>,
    pub ty: Type<'a>,
}

impl std::fmt::Display for Field<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = self.name {
            f.write_fmt(format_args!("{name}:{}", self.ty))
        } else {
            self.ty.fmt(f)
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OutputType<'a> {
    pub ty: &'a str,
    pub ty_param: Option<Box<Type<'a>>>,
}

impl std::fmt::Display for OutputType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty_param {
            None => f.write_str(self.ty),
            Some(ty_param) => f.write_fmt(format_args!("{} {ty_param}", self.ty)),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type<'a> {
    /// Simple `#` (equivalent to `u32`)
    Int,
    /// Full path to identifier
    Named { ty: &'a str },
    /// Full path to identifier with type param
    Generic {
        ty: &'a str,
        ty_param: Box<Type<'a>>,
    },
    /// Conditional type
    Flagged {
        flags_field: &'a str,
        bit: u8,
        ty: Box<Type<'a>>,
    },
    /// Tuple type which can be multiplied
    Repeated {
        multiplicity: Option<u32>,
        ty: Vec<Field<'a>>,
    },
}

impl std::fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => f.write_str("#"),

            Self::Named { ty } => f.write_str(ty),

            Self::Generic { ty, ty_param } => f.write_fmt(format_args!("{ty} {ty_param}")),

            Self::Flagged {
                flags_field,
                bit,
                ty,
            } => f.write_fmt(format_args!("{flags_field}.{bit}?{ty}")),

            Self::Repeated { multiplicity, ty } => {
                if let Some(multiplicity) = multiplicity {
                    f.write_fmt(format_args!("{multiplicity} * [ "))?
                } else {
                    f.write_str("[ ")?
                }

                for ty in ty {
                    f.write_fmt(format_args!("{ty} "))?
                }

                f.write_str("]")
            }
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ValidationError {
    #[error("unknown field: {0}")]
    UnknownField(String),
    #[error("unknown type: {0}")]
    UnknownType(String),
}

static CRC: Crc<u32> = Crc::<u32>::new(&CRC_32_ISO_HDLC);
