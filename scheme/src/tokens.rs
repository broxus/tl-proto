use crc::{Crc, CRC_32_ISO_HDLC};

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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Constructor<'a> {
    pub variant: &'a str,
    pub tl_id: Option<u32>,
    pub type_parameters: Vec<Field<'a>>,
    pub fields: Vec<Field<'a>>,
    pub output: OutputType<'a>,
}

impl<'a> Constructor<'a> {
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

static CRC: Crc<u32> = Crc::<u32>::new(&CRC_32_ISO_HDLC);
