use crc::{Crc, CRC_32_ISO_HDLC};

#[derive(Debug, Copy, Clone)]
pub struct BuiltinConstructor<'a> {
    pub variant: &'a str,
    pub tl_id: Option<u32>,
    pub output_ty: &'a str,
}

#[derive(Debug, Clone)]
pub struct Constructor<'a> {
    pub variant: &'a str,
    pub tl_id: Option<u32>,
    pub type_parameters: Vec<Field<'a>>,
    pub fields: Vec<Field<'a>>,
    pub output: Type<'a>,
}

impl<'a> Constructor<'a> {
    pub fn compute_tl_id(&self) -> u32 {
        if let Some(id) = self.tl_id {
            return id;
        }

        struct NormalizedConstructor<'c, 'a>(&'c Constructor<'a>);

        impl std::fmt::Display for NormalizedConstructor<'_, '_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.variant.fmt(f)?;

                for ty in &self.0.type_parameters {
                    f.write_fmt(format_args!(" {{{}}}", ty))?;
                }

                for field in &self.0.fields {
                    f.write_fmt(format_args!(" {}", field))?
                }

                f.write_fmt(format_args!(" = {}", self.0.output))
            }
        }

        let normalized = NormalizedConstructor(self).to_string();

        CRC.checksum(normalized.as_bytes())
    }
}

#[derive(Debug, Clone)]
pub struct Field<'a> {
    pub name: Option<&'a str>,
    pub ty: Type<'a>,
}

impl std::fmt::Display for Field<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = self.name {
            f.write_fmt(format_args!("{}:{}", name, self.ty))
        } else {
            self.ty.fmt(f)
        }
    }
}

#[derive(Debug, Clone)]
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

            Self::Generic { ty, ty_param } => f.write_fmt(format_args!("{} {}", ty, ty_param)),

            Self::Flagged {
                flags_field,
                bit,
                ty,
            } => f.write_fmt(format_args!("{}.{}?{}", flags_field, bit, ty)),

            Self::Repeated { multiplicity, ty } => {
                if let Some(multiplicity) = multiplicity {
                    f.write_fmt(format_args!("{} * [ ", multiplicity))?
                } else {
                    f.write_str("[ ")?
                }

                for ty in ty {
                    f.write_fmt(format_args!("{} ", ty))?
                }

                f.write_str("]")
            }
        }
    }
}

static CRC: Crc<u32> = Crc::<u32>::new(&CRC_32_ISO_HDLC);
