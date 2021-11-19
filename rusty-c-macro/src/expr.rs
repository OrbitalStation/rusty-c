use crate::*;

#[derive(Debug, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum ExprType {
    ///
    /// Special property that shows that expr
    /// can be converted into any integer type
    ///
    Integer,

    ///
    /// Special property that shows that expr
    /// can be converted into any floating point type
    ///
    #[allow(dead_code)]
    Float,

    /// Just an ordinary type
    Ord(Type)
}

impl ExprType {
    pub fn is_dominant_of(&self, other: &Self) -> bool {
        match self {
            Self::Float if *other == Self::Integer => true,
            Self::Ord(ty) => match other {
                Self::Ord(ty2) => ty.is_dominant_of(ty2),
                _ => true
            },
            _ => false
        }
    }

    pub fn to_type(&self) -> Type {
        match self {
            Self::Integer => Type::int(),
            Self::Float => Type::double(),
            Self::Ord(ty) => ty.clone()
        }
    }

    #[allow(dead_code)]
    pub fn is_builtin(&self) -> bool {
        match self {
            Self::Integer | Self::Float => true,
            Self::Ord(ty) => ty.is_builtin()
        }
    }
}

// impl core::fmt::Display for ExprType {
//     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
//         f.write_str(match self {
//             Self::Integer => "{{integer}}",
//             Self::Float => "{{float}}",
//             Self::Ord(ty) => return f.write_str(&ty.c_type())
//         })
//     }
// }

pub struct Expr {
    pub value: String,
    pub ty: ExprType
}

impl Expr {
    pub const fn new(value: String, ty: ExprType) -> Self {
        Self {
            value,
            ty
        }
    }

    pub const fn integer(value: String) -> Self {
        Self {
            value,
            ty: ExprType::Integer
        }
    }

    pub const fn ord(value: String, ty: Type) -> Self {
        Self {
            value,
            ty: ExprType::Ord(ty)
        }
    }
}
