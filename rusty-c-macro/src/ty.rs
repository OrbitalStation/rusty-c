use std::borrow::Cow;
use core::mem::size_of;
use bit_vec::BitVec;
use crate::*;
use check_keyword::CheckKeyword;

bitflags::bitflags! {
    pub struct TypeFlags: u8 {
        ///
        /// Is const?
        ///
        /// int --> mutable
        ///
        /// const int --> non-mutable
        ///
        const MUTABLE      = 1 << 0;

        ///
        /// Needs `struct` before?
        ///
        /// struct A {} --> needs `struct`(e.g. `struct A var`)
        ///
        /// typedef struct A B --> doesn't need `struct`(e.g. `B var`)
        ///
        const NEEDS_STRUCT = 1 << 1;
    }
}

#[derive(Debug)]
pub enum TypeData {
    Ord {
        cname: String,
        rustname: String,
        size: usize,
        flags: TypeFlags
    },
    Fun {
        args: Vec <Type>,
        ret: Type
    },
    Alias {
        name: String,
        ty: Type
    }
}

impl TypeData {
    pub fn is_mutable(&self) -> bool {
        match self {
            Self::Ord { flags, .. } => flags.contains(TypeFlags::MUTABLE),
            Self::Fun { .. } => false,
            Self::Alias { ty, .. } => ty.mutable
        }
    }

    pub fn needs_struct(&self) -> bool {
        match self {
            Self::Ord { flags, .. } => flags.contains(TypeFlags::NEEDS_STRUCT),
            Self::Fun { .. } => false,
            Self::Alias { .. } => false
        }
    }

    pub fn rusty(&self) -> Cow <String> {
        match self {
            Self::Ord { rustname, .. } => Cow::Borrowed(rustname),
            Self::Fun { args, ret } => Cow::Owned(format!("fn({}){}", {
                let mut s = String::new();
                for arg in args {
                    s.push_str(&arg.rusty());
                    s.push_str(", ")
                }
                if !args.is_empty() {
                    s.pop();
                    s.pop();
                }
                s
            }, if *ret == Type::void() { String::new() } else { format!(" -> {}", ret.rusty()) })),
            Self::Alias { ty, name } => if name.is_keyword() {
                Cow::Owned(ty.rusty())
            } else {
                Cow::Borrowed(name)
            }
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Ord { size, .. } => *size,
            Self::Fun { .. } => core::mem::size_of::<usize>(),
            Self::Alias { ty, .. } => ty.size()
        }
    }

    pub fn cname(&self) -> String {
        match self {
            Self::Ord { cname, .. } => cname.clone(),
            Self::Fun { .. } => String::new(),
            Self::Alias { name, ..} => name.clone()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub data: &'static TypeData,
    pub ptr: BitVec <u8>,
    pub mutable: bool
}

impl Eq for Type {}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.data as *const _ as usize == other.data as *const _ as usize && self.ptr == other.ptr
    }
}

impl Type {
    #[inline]
    pub fn types() -> &'static mut Vec <TypeData> {
        static mut TYPES: Vec <TypeData> = Vec::new();
        unsafe { &mut TYPES }
    }

    pub fn size(&self) -> usize {
        if !self.ptr.is_empty() {
            core::mem::size_of::<usize>()
        } else {
            self.data.size()
        }
    }

    pub fn assume_nonexisting(outer_name: &str, needs_struct: bool) {
        for ty in Self::types() {
            match ty {
                TypeData::Ord { cname, flags, .. } => {
                    if outer_name == cname && (if needs_struct {
                        flags.contains(TypeFlags::NEEDS_STRUCT)
                    } else {
                        !flags.contains(TypeFlags::NEEDS_STRUCT)
                    }) {
                        panic!("type {} already exists", outer_name)
                    }
                },
                TypeData::Alias { name, .. } if !needs_struct && outer_name == name => panic!("type {} already exists", outer_name),
                _ => ()
            }
        }
    }

    pub fn add_ord <T1: ToString, T2: ToString> (cname: T1, rustname: T2, size: usize, flags: TypeFlags) {
        #[inline]
        fn add(cname: String, rustname: String, size: usize, flags: TypeFlags) {
            Type::types().push(TypeData::Ord { cname, rustname, size, flags })
        }

        add(cname.to_string(), rustname.to_string(), size, flags)
    }

    pub fn add_fun_ptr_from_existing(fun: &Function) -> &'static TypeData {
        let c_args = fun.vars[..fun.args].iter().map(|arg| arg.ty.clone()).collect();

        match Self::types().iter().find(|x| match x {
            TypeData::Fun { args, ret } if *args == c_args && *ret == fun.ret => true,
            _ => false
        }) {
            None => {
                Self::types().push(TypeData::Fun {
                    args: c_args,
                    ret: fun.ret.clone()
                });

                Self::types().last().unwrap()
            },
            Some(data) => data
        }
    }

    pub fn find <F: FnMut(&&TypeData) -> bool> (predicate: F) -> Result <Self, &'static str> {
        match Self::types().iter().find(predicate) {
            Some(data) => Ok(Self {
                data,
                ptr: Default::default(),
                mutable: data.is_mutable()
            }),
            None => Err("type")
        }
    }

    pub fn rusty(&self) -> String {
        let mut result = String::new();
        for ptr in &self.ptr {
            result.push('*');
            result.push_str(const_or_mut_depends_on_mutability(ptr));
            result.push(' ');
        }
        result.push_str(&self.data.rusty());
        result
    }

    // pub fn c_type(&self) -> String {
    //     let mut result = const_and_space_or_nothing_depends_on_mutability(self.mutable).to_string() + &self.data.cname;
    //     for ptr in &self.ptr {
    //         result.push('*');
    //         result.push_str(const_and_space_or_nothing_depends_on_mutability(ptr));
    //         result.push(' ');
    //     }
    //     if !self.ptr.is_empty() {
    //         result.pop();
    //     }
    //     result
    // }

    fn is_template(&self, fun: impl Fn(&Self) -> bool) -> bool {
        match self.data {
            TypeData::Ord { .. } => fun(self),
            TypeData::Fun { .. } => false,
            TypeData::Alias { ty, .. } => fun(ty)
        }
    }

    fn is_with_range_template(&self, start: usize, finish: usize) -> bool {
        self.is_template(move |ty| {
            let me = ty.data as *const _ as usize;
            let min = (&Type::types()[start]) as *const _ as usize;
            let max = (&Type::types()[finish]) as *const _ as usize;
            me >= min && me <= max
        })
    }

    pub fn is_pointer(&self) -> bool {
        !self.ptr.is_empty()
    }

    pub fn is_integer(&self) -> bool {
        self.is_with_range_template(/* i8 */ 2, /* u64 */ 9)
    }

    pub fn is_float(&self) -> bool {
       self.is_with_range_template(/* f32 */ 10, /* f64 */ 11)
    }

    pub fn is_signed(&self) -> bool {
        self.is_template(|ty| *ty == Self::schar() || *ty == Self::short() || *ty == Self::int() || *ty == Self::long())
    }

    pub fn is_unsigned(&self) -> bool {
        self.is_template(|ty| *ty == Self::char() || *ty == Self::ushort() || *ty == Self::uint() || *ty == Self::ulong())
    }

    pub fn is_arithmetic(&self) -> bool {
        self.is_float() || self.is_integer()
    }

    pub fn convert(a: &mut Expr, ty: &Self) {
        match &a.ty {
            ExprType::Integer if ty.is_integer() => (),
            ExprType::Float if ty.is_float() => (),
            ExprType::Ord(ty2) if ty == ty2 => return,
            _ => a.value = format!("{} as {}", parentify(a.value.clone()), ty.rusty())
        }
        a.ty = ExprType::Ord(ty.clone())
    }

    pub fn is_dominant_of(&self, other: &Self) -> bool {
        if self == other {
            if self.mutable != other.mutable {
                !self.mutable
            } else {
                false
            }
        } else if self.is_float() && !other.is_float() {
            true
        } else if self.is_unsigned() && other.is_signed() {
            self.size() >= other.size()
        } else {
            self.size() > other.size()
        }
    }

    #[cfg(target_pointer_width = "32")]
    pub fn usize() -> Self {
        Self::uint()
    }

    #[cfg(target_pointer_width = "64")]
    pub fn usize() -> Self {
        Self::ulong()
    }
}

macro_rules! add_builtins {
    ($( $ty:ty = $name:ident($idx:literal) ),* $( , )?) => {
        impl Type {
            pub fn add_builtins() {
                $( Self::add_ord(String::new(), stringify!($ty), size_of::<$ty>(), TypeFlags::MUTABLE); )*
            }

            pub fn is_builtin(&self) -> bool {
                let me = self.data as *const _ as usize;
                let max = (&Self::types()[add_builtins!(@last $( $idx )*)]) as *const _ as usize;
                me <= max
            }

            $(
                pub fn $name() -> Self {
                    Self {
                        data: &Self::types()[$idx],
                        ptr: Default::default(),
                        mutable: true
                    }
                }
            )*
        }
    };

    (@last $skip:literal $( $other:literal )+) => {
        add_builtins!(@last $( $other )+)
    };

    (@last $last:literal) => {
        $last
    }
}

add_builtins! {
    ()   = void(0),
    bool = bool(1),
    i8   = schar(2),
    u8   = char(3),
    i16  = short(4),
    u16  = ushort(5),
    i32  = int(6),
    u32  = uint(7),
    i64  = long(8),
    u64  = ulong(9),
    f32  = float(10),
    f64  = double(11)
}
