mod builtin;

use crate::*;

bitflags::bitflags! {
    pub struct FnFlags: u8 {
        ///
        /// Is parser inside this function(at the moment) or not
        ///
        const IS_INSIDE = 1 << 0;

        ///
        /// Is function safe or not
        ///
        const SAFE = 1 << 2;

        ///
        /// Is function extern or not
        ///
        const EXTERN = 1 << 3;

        ///
        /// Is function variadic or not
        ///
        const VARIADIC = 1 << 4;
    }
}

bitflags::bitflags! {
    pub struct VariableFlags: u8 {
        ///
        /// Is variable mutable or not
        ///
        const MUTABLE = 1 << 0;

        ///
        /// Is variable chosen as representative of `...`
        ///
        /// Applyable only to `va_list` variables
        ///
        const CHOSEN_AS_VA_LIST = 1 << 1;

        ///
        /// Is variable, chosen as representative of `...`, finished using `va_end`
        ///
        /// Applyable only to `va_list` variables
        ///
        const FINISHED_VA_LIST = 1 << 2;
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub ret: Type,
    pub vars: Vec <Variable>,
    pub args: usize,
    pub flags: FnFlags,
    pub local_types: Vec <usize>
}

impl Function {
    #[inline]
    pub fn functions() -> &'static mut Vec <Function> {
        static mut FUNCTIONS: Vec <Function> = Vec::new();
        unsafe { &mut FUNCTIONS }
    }

    pub fn current() -> Option <&'static mut Function> {
        Self::functions().iter_mut().rev().find(|fun| fun.flags.contains(FnFlags::IS_INSIDE))
    }

    pub fn leave_current() {
        let current = Self::functions().len() - 1 - Self::functions().iter_mut().rev().enumerate().find(|(_, fun)| fun.flags.contains(FnFlags::IS_INSIDE)).unwrap().0;
        Self::functions().drain(current + 1..);
        let current = &mut Self::functions()[current];
        for ty in core::mem::replace(&mut current.local_types, vec![]) {
            Type::types().remove(ty);
        }
        current.flags.remove(FnFlags::IS_INSIDE)
    }

    pub fn get <F: Fn(&&mut Function) -> bool> (predicate: F) -> Option <&'static mut Function> {
        Self::functions().iter_mut().find(predicate)
    }

    pub fn add(new: Self) {
        Self::functions().push(new);
    }

    pub fn make_unsafe(&mut self, value: &mut String) {
        if self.name == "main" {
            *value = format!("unsafe {{ {} }}", value)
        } else {
            self.flags.remove(FnFlags::SAFE)
        }
    }

    pub fn request_make_variable_mutable(name: &str) -> Option <bool> {
        Self::current().expect("not in function").vars.iter_mut().find(|var| var.name == name).map(|var| if !var.ty.mutable {
            false
        } else {
            var.flags.insert(VariableFlags::MUTABLE);
            true
        })
    }

    pub fn assume_same_signature(&self, ret: &Type, args: &[Variable]) {
        assert!(self.ret == *ret && self.args == args.len(), "different signature");
        let mut i = 0;
        while i < self.args {
            assert_eq!(self.vars[i], args[i], "different signatures");
            i += 1
        }
    }
}

#[derive(Debug, Eq)]
pub struct Variable {
    pub name: String,
    pub ty: Type,
    pub flags: VariableFlags
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ty == other.ty
    }
}

impl Variable {
    pub fn as_struct(&self) -> String {
        format!("\t{}: {},\n", self.name, self.ty.rusty())
    }
}
