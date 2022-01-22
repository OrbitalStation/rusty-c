use crate::*;

pub fn digits2char_maybe_non_valid_unicode(digits: &str, radix: u32, on_err: &str) -> char {
    unsafe {
        core::mem::transmute(u32::from_str_radix(digits, radix).expect(&format!("non-valid: {}", on_err)))
    }
}

pub fn const_or_mut_depends_on_mutability(mutable: bool) -> &'static str {
    if mutable {
        "mut"
    } else {
        "const"
    }
}

pub fn mut_and_space_or_nothing_depends_on_mutability(mutable: bool) -> &'static str {
    if mutable {
        "mut "
    } else {
        ""
    }
}

pub fn const_and_space_or_nothing_depends_on_mutability(mutable: bool) -> &'static str {
    if mutable {
        ""
    } else {
        "const "
    }
}

pub fn parentify(mut s: String) -> String {
    if s.contains(' ') && !is_surrounded_with_parents(&s) {
        s.insert(0, '(');
        s.push(')')
    }
    s
}

pub fn is_surrounded_with_parents(s: &str) -> bool {
    let mut stack = Vec::new();
    let mut is_surrounded = false;

    for (idx, c) in s.chars().enumerate() {
        is_surrounded = false;
        match c {
            '(' => stack.push(idx == 0),
            ')' => is_surrounded = stack.pop().unwrap_or_default(),
            _ => ()
        }
    }

    is_surrounded
}

pub fn convert_to_strongest_type(x: &mut Expr, y: &mut Expr) {
    if x.ty.is_dominant_of(&y.ty) {
        Type::convert(y, &x.ty.to_type())
    } else {
        Type::convert(x, &y.ty.to_type())
    }
}

pub fn plain_text_to_lit_string(text: &str) -> String {
    format!("\"{}{}\".as_ptr()", text, if text.chars().next_back().unwrap_or('1') != '\0' {
        "\0"
    } else {
        ""
    })
}

pub fn request_variable_to_be_mutable(name: &str) -> bool {
    match Function::request_make_variable_mutable(name) {
        None => if Function::functions().iter().find(|fun| fun.name == name).is_some() {
            return false
        } else {
            todo!()
        },
        Some(x) => x
    }
}

pub fn function_args(me: &Function) -> String {
    let mut s = String::new();
    for arg in &me.vars[..me.args] {
        s.push_str(&format!("{}{}: {}, ", mut_and_space_or_nothing_depends_on_mutability(arg.flags.contains(VariableFlags::MUTABLE)), arg.name, arg.ty.rusty()))
    }
    if me.flags.contains(FnFlags::VARIADIC) {
        assert!(!s.is_empty(), "variadic function requires at least one normal parameter");
        if !me.flags.contains(FnFlags::EXTERN) {
            if let Some(var) = me.vars[me.args..].iter().find(|var| var.flags.contains(VariableFlags::CHOSEN_AS_VA_LIST)) {
                s.push_str(&format!("mut {}: ", var.name))
            }
        }
        s.push_str("...");
    } else if !s.is_empty() {
        s.pop();
        s.pop();
    }
    s
}

pub fn function_return_type(me: &Function) -> String {
    if me.ret == Type::void() {
        String::new()
    } else {
        format!(" -> {}", me.ret.rusty())
    }
}

pub fn letter(x: &str) -> Result <&str, &'static str> {
    let char = x.chars().next().unwrap();
    if char.is_alphabetic() || char == '_' { Ok(x) } else { Err("") }
}

pub fn not_full_letter(char: char) -> bool {
    !(char.is_alphanumeric() || char == '_')
}
