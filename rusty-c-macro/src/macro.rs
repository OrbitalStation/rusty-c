use crate::*;
use check_keyword::CheckKeyword;

pub use clang::clang;

peg::parser! { grammar clang() for str {

    /// Parse whitespace
    rule _ = [' ' | '\t' | '\n']*

    /// Like above, but required at least one
    rule __ = [' ' | '\t' | '\n']+

    /// Parse 1 digit with the specified radix
    rule digit(radix: u32) -> &'input str = digit:$(['0'..='9' | 'a'..='z' | 'A'..='Z']) {?
        match digit.chars().next().unwrap().to_digit(radix) {
            Some(_) => Ok(digit),
            None => Err("digit")
        }
    }

    /// Parse 1 letter
    rule letter() -> &'input str = x:$(['a'..='z' | 'A'..='Z' | '_'])

    /// Parse an identifier(and add `r#` if is a keyword)
    rule ident() -> String = x:$(letter() (letter() / digit(10))*) {
        x.to_safe()
    }

    /// Parse 1 reserved C keyword
    rule keyword() -> &'input str
        = x:$("auto" / "break" / "case" / "char" / "const" / "continue" / "default" / "do" / "double" / "else" / "enum" / "extern" / "float" / "for" / "goto" / "if" / "inline" / "int" / "long" / "register" / "restrict" / "return" / "short" / "signed" / "sizeof" / "static" / "struct" / "switch" / "typedef" / "union" / "unsigned" / "void" / "volatile" / "while" / "_Alignas" / "_Alignof" / "_Atomic" / "_Bool" / "_Complex" / "_Generic" / "_Imaginary" / "_Noreturn" / "_Static_assert" / "_Thread_local")

    /// Parse 1 punctuation unit
    rule punct() -> &'input str
        = x:$(['+' | '-' | '*' | '/' | '%' | '&' | '|' | '=' | '<' | '>' | '!' | '^' | '~'])

    /// Parse integer number in possible notations
    rule int_number() -> String
        = digits:$("0" ("x" / "X") (digit(16) / "'")+) { digits.replace('\'', "_") }
        / digits:$("0" (digit(8) / "'")+) { digits.replace('\'', "_") }
        / digits:$((digit(10) / "'")+) { digits.replace('\'', "_") }
        / digits:$("0b" (digit(2) / "'")+) { digits.replace('\'', "_") }

    /// Helper for `__single_comment` rule
    rule __single_comment_helper() -> String
        = any:$([^ '*']) { any.to_string() }
        / "*" !(&"/") { String::from("*") }

    /// Helper for `comments` rule
    rule __single_comment() -> String
        = "//" comments:$([^ '\n']*) { comments.to_string() }
        / "/*" comments:__single_comment_helper()* "*/" {
            comments.join("")
        }

    /// Parse comments(both // and /**/)
    rule comments() -> String = comments:__single_comment() ++ _ {
        comments.join("\n")
    }

    /// Parse one `raw` char(without `'` around it),
    /// including escape-sequences
    rule __raw_char(ignore_double_quotes: bool) -> String
        = r"\a" { r"\x07".to_string() }
        / r"\b" { r"\x08".to_string() }
        / r"\e" { r"\x1b".to_string() }
        / r"\f" { r"\x0c".to_string() }
        / r"\n" { r"\n".to_string() }
        / r"\r" { r"\r".to_string() }
        / r"\t" { r"\t".to_string() }
        / r"\v" { r"\x0b".to_string() }
        / r"\\" { r"\\".to_string() }
        / r"\'" { r"\'".to_string() }
        / "\\\"" { "\\\"".to_string() }
        / r"\?" { r"\x3f".to_string() }
        / "\\" digits:$(digit(8) * <1, 3>) { digits2char_maybe_non_valid_unicode(digits, 8, "octal").to_string() }
        / "\\x" digits:$(digit(16)+) {
            assert_eq!(digits.len() & 1, 0, "number of hex digits is odd");
            digits2char_maybe_non_valid_unicode(digits, 16, "hex").to_string()
        }
        / "\\" escape:$([_]) { panic!("wrong escape-sequence: \\{}", escape) }
        / char:$([_]) {?
            if ignore_double_quotes && char == "\"" {
                Err("char")
            } else {
                Ok(char.to_string())
            }
        }

    /// Parse char
    rule lit_char() -> String = "'" char:__raw_char(false) "'" {
        char
    }

    /// Parse string
    rule lit_string() -> String = "\"" chars:__raw_char(true)* "\"" {
        chars.join("")
    }

    /// Expect whitespace after idents
    rule force_whitespace_if_ident() = (_ &punct() / __)

    /// Parse 1 pointer entity
    rule __pure_and_complex_pointer() -> bool
        = _ "*" _ "const" force_whitespace_if_ident() { false }
        / _ "*" { true }

    /// Parse pure(i.e. without qualifiers) type
    rule __pure_type() -> Type
        = "void" { Type::void() }
        / "_Bool" { Type::bool() }
        / "signed" _ "char" { Type::schar() }
        / ("unsigned" _ "char" / "char") { Type::char() }
        / ("signed" _ "short" _ "int" / "signed" _ "short" / "short" _ "int" / "short") { Type::short() }
        / ("unsigned" _ "short" _ "int" / "unsigned" _ "short") { Type::ushort() }
        / ("signed" _ "long" _ "long" _ "int" / "long" _ "long" _ "int" / "signed" _ "long" _ "long" / "signed" _ "long" _ "int" / "signed" _ "long" / "long" _ "int" / "long" _ "long" / "long") { Type::long() }
        / ("unsigned" _ "long" _ "long" _ "int" / "unsigned" _ "long" _ "int" / "unsigned" _ "long" _ "long" / "unsigned" _ "long") { Type::ulong() }
        / "float" { Type::float() }
        / "double" { Type::double() }
        / ("signed" _ "int" / "int" / "signed") { Type::int() }
        / ("unsigned" _ "int" / "unsigned") { Type::uint() }
        / structural:("struct" __)? ty:ident() {?
            let about_struct = if structural.is_some() { |x: bool| x } else { |x: bool| !x };
            Type::find(|data| data.cname() == ty && about_struct(data.needs_struct()))
        }

    /// Parse complete type
    rule ty() -> Type = constness:("const" __)? ty:__pure_type() ptr:__pure_and_complex_pointer()* {
        let mut ty = ty;
        let mut ptr = ptr;
        let mut mutable = constness.is_none();

        if !ptr.is_empty() {
            let last = ptr.pop().unwrap();
            ptr.insert(0, mutable);
            mutable = last;

            ty.ptr.extend(ptr.into_iter())
        }

        if !mutable {
            ty.mutable = false
        }

        ty
    }

    /// Parse an identifier, identifier must be non-type
    rule only_ident() -> String
        = (keyword() / ty()) {? Err("ident") }
        / ident:ident() { ident }

    /// Parse variable of form `<ty> <name`
    rule variable(reset_mutability: bool) -> Variable = ty:ty() _ name:only_ident() {
        Variable {
            name,
            mutable: !reset_mutability && ty.mutable,
            ty
        }
    }

    /// Parse single identifier as `Expr`
    rule __expr_ident() -> Expr = ident:only_ident() {?
        match Function::current() {
            None => (),
            Some(cur) => {
                match cur.vars.iter().find_map(|var| if var.name == ident {
                    Some(var.ty.clone())
                } else {
                    None
                }) {
                    Some(ty) => return Ok(Expr::ord(ident, ty)),
                    None => ()
                }
            }
        }

        match Function::functions().iter().find(|fun| fun.name == ident) {
            Some(fun) => return Ok(Expr::ord(ident, Type {
                data: Type::add_fun_ptr_from_existing(fun),
                ptr: Default::default(),
                mutable: false
            })),
            None => ()
        }

        // TODO: Add `statics` check here!

       Err("unknown variable")
    }

    /// Parse an expression
    rule expr() -> Expr = precedence! {
        variable:@ _ "=" _ value:(@) {
            let mut value = value;
            let mut variable = variable;
            let variable_ty = match variable.ty {
                ExprType::Integer | ExprType::Float => panic!("cannot assign to literal"),
                ExprType::Ord(ref mut ty) => ty
            };
            Type::convert(&mut value, variable_ty);
            if variable.value.find(|c: char| !c.is_alphanumeric()).is_none() {
                assert!(request_variable_to_be_mutable(&variable.value), "`{}` is not mutable", variable.value);
                variable_ty.mutable = true
            }
            assert!(variable_ty.mutable, "cannot assign to an immutable value");
            variable.value = format!("rusty_c::ops::assign(&mut {}, {})", variable.value, value.value);
            variable
        }

        --

        cond:@ _ "?" _ s1:expr() _ ":" _ s2:(@) {
            let mut cond = cond;
            let mut s1 = s1;
            let mut s2 = s2;

            Type::convert(&mut cond, &Type::bool());
            convert_to_strongest_type(&mut s1, &mut s2);

            Expr::new(format!("if {} {{\n\t{}\n}} else {{\n\t{}\n}}", cond.value, s1.value, s2.value), s1.ty)
        }

        --

        x:(@) _ symbol:$("==" / "!=") _ y:@ {
            arithmetic_expr(x, y, symbol, Some(Type::bool()))
        }

        --

        x:(@) _ symbol:$(">" / "<" / ">=" / "<=") _ y:@ {
            arithmetic_expr(x, y, symbol, Some(Type::bool()))
        }

        --

        x:(@) _ symbol:$("+" / "-") _ y:@ {
            arithmetic_expr(x, y, symbol, None)
        }

        --

        x:(@) _ symbol:$("*" / "/") _ y:@ {
            arithmetic_expr(x, y, symbol, None)
        }

        x:(@) _ "%" _ y:@ {
            let result = expr_binop(x, y, "%", None);
            assert!(result.ty.to_type().is_integer(), "% is applyable only to integers");
            result
        }

        --

        "sizeof" force_whitespace_if_ident() ty:ty() {
            Expr::new(format!("rusty_c::ops::sizeof::<{}>()", ty.rusty()), ExprType::Ord(Type::usize()))
        }

        "&" _ x:(@) {
            let fun = Function::current().expect("cannot use * outside of function");
            let mut x = x;

            let ty = match x.ty {
                ExprType::Ord(ref mut ty) => ty,
                _ => panic!("cannot take reference of a literal")
            };

            ty.mutable = request_variable_to_be_mutable(&x.value);

            x.value = format!("&{} as *const {}{}", x.value, ty.rusty(), if ty.mutable {
                format!(" as *mut {}", ty.rusty())
            } else {
                String::new()
            });

            ty.ptr.push(ty.mutable);

            x
        }

        "*" _ x:(@) {
            let fun = Function::current().expect("cannot use * outside of function");
            let mut x = x;

            let ty = match x.ty {
                ExprType::Ord(ref mut ty) if ty.is_pointer() => ty,
                _ => panic!("cannot dereference non-ptr")
            };

            ty.mutable = ty.ptr.pop().unwrap();

            x.value.insert(0, '*');

            fun.make_unsafe(&mut x.value);

            x
        }

        --

        call:(@) _ "(" _ args:expr() ** ("," _) ")" {
            let mut args = args;

            let (c_args, c_ret, spec_extern, name) = match call.ty {
                ExprType::Ord(ty) => match ty.data {
                    TypeData::Fun { args, ret } => {
                        let mut result = (args, ret, false, "");
                        if call.value.find(|c: char| !c.is_alphanumeric()).is_none() {
                            match Function::get(|fun| fun.name == call.value) {
                                Some(fun) => {
                                    result.2 = fun.flags.contains(FnFlags::EXTERN);
                                    result.3 = fun.name.as_str()
                                },
                                None => (),
                            }
                        }
                        result
                    },
                    _ => panic!("cannot call non-function")
                },
                _ => panic!("cannot call non-function")
            };

            assert!(args.len() == c_args.len(), "wrong number of args: expected {}, found {}", c_args.len(), args.len());

            let result = format!("{}{}({}){}",
                if spec_extern {
                    let mut t = String::new();
                    Function::current().expect("cannot call function not in function").make_unsafe(&mut t);
                    if t.is_empty() {
                        "~~1"
                    } else {
                        "~~2"
                    }
                } else {
                    ""
                },
                call.value,
                {
                    let mut s = String::new();
                    let mut i = 0;
                    while i < args.len() {
                        Type::convert(&mut args[i], &c_args[i]);
                        s.push_str(&format!("{}, ", args[i].value));
                        i += 1
                    }
                    if !s.is_empty() {
                        s.pop();
                        s.pop();
                    }
                    s
                },
                if spec_extern {
                    "~"
                } else {
                    ""
                }
            );

            Expr::new(result, ExprType::Ord(c_ret.clone()))
        }

        --

        number:int_number() {
            Expr::integer(number)
        }

        ident:__expr_ident() {
            ident
        }

        string:lit_string() {
            let mut ptr = Type::char();
            ptr.ptr.push(false);
            Expr::new(plain_text_to_lit_string(&string), ExprType::Ord(ptr))
        }

        "(" _ expr:expr() _ ")" {
            let mut expr = expr;
            expr.value = format!("({})", expr.value);
            expr
        }
    }

    /// Helper `rule` to add function <i>before</i> body
    rule __add_fn_and_skip_whitespaces(ret: Type, name: String, args: Vec <Variable>) = _ {
        match Function::functions().iter().enumerate().find(|(_, fun)| fun.name == name) {
            None => (),
            Some((idx, fun)) => if fun.flags.contains(FnFlags::EXTERN) {
                fun.assume_same_signature(&ret, &args);
                Function::functions().remove(idx);
            } else {
                panic!("function {} already exists", name)
            }
        }
        Function::add(Function {
            name,
            ret,
            args: args.len(),
            vars: args,
            flags: FnFlags::IS_INSIDE | FnFlags::SAFE,
            local_types: Vec::new()
        })
    }

    /// `return` helper #1
    rule __return_helper() -> Expr = __ e:expr() { e }

    /// `return` helper #2
    rule __return_helper2() -> () = &"}"

    /// Parse statement
    rule stmt() -> String = precedence! {
        // `return` statement
        "return" ret:__return_helper()? _ ";" _ is_last:__return_helper2()? {
            let cur = Function::current().expect("return outside of function");
            let is_not_last = is_last.is_none();
            let result = match ret {
                None => {
                    assert_eq!(cur.ret, Type::void(), "expected value");
                    if is_not_last {
                        String::from("return;")
                    } else {
                        String::new()
                    }
                },
                Some(mut e) => {
                    assert_ne!(cur.ret, Type::void(), "unexpected value");
                    Type::convert(&mut e, &cur.ret);
                    if is_not_last {
                        format!("return {};", e.value)
                    } else {
                        e.value
                    }
                }
            };
            result + "\n"
        }

        // `typedef` statement
        "typedef" __ ty:ty() _ name:only_ident() _ ";" {
            Type::assume_nonexisting(&name, false);
            let result = if name.is_keyword() {
                String::new()
            } else {
                format!("type {} = {};", name, ty.rusty())
            };
            match Function::current() {
                None => (),
                Some(cur) => cur.local_types.push(Type::types().len())
            }
            Type::types().push(TypeData::Alias {
                ty,
                name
            });
            result
        }

        // Function declaration
        ("extern" __)? ret:ty() _ name:only_ident() _ "(" _ args:variable(true) ** ("," _) ")" _ ";" {
            match Function::get(|fun| fun.name == name) {
                Some(fun) => {
                    fun.assume_same_signature(&ret, &args);
                    String::new()
                },
                None => {
                    let x = format!("~~~{}\n", name);
                    Function::add(Function {
                        name,
                        ret,
                        args: args.len(),
                        vars: args,
                        flags: FnFlags::EXTERN,
                        local_types: Vec::new()
                    });
                    x
                }
            }
        }

        // Function definition
        comments:comments()? _ ret:ty() _ name:only_ident() _ "(" _ args:variable(true) ** ("," _) ")" __add_fn_and_skip_whitespaces(ret, name, args) "{" _ body:raw_clang("\n\t")? _ "}" {
            let me = Function::current().unwrap();
            let result = format!(
                "{comments}{unsafety}fn {name}({args}){ret} {{\n{body}\n}}\n",

                unsafety = if me.flags.contains(FnFlags::SAFE) {
                    ""
                } else {
                    "unsafe "
                },

                comments = match comments {
                    None => String::new(),
                    Some(x) => format!("///{}\n", x.replace("\n", "\n///"))
                },

                name = me.name,

                args = function_args(me),

                ret = function_return_type(me),

                body = body.unwrap_or_else(|| if me.ret == Type::void() {
                    String::new()
                } else {
                    panic!("function with non-void return type cannot have empty body")
                })
            );

            Function::leave_current();

            result
        }

        // Comments
        comments:comments() {
            format!("//{}", comments.replace("\n", "\n//"))
        }

        // Ordinary `expression` statement
        expr:expr() _ ";" {
            expr.value + ";"
        }
    }

    /// Parse whole code
    rule raw_clang(separator: &str) -> String = _ stmts:stmt() ** _ _ {
        let mut s = String::new();
        for stmt in stmts {
            s.push_str(separator);
            s.push_str(&stmt.replace("\n", separator))
        }
        s
    }

    /// Entry point
    pub rule clang() -> String = code:raw_clang("\n")
} }

fn arithmetic_expr(x: Expr, y: Expr, symbol: &str, ret: Option <Type>) -> Expr {
    assert!(x.ty.to_type().is_arithmetic(), "{} applyable only to arithmetic types", symbol);
    let result = expr_binop(x, y, symbol, ret);
    result
}

fn expr_binop(mut x: Expr, mut y: Expr, symbol: &str, ret: Option <Type>) -> Expr {
    convert_to_strongest_type(&mut x, &mut y);

    Expr::new(format!("{} {} {}", x.value, symbol, y.value), match ret {
        None => x.ty,
        Some(ret) => ExprType::Ord(ret)
    })
}
