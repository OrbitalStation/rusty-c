use crate::*;
use check_keyword::CheckKeyword;

pub use grammar::*;

peg::parser! { grammar grammar() for str {

    rule whitespace_single() = [' ' | '\t' | '\n']

    /// Parse whitespace
    rule _ = whitespace_single()*

    /// Like above, but required at least one
    rule __ = whitespace_single()+

    /// Parse 1 digit with the specified radix
    rule digit(radix: u32) -> &'input str = digit:$([_]) {?
        match digit.chars().next().unwrap().to_digit(radix) {
            Some(_) => Ok(digit),
            None => Err("digit")
        }
    }

    /// Parse 1 letter
    rule letter() -> &'input str = x:$([_]) {?
        letter(x)
    }

    /// Parse an identifier(and add `r#` if is a keyword)
    pub rule ident() -> String = x:$(letter() (letter() / digit(10))*) {
        x.to_safe()
    }

    /// Parse 1 reserved C keyword
    rule keyword() -> &'input str
        = x:$("auto" / "break" / "case" / "char" / "const" / "continue" / "default" / "do" / "double" / "else" / "enum" / "extern" / "float" / "for" / "goto" / "if" / "inline" / "int" / "long" / "register" / "restrict" / "return" / "short" / "signed" / "sizeof" / "static" / "struct" / "switch" / "typedef" / "union" / "unsigned" / "void" / "volatile" / "while" / "_Alignas" / "_Alignof" / "_Atomic" / "_Bool" / "_Complex" / "_Generic" / "_Imaginary" / "_Noreturn" / "_Static_assert" / "_Thread_local")

    /// Parse 1 punctuation unit
    rule punct() -> &'input str
        = x:$(['+' | '-' | '*' | '/' | '%' | '&' | '|' | '=' | '<' | '>' | '!' | '^' | '~'])

    /// Parse integer number in possible notations
    rule int_number() -> String = digits:$(("0" ("x" / "X") (digit(16) / "'")+) / ("0" (digit(8) / "'")+) / ((digit(10) / "'")+) / ("0b" (digit(2) / "'")+)) {
        digits.replace('\'', "_")
    }

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

    rule __pure_type_part() -> ParsedTypePart
        = "signed" { ParsedTypePart::Sign(ParsedTypePartSign::Signed) }
        / "unsigned" { ParsedTypePart::Sign(ParsedTypePartSign::Unsigned) }

        / "short" { ParsedTypePart::Qualifier(ParsedTypePartQualifier::Short) }
        / "long" { ParsedTypePart::Qualifier(ParsedTypePartQualifier::Long) }

        / "void" { ParsedTypePart::Ty(ParsedTypePartType::Void) }
        / "_Bool" { ParsedTypePart::Ty(ParsedTypePartType::Bool) }
        / "char" { ParsedTypePart::Ty(ParsedTypePartType::Char) }
        / "int" { ParsedTypePart::Ty(ParsedTypePartType::Int) }
        / "float" { ParsedTypePart::Ty(ParsedTypePartType::Float) }
        / "double" { ParsedTypePart::Ty(ParsedTypePartType::Double) }


    /// Parse pure(i.e. without qualifiers) type
    rule __pure_type() -> Type
        = parts:__pure_type_part() ++ _ {
            use {ParsedTypePart::*, ParsedTypePartType::*, ParsedTypePartQualifier::*, ParsedTypePartSign::*};

            let mut parts = parts;
            parts.sort();
            ParsedTypePart::add_defaults(&mut parts);
            match parts[..] {
                [Ty(Void)] => Type::void(),
                [Ty(Bool)] => Type::bool(),
                [Sign(Signed), Ty(Char)] => Type::schar(),
                [Sign(Unsigned), Ty(Char)] => Type::char(),
                [Sign(Signed), Qualifier(Short), Ty(Int)] => Type::short(),
                [Sign(Unsigned), Qualifier(Short), Ty(Int)] => Type::ushort(),
                [Sign(Signed), Ty(Int)] => Type::int(),
                [Sign(Unsigned), Ty(Int)] => Type::uint(),
                [Sign(Signed), Qualifier(Long), Ty(Int)] | [Sign(Signed), Qualifier(Long), Qualifier(Long), Ty(Int)] => Type::long(),
                [Sign(Unsigned), Qualifier(Long), Ty(Int)] | [Sign(Unsigned), Qualifier(Long), Qualifier(Long), Ty(Int)] => Type::ulong(),
                [Ty(Float)] => Type::float(),
                [Ty(Double)] => Type::double(),
                _ => panic!("unknown builtin type configuration: {:?}", parts)
            }
        }
        / "__builtin_va_list" { Type::va_list() }
        / "struct" __ ty:ident() {?
            Type::find(|data| data.cname() == ty && data.needs_struct()).map_err(|_| "type")
        }
        / ty:ident() {?
            Type::find(|data| data.cname() == ty && !data.needs_struct()).map_err(|_| "type")
        }

    /// Parse complete type
    pub rule ty() -> Type = constness:("const" __)? ty:__pure_type() ptr:__pure_and_complex_pointer()* {
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

    /// Parse variable of form `<ty> <name>`
    rule variable(reset_mutability: bool) -> Variable = ty:ty() _ name:only_ident() {
        let mut flags = VariableFlags::empty();
        if !reset_mutability && ty.mutable {
            flags.insert(VariableFlags::MUTABLE)
        }

        Variable {
            name,
            flags,
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

    /// Check builtin function
    rule __builtin_function() -> Expr = name:ident() _ "(" _ args:$([^ ',' | ')']*) ** ("," _) ")" {?
        Function::check_builtin(&name, &args).ok_or("builtin")
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
            if variable.value.find(not_full_letter).is_none() {
                assert!(request_variable_to_be_mutable(&variable.value), "`{}` is not mutable", variable.value);
                variable_ty.mutable = true
            }
            assert!(variable_ty.mutable, "cannot assign to an immutable value");
            variable.value = format!("rusty_c::assign!({}, {})", variable.value, value.value);
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

            x.value = format!("*{}", parentify(x.value));

            fun.make_unsafe(&mut x.value);

            x
        }

        "!" _ x:(@) {
            let mut x = x;
            Type::convert(&mut x, &Type::bool());
            x.value = format!("!{}", parentify(x.value));
            x
        }

        "(" _ ty:ty() _ ")" _ expr:expr() {
            let mut expr = expr;
            Type::convert(&mut expr, &ty);
            expr
        }

        --

        // name:(@) _ "." _ field:only_ident() {
        //     fn do_job(name: &str, ty: &Type) -> String {
        //         match ty.data {
        //             TypeData::Ord { flags, .. } if flags.contains(TypeFlags::STRUCT) => {
        //                 Expr::new()
        //             },
        //             TypeData::Alias { ty, .. } => do_job(name, ty),
        //             _ => panic!("cannot access field of non-struct")
        //         }
        //     }
        //
        //     match name.ty {
        //         ExprType::Ord(ref ty) => do_job(&name.value, &ty),
        //         _ => panic!("cannot access field of non-struct")
        //     }
        // }

        call:(@) _ "(" _ args:expr() ** ("," _) ")" {
            let cur = Function::current().expect("cannot call function not in function");

            let mut args = args;

            fn extract <'a> (call: &Expr, ty: &'a Type) -> (&'a Vec <Type>, &'a Type, bool, &'static str, bool) {
                match ty.data {
                    TypeData::Fun { args, ret, flags } => {
                        let mut result = (args, ret, false, "", flags.contains(FnFlags::VARIADIC));
                        if call.value.find(not_full_letter).is_none() {
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
                    TypeData::Alias { ty, .. } => extract(call, ty),
                    _ => panic!("cannot call non-function")
                }
            }

            let (c_args, c_ret, spec_extern, name, variadic) = match call.ty {
                ExprType::Ord(ref ty) => extract(&call, &ty),
                _ => panic!("cannot call non-function")
            };

            if variadic {
                assert!(args.len() >= c_args.len(), "not enough arguments: expected {}+, found {}", c_args.len(), args.len())
            } else {
                assert_eq!(args.len(), c_args.len(), "wrong number of args: expected {}, found {}", c_args.len(), args.len())
            }

            let result = format!("{}{}({}){}",
                if spec_extern {
                    let mut t = String::new();
                    cur.make_unsafe(&mut t);
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
                    while i < c_args.len() {
                        Type::convert(&mut args[i], &c_args[i]);
                        s.push_str(&format!("{}, ", args[i].value));
                        i += 1
                    }
                    while i < args.len() {
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

        builtin:__builtin_function() {
            builtin
        }

        --

        number:int_number() {
            Expr::integer(number)
        }

        "__func__" {
            let mut ptr = Type::char();
            ptr.ptr.push(false);
            Expr::new(plain_text_to_lit_string(Function::current().map(|fun| &fun.name).unwrap_or(&String::new())), ExprType::Ord(ptr))
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
    rule __add_fn_and_skip_whitespaces(ret: Type, name: String, args: Vec <Variable>, variadic: Option <()>) = _ {
        match Function::functions().iter().enumerate().find(|(_, fun)| fun.name == name) {
            None => (),
            Some((idx, fun)) => if fun.flags.contains(FnFlags::EXTERN) {
                fun.assume_same_signature(&ret, &args);
                Function::functions().remove(idx);
            } else {
                panic!("function {} already exists", name)
            }
        }

        let mut flags = FnFlags::IS_INSIDE | FnFlags::SAFE;
        if variadic.is_some() {
            flags.insert(FnFlags::VARIADIC);
            flags.remove(FnFlags::SAFE)
        }

        Global::get().allow_return_sugar = true;

        let mut ret = ret;
        if name == "main" {
            ret = Type::void()
        }

        Function::add(Function {
            name,
            ret,
            args: args.len(),
            vars: args,
            flags,
            local_types: Vec::new()
        })
    }

    /// `return` helper #1
    rule __return_helper() -> Expr = __ e:expr() { e }

    /// `return` helper #2
    rule __return_helper2() -> () = &"}"

    /// Helper for init-in-place variable statement
    rule __variable_init() -> Expr = _ "=" _ expr:expr() {
        expr
    }

    /// Helper for remembering `Global::allow_return_sugar` and resetting it
    rule __remember_return_sugar_and_reset() -> bool = _ {
        let sugar = Global::get().allow_return_sugar;
        Global::get().allow_return_sugar = false;
        sugar
    }

    rule __stmt_body() -> String
        = "{" _ body:raw_clang("\n\t")? _ "}" { body.unwrap_or_default() }
        / body:stmt() { "\t".to_string() + &body }

    rule __struct_helper() -> Variable = v:variable(true) _ ";" { v }

    rule __struct() -> (String, Vec <Variable>) = "struct" __ name:only_ident() _ "{" _ fields:__struct_helper() ** _ _ "}" {
        (name, fields)
    }

    rule __struct_stmt() -> (String, Vec <Variable>, Option <String>)
        = s:__struct() _ ";" { (s.0, s.1, None) }
        / "typedef" __ "struct" __ name:only_ident()? _ "{" _ fields:__struct_helper() ** _ _ "}" _ alias:only_ident() _ ";" {
            (alias, fields, Some(name.unwrap_or(String::new())))
        }

    rule __else() -> String
        = _ "else" _ body:__stmt_body() { body }

    /// Parse statement
    rule stmt() -> String = precedence! {
        // `struct` statement
        s:__struct_stmt() {
            let (name, fields, alias) = s;
            Type::assume_nonexisting(&name, TypeFlags::NEEDS_STRUCT, TypeFlags::empty());
            let mut s = String::new();
            for v in fields {
                s.push_str(&v.as_struct())
            }
            let result = format!("#[derive(Copy, Clone)]\n#[repr(C)]\nstruct {name} {{\n{s}}}");
            let mut flags = TypeFlags::MUTABLE | TypeFlags::STRUCT;
            if let Some(ref alias) = alias {
                if !alias.is_empty() {
                    flags.insert(TypeFlags::NEEDS_STRUCT)
                }
            }
            Type::types().push(TypeData::Ord {
                cname: name.clone(),
                rustname: name,
                size: 0,
                flags
            });
            if let Some(alias) = alias {
                if !alias.is_empty() {
                    Type::assume_nonexisting(&alias, TypeFlags::empty(), TypeFlags::NEEDS_STRUCT);
                    Type::types().push(TypeData::Alias {
                        name: alias,
                        ty: Type {
                            data: &Type::types().last().unwrap(),
                            ptr: Default::default(),
                            mutable: true
                        }
                    })
                }
            }
            result
        }

        // `if` statement
        "if" _ "(" _ cond:expr() _ ")" sugar:__remember_return_sugar_and_reset() body:__stmt_body() else_:__else()? {
            let mut cond = cond;
            Type::convert(&mut cond, &Type::bool());
            Global::get().allow_return_sugar = sugar;
            format!("if {} {{\n{}\n}}{}", cond.value, body, match else_ {
                None => String::new(),
                Some(body) => format!(" else {{\n{body}\n}}")
            })
        }

        // `while` statement
        "while" _ "(" _ cond:expr() _ ")" sugar:__remember_return_sugar_and_reset() body:__stmt_body() {
            let mut cond = cond;
            Type::convert(&mut cond, &Type::bool());
            Global::get().allow_return_sugar = sugar;
            format!("while {} {{\n{}\n}}", cond.value, body)
        }

        // Ordinary `expression` statement
        expr:expr() _ ";" {
            expr.value + ";"
        }

        // `return` statement
        "return" ret:__return_helper()? _ ";" _ is_last:__return_helper2()? {
            let cur = Function::current().expect("return outside of function");
            let is_not_last = is_last.is_none() || !Global::get().allow_return_sugar;
            let result = match ret {
                Some(mut e) if cur.name != "main" => {
                    assert_ne!(cur.ret, Type::void(), "unexpected value");
                    Type::convert(&mut e, &cur.ret);
                    if is_not_last {
                        format!("return {};", e.value)
                    } else {
                        e.value
                    }
                },
                _ => {
                    assert_eq!(cur.ret, Type::void(), "expected value");
                    if is_not_last {
                        String::from("return;")
                    } else {
                        String::new()
                    }
                }
            };
            result + "\n"
        }

        // `typedef` statement
        "typedef" __ ty:ty() _ name:only_ident() _ ";" {
            Type::assume_nonexisting(&name, TypeFlags::empty(), TypeFlags::NEEDS_STRUCT);
            let rusty = ty.rusty();
            let result = if name.is_keyword() || name == rusty {
                String::new()
            } else {
                if ty == Type::va_list() {
                    format!("type {} <'a, 'f> = {} <'a, 'f>;", name, rusty)
                } else {
                    format!("type {} = {};", name, rusty)
                }
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
        ("extern" __)? ret:ty() _ name:only_ident() _ "(" _ args:variable(true) ** ("," _) variadic:("," _ "..." _)? ")" _ ";" {
            match Function::get(|fun| fun.name == name) {
                Some(fun) => {
                    fun.assume_same_signature(&ret, &args);
                    String::new()
                },
                None => {
                    let x = format!("~~~{}\n", name);
                    let mut flags = FnFlags::EXTERN;
                    if variadic.is_some() {
                        flags.insert(FnFlags::VARIADIC)
                    }
                    Function::add(Function {
                        name,
                        ret,
                        args: args.len(),
                        vars: args,
                        flags,
                        local_types: Vec::new()
                    });
                    x
                }
            }
        }

        // Function definition
        comments:comments()? _ ret:ty() _ name:only_ident() _ "(" _ args:variable(true) ** ("," _) variadic:("," _ "..." _)? ")" __add_fn_and_skip_whitespaces(ret, name, args, variadic) "{" _ body:raw_clang("\n\t")? _ "}" {
            let me = Function::current().unwrap();
            let result = format!(
                "{comments}{unsafety}{extern_c}fn {name}({args}){ret} {{\n{body}\n}}\n",

                unsafety = if me.flags.contains(FnFlags::SAFE) {
                    ""
                } else {
                    "unsafe "
                },

                extern_c = if me.flags.contains(FnFlags::VARIADIC) {
                    "extern \"C\" "
                } else {
                    ""
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

        // Variable definition statement
        ty:ty() _ name:only_ident() init:__variable_init()? _ ";" {
            match Function::current() {
                Some(fun) => {
                    assert!(fun.vars.iter().find(|var| var.name == name).is_none(), "variable `{}` already exists", name);

                    let mut flags = VariableFlags::empty();

                    if ty.mutable {
                        flags.insert(VariableFlags::MUTABLE)
                    }

                    let mut result = format!("{}let {}{}", if ty == Type::va_list() {
                        "~"
                    } else {
                        ""
                    }, mut_and_space_or_nothing_depends_on_mutability(ty.mutable), name);

                    match init {
                        None => result.push_str(&format!(": {}", ty.rusty())),
                        Some(mut init) => {
                            Type::convert(&mut init, &ty);
                            result.push_str(&format!(" = {}", init.value))
                        }
                    }

                    result.push(';');

                    fun.vars.push(Variable {
                        name,
                        ty,
                        flags
                    });

                    result
                },
                None => todo!("no static support yet")
            }
        }

        // Comments
        comments:comments() {
            format!("//{}", comments.replace("\n", "\n//"))
        }

        // other:$([^ '\n']+) {
        //     panic!("unknown syntax:\n\t`{}`", other)
        // }
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
