use crate::*;
use check_keyword::CheckKeyword;

peg::parser! { grammar clang() for str {
    rule __comment() = [^ '*'] / "*" !(&"/")

    rule _ = [' ' | '\n' | '\t']*

    rule __ = [' ' | '\n' | '\t']+

    rule letter() -> &'input str = x:$([_]) {?
        letter(x)
    }

    rule digit() -> &'input str = x:$(['0'..='9'])

    rule ident() -> String = x:$(letter() (letter() / digit())*) {
        x.to_safe()
    }

    rule __any(inside: bool) -> &'input str = src:$([_]) {?
        if inside && src.chars().next().unwrap() == '~' { Err("") }
        else { Ok(src) }
    }

    rule stmt(inside: bool) -> String = precedence! {
        src:$("\"" [^ '"']* "\"") {
            src.to_string()
        }

        src:$("//" [^ '\n']* "\n") {
            src.to_string()
        }

        src:$("/*" __comment()* "*/") {
            src.to_string()
        }

        "~let " r#mut:$("mut ")? name:ident() rest:$([^ ';']*) ";" {
            let cur = Function::current().unwrap();
            let me = cur.vars[cur.args..].iter().find(|var| var.name == name).unwrap();
            if me.flags.contains(VariableFlags::CHOSEN_AS_VA_LIST) {
                assert!(me.flags.contains(VariableFlags::FINISHED_VA_LIST), "unterminated va_list: `{}`", me.name);
                String::new()
            } else {
                format!("let {}{}{}", r#mut.unwrap_or_default(), name, rest)
            }
        }

        "~~~" name:ident() {
            let fun = Function::get(|fun| fun.name == name).unwrap();
            if fun.flags.contains(FnFlags::EXTERN) {
                format!("extern \"C\" {{\n\tfn {name}({args}){ret};\n}}",
                    name = fun.name,
                    args = function_args(fun),
                    ret = function_return_type(fun)
                )
            } else {
                String::new()
            }
        }

        "~~" ty:digit() name:ident() stmts:raw_extra(true) "~" {
            let mut data = name + &stmts;
            if !inside && ty == "2" {
                data = format!("unsafe {{ {} }}", data)
            }
            data
        }

        "fn " name:ident() rest:$([^ '{']*) {
            for fun in Function::functions() {
                if fun.name == name {
                    fun.flags.insert(FnFlags::IS_INSIDE)
                } else {
                    fun.flags.remove(FnFlags::IS_INSIDE)
                }
            }
            format!("fn {}{}", name, rest)
        }

        x:$(__) ";" "\n" {
            format!("{}\n", x)
        }

        src:__any(inside) {
            src.to_string()
        }
    }

    rule raw_extra(inside: bool) -> String = stmts:stmt(inside)* { stmts.join("") }

    pub rule extra() -> String = x:raw_extra(false)
} }

pub use clang::extra;
