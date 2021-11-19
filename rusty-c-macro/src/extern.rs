use crate::*;

peg::parser! { grammar clang() for str {
    rule __comment() = [^ '*'] / "*" !(&"/")

    rule letter() -> &'input str = x:$(['a'..='z' | 'A'..='Z' | '_'])

    rule digit() -> &'input str = x:$(['0'..='9'])

    rule ident() -> &'input str = x:$(letter() (letter() / digit())*)

    rule __any(inside: bool) -> &'input str = src:$([_]) {?
        if inside && src.chars().next().unwrap() == '~' { Err("") }
        else { Ok(src) }
    }

    rule stmt(inside: bool) -> String = precedence! {
        src:$("\"" [^ '\"']* "\"") {
            src.to_string()
        }

        src:$("//" [^ '\n']* "\n") {
            src.to_string()
        }

        src:$("/*" __comment()* "*/") {
            src.to_string()
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

        "~~" ty:digit() name:ident() stmts:raw_deextern(true) "~" {
            let mut data = name.to_string() + &stmts;
            if !inside && ty == "2" {
                data = format!("unsafe {{ {} }}", data)
            }
            data
        }

        src:__any(inside) {
            src.to_string()
        }
    }

    rule raw_deextern(inside: bool) -> String = stmts:stmt(inside)* { stmts.join("") }

    pub rule deextern() -> String = x:raw_deextern(false)
} }

pub use clang::deextern;
