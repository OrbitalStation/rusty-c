mod builtin;

use core::fmt::Formatter;
use crate::*;
use rt_format::{Arguments, FormattableValue, Specifier};
use std::path::Path;

bitflags::bitflags! {
    pub struct DataFlags: u8 {
        const WAS_ELSE = 1 << 0;
        const WAS_ELIF = 1 << 1;
    }
}

#[derive(Debug)]
pub enum MacroData {
    Ord(String),
    Fun {
        fmt: String,
        len: usize
    }
}
#[derive(Debug)]
pub struct Macro {
    pub name: String,
    pub data: MacroData
}

pub struct GlobalMacroData {
    pub line: usize,
    pub file: String,
    pub if_level: usize,
    pub ignore_level: usize,
    pub flags: DataFlags
}

static mut DATA: Vec <GlobalMacroData> = Vec::new();
static mut MACROS: Vec <Macro> = Vec::new();

pub fn global_macro_data() -> &'static mut GlobalMacroData {
    unsafe { DATA.last_mut().unwrap() }
}

fn macros() -> &'static mut Vec <Macro> {
    unsafe { &mut MACROS }
}

fn nl() -> String {
    String::from("\n")
}

impl Macro {
    pub fn preprocess(mut code: String) -> String {
        unsafe {
            DATA.push(GlobalMacroData {
                line: 0,
                file: Global::get().file.clone(),
                if_level: 0,
                ignore_level: usize::MAX,
                flags: DataFlags::empty()
            })
        }

        code.push('\n');

        let result = clang::stmts(&code, &Vec::new()).unwrap();

        assert_eq!(global_macro_data().if_level, 0, "unterminated conditional directive");

        unsafe { DATA.pop(); }

        result
    }

    pub fn is_defined(name: &str) -> bool {
        macros().iter().find(|m| m.name == name).is_some()
    }

    pub fn add(m: Self) {
        match macros().iter_mut().find(|m2| m2.name == m.name) {
            None => macros().push(m),
            Some(m2) => m2.data = m.data
        }
    }
}

peg::parser! { grammar clang() for str {
    rule _ = ([' ' | '\t'] / comments())*

    rule __ = ([' ' | '\t'] / comments())+

    rule newline() = "\n" {
        global_macro_data().line += 1
    }

    rule __single_comment_helper() = ([^ '*']) / ("*" !(&"/"))

    rule __single_comment() = ("//" [^ '\n']*) / ("/*" __single_comment_helper()* "*/")

    rule comments() = __single_comment() ++ [' ' | '\t']

    rule letter() -> &'input str = x:$([_]) {?
        letter(x)
    }

    rule digit(radix: u32) -> &'input str = digit:$(['0'..='9' | 'a'..='z' | 'A'..='Z']) {?
        match digit.chars().next().unwrap().to_digit(radix) {
            Some(_) => Ok(digit),
            None => Err("digit")
        }
    }

    rule int_number() -> usize = digits:$(("0" ("x" / "X") (digit(16) / "'")+) / ("0" (digit(8) / "'")+) / ((digit(10) / "'")+) / ("0b" (digit(2) / "'")+)) {
        digits.replace('\'', "_").parse().unwrap()
    }

    rule ident() -> &'input str = x:$(letter() (letter() / digit(10))*)

    rule __define_text() -> &'input str
        = __ text:$([^ '\n']+) { text }
        / _ { "" }

    rule __fmt_arg() -> StrRtHack <'input>
        = s:lit_string() { StrRtHack::String(format!("\"{}\"", s)) }
        / arg:$([^ ',' | ')']*) { StrRtHack::Str(arg) }

    rule __expect_fmacro() -> &'static Macro = name:ident() {?
        macros().iter().find(|m| m.name == name && matches!(m.data, MacroData::Fun { .. })).ok_or("macro")
    }

    rule __ctrs_arg() -> &'input str = s:$("{" digit(10)+ "}")

    rule __complete_text_replace_stmt() -> String = precedence! {
        x:(@) _ "##" _ y:@ {
            format!("{}{}", x, y)
        }

        --

        "#" _ x:__ctrs_arg() {
            format!("\"{}\"", x)
        }

        --

        comments() {
            String::new()
        }

        any:$([_]) {
            String::from(any)
        }
    }

    pub(in self) rule complete_text_replace() -> String = stmts:__complete_text_replace_stmt()* {
        stmts.join("")
    }

    rule __defined() -> bool
        = "defined" __ name:ident() { Macro::is_defined(name) }
        / "defined" _ "(" _ name:ident() _ ")" { Macro::is_defined(name) }

    rule defined(preprocessor: bool) -> bool = cond:__defined() {?
        if preprocessor {
            Ok(cond)
        } else {
            Err("")
        }
    }

    rule __line_rest(ignore: &Vec <&str>, inside_if: bool) -> String = precedence! {
        "__LINE__" {
            global_macro_data().line.to_string()
        }

        "__FILE__" {
            format!("\"{}\"", global_macro_data().file)
        }

        cond:defined(inside_if) {
            (cond as u8).to_string()
        }

        name:__expect_fmacro() _ "(" _ args:__fmt_arg() ** ("," _) ")" {
            match &name.data {
                MacroData::Fun { fmt, len } => {
                    assert_eq!(*len, args.len(), "wrong number of arguments");
                    stmt(&Arguments::parse(&complete_text_replace(&fmt).unwrap(), &args, &StubMap).unwrap().to_string(), &vec![&name.name]).unwrap()
                },
                _ => unreachable!()
            }
        }

        name:ident() {
            if ignore.contains(&name) {
                return name.to_string()
            }
            match macros().iter().find(|m| m.name == name) {
                None => if inside_if {
                    String::from("0")
                } else {
                    String::from(name)
                },
                Some(m) => match &m.data {
                    MacroData::Ord(text) => {
                        line_rest(&text, &vec![name], inside_if).unwrap()
                    },
                    _ => String::new()
                }
            }
        }

        any:$([^ '\n']) {
            String::from(any)
        }
    }

    pub(in self) rule line_rest(ignore: &Vec <&str>, inside_if: bool) -> String = rest:__line_rest(ignore, inside_if)* {
        rest.join("")
    }

    rule __raw_char(ignore_symbol: char) -> String
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
        / x:$("{" / "}") { String::from(x) + x }
        / char:$([_]) {?
            if ignore_symbol == char.chars().next().unwrap() {
                Err("char")
            } else {
                Ok(char.to_string())
            }
        }

    rule lit_string() -> String = "\"" chars:__raw_char('"')* "\"" {
        chars.join("")
    }

    rule __line_helper() -> String = __ file:lit_string() { file }

    rule __fun_macro_helper() -> &'input str = arg:ident()? {
        arg.unwrap_or_default()
    }

    rule __fun_macro_hh2(args: &Vec <&str>) -> String = precedence! {
        string:lit_string() { format!("\"{}\"", string) }

        x:$("{" / "}") {
            String::from(x) + x
        }

        name:ident() {
            match args.iter().enumerate().find(|(_, x)| name == **x) {
                None => name.to_string(),
                Some((idx, _)) => format!("{{{}}}", idx)
            }
        }

        any:$([_]) {
            String::from(any)
        }
    }

    pub(in self) rule __fun_macro_helper2(args: &Vec <&str>) -> String = stmts:__fun_macro_hh2(args)* {
        stmts.join("")
    }

    rule __if() -> bool
        = "ifdef" __ name:ident() { Macro::is_defined(name) }
        / "ifndef" __ name:ident() { !Macro::is_defined(name) }
        / "if" __ cond:$([^ '\n']+) {
            const_expr(&line_rest(cond, &Vec::new(), true).unwrap(), true).unwrap() != 0
        }

    rule __include() -> String
        = path:lit_string() { path }
        / "<" path:__raw_char('>')* ">" {
            let path = path.join("");
            for dir in &Global::get().search_paths {
                let path = Path::new(dir).join(&path);
                if path.exists() {
                    return path.to_str().unwrap().to_string()
                }
            }
            path
        }

    pub rule stmts(ignore: &Vec <&str>) -> String = stmts:stmt(ignore)* {
        stmts.join("")
    }

    pub(in self) rule stmt(ignore: &Vec <&str>) -> String = precedence! {
        "#" _ "define" __ name:ident() "(" _ args:__fun_macro_helper() ** ("," _) ")" text:__define_text() newline() {
            if global_macro_data().ignore_level == usize::MAX {
                Macro::add(Macro {
                    name: String::from(name),
                    data: MacroData::Fun {
                        len: args.len(),
                        fmt: __fun_macro_helper2(text, &args).unwrap()
                    }
                })
            }
            nl()
        }

        "#" _ "include" _ path:__include() _ newline() {
            if global_macro_data().ignore_level == usize::MAX {
                include_only_preprocess(&path)
            } else {
                nl()
            }
        }

        "#" _ "define" __ name:ident() text:__define_text() newline() {
            if global_macro_data().ignore_level == usize::MAX {
                Macro::add(Macro {
                    name: String::from(name),
                    data: MacroData::Ord(String::from(text))
                })
            }
            nl()
        }

        "#" _ cond:__if() _ newline() {
            global_macro_data().if_level += 1;
            if global_macro_data().ignore_level == usize::MAX {
                if !cond {
                    global_macro_data().ignore_level = global_macro_data().if_level
                }
            }
            nl()
        }

        "#" _ "elif" __ cond:$([^ '\n']+) newline() {
            assert_ne!(global_macro_data().if_level, 0, "cannot use #elif without #if");
            if global_macro_data().flags.contains(DataFlags::WAS_ELIF) {
                global_macro_data().ignore_level = global_macro_data().if_level
            } else if global_macro_data().ignore_level == usize::MAX || global_macro_data().ignore_level == global_macro_data().if_level {
                if const_expr(&line_rest(cond, &Vec::new(), true).unwrap(), true).unwrap() != 0 {
                    global_macro_data().flags.insert(DataFlags::WAS_ELIF);
                    global_macro_data().ignore_level = usize::MAX
                }
            }
            nl()
        }

        "#" _ "endif" _ newline() {
            assert_ne!(global_macro_data().if_level, 0, "extra endif");
            if global_macro_data().ignore_level == global_macro_data().if_level {
                global_macro_data().ignore_level = usize::MAX
            }
            global_macro_data().if_level -= 1;
            global_macro_data().flags.remove(DataFlags::WAS_ELSE);
            global_macro_data().flags.remove(DataFlags::WAS_ELIF);
            nl()
        }

        "#" _ "else" _ newline() {
            assert_ne!(global_macro_data().if_level, 0, "cannot use #else without #if");
            if !global_macro_data().flags.contains(DataFlags::WAS_ELIF) && (global_macro_data().ignore_level == usize::MAX || global_macro_data().ignore_level == global_macro_data().if_level) {
                assert!(!global_macro_data().flags.contains(DataFlags::WAS_ELSE), "duplicated else");
                global_macro_data().flags.insert(DataFlags::WAS_ELSE);
                if global_macro_data().ignore_level == usize::MAX {
                    global_macro_data().ignore_level = global_macro_data().if_level
                } else {
                    global_macro_data().ignore_level = usize::MAX
                }
            }
            nl()
        }

        "#" _ "undef" __ name:ident() _ newline() {
            if global_macro_data().ignore_level == usize::MAX {
                match macros().iter().enumerate().find(|(_, m)| m.name == name) {
                    None => (),
                    Some((idx, _)) => drop(macros().remove(idx))
                }
            }
            nl()
        }

        "#" _ "line" __ line:int_number() file:__line_helper()? _ newline() {
            if global_macro_data().ignore_level == usize::MAX {
                global_macro_data().line = line;
                match file {
                    None => (),
                    Some(file) => global_macro_data().file = file
                }
            }
            nl()
        }

        "#" _ "error" msg:$(__ [^ '\n']+)? newline() {
            if global_macro_data().ignore_level == usize::MAX {
                panic!("{}", msg.unwrap_or_default())
            }
            nl()
        }

        "#" _ "warning" msg:$(__ [^ '\n']+)? newline() {
            if global_macro_data().ignore_level == usize::MAX {
                println!("warning: {}", msg.unwrap_or_default())
            }
            nl()
        }

        "#" _ directive:$([^ '\n']+) newline() {
            if global_macro_data().ignore_level == usize::MAX {
                panic!("unknown directive: {}", directive)
            }
            nl()
        }

        "#" _ newline() {
            nl()
        }

        code:line_rest(ignore, false) newline() {
            if global_macro_data().ignore_level == usize::MAX {
                code + "\n"
            } else {
                nl()
            }
        }
    }
} }

/* Some dirty hacks for `rt-format` crate */

#[derive(Debug)]
enum StrRtHack <'a> {
    Str(&'a str),
    String(String)
}

impl <'a> TryInto <usize> for &StrRtHack <'a> {
    type Error = ();

    fn try_into(self) -> Result <usize, ()> {
        Err(())
    }
}

impl <'a> FormattableValue for StrRtHack <'a> {
    fn supports_format(&self, spec: &Specifier) -> bool {
        spec.format == rt_format::Format::Display
    }

    fn fmt_display(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(match self {
            Self::Str(s) => s,
            Self::String(s) => &s
        })
    }

    fn fmt_debug(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
    fn fmt_octal(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
    fn fmt_lower_hex(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
    fn fmt_upper_hex(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
    fn fmt_binary(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
    fn fmt_lower_exp(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
    fn fmt_upper_exp(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
}

struct StubMap;

impl <Q: core::hash::Hash + Eq + ?Sized, V> rt_format::map::Map <Q, V> for StubMap {
    fn get(&self, _: &Q) -> Option <&V> {
        unreachable!()
    }
}
