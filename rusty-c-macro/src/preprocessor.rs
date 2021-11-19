use core::fmt::Formatter;
use crate::*;
use rt_format::{Arguments, FormattableValue, Specifier};

pub enum MacroData {
    Ord(String),
    Fun {
        fmt: String,
        len: usize
    }
}

pub struct Macro {
    pub name: String,
    pub data: MacroData
}

struct Data {
    pub macros: Vec <Macro>,
    pub line: usize,
    pub file: String,
    pub if_level: usize,
    pub ignore_level: usize,
    pub was_else: bool
}

static mut DATA: Vec <Data> = Vec::new();

fn data() -> &'static mut Data {
    unsafe { DATA.last_mut().unwrap() }
}

impl Macro {
    pub fn add_predefined() {
    }

    pub fn preprocess(code: String) -> String {
        unsafe {
            DATA.push(Data {
                macros: Vec::new(),
                line: 0,
                file: Global::get().file.clone(),
                if_level: 0,
                ignore_level: usize::MAX,
                was_else: false
            })
        }

        Self::add_predefined();

        let mut result = String::new();

        for line in code.lines() {
            data().line += 1;
            let code = clang::line(line).unwrap();
            if data().ignore_level == usize::MAX {
                result.push_str(&code)
            }
            result.push('\n')
        }

        assert_eq!(data().if_level, 0, "unterminated conditional directive");

        unsafe { DATA.pop(); }

        result
    }

    pub fn is_defined(name: &str) -> bool {
        data().macros.iter().find(|m| m.name == name).is_some()
    }

    pub fn add(m: Self) {
        match data().macros.iter_mut().find(|m2| m2.name == m.name) {
            None => data().macros.push(m),
            Some(m2) => m2.data = m.data
        }
    }
}

peg::parser! { grammar clang() for str {
    rule _ = [' ' | '\t']*

    rule __ = [' ' | '\t']+

    rule letter() -> &'input str = x:$(['a'..='z' | 'A'..='Z' | '_'])

    rule digit(radix: u32) -> &'input str = digit:$(['0'..='9' | 'a'..='z' | 'A'..='Z']) {?
        match digit.chars().next().unwrap().to_digit(radix) {
            Some(_) => Ok(digit),
            None => Err("digit")
        }
    }

    rule int_number() -> usize
        = digits:$("0" ("x" / "X") (digit(16) / "'")+) { digits.replace('\'', "").parse().unwrap() }
        / digits:$("0" (digit(8) / "'")+) { digits.replace('\'', "").parse().unwrap() }
        / digits:$((digit(10) / "'")+) { digits.replace('\'', "").parse().unwrap() }
        / digits:$("0b" (digit(2) / "'")+) { digits.replace('\'', "").parse().unwrap() }

    rule ident() -> &'input str = x:$(letter() (letter() / digit(10))*) {
        x
    }

    rule __define_text() -> &'input str
        = __ text:$([_]+) { text }
        / _ { "" }

    rule __fmt_arg() -> StrRtHack <'input> = arg:$([^ ',' | ')']*) {
        StrRtHack(arg)
    }

    rule __expect_fmacro() -> &'static Macro = name:ident() {?
        data().macros.iter().find(|m| m.name == name && matches!(m.data, MacroData::Fun { .. })).ok_or("macro")
    }

    rule __line_rest() -> String = precedence! {
        "__LINE__" {
            data().line.to_string()
        }

        "__FILE__" {
            format!("\"{}\"", data().file)
        }

        name:__expect_fmacro() _ "(" _ args:__fmt_arg() ** ("," _) ")" {
            match &name.data  {
                MacroData::Fun { fmt, len } => {
                    assert_eq!(*len, args.len(), "wrong number of arguments");
                    Arguments::parse(fmt, &args, &StubMap).unwrap().to_string()
                },
                _ => unreachable!()
            }
        }

        name:ident() {
            match data().macros.iter().find(|m| m.name == name) {
                None => String::from(name),
                Some(m) => match &m.data {
                    MacroData::Ord(text) => text.clone(),
                    _ => String::new()
                }
            }
        }

        any:$([_]) {
            String::from(any)
        }
    }

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

    rule lit_string() -> String = "\"" chars:__raw_char(true)* "\"" {
        chars.join("")
    }

    rule __line_helper() -> String = __ file:lit_string() { file }

    rule __fun_macro_helper() -> &'input str = arg:ident()? {
        arg.unwrap_or_default()
    }

    rule __fun_macro_hh2(args: &Vec <&str>) -> String = precedence! {
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

    pub rule line() -> String = precedence! {
        "#" _ "define" __ name:ident() _ "(" _ args:__fun_macro_helper() ** ("," _) ")" text:__define_text() {
            if data().ignore_level == usize::MAX {
                Macro::add(Macro {
                    name: String::from(name),
                    data: MacroData::Fun {
                        len: args.len(),
                        fmt: __fun_macro_helper2(text, &args).unwrap()
                    }
                })
            }
            String::new()
        }

        "#" _ "define" __ name:ident() text:__define_text() {
            if data().ignore_level == usize::MAX {
                Macro::add(Macro {
                    name: String::from(name),
                    data: MacroData::Ord(String::from(text))
                })
            }
            String::new()
        }

        "#" _ cond:__if() {
            data().if_level += 1;
            if data().ignore_level == usize::MAX {
                if !cond {
                    data().ignore_level = data().if_level
                }
            }
            String::new()
        }

        "#" _ "endif" {
            assert_ne!(data().if_level, 0, "extra endif");
            if data().ignore_level == data().if_level {
                data().ignore_level = usize::MAX
            }
            data().if_level -= 1;
            data().was_else = false;
            String::new()
        }

        "#" _ "else" {
            if data().ignore_level == usize::MAX || data().ignore_level == data().if_level {
                assert!(!data().was_else, "duplicated else");
                data().was_else = true;
                if data().ignore_level == usize::MAX {
                    data().ignore_level = data().if_level
                } else {
                    data().ignore_level = usize::MAX
                }
            }
            String::new()
        }

        "#" _ "undef" __ name:ident() {
            match data().macros.iter().enumerate().find(|(_, m)| m.name == name) {
                None => (),
                Some((idx, _)) => drop(data().macros.remove(idx))
            }
            String::new()
        }

        "#" _ "line" __ line:int_number() file:__line_helper()? {
            data().line = line;
            match file {
                None => (),
                Some(file) => data().file = file
            }
            String::new()
        }

        "#" _ directive:$([_]+) {
            panic!("unknown directive: {}", directive)
        }

        "#" _ {
            String::new()
        }

        code:__line_rest()* {
            code.join("")
        }
    }
} }

#[derive(Debug)]
struct StrRtHack <'a> (&'a str);

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
        f.write_str(self.0)
    }

    fn fmt_debug(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
    fn fmt_octal(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
    fn fmt_lower_hex(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
    fn fmt_upper_exp(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
    fn fmt_binary(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
    fn fmt_lower_exp(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
    fn fmt_upper_hex(&self, _: &mut Formatter) -> std::fmt::Result { unreachable!() }
}

struct StubMap;

impl <Q: core::hash::Hash + Eq + ?Sized, V> rt_format::map::Map <Q, V> for StubMap {
    fn get(&self, _: &Q) -> Option <&V> {
        unreachable!()
    }
}
