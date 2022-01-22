use crate::*;

peg::parser! { grammar clang() for str {
    rule _ = [' ' | '\t' | '\n']*

    rule __ = [' ' | '\t' | '\n']+

    rule letter() -> &'input str = x:$([_]) {?
        letter(x)
    }

    rule ident() -> &'input str = x:$(letter() (letter() / digit(10))*)

    rule digit(radix: u32) -> &'input str = digit:$(['0'..='9' | 'a'..='z' | 'A'..='Z']) {?
        match digit.chars().next().unwrap().to_digit(radix) {
            Some(_) => Ok(digit),
            None => Err("digit")
        }
    }

    rule int_number() -> i128 = digits:$(("0" ("x" / "X") (digit(16) / "'")+) / ("0" (digit(8) / "'")+) / ((digit(10) / "'")+) / ("0b" (digit(2) / "'")+)) {
        digits.replace('\'', "_").parse().unwrap()
    }

    pub rule expr(preprocessor: bool) -> i128 = precedence! {
        cond:@ _ "?" _ s1:expr(preprocessor) _ ":" _ s2:(@) {
            if cond != 0 {
                s1
            } else {
                s2
            }
        }

        --

        x:(@) _ "||" _ y:@ { ((x != 0) || (y != 0)) as _ }

        --

        x:(@) _ "&&" _ y:@ { ((x != 0) && (y != 0)) as _ }

        --

        x:(@) _ "|" _ y:@ { x | y }

        --

        x:(@) _ "^" _ y:@ { x ^ y }

        --

        x:(@) _ "&" _ y:@ { x & y }

        --

        x:(@) _ "==" _ y:@ { (x == y) as _ }
        x:(@) _ "!=" _ y:@ { (x == y) as _ }

        --

        x:(@) _ ">" _ y:@ { (x > y) as _ }
        x:(@) _ ">=" _ y:@ { (x >= y) as _ }
        x:(@) _ "<" _ y:@ { (x < y) as _ }
        x:(@) _ "<=" _ y:@ { (x <= y) as _ }

        --

        x:(@) _ "<<" _ y:@ { x << y }
        x:(@) _ ">>" _ y:@ { x >> y }

        --

        x:(@) _ "+" _ y:@ { x + y }
        x:(@) _ "-" _ y:@ { x - y }

        --

        x:(@) _ "*" _ y:@ { x * y }
        x:(@) _ "/" _ y:@ { x / y }
        x:(@) _ "%" _ y:@ { x % y }

        --

        "!" _ x:(@) { !(x != 0) as _ }

        "~" _ x:(@) { !x }

        "+" _ x:(@) { x }

        "-" _ x:(@) { -x }

        --

        "(" _ x:expr(preprocessor) _ ")" { x }

        number:int_number() { number }
    }
} }

pub use clang::expr as const_expr;
