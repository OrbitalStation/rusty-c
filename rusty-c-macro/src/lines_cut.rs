pub use clang::cut_lines;

peg::parser! { grammar clang() for str {
    rule __comment() = [^ '*'] / "*" !(&"/")

    rule any(newline: bool) -> String = any:$([_]) {?
        if newline && any == "\n" {
            Err("")
        } else {
            Ok(any.to_string())
        }
    }

    rule _ = [' ' | '\t']*

    rule cut(newline: bool) -> String = precedence! {
        src:$("\"" [^ '"']* "\"") {
            src.to_string()
        }

        src:$("//" [^ '\n']* "\n") {
            src.to_string()
        }

        src:$("/*" __comment()* "*/") {
            src.to_string()
        }

        _ "\\" [^ '\n']* "\n" _ next:cut(true)* "\n" {
            format!(" {}\n\n", next.join(""))
        }

        any:any(newline) {
            any
        }
    }

    ///
    /// This function cuts lines(see an example below)
    /// ```c
    /// #define my_assert(expr) abc\
    /// def
    /// ```
    /// would be
    /// ```c
    /// #define my_assert(expr) abc def
    ///
    /// ```
    ///
    pub rule cut_lines() -> String = cut:cut(false)* {
        cut.join("")
    }
} }
