use crate::*;
use chrono::{Datelike, Timelike};

macro_rules! predefine {
    ($(
        $( #[$meta:meta] )? $name:ident $text:literal

        $(
            { $( $code:tt )* }
        )?
    )*) => {
        impl Macro {
            pub fn predefine_all() {
                fn add(name: &str, text: &str) {
                    Macro::add(Macro {
                        name: name.to_string(),
                        data: MacroData::Ord(text.to_string())
                    })
                }

                $(
                    $( #[$meta] )?
                    add(stringify!($name), $text);

                    $(
                        $( $code )*
                    )?
                )*
            }
        }
    };
}

predefine! {
    __STDC__ "1"
    __STDC_VERSION "199901"

    __RUST__ ""

    __rusty_c_minor__ "0"
    __rusty_c_major__ "0"
    __rusty_c_patch__ "0"

    #[cfg(target_arch = "x86_64")]
    __x86_64__ ""

    #[cfg(target_os = "linux")]
    __linux__ ""

    {
        let now = chrono::Local::now();
        add("__DATE__", &format!("\"{} {:02} {}\"", match now.month0() {
             0 => "Jan",
             1 => "Feb",
             2 => "Mar",
             3 => "Apr",
             4 => "May",
             5 => "Jun",
             6 => "Jul",
             7 => "Aug",
             8 => "Sep",
             9 => "Oct",
            10 => "Nov",
            11 => "Dec",
            _ => unreachable!()
        }, now.day(), now.year()));
    }

    #[cfg(not(debug_assertions))]
    NDEBUG ""

    {
        add("__TIME__", &format!("\"{:02}:{:02}:{:02}\"", now.hour(), now.minute(), now.second()));
    }
}
