#![feature(c_variadic)]
#![feature(try_trait_v2)]

use clap::{App, AppSettings, Arg};
use std::process::{Command, Stdio};

#[cfg(not(feature = "debug"))]
macro_rules! modules {
    ($( $mod:ident )*) => { $(
        mod $mod;
        use $mod::*;
    )* };
}

#[cfg(feature = "debug")]
macro_rules! modules {
    ($( $mod:ident )*) => { $(
        mod $mod;
        pub use $mod::*;
    )* };
}

modules!(tools r#macro ty fun expr preprocessor global extra r#const lines_cut);

#[cfg(not(feature = "debug"))]
use proc_macro::TokenStream;

#[cfg(not(feature = "debug"))]
#[proc_macro]
pub fn include_c(args: TokenStream) -> TokenStream {
    prepare(args.to_string());

    let result = include_file();

    if Global::get().print_result {
        println!("{}", result);
    }

    result.parse().unwrap()
}

#[cfg(feature = "debug")]
pub fn include_c(args: &str) {
    prepare(format!("\"{}\"", args));

    println!("{}", include_file());
}

fn prepare(args: String) {
    let default_panic = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let /*mut*/ file /*= &Global::get().file*/;
        let line = match Global::get().stage {
            TranslationStage::LinesCut => unreachable!(),
            TranslationStage::Preprocessing => {
                file = &global_macro_data().file;
                global_macro_data().line
            },
            stage => {
                println!("not yet implemented for `{:?}` stage", stage);
                return default_panic(info)
            }
        };

        println!("extra panic info: {}:{}", file, line);
        default_panic(info)
    }));

    assert!(args.chars().next().unwrap() == '"' && args.chars().next_back().unwrap() == '"', "input should be enclosed in quotes");
    let args = &args[1..args.len() - 1];

    let matches = App::new("rusty-c")
        .setting(AppSettings::NoBinaryName)
        .arg(Arg::new("file")
            .required(true)
            .index(1))
        .arg(Arg::new("print result")
            .long("print-result")
            .short('p'))
        .arg(Arg::new("gnu search")
            .long("gnu-search")
            .alias("gcc-search"))
        .get_matches_from(args.split_whitespace());

    let global = Global::get();

    global.print_result = matches.is_present("print result");

    global.file = std::fs::canonicalize(matches.value_of("file").unwrap()).unwrap().as_path().to_str().unwrap().to_string();

    global.search_paths = Vec::new();
    if matches.is_present("gnu search") {
        let output = core::str::from_utf8(&Command::new("gcc")
            .args(&["-xc", "-E", "-v", "-"])
            .stdin(Stdio::null())
            .output()
            .expect("gcc unexpectable failed").stderr).unwrap().to_string();
        let list_start = output.find("#include <...> search starts here:").expect("failed to find stdlib paths") + 34 /* length of  "#include <...> search starts here:"*/;
        let list_finish = list_start + output[list_start..].find("End of search list.").expect("failed to find stdlib paths");
        let output = output[list_start..list_finish].split_whitespace();
        global.search_paths.reserve(output.size_hint().1.unwrap_or_default());
        for path in output {
            global.search_paths.push(path.trim().to_string())
        }
    } else {
        #[cfg(target_family = "unix")] { global.search_paths = vec!["/usr/include".to_string()] }
    }

    Type::add_builtins();

    Macro::predefine_all()
}

pub(crate) fn include_only_preprocess(file: &str) -> String {
    let code = std::fs::read_to_string(file).unwrap();

    Global::get().stage = TranslationStage::LinesCut;
    let code = cut_lines(&code).unwrap();

    Global::get().stage = TranslationStage::Preprocessing;
    let code = Macro::preprocess(code);

    code
}

pub(crate) fn include_file() -> String {
    let code = include_only_preprocess(&Global::get().file);

    Global::get().stage = TranslationStage::Translation;
    let code = clang(&code).unwrap();

    Global::get().stage = TranslationStage::Extra;
    let code = extra(&code).unwrap();

    code
}
