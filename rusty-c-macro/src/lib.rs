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

modules!(tools r#macro ty fun expr preprocessor global r#extern);

#[cfg(not(feature = "debug"))]
use proc_macro::TokenStream;

#[cfg(not(feature = "debug"))]
#[proc_macro]
pub fn include_c(filepath: TokenStream) -> TokenStream {
    prepare(filepath.to_string());

    let result = include_file(&Global::get().file);

    println!("{}", result);

    result.parse().unwrap()
}

#[cfg(feature = "debug")]
pub fn include_c(filepath: &str) {
    prepare(filepath.to_string());

    println!("{}", include_file(&Global::get().file));
}

fn prepare(filepath: String) {
    Global::get().file = filepath;

    Type::add_builtins();
}

pub(crate) fn include_file(file: &str) -> String {
    let code = std::fs::read_to_string(file).unwrap();

    let code = Macro::preprocess(code);

    let code = clang(&code).unwrap();

    let code = deextern(&code).unwrap();

    code
}
