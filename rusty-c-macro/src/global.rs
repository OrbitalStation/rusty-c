use core::mem::MaybeUninit;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum TranslationStage {
    LinesCut,

    Preprocessing,

    Translation,

    Extra
}

pub struct Global {
    ///
    /// Path of source code
    ///
    pub file: String,

    /// Allow using short-return form
    pub allow_return_sugar: bool,

    /// Print result in the end
    pub print_result: bool,

    /// Standard library search paths
    pub search_paths: Vec <String>,

    ///
    /// Current stage
    ///
    pub stage: TranslationStage
}

impl Global {
    #[inline]
    pub fn get() -> &'static mut Self {
        static mut GLOBAL: MaybeUninit <Global> = MaybeUninit::uninit();
        unsafe { GLOBAL.assume_init_mut() }
    }
}
