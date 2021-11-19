use core::mem::MaybeUninit;

pub struct Global {
    ///
    /// Path of source code
    ///
    pub file: String
}

impl Global {
    #[inline]
    pub fn get() -> &'static mut Self {
        static mut GLOBAL: MaybeUninit <Global> = MaybeUninit::uninit();
        unsafe { GLOBAL.assume_init_mut() }
    }
}
