///
/// Helper trait to convert C string into Rust one.
///
pub trait CStringToRust {
    ///
    /// Unsafe because caller has to guarantee that `self` is a valid pointer
    ///
    unsafe fn to_rust(&self) -> &str;
}

///
/// Helper trait to convert Rust string into C one.
///
/// WARNING: the `to_c` method <i>does</i> check if string has null-terminator,
///
/// but `to_c_unchecked` <i>doesn't</i>.
///
/// if you want to convert a <i>C string</i> that was converted into Rust, you can just call
///
/// `to_c` or even `to_c_unchecked`,
///
/// but if you want to convert a <i>native</i> Rust string into C String,
///
/// you <i>really</i> have to ensure that you string contains null-terminator - that's what `to_c` does.
///
///
pub trait RustStringToC {
    fn to_c(&self) -> *const u8;

    ///
    /// Unsafe because caller must uphold the contract
    ///
    unsafe fn to_c_unchecked(&self) -> *const u8;
}

impl CStringToRust for *const u8 {
    unsafe fn to_rust(&self) -> &str {
        let len = strlen(*self);
        let raw = core::slice::from_raw_parts(*self, len);

        core::str::from_utf8(raw).unwrap()
    }
}

impl RustStringToC for str {
    fn to_c(&self) -> *const u8 {
        match self.chars().next_back() {
            Some(zero) if zero == '\0' => (),
            _ => {
                let len = self.len();
                let start = self.as_ptr();
                let byte_after_address = (start as usize + len) as *const u8;
                let byte_after = unsafe { *byte_after_address };
                assert_eq!(byte_after, b'\0', "string is not terminated by null")
            }
        }
        unsafe { self.to_c_unchecked() }
    }

    #[inline(always)]
    unsafe fn to_c_unchecked(&self) -> *const u8 {
        self.as_ptr()
    }
}

///
/// Calculates length of C String
///
unsafe fn strlen(mut s: *const u8) -> usize {
    let start = s;
    while *s != 0 {
        s = (s as usize + 1) as *const u8;
    }
    s as usize - start as usize
}
