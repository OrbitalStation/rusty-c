#[cfg(target_pointer_width = "32")]
type TrueUsize = u32;

#[cfg(target_pointer_width = "64")]
type TrueUsize = u64;

#[inline(always)]
pub const fn sizeof <T> () -> TrueUsize {
    core::mem::size_of::<T>() as _
}

#[inline]
pub fn assign <T: Copy> (x: &mut T, y: T) -> T {
    *x = y;
    y
}
