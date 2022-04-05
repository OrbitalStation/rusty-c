# Rusty-C
## Rusty-C is a program designed to convert legacy C to modern Rust.
### Example:

main.c
```c
int fibonacci(int n) {
    return n < 2 ? 1 : fibonacci(n - 1) + fibonacci(n - 2);
}
```

Start translation:

```commandline
rusty-c main.c --gnu-search
```

The C code above is translated into Rust equivalent:
main.rs
```rust
fn fibonacci(n: i32) -> i32 {
    if n < 2 {
        1
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}
```

## Why not to use existing `c2rust`?
Well, main reason is that `c2rust`'s output is... scary.

And of course, `c2rust` supposes that <i>every</i> C function is unsafe,
even if it's just a simple fibonacci.

In order not to be unfounded, take a look at the example below.
```rust
#![allow(dead_code, mutable_transmutes, non_camel_case_types, non_snake_case,
         non_upper_case_globals, unused_assignments, unused_mut)]
#![register_tool(c2rust)]
#![feature(register_tool)]
#[no_mangle]
pub unsafe extern "C" fn fibonacci(mut n: libc::c_int) -> libc::c_int {
    return if n < 2 as libc::c_int {
               1 as libc::c_int
           } else {
               (fibonacci(n - 1 as libc::c_int)) +
                   fibonacci(n - 2 as libc::c_int)
           };
}
```
Wow... <i>This</i> is what I call scary.
Just compare to `rusty-c`'s version of this simple function.

Anyway, choosing tool is only your decision.
Goodbye!
