# Rusty-C
## Rusty-C is a Cargo crate designed to convert legacy C to modern Rust.
### Usage:
Add `rusty-c` of last version to your dependencies and use it:

### Example:

main.c
```c
int fibonacci(int n) {
    return n < 2 ? 1 : fibonacci(n - 1) + fibonacci(n - 2);
}
```

main.rs
```rust
use rusty_c::prelude::*;

include_c!(code.c);

fn main() {
    println!("{}", fibonacci(9)) // 55
}
```

The C code above is translated into Rust equivalent:
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
Well, main reason is that `c2rust`'s output is... kinda scary =)

And of course, `c2rust` supposes that <i>every</i> C function is unsafe,
even if it is just a simple fibonacci.

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
Wow, you know... <i>This</i> is what I call scary.

Anyway, choosing tool is only your decision.
Be happy and - goodbye!
