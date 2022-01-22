use crate::*;

fn expect_args(need: usize, actual: usize) {
    assert_eq!(actual, need, "wrong number of args: expected {}, found {}", need, actual);
}

fn _expect_template <T, E> (args: &Vec <&str>, idx: usize, predicate: fn(&str) -> Result <T, E>, err: &str) -> T {
    match predicate(args[idx]) {
        Ok(x) => x,
        Err(_) => panic!("expected {} at arg {}, found {}", err, idx, args[idx])
    }
}

fn expect_variable(args: &Vec <&str>, idx: usize) -> String {
    _expect_template(args, idx, ident, "variable")
}

fn expect_type(args: &Vec <&str>, idx: usize) -> Type {
    _expect_template(args, idx, ty, "type")
}

fn expect_va_list(cur: &mut Function, va_list: String) -> &mut Variable {
    let va_list = cur.vars[cur.args..].iter_mut().find(|var| var.name == va_list).expect("`va_list` must be defined in function that uses it");
    assert_eq!(va_list.ty, Type::va_list(), "provided variable is not a `va_list`");
    va_list
}

fn none() -> Option <Expr> {
    Some(Expr::new(String::new(), ExprType::Ord(Type::void())))
}

impl Function {
    pub fn check_builtin(name: &str, args: &Vec <&str>) -> Option <Expr> {
        match name {
            "__builtin_va_start" => {
                expect_args(2, args.len());

                let cur = Self::current().unwrap();

                assert!(cur.vars[cur.args..].iter().find(|var| var.flags.contains(VariableFlags::CHOSEN_AS_VA_LIST)).is_none(), "`...` is already captured into other `va_list`");
                assert_eq!(cur.vars[cur.args - 1].name, expect_variable(args, 1), "provided variable is not last argument of function");

                let va_list = expect_va_list(cur, expect_variable(args, 0));

                assert!(va_list.ty.mutable, "va_list must be mutable");

                va_list.flags.insert(VariableFlags::CHOSEN_AS_VA_LIST);

                none()
            },
            "__builtin_va_end" => {
                expect_args(1, args.len());

                let cur = Self::current().unwrap();

                let va_list = expect_va_list(cur, expect_variable(args, 0));

                assert!(va_list.flags.contains(VariableFlags::CHOSEN_AS_VA_LIST), "cannot finish va_list that is not captured as `...`");
                assert!(!va_list.flags.contains(VariableFlags::FINISHED_VA_LIST), "cannot finish va_list twice");

                va_list.flags.insert(VariableFlags::FINISHED_VA_LIST);

                none()
            },
            "__builtin_va_arg" => {
                expect_args(2, args.len());

                let cur = Self::current().unwrap();

                let va_list = expect_va_list(cur, expect_variable(args, 0));

                assert!(va_list.flags.contains(VariableFlags::CHOSEN_AS_VA_LIST), "cannot finish va_list that is not captured as `...`");
                assert!(!va_list.flags.contains(VariableFlags::FINISHED_VA_LIST), "cannot take arg because va_list is finished");

                let ty = expect_type(args, 1);

                Some(Expr::new(format!("{}.arg::<{}>()", va_list.name, ty.rusty()), ExprType::Ord(ty)))
            },
            _ => None
        }
    }
}
