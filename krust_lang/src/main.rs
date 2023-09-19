#![deny(clippy::all)]
#![deny(clippy::pedantic)]

use krust::cli_reader::{read_command_line, CLIInfo};
use krust::compiler::{compile, CompilerOutput};
use krust::lexer::{lex, LexerOutput};
use krust::log::{ErrorType, Log, LogType};
use krust::parser::{parse, ParserOutput};
use krust::vm;

fn main() {
    let cli_output: (Option<CLIInfo>, Vec<Log>) = read_command_line();
    for log in cli_output.1 {
        eprintln!("{log}");
    }

    if cli_output.0.is_some() {
        let cli_output: CLIInfo = cli_output.0.expect("checked by if statement");
        let output: (Vec<String>, Vec<Log>) =
            run(cli_output.file_path.as_str(), cli_output.cli_args);
        for string in output.0 {
            println!("{string}");
        }
        for string in output.1 {
            eprintln!("{string}");
        }
    }
}

// Runs the code in the file.
// TODO: Don't save all the printing till the end, instead print when printing should happen.
fn run(file_path: &str, cli_args: [u8; 2]) -> (Vec<String>, Vec<Log>) {
    let lex_output: LexerOutput = lex(file_path);
    let parse_output: ParserOutput = parse(lex_output);
    let compiler_output: CompilerOutput = compile(parse_output, cli_args);
    let mut output: Vec<String> = Vec::new();
    let mut logs: Vec<Log> = Vec::new();

    for log in compiler_output.logs {
        logs.push(log);
    }
    if let Some(bytecode) = compiler_output.bytecode {
        let out_log: (Vec<String>, Vec<Log>) = vm::run(&bytecode);
        output.append(&mut out_log.0.clone());
        for log in out_log.1 {
            logs.push(log);
        }
    } else {
        logs.push(Log {
            log_type: LogType::Error(ErrorType::CantCompile),
            line_and_col: None,
        });
    }

    (output, logs)
}

/// The module for running tests.
#[cfg(test)]
mod tests {
    use super::run;
    use krust::log;
    use krust::vm::math;

    use log::all_to_string;
    use std::fs;

    use proptest::prelude::*;

    // Runs the given code and checks the output against out and err.
    fn test_code(test_name: &str, code: &str, out: &[String], err: &[String]) {
        let file_path: String = format!("tests/{test_name}.txt");
        fs::write(&file_path, code).expect("file will be created if it doesn't exist");
        let out_err = run(
            file_path.as_str(),
            [
                (usize::BITS / 8)
                    .try_into()
                    .expect("length of usize shouldn't be over 1024 bits"),
                1,
            ],
        );
        assert_eq!(out_err.0, out);
        assert_eq!(all_to_string(&out_err.1), err);
    }

    #[test]
    fn above_max_int() {
        test_code(
            "above_max_int",
            format!("{}", 0x8000_0001u32).as_str(),
            &Vec::new(),
            &vec![
                format!(
                    "error (line 1:1): int literal \"{}\" must be at most {}.",
                    0x8000_0001u32, 0x8000_0000u32
                ),
                "error: could not compile due to errors.".to_string(),
            ],
        );
    }

    #[test]
    fn max_int_no_sign() {
        test_code(
            "max_int_no_sign", 
            format!("{}", 0x8000_0000u32).as_str(), 
            &Vec::new(), 
            &vec![
                format!("error (line 1:1): the int literal {} must be preceded by a unary \'-\' operator.", 0x8000_0000u32),
                "error: could not compile due to errors.".to_string()
            ]
        );
    }

    #[test]
    fn max_pos_int() {
        test_code(
            "max_int_pos",
            format!("{}", 0x8000_0000u32 - 1).as_str(),
            &vec![format!("{}", 0x8000_0000u32 - 1)],
            &Vec::new(),
        );
    }

    #[test]
    fn below_min_int() {
        test_code(
            "below_min_int",
            format!("-{}", 0x8000_0001u32).as_str(),
            &Vec::new(),
            &vec![
                format!(
                    "error (line 1:2): int literal \"{}\" must be at most {}.",
                    0x8000_0001u32, 0x8000_0000u32
                ),
                "error: could not compile due to errors.".to_string(),
            ],
        );
    }

    #[test]
    fn min_int() {
        test_code(
            "min_int",
            format!("-{}", 0x8000_0000u32).as_str(),
            &vec![format!("-{}", 0x8000_0000u32)],
            &Vec::new(),
        );
    }

    #[test]
    fn open_left_paren() {
        test_code(
            "open_left_paren",
            "(",
            &Vec::new(),
            &[
                "error (line 1:2): unexpected end of file.".to_string(),
                "error (line 1:2): expected \')\' following \'(\'.".to_string(),
                "error: could not compile due to errors.".to_string(),
            ],
        )
    }

    #[test]
    fn open_right_paren() {
        test_code(
            "open_right_paren",
            ")",
            &Vec::new(),
            &[
                "error (line 1:1): unexpected token.".to_string(),
                "error (line 1:2): unexpected end of file.".to_string(),
                "error: could not compile due to errors.".to_string(),
            ],
        )
    }

    #[test]
    fn empty_parens() {
        test_code(
            "empty_parens",
            "()",
            &Vec::new(),
            &[
                "error (line 1:2): expected expression within parentheses.".to_string(),
                "error: could not compile due to errors.".to_string(),
            ],
        )
    }

    #[test]
    fn bad_type_unary() {
        test_code(
            "bad_type_unary",
            "!1",
            &Vec::new(),
            &[
                "error (line 1:1): the operator \"!\" has no definition over the type \"int\"."
                    .to_string(),
                "error: could not compile due to errors.".to_string(),
            ],
        )
    }

    #[test]
    fn bad_types_binary() {
        test_code(
            "bad_types_binary",
            "1+true",
            &Vec::new(),
            &["error (line 1:2): the operator \"+\" has no definition over the types \"int\" and \"bool\".".to_string(),
                "error: could not compile due to errors.".to_string()],
        )
    }

    proptest! {
        #[test]
        fn random_int(value in proptest::num::i32::ANY)
        {
            test_code(
                "random_int",
                format!("{value}").as_str(),
                &vec![format!("{value}")],
                &Vec::new()
            );
        }

        #[test]
        fn add_ints(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "add_ints",
                format!("{a}+{b}").as_str(),
                &vec![format!("{}", i32::wrapping_add(a, b))],
                &Vec::new()
            );
        }

        #[test]
        fn sub_ints(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "sub_ints",
                format!("{a}-{b}").as_str(),
                &vec![format!("{}", i32::wrapping_sub(a, b))],
                &Vec::new()
            );
        }

        #[test]
        fn mul_ints(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "mul_ints",
                format!("{a}*{b}").as_str(),
                &vec![format!("{}", i32::wrapping_mul(a, b))],
                &Vec::new()
            );
        }

        #[test]
        fn div_ints(
            a in proptest::num::i32::ANY,
            b in proptest::num::i32::ANY.prop_filter
            (
                "Division by zero is invalid",
                |b| *b != 0
            )
        )
        {
            test_code(
                "div_ints",
                format!("{a}/{b}").as_str(),
                &vec![format!("{}", i32::wrapping_div(a, b))],
                &Vec::new()
            );
        }

        #[test]
        fn div_by_zero(a in proptest::num::i32::ANY)
        {
            test_code(
                "div_by_zero",
                format!("{a}/0").as_str(),
                &Vec::new(),
                &vec![format!("error (line 1:{}): division by zero.", format!("{a}").chars().count() + 1)]
            );
        }

        #[test]
        fn mod_ints(
            a in proptest::num::i32::ANY,
            b in proptest::num::i32::ANY.prop_filter
            (
                "Modulo by zero is invalid",
                |b| *b != 0
            )
        )
        {
            test_code(
                "mod_ints",
                format!("{a}%{b}").as_str(),
                &vec![format!("{}", i32::wrapping_rem_euclid(a, b))],
                &Vec::new()
            );
        }

        #[test]
        fn mod_by_zero(a in proptest::num::i32::ANY)
        {
            test_code(
                "mod_by_zero",
                format!("{a}%0").as_str(),
                &Vec::new(),
                &vec![format!("error (line 1:{}): division by zero.", format!("{a}").chars().count() + 1)]
            );
        }

        #[test]
        fn not(a in proptest::bool::ANY)
        {
            test_code(
                "not",
                format!("!{a}").as_str(),
                &vec![format!("{}", !a)],
                &Vec::new()
            );
        }

        #[test]
        fn complement_int(a in proptest::num::i32::ANY)
        {
            test_code(
                "complement_int",
                format!("~{a}").as_str(),
                &vec![format!("{}", !a)],
                &Vec::new()
            );
        }

        #[test]
        fn and_int(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "and_int",
                format!("{a} & {b}").as_str(),
                &vec![format!("{}", a & b)],
                &Vec::new()
            );
        }

        #[test]
        fn and_bool(a in proptest::bool::ANY, b in proptest::bool::ANY)
        {
            test_code(
                "and_bool",
                format!("{a} & {b}").as_str(),
                &vec![format!("{}", a & b)],
                &Vec::new()
            );
        }

        #[test]
        fn xor_int(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "xor_int",
                format!("{a} ^ {b}").as_str(),
                &vec![format!("{}", a ^ b)],
                &Vec::new()
            );
        }

        #[test]
        fn xor_bool(a in proptest::bool::ANY, b in proptest::bool::ANY)
        {
            test_code(
                "xor_bool",
                format!("{a} ^ {b}").as_str(),
                &vec![format!("{}", a ^ b)],
                &Vec::new()
            );
        }

        #[test]
        fn or_int(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "or_int",
                format!("{a} | {b}").as_str(),
                &vec![format!("{}", a | b)],
                &Vec::new()
            );
        }

        #[test]
        fn or_bool(a in proptest::bool::ANY, b in proptest::bool::ANY)
        {
            test_code(
                "or_bool",
                format!("{a} | {b}").as_str(),
                &vec![format!("{}", a | b)],
                &Vec::new()
            );
        }

        #[test]
        fn left_shift_ints(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "left_shift_ints",
                format!("{a}<<{b}").as_str(),
                &vec![format!("{}", math::shift_int(a, b))],
                &Vec::new()
            );
        }

        #[test]
        fn right_shift_ints(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "right_shift_ints",
                format!("{a}>>{b}").as_str(),
                &vec![format!("{}", math::shift_int(a, i32::wrapping_neg(b)))],
                &Vec::new()
            );
        }

        #[test]
        fn equality_ints_eq(a in proptest::num::i32::ANY)
        {
            test_code(
                "equality_ints_eq",
                format!("{a}=={a}").as_str(),
                &["true".to_owned()],
                &Vec::new()
            );
        }

        #[test]
        fn equality_ints_ineq (a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            prop_assume!(a != b);
            test_code(
                "equality_ints_ineq",
                format!("{a}=={b}").as_str(),
                &vec![format!("{}", a == b)],
                &Vec::new()
            );
        }

        #[test]
        fn equality_bools_eq(a in proptest::bool::ANY)
        {
            test_code(
                "equality_bools_eq",
                format!("{a}=={a}").as_str(),
                &["true".to_owned()],
                &Vec::new()
            );
        }

        #[test]
        fn equality_bools_ineq (a in proptest::bool::ANY, b in proptest::bool::ANY)
        {
            prop_assume!(a != b);
            test_code(
                "equality_bools_ineq",
                format!("{a}=={b}").as_str(),
                &vec![format!("{}", a == b)],
                &Vec::new()
            );
        }


        #[test]
        fn inequality_ints_eq(a in proptest::num::i32::ANY)
        {
            test_code(
                "inequality_ints_eq",
                format!("{a}!={a}").as_str(),
                &["false".to_owned()],
                &Vec::new()
            );
        }

        #[test]
        fn inequality_ints_ineq (a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            prop_assume!(a != b);
            test_code(
                "inequality_ints_ineq",
                format!("{a}!={b}").as_str(),
                &vec![format!("{}", a != b)],
                &Vec::new()
            );
        }

        #[test]
        fn inequality_bools_eq(a in proptest::bool::ANY)
        {
            test_code(
                "inequality_bools_eq",
                format!("{a}!={a}").as_str(),
                &["false".to_owned()],
                &Vec::new()
            );
        }

        #[test]
        fn inequality_bools_ineq (a in proptest::bool::ANY, b in proptest::bool::ANY)
        {
            prop_assume!(a != b);
            test_code(
                "inequality_bools_ineq",
                format!("{a}!={b}").as_str(),
                &vec![format!("{}", a != b)],
                &Vec::new()
            );
        }

        #[test]
        fn double_add(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "double_add",
                format!("{a}++{b}").as_str(),
                &Vec::new(),
                &vec![
                    format!("error (line 1:{}): unexpected token.", format!("{a}").chars().count() + 2),
                    "error: could not compile due to errors.".to_string()
                ]
            );
        }

        #[test]
        fn order_of_ops(
            a in proptest::num::i32::ANY,
            b in proptest::num::i32::ANY,
            c in proptest::num::i32::ANY
        )
        {
            test_code(
                "order_of_ops",
                format!("{a}+{b}*{c}").as_str(),
                &vec![format!("{}", i32::wrapping_add(a, i32::wrapping_mul(b, c)))],
                &Vec::new()
            );
        }

        #[test]
        fn bitwise_order_of_ops(
            a in proptest::num::i32::ANY,
            b in proptest::num::i32::ANY,
            c in proptest::num::i32::ANY,
            d in proptest::num::i32::ANY
        )
        {
            test_code(
                "bitwise_order_of_ops",
                format!("{a} | {b} ^ {c} & {d}").as_str(),
                &vec![format!("{}", a | (b ^ (c & d)))],
                &Vec::new()
            );
        }

        #[test]
        fn mixed_order_of_ops(
            a in proptest::num::i32::ANY,
            b in proptest::num::i32::ANY,
            c in proptest::num::i32::ANY
        )
        {
            test_code(
                "mixed_order_of_ops",
                format!("{a} & {b} + {c}").as_str(),
                &vec![format!("{}", a  & i32::wrapping_add(b, c))],
                &Vec::new()
            );
        }

        #[test]
        fn bool_order_of_ops(
            a in proptest::bool::ANY,
            b in proptest::bool::ANY,
            c in proptest::bool::ANY,
            d in proptest::bool::ANY
        )
        {
            test_code(
                "bool_order_of_ops",
                format!("{a} | {b} ^ {c} & {d}").as_str(),
                &vec![format!("{}", a | (b ^ (c & d)))],
                &Vec::new()
            );
        }


        #[test]
        fn paren_test(
            a in proptest::num::i32::ANY,
            b in proptest::num::i32::ANY,
            c in proptest::num::i32::ANY
        )
        {
            test_code(
                "paren_test",
                format!("{a}*({b}+{c})").as_str(),
                &vec![format!("{}", i32::wrapping_mul(a, i32::wrapping_add(b, c)))],
                &Vec::new()
            );
        }
    }
}
