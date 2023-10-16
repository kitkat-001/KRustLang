#![deny(clippy::all)]
#![deny(clippy::pedantic)]

use krust::cli_reader::{read_command_line, CLIInfo};
use krust::compiler::{compile, CompilerOutput};
use krust::lexer::{lex, LexerOutput};
use krust::parser::{parse, ParserOutput};
use krust::util::log::{ErrorType, Log, LogType};
use krust::vm;

use std::fs::read_to_string;

pub enum FileInput {
    FilePath(String),
    FileText(String)
}

impl FileInput {
    fn get_file_text(&self) -> String{
        match self {
            Self::FilePath(path) => read_to_string(path).expect("this method should only be called on FilePath variant if previously checked."),
            Self::FileText(text) => text.clone()
        }
    }
}

fn main() {
    let cli_output: (Option<CLIInfo>, Vec<Log>) = read_command_line();
    for log in cli_output.1 {
        eprintln!("{log}");
    }

    if cli_output.0.is_some() {
        let cli_output: CLIInfo = cli_output.0.expect("checked by if statement");
        run(FileInput::FilePath(cli_output.file_path), cli_output.cli_args);
    }
}

// Runs the code in the file.
// TODO: Print every compiler thing before the program actually runs.
fn run(file_input: FileInput, cli_args: [u8; 2]) -> (Vec<String>, Vec<Log>) {
    let lex_output: LexerOutput = lex(file_input.get_file_text());
    let parse_output: ParserOutput = parse(lex_output);
    let compiler_output: CompilerOutput = compile(parse_output, cli_args);
    let mut output: Vec<String> = Vec::new();
    let mut logs: Vec<Log> = Vec::new();

    for log in compiler_output.logs {
        eprintln!("{log}");
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
        eprintln!("{}", logs.iter().last().expect("list was just pushed to"));
    }

    (output, logs)
}

/// The module for running tests.
#[cfg(test)]
mod tests {
    use super::run;
    use super::FileInput;
    use krust::util::log;
    use krust::vm::test_func::shift_int;

    use log::all_to_string;

    use proptest::prelude::*;

    // Runs the given code and checks the output against out and err.
    fn test_code(code: &str, out: &[String], err: &[String]) {
        let out_err = run(
            FileInput::FileText(code.to_string()),
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
    fn empty() {
        test_code("", &Vec::new(), &Vec::new());
    }

    #[test]
    fn above_max_int() {
        test_code(
            format!("{}", 0x8000_0001u32).as_str(),
            &Vec::new(),
            &[
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
            format!("{}", 0x8000_0000u32).as_str(), 
            &Vec::new(), 
            &[
                format!("error (line 1:1): the int literal {} must be preceded by a unary \'-\' operator.", 0x8000_0000u32),
                "error: could not compile due to errors.".to_string()
            ]
        );
    }

    #[test]
    fn max_pos_int() {
        test_code(
            format!("{}", 0x8000_0000u32 - 1).as_str(),
            &[format!("{}", 0x8000_0000u32 - 1)],
            &Vec::new(),
        );
    }

    #[test]
    fn below_min_int() {
        test_code(
            format!("-{}", 0x8000_0001u32).as_str(),
            &Vec::new(),
            &[
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
            format!("-{}", 0x8000_0000u32).as_str(),
            &[format!("-{}", 0x8000_0000u32)],
            &Vec::new(),
        );
    }

    #[test]
    fn open_left_paren() {
        test_code(
            "(",
            &Vec::new(),
            &[
                "error (line 1:2): unexpected end of file.".to_string(),
                "error (line 1:2): expected \')\' following \'(\'.".to_string(),
                "error: could not compile due to errors.".to_string(),
            ],
        );
    }

    #[test]
    fn open_right_paren() {
        test_code(
            ")",
            &Vec::new(),
            &[
                "error (line 1:1): unexpected token \")\".".to_string(),
                "error (line 1:2): unexpected end of file.".to_string(),
                "error: could not compile due to errors.".to_string(),
            ],
        );
    }

    #[test]
    fn empty_parens() {
        test_code(
            "()",
            &Vec::new(),
            &[
                "error (line 1:2): expected expression within parentheses.".to_string(),
                "error: could not compile due to errors.".to_string(),
            ],
        );
    }

    #[test]
    fn bad_type_unary() {
        test_code(
            "!1",
            &Vec::new(),
            &[
                "error (line 1:1): the operator \"!\" has no definition over the type \"int\"."
                    .to_string(),
                "error: could not compile due to errors.".to_string(),
            ],
        );
    }

    #[test]
    fn bad_types_binary() {
        test_code(
            "1+true",
            &Vec::new(),
            &["error (line 1:2): the operator \"+\" has no definition over the types \"int\" and \"bool\".".to_string(),
                "error: could not compile due to errors.".to_string()],
        );
    }

    #[test]
    fn statements_no_output_test() {
        test_code(
            "true;",
            &Vec::new(),
            &Vec::new(),
        );
    }
    #[test]
    fn statement_and_expression() {
        test_code(
            "true; false",
            &["false".to_string()],
            &Vec::new(),
        );
    }

    #[test]
    fn chained_assignments() {
        test_code(
            format!("int a = int b = 1; a").as_str(),
            &["1".to_string()],
            &Vec::new(),
        );
    }

    #[test]
    fn assignments_as_values() {
        test_code(
            format!("(int a = 6) * a").as_str(),
            &["36".to_string()],
            &Vec::new(),
        );
    }

    #[test]
    fn vars_of_same_name() {
        test_code(
            format!("int var = 1; bool var = true; var").as_str(),
            &["true".to_string()],
            &Vec::new(),
        );
    }

    #[test]
    fn chained_vars_of_same_name() {
        test_code(
            format!("int var = 1; int var = var + 2; var").as_str(),
            &["3".to_string()],
            &Vec::new(),
        );
    }

    #[test]
    fn no_declaration() {
        test_code(
            format!("var = 1;").as_str(),
            &Vec::new(),
            &[
                "error (line 1:1): expected a variable declaration for var".to_string(),
                "error: could not compile due to errors.".to_string(),
            ],
        );
    }

    proptest! {
        #[test]
        fn random_int(value in proptest::num::i32::ANY) {
            test_code(
                format!("{value}").as_str(),
                &[format!("{value}")],
                &Vec::new()
            );
        }

        #[test]
        fn var_int(value in proptest::num::i32::ANY) {
            test_code(
                format!("int var = {value}; var").as_str(),
                &[format!("{value}")],
                &Vec::new()
            );
        }

        #[test]
        fn var_bool(value in proptest::bool::ANY) {
            test_code(
                format!("bool var = {value}; var").as_str(),
                &[format!("{value}")],
                &Vec::new()
            );
        }

        #[test]
        fn int_to_bool(value in proptest::num::i32::ANY) {
            test_code(
                format!("(bool) {value}").as_str(),
                &[format!("{}", value != 0)],
                &Vec::new()
            );
        }

        #[test]
        fn bool_to_int(value in proptest::bool::ANY) {
            test_code(
                format!("(int) {value}").as_str(),
                &[format!("{}", value as i32)],
                &Vec::new()
            );
        }

        #[test]
        fn add_ints(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            test_code(
                format!("{a}+{b}").as_str(),
                &[format!("{}", i32::wrapping_add(a, b))],
                &Vec::new()
            );
        }

        #[test]
        fn sub_ints(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            test_code(
                format!("{a}-{b}").as_str(),
                &[format!("{}", i32::wrapping_sub(a, b))],
                &Vec::new()
            );
        }

        #[test]
        fn mul_ints(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            test_code(
                format!("{a}*{b}").as_str(),
                &[format!("{}", i32::wrapping_mul(a, b))],
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
        ) {
            test_code(
                format!("{a}/{b}").as_str(),
                &[format!("{}", i32::wrapping_div(a, b))],
                &Vec::new()
            );
        }

        #[test]
        fn div_by_zero(a in proptest::num::i32::ANY) {
            test_code(
                format!("{a}/0").as_str(),
                &Vec::new(),
                &[format!("error (line 1:{}): division by zero.", format!("{a}").chars().count() + 1)]
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
        ) {
            test_code(
                format!("{a}%{b}").as_str(),
                &[format!("{}", i32::wrapping_rem_euclid(a, b))],
                &Vec::new()
            );
        }

        #[test]
        fn mod_by_zero(a in proptest::num::i32::ANY) {
            test_code(
                format!("{a}%0").as_str(),
                &Vec::new(),
                &[format!("error (line 1:{}): division by zero.", format!("{a}").chars().count() + 1)]
            );
        }

        #[test]
        fn less_ints_eq(a in proptest::num::i32::ANY) {
            test_code(
                format!("{a}<{a}").as_str(),
                &["false".to_owned()],
                &Vec::new()
            );
        }

        #[test]
        fn less_ints_ineq (a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            prop_assume!(a != b);
            test_code(
                format!("{a}<{b}").as_str(),
                &[format!("{}", a < b)],
                &Vec::new()
            );
        }

        #[test]
        fn less_equal_ints_eq(a in proptest::num::i32::ANY) {
            test_code(
                format!("{a}<={a}").as_str(),
                &["true".to_owned()],
                &Vec::new()
            );
        }

        #[test]
        fn less_equal_ints_ineq (a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            prop_assume!(a != b);
            test_code(
                format!("{a}<={b}").as_str(),
                &[format!("{}", a <= b)],
                &Vec::new()
            );
        }

        #[test]
        fn greater_ints_eq(a in proptest::num::i32::ANY) {
            test_code(
                format!("{a}>{a}").as_str(),
                &["false".to_owned()],
                &Vec::new()
            );
        }

        #[test]
        fn greater_ints_ineq (a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            prop_assume!(a != b);
            test_code(
                format!("{a}>{b}").as_str(),
                &[format!("{}", a > b)],
                &Vec::new()
            );
        }

        #[test]
        fn greater_equal_ints_eq(a in proptest::num::i32::ANY) {
            test_code(
                format!("{a}>={a}").as_str(),
                &["true".to_owned()],
                &Vec::new()
            );
        }

        #[test]
        fn greater_equal_ints_ineq (a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            prop_assume!(a != b);
            test_code(
                format!("{a}>={b}").as_str(),
                &[format!("{}", a >= b)],
                &Vec::new()
            );
        }

        #[test]
        fn not(a in proptest::bool::ANY) {
            test_code(
                format!("!{a}").as_str(),
                &[format!("{}", !a)],
                &Vec::new()
            );
        }

        #[test]
        fn complement_int(a in proptest::num::i32::ANY) {
            test_code(
                format!("~{a}").as_str(),
                &[format!("{}", !a)],
                &Vec::new()
            );
        }

        #[test]
        fn and_int(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            test_code(
                format!("{a} & {b}").as_str(),
                &[format!("{}", a & b)],
                &Vec::new()
            );
        }

        #[test]
        fn and_bool(a in proptest::bool::ANY, b in proptest::bool::ANY) {
            test_code(
                format!("{a} & {b}").as_str(),
                &[format!("{}", a & b)],
                &Vec::new()
            );
        }

        #[test]
        fn xor_int(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            test_code(
                format!("{a} ^ {b}").as_str(),
                &[format!("{}", a ^ b)],
                &Vec::new()
            );
        }

        #[test]
        fn xor_bool(a in proptest::bool::ANY, b in proptest::bool::ANY) {
            test_code(
                format!("{a} ^ {b}").as_str(),
                &[format!("{}", a ^ b)],
                &Vec::new()
            );
        }

        #[test]
        fn or_int(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            test_code(
                format!("{a} | {b}").as_str(),
                &[format!("{}", a | b)],
                &Vec::new()
            );
        }

        #[test]
        fn or_bool(a in proptest::bool::ANY, b in proptest::bool::ANY) {
            test_code(
                format!("{a} | {b}").as_str(),
                &[format!("{}", a | b)],
                &Vec::new()
            );
        }

        #[test]
        fn left_shift_ints(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            test_code(
                format!("{a}<<{b}").as_str(),
                &[format!("{}", shift_int(a, b))],
                &Vec::new()
            );
        }

        #[test]
        fn right_shift_ints(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            test_code(
                format!("{a}>>{b}").as_str(),
                &[format!("{}", shift_int(a, -b))],
                &Vec::new()
            );
        }

        #[test]
        fn equality_ints_eq(a in proptest::num::i32::ANY) {
            test_code(
                format!("{a}=={a}").as_str(),
                &["true".to_owned()],
                &Vec::new()
            );
        }

        #[test]
        fn equality_ints_ineq (a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            prop_assume!(a != b);
            test_code(
                format!("{a}=={b}").as_str(),
                &[format!("{}", a == b)],
                &Vec::new()
            );
        }

        #[test]
        fn equality_bools_eq(a in proptest::bool::ANY) {
            test_code(
                format!("{a}=={a}").as_str(),
                &["true".to_owned()],
                &Vec::new()
            );
        }

        #[test]
        fn equality_bools_ineq (a in proptest::bool::ANY, b in proptest::bool::ANY) {
            prop_assume!(a != b);
            test_code(
                format!("{a}=={b}").as_str(),
                &[format!("{}", a == b)],
                &Vec::new()
            );
        }


        #[test]
        fn inequality_ints_eq(a in proptest::num::i32::ANY) {
            test_code(
                format!("{a}!={a}").as_str(),
                &["false".to_owned()],
                &Vec::new()
            );
        }

        #[test]
        fn inequality_ints_ineq (a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            prop_assume!(a != b);
            test_code(
                format!("{a}!={b}").as_str(),
                &[format!("{}", a != b)],
                &Vec::new()
            );
        }

        #[test]
        fn inequality_bools_eq(a in proptest::bool::ANY) {
            test_code(
                format!("{a}!={a}").as_str(),
                &["false".to_owned()],
                &Vec::new()
            );
        }

        #[test]
        fn inequality_bools_ineq (a in proptest::bool::ANY, b in proptest::bool::ANY) {
            prop_assume!(a != b);
            test_code(
                format!("{a}!={b}").as_str(),
                &[format!("{}", a != b)],
                &Vec::new()
            );
        }

        #[test]
        fn double_add(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY) {
            test_code(
                format!("{a}++{b}").as_str(),
                &Vec::new(),
                &[
                    format!("error (line 1:{}): unexpected token \"+\".", format!("{a}").chars().count() + 2),
                    "error: could not compile due to errors.".to_string()
                ]
            );
        }

        #[test]
        fn order_of_ops(
            a in proptest::num::i32::ANY,
            b in proptest::num::i32::ANY,
            c in proptest::num::i32::ANY
        ) {
            test_code(
                format!("{a}+{b}*{c}").as_str(),
                &[format!("{}", i32::wrapping_add(a, i32::wrapping_mul(b, c)))],
                &Vec::new()
            );
        }

        #[test]
        fn bitwise_order_of_ops(
            a in proptest::num::i32::ANY,
            b in proptest::num::i32::ANY,
            c in proptest::num::i32::ANY,
            d in proptest::num::i32::ANY
        ) {
            test_code(
                format!("{a} | {b} ^ {c} & {d}").as_str(),
                &[format!("{}", a | (b ^ (c & d)))],
                &Vec::new()
            );
        }

        #[test]
        fn mixed_order_of_ops(
            a in proptest::num::i32::ANY,
            b in proptest::num::i32::ANY,
            c in proptest::num::i32::ANY
        ) {
            test_code(
                format!("{a} & {b} + {c}").as_str(),
                &[format!("{}", a  & i32::wrapping_add(b, c))],
                &Vec::new()
            );
        }

        #[test]
        fn bool_order_of_ops(
            a in proptest::bool::ANY,
            b in proptest::bool::ANY,
            c in proptest::bool::ANY,
            d in proptest::bool::ANY
        ) {
            test_code(
                format!("{a} | {b} ^ {c} & {d}").as_str(),
                &[format!("{}", a | (b ^ (c & d)))],
                &Vec::new()
            );
        }


        #[test]
        fn paren_test(
            a in proptest::num::i32::ANY,
            b in proptest::num::i32::ANY,
            c in proptest::num::i32::ANY
        ) {
            test_code(
                format!("{a}*({b}+{c})").as_str(),
                &[format!("{}", i32::wrapping_mul(a, i32::wrapping_add(b, c)))],
                &Vec::new()
            );
        }
    }
}
