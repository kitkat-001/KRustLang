mod cli_reader;
mod lexer;
mod parser;
mod compiler;
mod vm;

mod math;

use cli_reader::{CLIInfo, read_command_line};
use lexer::{LexerOutput, lex};
use parser::{ParserOutput, parse};
use compiler::{CompilerOutput, compile};

fn main() {
    let cli_output: Result<CLIInfo, Vec<String>>  = read_command_line();
    if cli_output.is_err()
    {
        for error in cli_output.err().expect("checked by if statement")
        {
            eprintln!("{}", error);
        }
    }
    else
    {
        let cli_output: CLIInfo = cli_output.ok().expect("checked by if statement");
        let output: (Vec<String>, Vec<String>) = run(
            cli_output.file_path,
            cli_output.cli_args);
        for string in output.0
        {
            println!("{}", string);
        }
        for string in output.1
        {
            eprintln!("{}", string);
        }
    }
}

// Runs the code in the file.
// TODO: Don't save all the printing till the end, instead print when printing should happen.
fn run(file_path: String, cli_args: [u8; 1]) -> (Vec<String>, Vec<String>)
{
    let lex_output: LexerOutput = lex(&file_path);
    let parse_output: ParserOutput = parse(lex_output);
    let compile_output: CompilerOutput = compile(parse_output, cli_args);
    let mut output: Vec<String> = Vec::new();
    let mut err: Vec<String> = Vec::new();

    for error in compile_output.errors
    {
        err.push(error);
    }
    if let Some(bytecode) = compile_output.bytecode
    {
        let out_err: (Vec<String>, Option<String>) = vm::run(&bytecode);
        output.append(&mut out_err.0.clone());
        if let Some(err_value) = out_err.1
        {
            err.push(err_value);
        }
    }
    else 
    {
        err.push("could not compile due to above errors".to_string());
    }

    (output, err)
}

/// The module for running tests.
#[cfg(test)]
mod tests
{
    use crate::math;
    use super::run;

    use std::fs;

    use proptest;
    use proptest::prelude::*;

    // Runs the given code and checks the output against out and err.
    fn test_code(test_name: &str, code: &str, out: Vec<String>, err: Vec<String>)
    {
        let file_path: String = format!("tests/{test_name}.txt");
        fs::write(&file_path, code).expect("file will be created if it doesn't exist");
        let out_err = run(
            file_path,
            [(usize::BITS / 8).try_into().expect("length of usize shouldn't be over 1024 bits")]);
        assert_eq!(out_err.0, out);
        assert_eq!(out_err.1, err);
    }

    #[test]
    fn above_max_value()
    {
        test_code(
            "above_max_value", 
            format!("{}", 0x8000_0001u32).as_str(), 
            Vec::new(), 
            vec![
                format!("error (line 1:1): int literal \"{}\" must be at most {}", 0x8000_0001u32, 0x8000_0000u32),
                "could not compile due to above errors".to_string()
            ]
        );
    }

    #[test]
    fn max_value_no_sign()
    {
        test_code(
            "max_value_no_sign", 
            format!("{}", 0x8000_0000u32).as_str(), 
            Vec::new(), 
            vec![
                format!("error (line 1:1): the int literal {} must be preceded by a unary \'-\' operator", 0x8000_0000u32),
                "could not compile due to above errors".to_string()
            ]
        );
    }

    #[test]
    fn max_pos_value()
    {
        test_code(
            "max_value_pos", 
            format!("{}", 0x8000_0000u32 - 1).as_str(), 
            vec![format!("{}", 0x8000_0000u32 - 1)], 
            Vec::new()
        );
    }

    #[test]
    fn below_min_value()
    {
        test_code(
            "below_min_value", 
            format!("-{}", 0x8000_0001u32).as_str(), 
            Vec::new(), 
            vec![
                format!("error (line 1:2): int literal \"{}\" must be at most {}", 0x8000_0001u32, 0x8000_0000u32),
                "could not compile due to above errors".to_string()
            ]
        );
    }

    #[test]
    fn min_value()
    {
        test_code(
            "min_value", 
            format!("-{}", 0x8000_0000u32).as_str(), 
            vec![format!("-{}", 0x8000_0000u32)], 
            Vec::new()
        );
    }

    #[test]
    fn open_left_paren()
    {
        test_code(
            "open_left_paren",
            "(",
            Vec::new(),
            vec![
                "error (line 1:2): unexpected end of file".to_string(),
                "error (line 1:2): expected \')\' following \'(\'".to_string(),
                "could not compile due to above errors".to_string()
            ]
        )
    }

    #[test]
    fn open_right_paren()
    {
        test_code(
            "open_right_paren",
            ")",
            Vec::new(),
            vec![
                "error (line 1:1): unexpected token".to_string(),
                "error (line 1:2): unexpected end of file".to_string(),
                "could not compile due to above errors".to_string()
            ]
        )
    }

    #[test]
    fn empty_parens()
    {
        test_code(
            "empty_parens",
            "()",
            Vec::new(),
            vec![
                "error (line 1:2): expected expression within parentheses".to_string(),
                "could not compile due to above errors".to_string()
            ]
        )
    }
    
    proptest! {
        #[test]
        fn random_value(value in proptest::num::i32::ANY)
        {
            test_code(
                "random_value", 
                format!("{value}").as_str(), 
                vec![format!("{value}")], 
                Vec::new()
            );
        }

        #[test]
        fn add_values(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "add_values", 
                format!("{a}+{b}").as_str(), 
                vec![format!("{}", i32::wrapping_add(a, b))], 
                Vec::new()
            );
        }

        #[test]
        fn sub_values(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "sub_values", 
                format!("{a}-{b}").as_str(), 
                vec![format!("{}", i32::wrapping_sub(a, b))], 
                Vec::new()
            );
        }

        #[test]
        fn mul_values(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "mul_values", 
                format!("{a}*{b}").as_str(), 
                vec![format!("{}", i32::wrapping_mul(a, b))], 
                Vec::new()
            );
        }

        #[test]
        fn div_values(
            a in proptest::num::i32::ANY, 
            b in proptest::num::i32::ANY.prop_filter
            (
                "Division by zero is invalid", 
                |b| *b != 0
            )
        )
        {
            test_code(
                "div_values", 
                format!("{a}/{b}").as_str(), 
                vec![format!("{}", i32::wrapping_div(a, b))], 
                Vec::new()
            );
        }

        #[test]
        fn div_by_zero(a in proptest::num::i32::ANY)
        {
            test_code(
                "div_by_zero",
                format!("{a}/0").as_str(), 
                Vec::new(),
                vec![format!("error (line 1:{}): division by 0", format!("{a}").chars().count() + 1)]
            );
        }

        #[test]
        fn mod_values(
            a in proptest::num::i32::ANY, 
            b in proptest::num::i32::ANY.prop_filter
            (
                "Modulo by zero is invalid", 
                |b| *b != 0
            )
        )
        {
            test_code(
                "mod_values", 
                format!("{a}%{b}").as_str(), 
                vec![format!("{}", i32::wrapping_rem_euclid(a, b))], 
                Vec::new()
            );
        }

        #[test]
        fn mod_by_zero(a in proptest::num::i32::ANY)
        {
            test_code(
                "mod_by_zero",
                format!("{a}%0").as_str(), 
                Vec::new(),
                vec![format!("error (line 1:{}): modulo by 0", format!("{a}").chars().count() + 1)]
            );
        }

        #[test]
        fn complement_value(a in proptest::num::i32::ANY)
        {
            test_code(
                "complement_value", 
                format!("~{a}").as_str(), 
                vec![format!("{}", !a)], 
                Vec::new()
            );
        }

        #[test]
        fn left_shift_values(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "left_shift_values", 
                format!("{a}<<{b}").as_str(), 
                vec![format!("{}", math::shift_int(a, b))], 
                Vec::new()
            );
        }

        #[test]
        fn right_shift_values(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "right_shift_values", 
                format!("{a}>>{b}").as_str(), 
                vec![format!("{}", math::shift_int(a, i32::wrapping_neg(b)))], 
                Vec::new()
            );
        }

        #[test]
        fn and_values(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "and_values", 
                format!("{a} & {b}").as_str(), 
                vec![format!("{}", a & b)], 
                Vec::new()
            );
        }

        #[test]
        fn xor_values(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "xor_values", 
                format!("{a} ^ {b}").as_str(), 
                vec![format!("{}", a ^ b)], 
                Vec::new()
            );
        }

        #[test]
        fn or_values(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "or_values", 
                format!("{a} | {b}").as_str(), 
                vec![format!("{}", a | b)], 
                Vec::new()
            );
        }

        #[test]
        fn double_add(a in proptest::num::i32::ANY, b in proptest::num::i32::ANY)
        {
            test_code(
                "double_add",
                format!("{a}++{b}").as_str(), 
                Vec::new(),
                vec![
                    format!("error (line 1:{}): unexpected token", format!("{a}").chars().count() + 2),
                    "could not compile due to above errors".to_string()
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
                vec![format!("{}", i32::wrapping_add(a, i32::wrapping_mul(b, c)))],
                Vec::new()
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
                format!("a | b ^ c & d").as_str(),
                vec![format!("{}", a | (b ^ (c & d)))],
                Vec::new()
            );
        }

        #[test]
        fn mixed_order_of_ops(
            a in proptest::num::i32::ANY,
            b in proptest::num::i32::ANY,
            c in proptest::num::i32::ANY,
            d in proptest::num::i32::ANY
        )
        {
            test_code(
                "bitwise_order_of_ops", 
                format!("a | b ^ c & d").as_str(),
                vec![format!("{}", a | (b ^ (c & d)))],
                Vec::new()
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
                vec![format!("{}", i32::wrapping_mul(a, i32::wrapping_add(b, c)))],
                Vec::new()
            );
        }
    }
}