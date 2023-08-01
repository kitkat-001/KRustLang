mod cli_reader;
mod lexer;
mod parser;
mod compiler;
mod vm;

use cli_reader::{CLIOutput, read_command_line};
use lexer::{LexerOutput, lex};
use parser::{ParserOutput, parse};
use compiler::{CompilerOutput, compile};

fn main() {
    let cli_output: CLIOutput = read_command_line();
    match cli_output
    {
        CLIOutput::Error(errors) =>
        {
            for error in errors
            {
                eprintln!("{}", error);
            }
        }
        CLIOutput::CLIInfo { file_path, cli_args} =>
        {
            let output: (Vec<String>, Vec<String>) = run(file_path, cli_args);
            for string in output.0
            {
                println!("{}", string);
            }
            for string in output.1
            {
                println!("{}", string);
            }
        }
    }
}

// Runs the code in the file.
// TODO: Don't save all the printing till the end, instead print when printing should happen.
fn run(file_path: String, cli_args: [u8; 1]) -> (Vec<String>, Vec<String>)
{
    let lex_output: LexerOutput = lex(&file_path);
    let parse_output: ParserOutput = parse(lex_output);
    let compile_ouput: CompilerOutput = compile(parse_output, cli_args);
    let mut output: Vec<String> = Vec::new();
    let mut err: Vec<String> = Vec::new();

    for error in compile_ouput.errors
    {
        err.push(error);
    }
    if let Some(bytecode) = compile_ouput.bytecode
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
    use super::run;

    use std::fs;

    use proptest;
    use proptest::prelude::*;

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
                format!("error (line 1:1): int literal \"{}\" must be at most {}", 0x8000_0001u32, 0x8000_0000u32).to_string(),
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
                format!("error (line 1:1): the int literal {} must be preceded by a unary \'-\' operator", 0x8000_0000u32).to_string(),
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
                format!("error (line 1:2): int literal \"{}\" must be at most {}", 0x8000_0001u32, 0x8000_0000u32).to_string(),
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
            ))
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
    }
}