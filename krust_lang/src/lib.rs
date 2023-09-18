#![deny(clippy::all)]
#![deny(clippy::pedantic)]

pub mod cli_reader;
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod vm;

pub mod log;
