#![deny(clippy::all)]
#![warn(clippy::pedantic)]
#![warn(clippy::restriction)]
#![warn(clippy::nursery)]
#![warn(clippy::cargo)]

pub mod cli_reader;
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod vm;

pub mod log;
