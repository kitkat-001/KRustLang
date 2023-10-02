#![deny(clippy::all)]
#![deny(clippy::pedantic)]
#![allow(clippy::module_name_repetitions)] // Many modules have types of the form "<Name>Output", and if they all were just "Output" it would get confusing.

pub mod cli_reader;
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod vm;

pub mod util;
