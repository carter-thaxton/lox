use std::env;
use std::fs;
use std::io::{self, Write};

pub mod errors;
pub mod lexer;
pub mod parser;

use lexer::Lexer;
use parser::parse;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut lexer_error = false;
            let lexer = Lexer::new(&file_contents);
            for result in lexer {
                match result {
                    Ok(token) => {
                        println!("{}", token);
                    }
                    Err(err) => {
                        eprintln!("{}", err);
                        lexer_error = true;
                    }
                }
            }
            println!("EOF  null");

            if lexer_error {
                std::process::exit(65);
            }
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let result = parse(Lexer::new(&file_contents));

            match result {
                Ok(ast) => {
                    println!("{}", ast);
                }
                Err(err) => {
                    eprintln!("{}", err);
                    std::process::exit(65);
                }
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}
