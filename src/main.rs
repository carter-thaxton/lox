use std::env;
use std::fs;

pub mod errors;
pub mod lexer;
pub mod parser;

use lexer::Lexer;
use parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        std::process::exit(2);
    }

    let command = &args[1];
    let filename = &args[2];

    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Failed to read file {}", filename);
        String::new()
    });

    match command.as_str() {
        "tokenize" => {
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
            let parser = Parser::new(&file_contents);

            match parser.parse() {
                Ok(ast) => {
                    println!("{}", ast);
                }
                Err(err) => {
                    eprintln!("{}", err);
                    std::process::exit(65);
                }
            }
        }
        "evaluate" => {
            let parser = Parser::new(&file_contents);

            match parser.parse() {
                Ok(ast) => {
                    // TODO: evaluate AST
                    println!("{}", ast);
                }
                Err(err) => {
                    eprintln!("{}", err);
                    std::process::exit(65);
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            std::process::exit(2);
        }
    }
}
