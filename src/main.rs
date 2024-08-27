use std::env;
use std::fs;

pub mod ast;
pub mod errors;
pub mod interpreter;
pub mod lexer;
pub mod parser;

use interpreter::evaluate;
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
            let mut parser = Parser::new(&file_contents);

            while !parser.at_eof() {
                match parser.parse_expr() {
                    Ok(expr) => {
                        let result = evaluate(&expr);

                        match result {
                            Ok(value) => {
                                println!("{}", value);
                            }
                            Err(err) => {
                                eprintln!("{}", err);
                                std::process::exit(70);
                            }
                        }
                    }
                    Err(err) => {
                        eprintln!("{}", err);
                        std::process::exit(65);
                    }
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            std::process::exit(2);
        }
    }
}
