use std::env;
use std::fs;

pub mod ast;
pub mod errors;
pub mod interpreter;
pub mod lexer;
pub mod parser;

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} <command> <filename>\n  command may be one of: tokenize, parse, evaluate, run, test\n", args[0]);
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
            let lexer = Lexer::new(&file_contents, false);
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
            let mut parser = Parser::new(&file_contents, false);

            while !parser.at_eof() {
                match parser.parse_expr() {
                    Ok(expr) => {
                        println!("{}", expr);
                    }
                    Err(err) => {
                        eprintln!("{}", err);
                        std::process::exit(65);
                    }
                }
            }
        }
        "evaluate" => {
            let mut parser = Parser::new(&file_contents, false);

            while !parser.at_eof() {
                match parser.parse_expr() {
                    Ok(expr) => {
                        let mut int = Interpreter::new(false);
                        let result = int.evaluate(&expr);

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
        "run" => {
            let parser = Parser::new(&file_contents, false);
            let result = parser.parse();

            match result {
                Ok(program) => {
                    let mut int = Interpreter::new(false);
                    let result = int.run(&program);
                    if let Err(err) = result {
                        eprintln!("{}", err);
                        std::process::exit(70);
                    }
                }
                Err(err) => {
                    eprintln!("{}", err);
                    std::process::exit(65);
                }
            }
        }
        "test" => {
            let parser = Parser::new(&file_contents, true);
            let result = parser.parse();

            match result {
                Ok(program) => {
                    let mut int = Interpreter::new(true);
                    let result = int.run(&program);
                    if let Err(err) = result {
                        if err.is_test() {
                            eprintln!("{}", err);
                            std::process::exit(20);
                        } else {
                            todo!("Look for runtime error in upcoming statements");
                            // eprintln!("{}", err);
                            // std::process::exit(70);
                        }
                    }
                }
                Err(_err) => {
                    todo!("Look for parser error in upcoming tokens");
                    // eprintln!("{}", err);
                    // std::process::exit(65);
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            std::process::exit(2);
        }
    }
}
