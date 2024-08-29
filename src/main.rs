use std::borrow::Cow;
use std::env;
use std::fs;

pub mod ast;
pub mod errors;
pub mod interpreter;
pub mod lexer;
pub mod parser;

use interpreter::Interpreter;
use lexer::{Lexer, Token, TokenKind};
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
            let mut parser = Parser::new(&file_contents, false);
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
            // two passes

            // first, find all expected parser and runtime errors
            let (expected_parser_errors, expected_runtime_errors) = get_expected_test_errors(&file_contents);

            // second, run the program in test-mode, and check the errors
            let mut parser = Parser::new(&file_contents, true);
            let result = parser.parse();

            match result {
                Ok(program) => {
                    let mut int = Interpreter::new(true);
                    let result = int.run(&program);
                    if let Err(err) = result {
                        if err.is_test() {
                            // reached an output mismatch or other test error detected at runtime
                            eprintln!("{}", err);
                            std::process::exit(20);
                        } else {
                            // runtime error - check if it was expected
                            let actual = err.to_string();

                            if expected_runtime_errors.is_empty() {
                                eprintln!("FAIL: Unexpected runtime error: {}", err);
                                std::process::exit(20);
                            } else {
                                if expected_runtime_errors.iter().any(|e| *e == actual) {
                                    println!("PASS: expect runtime error: {}", actual);
                                } else {
                                    let expected = &expected_runtime_errors[0];
                                    eprintln!("FAIL: Expected runtime error: {} - got: {}", expected, actual);
                                    std::process::exit(20);
                                }
                            }
                        }
                    }
                }
                Err(err) => {
                    // parser error - check if it was expected
                    let actual = err.to_string();

                    if expected_parser_errors.is_empty() {
                        eprintln!("FAIL: Unexpected parser error: {}", err);
                        std::process::exit(20);
                    } else {
                        if expected_parser_errors.iter().any(|e| *e == actual) {
                            println!("PASS: expected parser error: {}", actual);
                        } else {
                            let expected = &expected_parser_errors[0];
                            eprintln!("FAIL: Expected parser error: {} - got: {}", expected, actual);
                            std::process::exit(20);
                        }
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

pub fn get_expected_test_errors(input: &str) -> (Vec<Cow<str>>, Vec<&str>) {
    let lexer = Lexer::new(input, true);
    let mut parser_errors: Vec<Cow<str>> = vec![];
    let mut runtime_errors: Vec<&str> = vec![];
    for token in lexer {
        match token {
            Ok(Token { kind: TokenKind::ExpectParserError(msg), .. }) => {
                parser_errors.push(msg);
            }
            Ok(Token { kind: TokenKind::ExpectRuntimeError(msg), .. }) => {
                runtime_errors.push(msg);
            }
            _ => {}
        }
    }
    (parser_errors, runtime_errors)
}
