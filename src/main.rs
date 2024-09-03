use std::borrow::Cow;
use std::env;
use std::fs;

use colored::Colorize;

pub mod ast;
pub mod errors;
pub mod globals;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod resolver;
pub mod runtime;

use interpreter::Interpreter;
use lexer::{Lexer, Token, TokenKind};
use parser::Parser;

const EX_USAGE: i32 = 64; // incorrect command line usage
const EX_DATAERR: i32 = 65; // used for lexer and parser errors
const EX_NOINPUT: i32 = 66; // invalid input file
const EX_SOFTWARE: i32 = 70; // used for runtime errors
const EX_CONFIG: i32 = 78; // used for test errors

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} <command> <filename>\n  command may be one of: tokenize, parse, evaluate, run, test\n", args[0]);
        std::process::exit(EX_USAGE);
    }

    let command = &args[1];
    let filename = &args[2];

    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Failed to read file {}", filename);
        std::process::exit(EX_NOINPUT);
    });

    match command.as_str() {
        "tokenize" => {
            tokenize(&file_contents, false);
        }
        "tokenize_test" => {
            tokenize(&file_contents, true);
        }
        "parse" => {
            parse(&file_contents);
        }
        "evaluate" => {
            evaluate(&file_contents);
        }
        "run" => {
            run(&file_contents);
        }
        "test" => {
            test(&file_contents);
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            std::process::exit(2);
        }
    }
}

fn tokenize(input: &str, enable_test_comments: bool) {
    let lexer = Lexer::new(input, enable_test_comments);

    let mut lexer_error = false;
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
        std::process::exit(EX_DATAERR);
    }
}

fn parse(input: &str) {
    let mut parser = Parser::new(input, false);

    while !parser.at_eof() {
        match parser.parse_expr() {
            Ok(expr) => {
                println!("{}", expr);
            }
            Err(err) => {
                eprintln!("{}", err);
                std::process::exit(EX_DATAERR);
            }
        }
    }
}

fn evaluate(input: &str) {
    let mut parser = Parser::new(input, false);

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
                        std::process::exit(EX_SOFTWARE);
                    }
                }
            }
            Err(err) => {
                eprintln!("{}", err);
                std::process::exit(EX_DATAERR);
            }
        }
    }
}

fn run(input: &str) {
    let mut parser = Parser::new(input, false);
    let result = parser.parse();

    match result {
        Ok(program) => {
            let mut int = Interpreter::new(false);
            let result = int.run(&program);
            if let Err(err) = result {
                eprintln!("{}", err);
                std::process::exit(EX_SOFTWARE);
            }
        }
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(EX_DATAERR);
        }
    }
}

fn test(input: &str) {
    // two passes

    // first, find all expected parser and runtime errors
    let (expected_parser_errors, expected_runtime_errors) = get_expected_test_errors(input);

    // second, run the program in test-mode, and check the errors
    let mut parser = Parser::new(input, true);
    let result = parser.parse();

    match result {
        Ok(program) => {
            if !expected_parser_errors.is_empty() {
                // no parser error, but one was expected
                let expected = &expected_parser_errors[0];
                println!("{}: Expected parser error: {}", "FAIL".red(), expected);
                std::process::exit(EX_CONFIG);
            }

            let mut int = Interpreter::new(true);
            let result = int.run(&program);
            if let Err(err) = result {
                if err.is_test() {
                    // reached an output mismatch or other test error detected at runtime
                    println!("{}", err);
                    std::process::exit(EX_CONFIG);
                } else {
                    // runtime error - check if it was expected
                    let actual = err.to_string();

                    if expected_runtime_errors.is_empty() {
                        println!("{}: Unexpected runtime error: {}", "FAIL".red(), err);
                        std::process::exit(EX_CONFIG);
                    } else {
                        if expected_runtime_errors.iter().any(|e| *e == actual) {
                            println!("{}: expect runtime error: {}", "PASS".green(), actual);
                        } else {
                            let expected = &expected_runtime_errors[0];
                            println!(
                                "{}: Expected runtime error: {} - got: {}",
                                "FAIL".red(),
                                expected,
                                actual
                            );
                            std::process::exit(EX_CONFIG);
                        }
                    }
                }
            }
        }
        Err(err) => {
            // parser error - check if it was expected
            let actual = err.to_string();

            if expected_parser_errors.is_empty() {
                println!("{}: Unexpected parser error: {}", "FAIL".red(), err);
                std::process::exit(EX_CONFIG);
            } else {
                if expected_parser_errors.iter().any(|e| *e == actual) {
                    println!("{}: expect parser error: {}", "PASS".green(), actual);
                } else {
                    let expected = &expected_parser_errors[0];
                    println!(
                        "{}: Expected parser error: {} - got: {}",
                        "FAIL".red(),
                        expected,
                        actual
                    );
                    std::process::exit(EX_CONFIG);
                }
            }
        }
    }
}

fn get_expected_test_errors(input: &str) -> (Vec<Cow<str>>, Vec<&str>) {
    let lexer = Lexer::new(input, true);
    let mut parser_errors: Vec<Cow<str>> = vec![];
    let mut runtime_errors: Vec<&str> = vec![];
    for token in lexer {
        match token {
            Ok(Token {
                kind: TokenKind::ExpectParserError(msg),
                ..
            }) => {
                parser_errors.push(msg);
            }
            Ok(Token {
                kind: TokenKind::ExpectRuntimeError(msg),
                ..
            }) => {
                runtime_errors.push(msg);
            }
            _ => {}
        }
    }
    (parser_errors, runtime_errors)
}
