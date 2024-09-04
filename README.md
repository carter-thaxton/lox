
lox
===

This is an implementation of the `lox` programming language, as described in https://craftinginterpreters.com/

This project was intially used to implement the [CodeCrafters - Build Your Own Interpreter](https://app.codecrafters.io/courses/interpreter/overview) challenge.
After completing the challenge, I continued to implement more of the book.

This project also includes a copy of the tests from the [craftinginterpreters book repository](https://github.com/munificent/craftinginterpreters/tree/master/test).

Usage
-----

This project tries to match the CLI of the CodeCrafters challenge, and remain backward-compatible, even though it now implements most of the rest of the language.

`cargo build`, followed by `target/debug/lox <command> <filename>`, or simply `cargo run <command> <filename>`.

or

`cargo build --release`, followed by `target/release/lox <command> <filename>`.

command may be one of: `tokenize`, `tokenize_test`, `parse`, `evaluate`, `run`, `test`.

Only `run` and `test` are really useful.  The others were used by the CodeCrafters challenge.


Testing
-------
This project has a built-in test runner.  It can recognize the specially formatted test comments used by the book's test suite.

Run tests with `./run_tests.sh [--no-pass] [--no-fail] [--quiet] <path>`.

e.g. to run the entire test suite, use:
`./run_tests.sh test`

or to just run one directory's tests, and only show failures:
`./run_tests.sh --no-pass test/custom`
