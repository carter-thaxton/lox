#!/bin/bash
set -e

cargo build --release

find test -type f -print0 | while IFS= read -r -d $'\0' file; do
  echo "$file";
  target/release/lox test "$file"
done
