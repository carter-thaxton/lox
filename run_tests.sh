#!/bin/bash
#set -e

cargo build --release

for dir in "$@"; do
  find test/$dir -type f -print0 | while IFS= read -r -d $'\0' file; do
    echo
    echo "$file";
    target/release/lox test "$file"
  done
done
