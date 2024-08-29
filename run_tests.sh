#!/bin/bash
#set -e

cargo build --release

for arg in "$@"; do
  find $arg -type f -print0 | while IFS= read -r -d $'\0' file; do
    echo
    echo "$file";
    target/release/lox test "$file"
  done
done
