#!/bin/bash
#set -e

cargo build --release

FAIL=0
for arg in "$@"; do
  while read -r file; do
    echo
    echo "$file";
    target/release/lox test "$file" || FAIL=1;
  done < <(find $arg -type f)
done

exit $FAIL
