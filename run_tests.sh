#!/bin/bash
#set -e

if [[ -z "$NO_BUILD" ]]; then
  cargo build --release
fi

FAIL=0
for arg in "$@"; do
  while read -r file; do
    echo
    echo "$file";
    target/release/lox test "$file" || FAIL=1;
  done < <(find $arg -type f)
done

exit $FAIL
