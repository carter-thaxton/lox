#!/bin/bash
#set -e

if [[ -z "$NO_BUILD" ]]; then
  cargo build --release
fi

PASS=0
FAIL=0
for arg in "$@"; do
  while read -r file; do
    echo
    echo "$file";
    if target/release/lox test "$file"; then
      ((PASS++))
    else
      ((FAIL++))
    fi
  done < <(find $arg -type f)
done

echo
echo "$PASS tests passed, $FAIL tests failed."
if (( FAIL > 0 )); then
  exit 1
fi
