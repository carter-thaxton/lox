#!/bin/bash
#set -e

if [[ -z "$NO_BUILD" ]]; then
  cargo build --release
fi

while [[ "$#" -gt 0 ]]; do case $1 in
    --no-pass) no_pass=1; shift 1;;
    --no-fail) no_fail=1; shift 1;;
    --quiet) no_pass=1; no_fail=1; shift 1;;
    *) break;;
  esac;
done

# default arg is test directory
[ $# -eq 0 ] && set -- test

PASS=0
FAIL=0
for arg in "$@"; do
  while read -r file; do
    output="$(target/release/lox test "$file" 2>&1)";

    if [[ "$?" == 0 ]]; then
      if [[ -z "$no_pass" ]]; then
        echo
        echo "$file";
        echo "$output"
      fi
      ((PASS++))
    else
      if [[ -z "$no_fail" ]]; then
        echo
        echo "$file";
        echo "$output"
      fi
      ((FAIL++))
    fi
  done < <(find "$arg" -type f -name '*.lox')
done

echo
echo "$PASS tests passed, $FAIL tests failed."
if (( FAIL > 0 )); then
  exit 1
fi
