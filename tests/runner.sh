#!/usr/bin/env bash

# A really basic test runner.

mica="./target/release/mica"

if [ -z "${MICAFLAGS}" ]; then
   MICAFLAGS=""
fi

run-suite() {
   local suite="$1"
   for test in "$suite"/*.mi; do
      printf "* $test... "
      if "$mica" "$test" $MICAFLAGS; then
         echo "PASS"
      fi
   done
}

for suite in tests/*; do
   if [ -d "$suite" ]; then
      echo "== SUITE $suite"
      run-suite "$suite"
   fi
done
