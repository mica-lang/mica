#!/usr/bin/env bash

# A really basic test runner.

mica="./target/release/mica"

if [ -z "${MICAFLAGS}" ]; then
   MICAFLAGS=""
fi

failed=0

run-suite() {
   local suite="$1"
   for test in "$suite"/*.mi; do
      printf "* $test... "
      if "$mica" "$test" $MICAFLAGS; then
         echo "PASS"
      else
         failed=1
      fi
   done
}

for suite in tests/*; do
   if [ -d "$suite" ]; then
      echo "== SUITE $suite"
      run-suite "$suite"
   fi
done

if [ "$failed" -eq 1 ]; then
   echo "One or more tests failed. Check output above for details"
   exit 1
fi
