#!/usr/bin/env bash

# A really basic test runner.
# It's simple but capable of executing both Mica scripts and bash scripts, with the latter's
# intention being testing REPL-specific things.
# Bash scripts have to end with the .sh extension, and have the $MICA environment variable set
# to the Mica REPL executable (defined below).

mica="./target/release/mica"

if [ -z "${MICAFLAGS}" ]; then
   MICAFLAGS=""
fi

failed=0

run-test() {
   local filename="$1"
   local testname="$(basename "$filename")"
   printf "* $testname... "
   local status
   if [[ "$filename" == *.mi ]]; then
      "$mica" "$filename" $MICAFLAGS
      status=$?
   elif [[ "$filename" == *.sh ]]; then
      MICA="$mica" MICAFLAGS="$MICAFLAGS" bash "$filename" > /dev/null
      status=$?
   fi
   if [ $status -eq 0 ]; then
      echo "PASS"
   else
      failed=1
   fi
}

run-suite() {
   local suite="$1"
   shopt -s nullglob
   for test in "$suite"/*.{mi,sh}; do
      run-test "$test"
   done
}

for suite in tests/*; do
   if [ -d "$suite" ]; then
      suitename="$(basename "$suite")"
      echo "== SUITE $suitename"
      run-suite "$suite"
   fi
done

if [ "$failed" -ne 0 ]; then
   echo "One or more tests failed. Check output above for details"
   exit 1
fi
