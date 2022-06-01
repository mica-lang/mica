#!/usr/bin/env bash

while true; do
   # This magic incantation sends the following three lines to the REPL:
   # "a"
   # "a"
   # Gc.collect
   printf '\"a\"\n\"a\"\nGc.collect\n' | "$MICA" $MICAFLAGS > /dev/null
   if [ $? -ne 0 ]; then
      break
   fi
done
