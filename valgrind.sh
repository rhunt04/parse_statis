#!/bin/sh

# RJH: Wrapper to valgrind call.

valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose \
         --log-file=valgrind-out.txt \
         ./build*/bin/parse_statis -l E,T -i 20 -o P_TEST
