#!/bin/sh

curl "https://adventofcode.com/2022/day/${1}/input" \
       --header "Cookie: $(cat .token)"
