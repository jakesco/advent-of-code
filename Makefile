fmt:
	isort aoc/ && black aoc/

get-input:
	[ -n "$(day)" ] && \
	curl "https://adventofcode.com/2022/day/$(day)/input" \
		--header "Cookie: $$(cat .token)" > input.txt

.PHONY: get-input fmt run
