fmt:
	isort aoc/ && black aoc/

get-input:
	[ -n "$(day)" ] && \
	curl "https://adventofcode.com/2022/day/$(day)/input" \
		--header "Cookie: $$(cat .token)" > input.txt

install:
	pip install -r requirements.txt -e .

.PHONY: get-input fmt install
