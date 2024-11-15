fix:
	uv run ruff format . && uv run ruff check . --fix

new:
	uv run aoc --download $(year) $(day) &
	cp -n aoc/solutions/template.py aoc/solutions/_$(year)/day$(day).py

install:
	uv sync

test:
	uv run pytest

watch-tests:
	find aoc tests -name *.py | entr -c uv run pytest

.PHONY: new fix install test
