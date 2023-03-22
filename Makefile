fix:
	black aoc/ && ruff aoc/ --fix

new:
	python aoc --download $(year) $(day) & \
	cp -n aoc/solutions/template.py aoc/solutions/_$(year)/day$(day).py

install:
	pip install -r requirements.txt -e .

.PHONY: new fix install
