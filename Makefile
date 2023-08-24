PYTHON_VENV := ./venv/bin/python

fix: venv
	$(PYTHON_VENV) -m black aoc/ tests/ && $(PYTHON_VENV) -m ruff aoc/ tests/ --fix

new: venv
	$(PYTHON_VENV) aoc --download $(year) $(day) &
	cp -n aoc/solutions/template.py aoc/solutions/_$(year)/day$(day).py

install: venv
	$(PYTHON_VENV) -m pip install -r requirements.txt -e .

venv:
	python3.11 -m venv venv
	$(PYTHON_VENV) -m pip install -U pip setuptools

.PHONY: new fix install
