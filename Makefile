VENV_PYTHON := ./venv/bin/python

fix: venv
	$(VENV_PYTHON) -m black aoc/ tests/ && $(VENV_PYTHON) -m ruff aoc/ tests/ --fix

new: venv
	$(VENV_PYTHON) aoc --download $(year) $(day) &
	cp -n aoc/solutions/template.py aoc/solutions/_$(year)/day$(day).py

install: venv
	$(VENV_PYTHON) -m pip install -r requirements.txt -e .

venv:
	python3.11 -m venv venv
	$(VENV_PYTHON) -m pip install -U pip setuptools

.PHONY: new fix install
