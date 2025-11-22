EL=emacs -Q --batch
VENV=.venv
SYS_PY=python3
PY=$(VENV)/bin/python3
PIP=$(PY) -m pip

TEST = ./tests

.PHONY: test test-py test-el lint

install:  ensure-venv
	$(PIP) install --upgrade -r requirements.txt

test: | install-test-deps test-py test-el

install-test-deps: ensure-venv
	$(PIP) install --upgrade -r tests/requirements.txt

ensure-venv:
	@[ -d $(VENV) ] || $(SYS_PY) -m venv $(VENV)
	$(PIP) install --upgrade pip
	$(PIP) install --upgrade setuptools
	$(PIP) install --upgrade wheel

run: 
	$(PY) img2orgtable.py ./assets/test.jpg

test-py:
	$(PY) -m pytest -q

test-el:
	$(EL) -L . -l img2orgtable.el -l $(TEST)/img2orgtable-test.el -f ert-run-tests-batch-and-exit

lint:
	$(PY) -m ruff check . --fix
	$(PY) -m black --check .
	$(EL) -Q --batch -l package-lint.el -f package-lint-batch-and-exit img2org.el

