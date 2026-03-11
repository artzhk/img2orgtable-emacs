EL=emacs -Q --batch
VENV=.venv
SYS_PY := $(shell for v in python3.13 python3.12 python3.11 python3.10; do \
	  command -v $$v >/dev/null 2>&1 && echo $$v && break; \
	  done)
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
ifndef SYS_PY
	$(error No Python 3.10–3.13 found in PATH)
endif
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

