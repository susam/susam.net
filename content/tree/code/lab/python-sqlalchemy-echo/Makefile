VENV = /tmp/venv

help:
	@echo 'Usage: make [target]'
	@echo
	@echo 'make venv    Create virtual Python environment.'
	@echo 'make checks  Perform linter and format checks.'
	@echo 'make run     Run this project.'
	@echo 'make txt     Run the project and record output in out.txt.'
	@echo 'make retxt   Reinitialise the project and record output in out.txt.'
	@echo 'make clean   Clean up current working directory.'

venv:
	rm -rf $(VENV)/
	python3 -m venv $(VENV)/
	$(VENV)/bin/pip3 install sqlalchemy ruff mypy

checks:
	$(VENV)/bin/ruff check
	$(VENV)/bin/ruff format --diff
	$(VENV)/bin/mypy .

run:
	rm -f library.db
	$(VENV)/bin/python3 sqlalchemy-echo.py

txt:
	rm -f library.db
	$(VENV)/bin/python3 sqlalchemy-echo.py > tmp.txt
	sed 's/[[:space:]]*$$//' tmp.txt > out.txt
	rm tmp.txt
	cat out.txt
	sqlite3 -echo -table library.db "select * from person" > sql.txt
	sqlite3 -echo -table library.db "select * from book" >> sql.txt
	cat sql.txt

retxt: venv checks txt

clean:
	rm -rf .mypy_cache/ .ruff_cache/
