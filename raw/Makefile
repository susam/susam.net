setup: FORCE
	sudo apt-get update
	sudo apt-get install python3-venv python3-tk

venv: FORCE
	python3 -m venv ~/.venv/susam.in
	echo . ~/.venv/susam.in/bin/activate > venv
	. ./venv && pip install matplotlib

clumsy-equations: clean
	. ./venv && python clumsy-equations.py
	cp *.png ../static/files/blog

channel-capacity: clean
	. ./venv && python channel-capacity.py
	cp *.png ../static/files/blog

view:
	for f in *.png; do xdg-open "$$f"; done

clean:
	rm -f *.png

FORCE:
