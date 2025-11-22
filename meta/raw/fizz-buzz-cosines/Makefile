fizz:
	python3 fizz1.py > out.txt
	python3 fizz2.py > out2.txt
	diff -u out.txt out2.txt
	python3 fizz3.py > out2.txt
	diff -u out.txt out2.txt
	python3 fizz4.py > out2.txt
	diff -u out.txt out2.txt
	python3 fizz5.py > out2.txt
	diff -u out.txt out2.txt
	rm -f out2.txt

jazz:
	python3 jazz1.py > out.txt
	python3 jazz2.py > out2.txt
	diff -u out.txt out2.txt
	python3 jazz3.py > out2.txt
	diff -u out.txt out2.txt

deps:
	python3 -m venv ~/.venv/matplotlib
	~/.venv/matplotlib/bin/pip3 install matplotlib
	rm -rf texmf/
	TEXMFHOME=texmf tlmgr init-usertree
	TEXMFHOME=texmf tlmgr --usermode install type1cm cm-super
	sudo tlmgr install dvipng

plot:
	TEXMFHOME="$$PWD/texmf" ~/.venv/matplotlib/bin/python3 plot.py
	cp fizz-buzz-*.png ../../../content/tree/files/blog/
