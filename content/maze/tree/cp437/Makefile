all: txt html com

txt:
	python3 cp437.py

html:
	python3 cp437html.py

com:
	nasm -f bin -o cp437.com cp437.asm
	dosbox cp437.com

clean:
	rm -rf *.com
