setup: FORCE
	sudo apt-get update
	sudo apt-get install python3-venv python3-tk

venv: FORCE
	python3 -m venv ~/.venv/susam.net
	echo . ~/.venv/susam.net/bin/activate > venv
	. ./venv && pip install matplotlib

clumsy-equations: clean
	. ./venv && python clumsy-equations.py
	cp clumsy-equations-1.png ../content/tree/files/blog/
	cp clumsy-equations-2.png ../content/tree/files/blog/
	cp clumsy-equations-3.png ../content/tree/files/blog/

channel-capacity: clean
	. ./venv && python channel-capacity.py
	cp channel-capacity.png ../content/tree/files/blog/

dosbox-logo: clean
	# PNGs
	convert logo_000.png -sample '640x480!' dosbox-logo-0.png
	convert logo_001.png -sample '640x480!' dosbox-logo-1.png
	# MP4
	ffmpeg -i logo_000.avi -an -c:v libx264 -crf 17 -preset veryslow \
	       -pix_fmt yuv420p -vf scale=640:480:flags=neighbor \
	       dosbox-logo-circles.mp4
	# GIF
	ffmpeg -i logo_000.avi -vf palettegen palette.png
	ffmpeg -i logo_000.avi -i palette.png \
	       -lavfi 'fps=30,scale=640:480:flags=neighbor,paletteuse' \
	       dosbox-logo-circles.gif
	# Copy
	cp dosbox-logo-0.png ../content/tree/files/blog/
	cp dosbox-logo-1.png ../content/tree/files/blog/
	cp dosbox-logo-circles.mp4 ../content/tree/files/blog/
	cp dosbox-logo-circles.gif ../content/tree/files/blog/

fd-100-logo: clean
	convert logo_001.png -sample '1920x1500!' logo-circles.png
	convert logo_002.png -sample '1920x1500!' logo-line.png
	convert logo_003.png -sample '1920x1500!' logo-square.png
	convert logo_004.png -sample '1920x1500!' logo-pentagon.png
	convert logo_005.png -sample '1920x1500!' logo-star.png
	convert logo_006.png -sample '1920x1500!' logo-square-centre.png
	convert logo_007.png -sample '1920x1500!' logo-pentagon-centre.png
	convert logo_008.png -sample '1920x1500!' logo-star-centre.png
	convert logo_009.png -sample '1920x1500!' logo-circle.png
	convert logo_010.png -sample '1920x1500!' logo-circle-centre.png
	cp logo-circles.png ../content/tree/files/blog/
	cp logo-line.png ../content/tree/files/blog/
	cp logo-square.png ../content/tree/files/blog/
	cp logo-pentagon.png ../content/tree/files/blog/
	cp logo-star.png ../content/tree/files/blog/
	cp logo-square-centre.png ../content/tree/files/blog/
	cp logo-pentagon-centre.png ../content/tree/files/blog/
	cp logo-star-centre.png ../content/tree/files/blog/
	cp logo-circle.png ../content/tree/files/blog/
	cp logo-circle-centre.png ../content/tree/files/blog/

gwbasic-ega-color: clean
	magick gwbasic_000.png -sample '1920x1440!' gwbasic-color.png
	cp gwbasic-color.png ../content/tree/files/blog/

debug-ega-color: clean
	magick debug_000.png  -sample '1920x1440!' debug-color.png
	cp debug-color.png ../content/tree/files/blog/

view:
	for f in *.png; do xdg-open "$$f"; done

clean:
	# Clumsy Equations
	rm -f clumsy-equations-1.png
	rm -f clumsy-equations-2.png
	rm -f clumsy-equations-3.png
	# When Worse is Better
	rm -f channel-capacity.png
	# Good Quality DOSBox Video Capture
	rm -f dosbox-logo-0.png
	rm -f dosbox-logo-1.png
	rm -f dosbox-logo-circles.mp4
	rm -f dosbox-logo-circles.gif
	rm -f palette.png
	rm -f gwbasic-color.png

FORCE:
