rm -f *.svg build-rcs.png
racket -l- plt-build-plot/plot --svg ++color red ++color blue ++color red ++color blue ++color red --only-major build-r.txt build-cs.txt
