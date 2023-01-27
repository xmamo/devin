.PHONY: all
all: thesis.pdf

thesis.pdf: PhdThesis theorems thesis.tex thesis.bib
	TEXINPUTS=:PhdThesis:theorems latexmk -pdf thesis.tex

PhdThesis:
	rm -rf PhdThesis
	wget -O - 'https://users.dimi.uniud.it/~marco.comini/LaTeX/PhdThesis/PhdThesis.tbz' | tar -xjf -
	$(MAKE) -C PhdThesis all

theorems:
	rm -rf theorems
	wget -O - 'https://users.dimi.uniud.it/~marco.comini/LaTeX/Theorems/theorems.tbz' | tar -xjf -
	$(MAKE) -C theorems all

.PHONY: clean
clean:
	if [ -f PhdThesis/Makefile ]; then $(MAKE) -C PhdThesis clean; fi
	if [ -f theorems/Makefile ]; then $(MAKE) -C theorems clean; fi
	latexmk -C
	rm -f *~
	rm -f *.bbl
	rm -f *.run.xml

.PHONY: gnome-latex
gnome-latex: PhdThesis theorems
	TEXINPUTS=:PhdThesis:theorems gnome-latex thesis.tex

.PHONY: latexila
latexila: PhdThesis theorems
	TEXINPUTS=:PhdThesis:theorems latexila thesis.tex
