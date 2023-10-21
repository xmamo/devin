.PHONY: all
all: thesis.pdf

thesis.pdf: PhdThesis theorems PhdThesis.cfg theorems.cfg thesis.bib thesis.tex
	TEXINPUTS=:PhdThesis:theorems latexmk -pdflua -Werror thesis.tex -interaction=nonstopmode

PhdThesis: PhdThesis.tbz
	tar --keep-newer-files -xf PhdThesis.tbz
	$(MAKE) -C PhdThesis all

theorems: theorems.tbz
	tar --keep-newer-files -xf theorems.tbz
	$(MAKE) -C theorems all

.PHONY: PhdThesis.tbz
PhdThesis.tbz:
	wget -N 'http://users.dimi.uniud.it/~marco.comini/LaTeX/PhdThesis/PhdThesis.tbz'

.PHONY: theorems.tbz
theorems.tbz:
	wget -N 'http://users.dimi.uniud.it/~marco.comini/LaTeX/Theorems/theorems.tbz'

.PHONY: clean
clean: mostlyclean
	rm -rf PhdThesis.tbz theorems.tbz PhdThesis theorems

.PHONY: mostlyclean
mostlyclean:
	if [ -f PhdThesis/Makefile ]; then $(MAKE) -C PhdThesis clean; fi
	if [ -f theorems/Makefile ]; then $(MAKE) -C theorems clean; fi
	latexmk -C -pdflua -bibtex
	rm -f *~ *.run.xml pdfa.xmpi
