.PHONY: all
all: thesis.pdf

thesis.pdf: PhdThesis theorems PhdThesis.cfg theorems.cfg thesis.bib thesis.tex
	TEXINPUTS=:PhdThesis:theorems latexmk -pdf -silent -Werror thesis.tex

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
	latexmk -bibtex -C
	rm -f *~
	rm -f *.run.xml
