.PHONY: thesis.pdf
thesis.pdf: PhdThesis theorems
	$(MAKE) -C PhdThesis
	$(MAKE) -C theorems
	TEXINPUTS=:PhdThesis:theorems latexmk -pdf thesis.tex

PhdThesis:
	rm -rf PhdThesis
	curl -k 'https://users.dimi.uniud.it/~marco.comini/LaTeX/PhdThesis/PhdThesis.tbz' | tar -xjf -

theorems:
	rm -rf theorems
	curl -k 'https://users.dimi.uniud.it/~marco.comini/LaTeX/Theorems/theorems.tbz' | tar -xjf -

.PHONY: clean
clean:
	if [ -f PhdThesis/Makefile ]; then $(MAKE) -C PhdThesis clean; fi
	if [ -f theorems/Makefile ]; then $(MAKE) -C theorems clean; fi
	latexmk -C
	rm -f *.bbl
	rm -f *.run.xml