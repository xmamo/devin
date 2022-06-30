.PHONY: thesis.pdf
thesis.pdf: PhdThesis theorems
	$(MAKE) -C PhdThesis
	$(MAKE) -C theorems
	TEXINPUTS=:PhdThesis:theorems latexmk -pdf thesis.tex

PhdThesis:
	rm -rf PhdThesis
	curl -k 'https://users.dimi.uniud.it/~marco.comini/LaTeX/PhdThesis/PhdThesis.tbz' | tar -xf -

theorems:
	rm -rf theorems
	curl -k 'https://users.dimi.uniud.it/~marco.comini/LaTeX/Theorems/theorems.tbz' | tar -xf -

.PHONY: clean
clean:
	test -f PhdThesis/Makefile && $(MAKE) -C PhdThesis clean
	test -f theorems/Makefile && $(MAKE) -C theorems clean
	latexmk -C
	rm -f *.bbl
	rm -f *.run.xml
