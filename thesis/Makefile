bachelorthesis.pdf: PhdThesis theorems *.cfg *.tex
	$(MAKE) -C PhdThesis
	$(MAKE) -C theorems
	TEXINPUTS=":PhdThesis:theorems" pdflatex -interaction nonstopmode bachelorthesis.tex

PhdThesis:
	curl 'https://users.dimi.uniud.it/~marco.comini/LaTeX/PhdThesis/PhdThesis.tbz' | tar -xf -

theorems:
	curl 'https://users.dimi.uniud.it/~marco.comini/LaTeX/Theorems/theorems.tbz' | tar -xf -

.PHONY: clean
clean:
	test -f PhdThesis/Makefile && $(MAKE) -C PhdThesis clean
	test -f theorems/Makefile  && $(MAKE) -C theorems clean
	rm -f *.aux *.lof *.log *.lot *.pdf *.toc
