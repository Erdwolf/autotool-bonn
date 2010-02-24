all: protokoll.pdf

.PHONY: all

%.pdf: %.tex again
	pdflatex $<

protokoll.pdf:: typen.tex rpc.tex

clean:
	rm -f *.log *.aux *.toc *.pdf *~

.PHONY: clean

again:

.PHONY: again
