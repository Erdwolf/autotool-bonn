MAIN = main

TARGETS = $(MAIN).8.ps $(MAIN).pdf

all : $(TARGETS)

.PHONY : *.tex

%.dvi : %.tex
	- latex "\\nonstopmode\\input $<"

%.ps : %.dvi
	dvips -t landscape $< -o $@

%.pdf : %.ps
	ps2pdf $<

%.8.ps : %.ps
	psnup -l -d -8 $< $@
