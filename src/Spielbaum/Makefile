MAIN = Main

# pfad zu GraphViz und helpers:
INC = -i..:../util

PACKS = -package data -package text

FLAGS = -fglasgow-exts \
        -fallow-overlapping-instances \
        -fallow-undecidable-instances

GHC = ghc

all : $(MAIN)

$(MAIN): $(wildcard *.hs)
	$(GHC) $(FLAGS) --make $(MAIN) $(INC) $(PACKS) -o $(MAIN)

.PHONY : $(wildcard *.hs)
