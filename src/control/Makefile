# $Id$

#DIRS = util

FLAGS = -fallow-overlapping-instances \
	-fglasgow-exts \
	-fallow-undecidable-instances 

PACKS = -package text -package data \
	-package posix -package network -package HaXml

OPT = -O2
GHC = ghc

BINS = HToolkit/testdb.bin

###############################################################################

# .PHONY: all $(DIRS)

# all: $(DIRS)

all : $(BINS)


$(DIRS):
	$(MAKE) -C $@

tags :
	find . -name "*.hs" -print | xargs etags

###############################################################################


%.bin : do
	$(GHC) --make $(OPT) $(PACKS) $(FLAGS) $*.hs -o $@
	#-i$(DIRS) $*.hs -o $@


%.cgi : do
	$(GHC) --make $(OPT) $(PACKS) $(FLAGS) -i$(DIRS) $*.hs -o $@

.PHONY : do

###############################################################################

.PHONY: objclean clean

forall = $(foreach DIR,$(DIRS),$(MAKE) -C $(DIR) $(1);)

objclean:
	rm -f `find . -name '*.o'`
	rm -f `find . -name '*.hi'`
	$(call forall,$@)

clean: objclean
	$(call forall,$@)
