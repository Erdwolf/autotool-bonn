# $Id$

# BINDIR = bin/
# EXERCISEDIRS = ws01/compiled

# DIRS = $(BINDIR) $(EXERCISEDIRS)

DIRS = util:fa:wash:control:challenger:bridge

FLAGS = -fallow-overlapping-instances \
	-fglasgow-exts \
	-fallow-undecidable-instances 

PACKS = -package text -package data \
	-package posix -package network # -package HaXml

OPT = -O2
GHC = ghc

BINS = DB/Server.bin DB/Client.bin DB/CGI.cgi

CGIS = SRS/Deleting/Demo.cgi \
       SRS/Closure/Terminator.cgi 

CGIINDEX = SRS/Interface/index.html

PUB =  $(HOME)/public_html
CGIBIN = $(PUB)/cgi-bin

CGIDIR = $(PUB)/bounded


DRIFT = $(basename $(shell find . -name "*.hs.drift" -print))

###############################################################################

# .PHONY: all $(DIRS)

# all: $(DIRS)

all : cgi


# $(DIRS):
# 	$(MAKE) -C $@

tags :
	find . -name "*.hs" -print | xargs etags

###############################################################################


%.bin : do
	$(GHC) --make $(OPT) $(PACKS) $(FLAGS) -i$(DIRS) $*.hs -o $@


%.cgi : do
	$(GHC) --make $(OPT) $(PACKS) $(FLAGS) -i$(DIRS) $*.hs -o $@

%.hs : %.hs.drift
	DERIVEPATH=  DrIFT $< > $@

drift : $(DRIFT) 

cgi : $(CGIS)
	cp $(CGIS) $(CGIINDEX) $(CGIDIR)

quiz : Graph/Quiz.cgi
	cp $< $(CGIBIN)/

check : Inter/Check.cgi
	cp $< $(CGIBIN)/

.PHONY : do

###############################################################################

.PHONY: objclean clean

forall = $(foreach DIR,$(DIRS),$(MAKE) -C $(DIR) $(1);)

objclean:
	rm -f `find . -name '*.o'`
	rm -f `find . -name '*.hi'`
#	$(call forall,$@)

clean: objclean
#	$(call forall,$@)
