# $Id$

# BINDIR = bin/
# EXERCISEDIRS = ws01/compiled

# DIRS = $(BINDIR) $(EXERCISEDIRS)

DIRS = util:fa

FLAGS = -fallow-overlapping-instances \
	-fglasgow-exts \
	-fallow-undecidable-instances 

PACKS = -package text -package data \
	-package posix -package network -package HaXml

OPT = -O2
GHC = ghc

BINS = DB/Server.bin DB/Client.bin DB/CGI.cgi

CGIS = SRS/Deleting/Demo.cgi \
       SRS/Bounded/MB.cgi \
       SRS/RFC/RFC.cgi 

CGIINDEX = SRS/Interface/index.html

CGIDIR = $(HOME)/public_html/cgi-bin

# location of DB script
# WWW = $(HOME)/public_html/db

DRIFT = $(basename $(shell find . -name "*.hs.drift" -print))

###############################################################################

# .PHONY: all $(DIRS)

# all: $(DIRS)

all : $(BINS)


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

# www : DB/CGI.cgi
#	cp DB/CGI.cgi DB/index.html $(WWW)/

cgi : $(CGIS)
	cp $(CGIS) $(CGIINDEX) $(CGIDIR)

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
