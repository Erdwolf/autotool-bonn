# $Id$

# BINDIR = bin/
# EXERCISEDIRS = ws01/compiled

# DIRS = $(BINDIR) $(EXERCISEDIRS)

# neu: fa/ nicht mehr im pfad (steht jetzt alles in NFA.*)
# desgl. für challenger

DIRS = util:wash:control:bridge:ss03/bk/aufgaben

FLAGS = -fallow-overlapping-instances \
	-fglasgow-exts \
	-fallow-undecidable-instances 


PACKS = -package text -package data \
	-package posix -package network 

HPACKS = $(PACKS) \
	-package HToolkit 


# OPT = -O2 -fvia-C -funbox-strict-fields # -fdicts-strict
PROF = -prof -auto-all -auto-dicts
GHC = ghc-6.0 -pgmc gcc-2.95
# GHC = ghc-5.04 -pgmc gcc-2.95


PUB =  $(HOME)/public_html


# for deleting/srs: ##################################################

CGIS = SRS/Deleting/Demo.cgi \
       SRS/Closure/Terminator.cgi 

HTMLS = SRS/Interface/*.html

CGIDIR = /var/www/matchbox


# for face: ########################################################

CGIBIN = $(PUB)/cgi-bin


# for drift ########################################################

DrIFT = DrIFT-2.0rc3/src/DrIFT
DRIFT = $(basename $(shell find . -name "*.hs.drift" -print))
DP = /usr/local/lib/hugs:$(HOME)/autotool/util

###############################################################################

# .PHONY: all $(DIRS)

# all: $(DIRS)

all : 


# $(DIRS):
# 	$(MAKE) -C $@

tags :
	find . -name "*hs" -print | xargs etags

###############################################################################

SRS/Matchbox : do
	$(GHC) --make $(PROF) $(OPT) $(PACKS) $(FLAGS) -i$(DIRS) $@.hs -o $@

matchbox : do
	$(GHC) --make $(OPT) $(PACKS) $(FLAGS) -i$(DIRS) \
		SRS/Closure/Terminator.hs -o $(CGIDIR)/Terminator.cgi
	$(GHC) --make $(OPT) $(PACKS) $(FLAGS) -i$(DIRS) \
		SRS/Deleting/Demo.hs -o $(CGIDIR)/Demo.cgi
	cp $(HTMLS) $(CGIDIR)/


%.cgi : do
	$(GHC) --make $(OPT) $(HPACKS) $(FLAGS) -i$(DIRS) $*.hs -o $@



%.hs : %.hs.drift
	DERIVEPATH=$(DP)  $(DrIFT) $< > $@

drift : $(DRIFT) 

drift-clean :
	rm -v ` find . -name "*.drift" -print | sed s/.drift$$// `

drift-commit : drift-clean drift
	cvs commit -m drift-commit $(DRIFT)



face : Inter/Face.cgi
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
