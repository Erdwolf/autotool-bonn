# $Id$

DIRS = util:wash:control:bridge:challenger:ss03/bk/aufgaben:ws03/compilerbau:ws03/aus

FLAGS = -fallow-overlapping-instances \
	-fglasgow-exts \
	-fallow-undecidable-instances 

PACKS = -package text -package data \
	-package posix -package network 

OPT = -O2 -fvia-C -funbox-strict-fields -fnumbers-strict # -fdicts-strict
# PROF = -prof -auto-all -auto-dicts
WARN =  -Werror -dcore-lint

########################################################################
# ugly ugly ugly : system-spezifische teile des makefiles
########################################################################

ifeq ($(shell hostname), nfa)
EXTRA_PACKS = -package hsql
DEFINES = -DSPACE=1
GHC = ghc-6.0
endif

ifeq ($(shell hostname), dfa)
EXTRA_PACKS = -package hsql
DEFINES = -DSPACE=1
GHC = ghc6 # which is 6.0.1, actually
endif

ifeq ($(shell hostname), aaron)
EXTRA_PACKS = -package hsql
DEFINES = -DSPACE=1
GHC = ghc-6.0
endif

ifeq ($(shell hostname), theo1)
EXTRA_PACKS = -package HToolkit
DEFINES = -DHOME=1
GHC = ghc-6.0
endif

########################################################################


GHC_COMMAND = $(GHC) --make \
	$(WARN) $(OPT) $(PROF) $(PACKS) $(FLAGS) $(DEFINES) -i$(DIRS)



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
	( find . -name "*hs" -print && find . -name "*drift" -print ) \
	| xargs etags

###############################################################################

SRS/Matchbox : do
	$(GHC_COMMAND)  $@.hs -o $@

matchbox : do
	$(GHC_COMMAND)  \
		SRS/Closure/Terminator.hs -o $(CGIDIR)/Terminator.cgi
	$(GHC_COMMAND)  \
		SRS/Deleting/Demo.hs -o $(CGIDIR)/Demo.cgi
	cp $(HTMLS) $(CGIDIR)/


%.cgi : do
	$(GHC_COMMAND)  $(EXTRA_PACKS)  $*.hs -o $@



%.hs : %.hs.drift
	DERIVEPATH=$(DP)  $(DrIFT) $< > $@

drift : $(DRIFT) 

drift-clean :
	rm ` find . -name "*.drift" -print | sed s/.drift$$// `

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
