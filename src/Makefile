# $Id$

include ../config.mk
include Makefile-defs

# for autotool: ########################################################

# TODO: configure

PUB =  $(HOME)/public_html
CGIBIN = $(PUB)/cgi-bin

###############################################################################

all : drift face

control : 
	( cd control && make autoan.cgi )

score :
	$(GHC_COMMAND) $(EXTRA_PACKS)  \
		Scorer/Main.hs -o Scorer/ScorerDB

rescore :
	$(GHC_COMMAND) $(EXTRA_PACKS)  \
		Inter/Rescore.hs -o Inter/Rescore

#########################################################################

tags :
	rm -f TAGS && \
	find . \( -name "*.hs" -o -name "*.hs.drift" \) \
	 -print -exec etags -a {} \;

HADDOCK_DIRS = Expression Grammatik Hanoi NFA NPDA PCP Pump RAM \
	Reporter Turing Dot Challenger Inter Boxing Machine Language \
	Boolean \
	SRS TES Rewriting Baum NFTA Exp Graph GVKnoten Util Scorer \
	ws03 util control 

# control? challenger?

HADDOCK_LIBS = base haskell98 parsec unix network readline
HADDOCK_LINK = http://www.haskell.org/ghc/docs/6.2/html/libraries
HADDOCK_DESC = /usr/local/share/ghc-6.2/html/libraries

HADDOCK_FILES = $(wildcard *.hs)
HADDOCK_OUTPUT = $(PUB)/autotool/haddock
HADDOCK = haddock
HADDOCK_OPTS = -v \
--html \
--source=http://theo1.informatik.uni-leipzig.de/cgi-bin/cvsweb/autotool/ \
$(foreach lib,$(HADDOCK_LIBS), \
--read-interface=$(HADDOCK_LINK)/$(lib),$(HADDOCK_DESC)/$(lib)/$(lib).haddock)



haddock : 
	( ls $(HADDOCK_FILES) && find $(HADDOCK_DIRS) -name "*.hs" -not -name "Datei.hs" -print ) \
	| xargs $(HADDOCK) $(HADDOCK_OPTS) -o $(HADDOCK_OUTPUT)

.PHONY : haddock

###############################################################################
# for drift 
###############################################################################

DRIFT = $(basename $(shell find . -name "*.hs.drift" -print))

%.hs : %.hs.drift
	$(AUTODRIFT) $< > $@

drift : $(DRIFT) 

drift-clean :
	rm -f `find . -name "*.drift" -print | sed s/.drift$$// `


###############################################################################
# cgi Inter/Face
###############################################################################

INTER_FACE_DIR=Inter
INTER_FACE=Face.hs
INTER_FACE_WITH_PATH=$(INTER_FACE_DIR)/$(INTER_FACE)
INTER_FACE_CGI=$(INTER_FACE_DIR)/$(subst .hs,.cgi,$(INTER_FACE))
INTER_FACE_TARGET=root@theo1:/usr/lib/cgi-bin/Face.cgi

face : $(INTER_FACE_CGI) 
	cp $(INTER_FACE_CGI) $(CGIBIN)/

get : Inter/Get.cgi
	cp $< $(CGIBIN)/


install-face : $(INTER_FACE_CGI)
	scp $(INTER_FACE_CGI)  $(INTER_FACE_TARGET)


ghci-face:
	$(GHCI_COMMAND)  $(EXTRA_PACKS)  $(INTER_FACE_WITH_PATH)



.PHONY : do

###############################################################################

.PHONY: objclean clean

forall = $(foreach DIR,$(DIRS),$(MAKE) -C $(DIR) $(1);)

objclean:
	rm -f `find . -name '*.o'`
	rm -f `find . -name '*.hi'`
	rm -f `find . -name '*.cgi'`
#	$(call forall,$@)

clean: objclean
#	$(call forall,$@)





