# $Id$

include Makefile-defs

# for deleting/srs: ##################################################

CGIS = SRS/Deleting/Demo.cgi \
       SRS/Closure/Terminator.cgi 

HTMLS = SRS/Interface/*.html

CGIDIR = /var/www/matchbox


# for face: ########################################################

PUB =  $(HOME)/public_html
CGIBIN = $(PUB)/cgi-bin


# for drift ########################################################

DrIFT = DrIFT-2.0rc3/src/DrIFT
DRIFT = $(basename $(shell find . -name "*.hs.drift" -print))
DP = /usr/local/lib/hugs:$(HOME)/autotool/util

###############################################################################

# .PHONY: all $(DIRS)

# all: $(DIRS)

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
	rm TAGS && \
	find . \( -name "*.hs" -o -name "*.hs.drift" \) \
	 -print -exec etags -a {} \;

HADDOCK_INPUTS = Expression Grammatik Hanoi NFA NPDA PCP Pump RAM \
	Reporter Turing Dot Challenger Inter Boxing Machine Language \
	SRS Exp Graph GVKnoten \
	ws03 util Util
HADDOCK_OUTPUT = $(PUB)/autotool/haddock
HADDOCK = haddock
HADDOCK_OPTS = -v \
--html \
--source='http://theo1.informatik.uni-leipzig.de/cgi-bin/cvsweb/autotool/' \
--read-interface=http://www.haskell.org/ghc/docs/6.2/html/libraries/base,/usr/local/share/ghc-6.2/html/libraries/base/base.haddock 


haddock : 
	( ls *.hs && find $(HADDOCK_INPUTS) -name "*.hs" -not -name "Datei.hs" -print ) \
	| xargs $(HADDOCK) $(HADDOCK_OPTS) -o $(HADDOCK_OUTPUT)

.PHONY : haddock

###############################################################################

matchbox : do
	$(GHC_COMMAND) \
		SRS/Matchbox.hs -o SRS/Matchbox
checker : do
	$(GHC_COMMAND) \
		SRS/Checker.hs -o SRS/Checker
loop :
	$(GHC_COMMAND)  \
		Exp/Loop.hs -o Exp/Loop

install-matchbox : do
	$(GHC_COMMAND)  \
		SRS/Closure/Terminator.hs -o $(CGIDIR)/Terminator.cgi
	$(GHC_COMMAND)  \
		SRS/Deleting/Demo.hs -o $(CGIDIR)/Demo.cgi
	cp $(HTMLS) $(CGIDIR)/


###############################################################################
# drift
###############################################################################

%.hs : %.hs.drift
	DERIVEPATH=$(DP)  $(DrIFT) $< > $@

drift : $(DRIFT) 

drift-clean :
	rm ` find . -name "*.drift" -print | sed s/.drift$$// `

drift-commit : drift-clean drift
	cvs commit -m drift-commit $(DRIFT)

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





