# $Id$

include Makefile-defs

# for deleting/srs: ##################################################

CGIS = SRS/Deleting/Demo.cgi \
       SRS/Closure/Terminator.cgi 

HTMLS = SRS/Interface/*.html

ifndef CGIDIR
CGIDIR = $(CGIBIN)/matchbox
endif

# for autotool: ########################################################

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

HADDOCK_DIRS = Expression Grammatik Hanoi NFA NPDA PCP Pump RAM \
	Reporter Turing Dot Challenger Inter Boxing Machine Language \
	Boolean \
	SRS TES Baum NFTA Exp Graph GVKnoten Util Scorer \
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

MATCHBOX_DIRS = util Util Reporter Inter \
	GVKnoten Graph Boxing Dot \
	NFA NFTA Exp TES SRS 
MATCHBOX_SRC = $(wildcard Makefile* *.hs) \
	$(shell find $(MATCHBOX_DIRS) -name "*.hs" -print)
MATCHBOX_DOWNLOAD = $(CGIDIR)/download
MATCHBOX_STORE = $(MATCHBOX_DOWNLOAD)/archive
MATCHBOX_ARCHIVE = Matchbox-src-$(shell date -I).tar.bz2
MATCHBOX_CURRENT_ARCHIVE = $(MATCHBOX_DOWNLOAD)/Matchbox-src-current.tar.bz2
MATCHBOX_EXE = Matchbox-exe-$(shell date -I)
MATCHBOX_CURRENT_EXE = $(MATCHBOX_DOWNLOAD)/Matchbox-exe-current


SRS/Config/Build.hs : do
	( echo module SRS.Config.Build where ; \
	  echo date  = \"$(shell date)\" ; \
	  echo uname = \"'$(shell uname -a)'\" ) > $@

SRS/Config/Release.hs : do
	( echo module SRS.Config.Release where ; \
	  echo date  = \"$(shell date)\" ; \
	  echo uname = \"'$(shell uname -a)'\" ) > $@

.PHONY : SRS/Config/Build.hs SRS/Config/Release.hs

matchbox : SRS/Config/Build.hs
	$(GHC_COMMAND) \
		SRS/Matchbox.hs -o SRS/Matchbox

checker : do
	$(GHC_COMMAND) \
		SRS/Checker.hs -o SRS/Checker
self : do
	$(GHC_COMMAND) \
		SRS/Self.hs -o SRS/Self
loop :
	$(GHC_COMMAND)  \
		Exp/Loop.hs -o Exp/Loop

# install source archive and executable
release : SRS/Config/Build.hs SRS/Config/Release.hs
	cvs commit -m release-$(shell date -I)
	$(GHC_COMMAND) SRS/Matchbox.hs -o  $(MATCHBOX_STORE)/$(MATCHBOX_EXE)
	bzip2 -v --force --best  $(MATCHBOX_STORE)/$(MATCHBOX_EXE)
	tar -cvjf $(MATCHBOX_STORE)/$(MATCHBOX_ARCHIVE) \
		-C.. $(patsubst %, autotool/%, $(MATCHBOX_SRC))
	rm -f $(MATCHBOX_CURRENT_EXE).bz2
	ln -s $(MATCHBOX_STORE)/$(MATCHBOX_EXE).bz2 $(MATCHBOX_CURRENT_EXE).bz2
	rm -f $(MATCHBOX_CURRENT_ARCHIVE)
	ln -s $(MATCHBOX_STORE)/$(MATCHBOX_ARCHIVE) $(MATCHBOX_CURRENT_ARCHIVE)

# install CGI programs and copy web pages
install-matchbox : do release
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





