# $Id$

include ../config.mk
include Makefile-defs

###############################################################################

IPROGS = Super
CPROGS = autoan
CGIS =	$(patsubst %,Inter/%.cgi,$(IPROGS)) 

#	$(patsubst %,control/%.cgi,$(CPROGS))

all : drift $(CGIS)

control : 
	( cd control && make autoan.cgi )

score :
	$(GHC_COMMAND) $(EXTRA_PACKS)  \
		Scorer/Main.hs -o Scorer/ScorerDB

rescore :
	$(GHC_COMMAND) $(EXTRA_PACKS)  \
		Inter/Rescore.hs -o Inter/Rescore

#########################################################################
# tags
#########################################################################

tags :
	rm -f TAGS && \
	find . \( -name "*.hs" -o -name "*.hs.drift" \) \
	 -print -exec etags -a {} \;


#######################################################################
# haddock
# note: HADDOCK will be set by configure in config.mk
#######################################################################

GHC_VERSION = 6.4

GHC_LIBS = base haskell98 parsec unix network readline
GHC_LINK = http://www.haskell.org/ghc/docs/$(GHC_VERSION)/html/libraries
GHC_DESC = /usr/local/share/ghc-$(GHC_VERSION)/html/libraries

HAXML_LINK = http://www.cs.york.ac.uk/fp/HaXml
HAXML_DESC = /usr/local/share/ghc-$(GHC_VERSION)/html/libraries/HaXml

AUTOLIB_LINK = http://141.57.11.163/auto/lib/haddock
AUTOLIB_DESC = /usr/local/share/ghc-$(GHC_VERSION)/html/libraries/Autolib

HADDOCK_OUTPUT = $(WWW)/auto/tool/haddock
HADDOCK_OPTS = -v \
--html \
--title=auto/tool \
--source=http://141.57.11.163/cgi-bin/cvsweb/tool/src \
$(foreach lib,$(GHC_LIBS), \
--read-interface=$(GHC_LINK)/$(lib),$(GHC_DESC)/$(lib)/$(lib).haddock) \
--read-interface=$(HAXML_LINK)/HaXml,$(HAXML_DESC)/HaXml.haddock \
--read-interface=$(AUTOLIB_LINK),$(AUTOLIB_DESC)/Autolib.haddock 

GHC_DEFINES = $(shell ./ghc-defs.sh)

haddock :
	mkdirhier $(HADDOCK_OUTPUT)
	for file in `find . -name "*.hi" -print | sed -e 's/hi$$/hs/'`; \
		do cpp -P -traditional $(GHC_DEFINES) $(HDDEFS) $$file >$$file.uncpp; \
		done
	( find . -name "*.uncpp" -print ) \
	| xargs $(HADDOCK) $(HADDOCK_OPTS) -o $(HADDOCK_OUTPUT)
	-rm `find . -name "*.uncpp" -print`

.PHONY : haddock

###############################################################################
# for drift 
# note: AUTODRIFT will be set by configure in config.mk
###############################################################################

DRIFT = $(basename $(shell find . -name "*.hs.drift" -print))

%.hs : %.hs.drift
	$(AUTODRIFT) $< | grep -v "^{- Generated" > $@

drift : $(DRIFT) 

drift-clean :
	rm -f `find . -name "*.drift" -print | sed s/.drift$$// `


###############################################################################
# cgi Inter/Face
###############################################################################


install : $(CGIS)
	cp $(CGIS) $(CGI)/

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
