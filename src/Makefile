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

# $(DIRS):
# 	$(MAKE) -C $@

tags :
	( find . -name "*hs" -print && find . -name "*drift" -print ) \
	| xargs etags

###############################################################################

matchbox : do
	$(GHC_COMMAND) \
		SRS/Matchbox.hs -o SRS/Matchbox

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





