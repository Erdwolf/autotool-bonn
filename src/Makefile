# $Id$

BINDIR = bin/
EXERCISEDIRS = ws01/compiled

DIRS = $(BINDIR) $(EXERCISEDIRS)

###############################################################################

.PHONY: all $(DIRS)

all: $(DIRS)

$(DIRS):
	$(MAKE) -C $@

tags :
	find . -name "*.hs" -print | xargs etags

###############################################################################

.PHONY: objclean clean

forall = $(foreach DIR,$(DIRS),$(MAKE) -C $(DIR) $(1);)

objclean:
	rm -f `find . -name '*.o'`
	rm -f `find . -name '*.hi'`
	$(call forall,$@)

clean: objclean
	$(call forall,$@)
