# must override to get it relativ to this dir
DIRS = ../wash
EXTRA_PACKS = -package-conf "../autotool.pkg"

include ../Makefile-defs

all: autoan.cgi autoadmin.cgi showgruppen.cgi

clean: 
	$(RM) *.hi *.o 
	$(RM) *.cgi


# empty commands, just for the completation

autoan.cgi: ;

autoadmin.cgi: ;

showgruppen.cgi: ;

#########################################################################
# mysql 
#########################################################################

mysql: 
	bash mysqldb.sh

mysql-init-db: 
	bash mysqldb.sh init

#########################################################################
# ghci shorthands : for testing
#########################################################################

ghci-sql: 
	$(GHCI_COMMAND) $(EXTRA_PACKS) SQLqueries.hs

ghci-schriftl:
	$(GHCI_COMMAND) $(EXTRA_PACKS) Schriftl.hs



