# must override to get it relativ to this dir

RELATIV_TO_MAIN_DIR=..
DIRS = $(RELATIV_TO_MAIN_DIR)/wash

include ../Makefile-defs

all: autoan.cgi autoadmin.cgi showgruppen.cgi

clean: 
	$(RM) *.hi *.o 
	$(RM) *.cgi


# empty commands, just for the completation

autoan.cgi: %.cgi : %.hs

autoadmin.cgi showgruppen.cgi: %.cgi : %.hs

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



