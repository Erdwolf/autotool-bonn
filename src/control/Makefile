SHELL=tcsh
HC=	       ghc
HC_SPEC_FLAGS= -fglasgow-exts		# -cpp -package posix -package data
HCLOADFLAGS=

#OPT=		-fvia-C -O2-for-C 
#OPT=		-O
OPT=

HCFLAGS=	$(OPT) $(HC_SPEC_FLAGS) $(HINCLUDES)

HMAKE=		ghc --make 	$(HMAKEFLAGS)
#HINCLUDES=	-i..:../wash -package-conf "../autotool.pkg" \
#	-package lang -package posix
HINCLUDES=	-i../wash -package"HToolkit" -package"lang"
HMAKEFLAGS=	$(HCFLAGS) $(HINCLUDES)
HAPPY=		happy
HAPPYFLAGS=	-i
RM=		rm -f

TARGET=		autoan
SRCCMN=		Helper.hs HTMLshortcuts.hs SQLqueries.hs 
SRCAN =		$(SRCCMN) autoan.hs
SRCAD =		$(SRCCMN) autoadmin.hs
SRCSG =		$(SRCCMN) showgruppen.hs

all: autoan.cgi autoadmin.cgi showgruppen.cgi

%.cgi : %.hs
	$(HMAKE) $< -o $@


 
#$(TARGET):: $(SRC:.hs=.o)
#	$(HMAKE) $(HMAKEFLAGS) $@.hs -o $@

clean: 
	$(RM) *.hi *.o 
	$(RM) *.cgi


install-autoan.cgi:
	scp autoan.cgi root@theo1:/usr/lib/cgi-bin/autoan.cgi

install-showgruppen.cgi:
	scp showgruppen.cgi root@theo1:/usr/lib/cgi-bin/showgruppen.cgi


test: 
	cp autoan.cgi ~/public_html/cgi-bin


ghci-sql: 
	ghci $(HINCLUDES) SQLqueries.hs

#%.o: %.hs
#	$(HMAKE) $(HCFLAGS) -c $< -o $@

#%.hi: %.o
#	@\:

mysql: 
	bash mysqldb.sh

mysql-init-db: 
	bash mysqldb.sh init


ghci-schriftl:
	ghci-6.0 $(HINCLUDES) Schriftl.hs

ghci-testdb:
	ghci-6.0 $(HINCLUDES) testdb.hs

punkte.cgi:
	$(HMAKE) $(HMAKEFLAGS) Schriftl.hs -o punkte.cgi

