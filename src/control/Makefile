HC=			ghc
HC_SPEC_FLAGS= -fglasgow-exts		# -cpp -package posix -package data
HCLOADFLAGS=

#OPT=		-fvia-C -O2-for-C 
#OPT=		-O
OPT=

HCFLAGS=	$(OPT) $(HC_SPEC_FLAGS) $(HINCLUDES)

HMAKE=		ghc --make 			# hmake
HMAKEFLAGS=	$(HCFLAGS) $(HINCLUDES)
HINCLUDES=	-i../wash -package"HToolkit" -package"lang"
# HINCLUDES=	-i../wash -i../HToolkit -package lang
HAPPY=		happy
HAPPYFLAGS=	-i
RM=			rm -f

TARGET=		autoan
SRCCMN=		Helper.hs HTMLshortcuts.hs SQLqueries.hs 
SRCAN =		$(SRCCMN) autoan.hs
SRCAD =		$(SRCCMN) autoadmin.hs
SRCSG =		$(SRCCMN) showgruppen.hs

all: autoan.cgi autoadmin.cgi showgruppen.cgi

autoan.cgi: $(SRCAN)
	$(HMAKE) $(HMAKEFLAGS) autoan.hs -o $@

autoadmin.cgi: $(SRCAD)
	$(HMAKE) $(HMAKEFLAGS) autoadmin.hs -o $@

showgruppen.cgi: $(SRCSG)
	$(HMAKE) $(HMAKEFLAGS) showgruppen.hs -o $@

 
#$(TARGET):: $(SRC:.hs=.o)
#	$(HMAKE) $(HMAKEFLAGS) $@.hs -o $@

clean: 
	$(RM) *.hi *.o 
	$(RM) *.cgi


install-autoan.cgi:
	scp autoan.cgi root@theo1:/usr/lib/cgi-bin/autoan.cgi

install-showgruppen.cgi:
	scp showgruppen.cgi root@theo1:/usr/lib/cgi-bin/showgruppen.cgi

ghci-sql: 
	ghci $(HINCLUDES) SQLqueries.hs

#%.o: %.hs
#	$(HMAKE) $(HCFLAGS) -c $< -o $@

#%.hi: %.o
#	@\:

mysql:
	mysql -u test --password="test" -D autoan 
