HC=			ghc
HC_SPEC_FLAGS=	 # -fglasgow-exts		# -cpp -package posix -package data
HCLOADFLAGS=

#OPT=		-fvia-C -O2-for-C 
#OPT=		-O
OPT=

HCFLAGS=	$(OPT) $(HC_SPEC_FLAGS) $(HINCLUDES)

HMAKE=		ghc --make 			# hmake
HMAKEFLAGS=	$(HCFLAGS) $(HINCLUDES)
HINCLUDES=	-i../wash -package"HToolkit" -package"lang"
HAPPY=		happy
HAPPYFLAGS=	-i
RM=			rm -f

TARGET=		autoan
SRCCMN=		Helper.hs HTMLshortcuts.hs SQLqueries.hs Main.hs
SRCAN =		$(SRCCMN) Main.hs
SRCAD =		$(SRCCMN) autoadmin.hs

all: autoan.cgi autoadmin.cgi

autoan.cgi: $(SRCAN)
	$(HMAKE) $(HMAKEFLAGS) Main.hs -o $@

autoadmin.cgi: $(SRCAD)
	$(HMAKE) $(HMAKEFLAGS) autoadmin.hs -o $@

#$(TARGET):: $(SRC:.hs=.o)
#	$(HMAKE) $(HMAKEFLAGS) $@.hs -o $@

clean: 
	$(RM) *.o 
	$(RM) *.hi
	$(RM) $(TARGET)

install-cgi:
	su -c "cp -f /home/alf/autotool/control/autoan.cgi /usr/lib/cgi-bin/autoan.cgi"

ghci-sql: 
	ghci $(HINCLUDES) SQLqueries.hs

#%.o: %.hs
#	$(HMAKE) $(HCFLAGS) -c $< -o $@

#%.hi: %.o
#	@\:
