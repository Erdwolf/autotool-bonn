AC_INIT([autotool],[0.0],[],[autotool])

AC_CONFIG_FILES([config.mk:config.mk.in 
    src/Local.hs:src/Local.hs.in 
    src/Util/Datei/Base.hs:src/Util/Datei/Base.hs.in 
    src/Inter/Boiler.hs:src/Inter/Boiler.hs.in])

AC_CANONICAL_SYSTEM

dnl ***********************************************
dnl GHC
dnl ***********************************************
AC_ARG_WITH(ghc,
[  --with-ghc=<ghc command>
        Use a different command instead of 'ghc' for the Haskell compiler.
],
[GHC="$withval"],
[AC_PATH_PROG(GHC,ghc)]
)

if test "$GHC" = "" || test ! -f $GHC; then
  AC_MSG_ERROR([GHC is required to build the package])
fi

AC_SUBST(GHC)


if test "$prefix" = "NONE"; then
  prefix=`$GHC --print-libdir`
fi

dnl ***********************************************
dnl ghc-pkg
dnl ***********************************************
AC_ARG_WITH(ghc-pkg,
[  --with-ghc-pkg=<ghc-pkg command>
        Use a different command instead of 'ghc-pkg'
],
[GHC_PKG="$withval"],
[AC_PATH_PROG(GHC_PKG,ghc-pkg)]
)

if test "$GHC_PKG" = "" || test ! -f $GHC_PKG; then
  AC_MSG_ERROR([ghc-pkg is required to build the package])
fi

AC_SUBST(GHC_PKG)

dnl ***********************************************
dnl AutoDrIFT
dnl ***********************************************
AC_ARG_WITH(AutoDrIFT,
[  --with-AutoDrIFT=<AutoDrIFT command>
        Use a different command instead of 'AutoDrIFT'
],
[AUTODRIFT="$withval"],
[AC_PATH_PROG(AUTODRIFT,AutoDrIFT)]
)

if test "$AUTODRIFT" = "" || test ! -f $AUTODRIFT; then
  AC_MSG_ERROR([AutoDrIFT is required to build the package])
fi

AC_SUBST(AUTODRIFT)

dnl ***********************************************
dnl HADDOCK
dnl ***********************************************
AC_ARG_WITH(haddock,
[  --with-haddock=<haddock command>
        Use a different command instead of 'haddock' for the documentation builder.
],
[HADDOCK="$withval"],
[AC_PATH_PROG(HADDOCK,haddock)]
)

if test "$HADDOCK" = "" || test ! -f $HADDOCK; then
  echo HADDOCK is required to build the documentations
fi

AC_SUBST(HADDOCK)


dnl ***********************************************
dnl HSQL
dnl ***********************************************
AC_ARG_WITH(hsql,
[  --with-hsql=<1.2|1.4|1.6>
        HSQL version
],
[HSQL="$withval"]
)

if test ! "$HSQL" = "1.2" && test ! "$HSQL" = "1.4" && test ! "$HSQL" = "1.6"; then
  AC_MSG_ERROR([only version 1.2, 1.4 and 1.6 of HSQL are supported]);
fi

AC_SUBST(HSQL)


dnl ***********************************************
dnl autotool base directory
dnl ***********************************************
BASE="/space"
AC_ARG_WITH(base,
[  --with-base=<autotool base directory>
        Where autotool stores its working files 
        (must be writable by the CGI program started by the web server)
],
[BASE="$withval"]
)

if test "$BASE" = "" || test ! -d $BASE; then
  AC_MSG_ERROR([need a base directory to build the package])
fi

AC_SUBST(BASE)


dnl ***********************************************
dnl autotool www directory
dnl ***********************************************
WWW="/var/www"
AC_ARG_WITH(www,
[  --with-www=<autotool www directory>
       haddockumentation goes to $www/auto
],
[WWW="$withval"]
)

if test "$WWW" = "" || test ! -d $WWW; then
  AC_MSG_ERROR([need a www directory to build the package])
fi

AC_SUBST(WWW)

dnl ***********************************************
dnl autotool cgi directory
dnl ***********************************************
CGI="/usr/lib"
AC_ARG_WITH(cgi,
[  --with-cgi=<autotool cgi directory>
        CGI programs go to $cgi/cgi-bin
        pictures go to $cgi/pics
],
[CGI="$withval"]
)

if test "$CGI" = "" || test ! -d $CGI; then
  AC_MSG_ERROR([need a cgi directory to build the package])
fi

AC_SUBST(CGI)


dnl ***********************************************
dnl debugging?
dnl ***********************************************
DEBUG="False"
AC_ARG_WITH(debug,
[  --with-debug=<True|False>
],
[DEBUG="$withval"]
)

AC_SUBST(DEBUG)


dnl ***********************************************
dnl other progs
dnl ***********************************************

AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL

AC_PATH_PROG(AR,ar)
AC_SUBST(AR)

AC_PATH_PROG(LD,ld)
AC_SUBST(LD)

dnl ***********************************************
dnl subst
dnl ***********************************************

LL="$LIBS $LDFLAGS"

if test "x$prefix" != xNONE; then
    LIB_DIRS='"'${prefix}'"'
else
    LIB_DIRS='"'${ac_default_prefix}'"'
fi

for lib_opt in ${LL}
do
   case $lib_opt in
	 -l*)
			if test x$DEP_LIBS = x; then
				DEP_LIBS='"'`echo ${lib_opt} | sed s,-l,,`'"'
			else
				DEP_LIBS=$DEP_LIBS,'"'`echo ${lib_opt} | sed s,-l,,`'"'
			fi;;
	 -L*)
			LIB_DIRS=$LIB_DIRS,'"'`echo ${lib_opt} | sed s,-L,, | sed s,"'",, | sed s,"'",,`'"'
   esac
done

AC_SUBST(CPPFLAGS)
AC_SUBST(DEP_LIBS)
AC_SUBST(LIB_DIRS)
AC_SUBST(prefix)
AC_SUBST(exec_prefix)
AC_SUBST(libdir)

AC_OUTPUT
