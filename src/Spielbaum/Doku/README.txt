-------------------------------------------------------------------------
-- Trouble installing hugs ???
-------------------------------------------------------------------------

1. READLINE:  Hugs works like any shell - with <ARROW-UP>-Key you can
   use latest commands again.
   If you want to use this feature in hugs you have to configure this:
   "./configure --with-readline"
   If this doesn't work install the latest readline-package. This
   should be available at http://www.rpmfind.net/

2. DOT / NEATO / GRAPHVIZ:  In case it doesn't work ("dot" or "neato" 
   are unknown commands) - install it first. It's a package called
   GRAPHVIZ (at least in suse-distribution - otherwise "rpmfind.net")

3. TRUETYPEFONTS: there's a command called  fetchmstfonts

-------------------------------------------------------------------------
-- Trouble starting hugs ???
-------------------------------------------------------------------------

4. SPIELBAUM: There are quite a few "imports". To avoid listing all
   the possible paths, use directory "autotool" and start hugs with:
   "hugs -98 +o -Putil: Spielbaum/Spielbaum.hs" (MAIN??)
