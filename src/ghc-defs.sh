#!/bin/bash

rm foo.hs* 2>/dev/null
touch foo.hs
ghc -E -optP-dM -cpp foo.hs && cat foo.hspp | awk '{print "-D"$2"="$3}' > ghc.defs
rm foo.hs*

GHC_DEFS=
for i in `cat ghc.defs`; do
  GHC_DEFS=$GHC_DEFS" "$i
done;

rm ghc.defs

echo $GHC_DEFS
