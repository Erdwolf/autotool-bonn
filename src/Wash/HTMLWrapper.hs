module Wash.HTMLWrapper (module Wash.HTMLMonad, module Wash.HTMLWrapper) where

-- with modifications by Johannes Waldmann joe@informatik.uni-leipzig.de
--   $Id$
import Prelude hiding ( span, div, head, map )

import Wash.HTMLMonad

standardPage ttl elems =
  html (Wash.HTMLMonad.head (title (text ttl))
     ## body (h1 (text ttl) ## elems))

