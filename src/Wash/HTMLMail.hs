module Wash.HTMLMail where

import Prelude hiding (head,div,span)
import Wash.HTMLMonad hiding (map)
import Wash.MIME 

htmlDOC :: Element -> DOC
htmlDOC el = 
  binaryDOC "text" "html" (show el)
